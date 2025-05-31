# ==============================================================================
# INTEGRATED FIRE SPREAD AND WIND ANALYSIS PACKAGE - FIXED VERSION
# ==============================================================================
# This package combines:
# 1. Wind direction analysis and variability calculation
# 2. Fire spread modeling with corrected opening angles
# 3. Interactive Shiny application for visualization

# Install required packages
required_packages <- c("shiny", "leaflet", "sf", "osmdata", "dplyr", "readr", 
                       "shinyWidgets", "geosphere", "DT", "plotly", "shinydashboard")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(osmdata)
library(dplyr)
library(readr)
library(shinyWidgets)
library(geosphere)
library(DT)
library(plotly)
library(shinydashboard)

# ==============================================================================
# WIND ANALYSIS FUNCTIONS - FIXED
# ==============================================================================

# Function to process wind data and calculate direction variability
process_wind_data <- function(csv_file_path) {
  tryCatch({
    # Load the dataset with more flexible reading
    df <- read.csv(csv_file_path, stringsAsFactors = FALSE, na.strings = c("", "NA", "NoData", "OffScan", "Samp<"))
    
    # Print column names for debugging
    cat("Column names found:", paste(colnames(df), collapse = ", "), "\n")
    
    # Check if WDD column exists
    if (!"WDD" %in% colnames(df)) {
      return(list(error = "WDD column not found. Available columns: " + paste(colnames(df), collapse = ", ")))
    }
    
    # Handle the WDD column - remove any non-numeric characters and convert
    df$WDD_clean <- as.character(df$WDD)
    
    # Replace problematic values with NA
    df$WDD_clean[df$WDD_clean %in% c("NoData", "OffScan", "Samp<", "", " ")] <- NA
    df$WDD_clean[is.na(df$WDD_clean)] <- NA
    
    # Handle commas and convert to numeric degrees
    df$WDD_clean <- gsub(",", ".", df$WDD_clean)
    df$WDD_degrees <- suppressWarnings(as.numeric(df$WDD_clean))
    
    # Remove invalid values (outside 0-360 range)
    df$WDD_degrees[df$WDD_degrees < 0 | df$WDD_degrees > 360] <- NA
    
    # Define function to convert degrees to 16 compass directions
    wind_direction_from_degrees <- function(degrees) {
      if (is.na(degrees)) return(NA)
      directions <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE',
                      'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')
      ix <- round((degrees + 11.25) / 22.5) %% 16
      if (ix == 0) ix <- 16
      return(directions[ix])
    }
    
    # Apply function to get compass direction
    df$WDD_compass <- sapply(df$WDD_degrees, wind_direction_from_degrees)
    
    # Handle date conversion more flexibly
    if ("Date" %in% colnames(df)) {
      # Try different date formats
      df$Date_clean <- as.character(df$Date)
      
      # Try DD/MM/YYYY format first
      df$Date_parsed <- as.Date(df$Date_clean, format = "%d/%m/%Y")
      
      # If that fails, try other common formats
      if (all(is.na(df$Date_parsed))) {
        df$Date_parsed <- as.Date(df$Date_clean, format = "%Y-%m-%d")
      }
      if (all(is.na(df$Date_parsed))) {
        df$Date_parsed <- as.Date(df$Date_clean, format = "%m/%d/%Y")
      }
      
      df$Date <- df$Date_parsed
    } else {
      return(list(error = "Date column not found. Available columns: " + paste(colnames(df), collapse = ", ")))
    }
    
    # Remove rows with invalid dates or no wind direction data
    df <- df[!is.na(df$Date) & !is.na(df$WDD_compass), ]
    
    if (nrow(df) == 0) {
      return(list(error = "No valid data rows found after cleaning. Check date format and WDD values."))
    }
    
    # Calculate circular mean for wind direction
    circular_mean <- function(degrees_vector) {
      degrees_vector <- degrees_vector[!is.na(degrees_vector)]
      if (length(degrees_vector) == 0) return(NA)
      
      radians <- degrees_vector * pi / 180
      sin_sum <- sum(sin(radians))
      cos_sum <- sum(cos(radians))  # Fixed: was using radians instead of cos(radians)
      mean_angle <- atan2(sin_sum, cos_sum)
      mean_degrees <- mean_angle * 180 / pi
      mean_degrees <- (mean_degrees + 360) %% 360
      return(mean_degrees)
    }
    
    # Group by date and calculate dominant and average directions
    daily_summary <- df %>%
      group_by(Date) %>%
      summarise(
        Dominant_Wind_Direction_Compass = {
          compass_table <- table(WDD_compass)
          if (length(compass_table) > 0) {
            names(sort(compass_table, decreasing = TRUE))[1]
          } else {
            NA
          }
        },
        Average_Wind_Direction_Degrees = circular_mean(WDD_degrees),
        Valid_Observations = sum(!is.na(WDD_compass)),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Dominant_Wind_Direction_Compass) & Valid_Observations > 0)
    
    if (nrow(daily_summary) == 0) {
      return(list(error = "No valid daily summaries could be calculated."))
    }
    
    # Compass to degrees mapping
    compass_to_degrees <- c(
      'N' = 0, 'NNE' = 22.5, 'NE' = 45, 'ENE' = 67.5,
      'E' = 90, 'ESE' = 112.5, 'SE' = 135, 'SSE' = 157.5,
      'S' = 180, 'SSW' = 202.5, 'SW' = 225, 'WSW' = 247.5,
      'W' = 270, 'WNW' = 292.5, 'NW' = 315, 'NNW' = 337.5
    )
    
    # Calculate wind direction variability
    daily_summary$Dominant_Wind_Direction_Degrees <- compass_to_degrees[daily_summary$Dominant_Wind_Direction_Compass]
    
    # Calculate angular difference (accounting for circular nature)
    angular_diff <- function(angle1, angle2) {
      if (is.na(angle1) || is.na(angle2)) return(NA)
      diff <- abs(angle1 - angle2)
      return(pmin(diff, 360 - diff))
    }
    
    daily_summary$Wind_Direction_Variability <- mapply(angular_diff,
                                                       daily_summary$Dominant_Wind_Direction_Degrees,
                                                       daily_summary$Average_Wind_Direction_Degrees
    )
    
    # Calculate average variability by wind direction
    variability_by_direction <- daily_summary %>%
      group_by(Dominant_Wind_Direction_Compass) %>%
      summarise(
        Average_Variability = mean(Wind_Direction_Variability, na.rm = TRUE),
        Count = n(),
        Total_Observations = sum(Valid_Observations, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Average_Variability)
    
    cat("Successfully processed", nrow(daily_summary), "days of wind data\n")
    cat("Wind directions found:", paste(unique(daily_summary$Dominant_Wind_Direction_Compass), collapse = ", "), "\n")
    
    return(list(
      daily_data = daily_summary,
      variability_summary = variability_by_direction,
      raw_data_rows = nrow(df)
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error processing wind data:", e$message)))
  })
}

# ==============================================================================
# FIRE SPREAD FUNCTIONS (CORRECTED WITH ADDITIVE φ ANGLE)
# ==============================================================================

# Function to create a wedge-shaped polygon
create_wedge <- function(center_lon, center_lat, direction, opening_angle, length_km, n_points = 50) {
  angles <- seq(direction - opening_angle / 2, direction + opening_angle / 2, length.out = n_points)
  arc_points <- geosphere::destPoint(p = c(center_lon, center_lat), b = angles, d = length_km * 1000)
  coords <- rbind(c(center_lon, center_lat), arc_points, c(center_lon, center_lat))
  
  wedge_polygon <- st_polygon(list(coords))
  wedge_sfc <- st_sfc(wedge_polygon, crs = 4326)
  wedge_sf <- st_sf(
    id = 1, 
    type = "wedge",
    direction = direction,
    opening_angle = opening_angle,
    length_km = length_km,
    area_km2 = round(as.numeric(st_area(wedge_sfc)) / 1e6, 4),
    geometry = wedge_sfc
  )
  return(wedge_sf)
}

# Function to get roads within the wedge area
get_roads_in_wedge <- function(wedge_geometry) {
  tryCatch({
    buffered_wedge <- st_transform(wedge_geometry, 3857) %>%
      st_buffer(300) %>%
      st_transform(4326)
    expanded_bbox <- st_bbox(buffered_wedge)
    
    opq_bbox <- opq(bbox = expanded_bbox)
    roads_osm <- opq_bbox %>%
      add_osm_feature(key = "highway") %>%
      osmdata_sf()
    
    roads_sf <- roads_osm$osm_lines
    
    if (is.null(roads_sf) || nrow(roads_sf) == 0) {
      return(st_sf(geometry = st_sfc(), crs = st_crs(wedge_geometry)))
    }
    
    roads_sf <- st_transform(roads_sf, st_crs(wedge_geometry))
    roads_sf <- st_intersection(roads_sf, wedge_geometry)
    roads_sf$length_km <- as.numeric(st_length(roads_sf)) / 1000
    roads_sf$name_clean <- ifelse(!is.na(roads_sf$name), roads_sf$name, paste("Unnamed", roads_sf$highway))
    
    return(roads_sf)
  }, error = function(e) {
    return(st_sf(geometry = st_sfc(), crs = 4326))
  })
}

# Convert wind direction to degrees (wind coming FROM direction)
wind_direction_to_degrees <- function(direction) {
  # Original directions represent where wind is coming FROM
  # Fire spreads in the opposite direction (where wind is going TO)
  directions <- c("N" = 180, "NNE" = 202.5, "NE" = 225, "ENE" = 247.5,
                  "E" = 270, "ESE" = 292.5, "SE" = 315, "SSE" = 337.5,
                  "S" = 0, "SSW" = 22.5, "SW" = 45, "WSW" = 67.5,
                  "W" = 90, "WNW" = 112.5, "NW" = 135, "NNW" = 157.5)
  return(directions[direction])
}

# ENHANCED: Opening angle calculation with additive φ angle
calculate_opening_angle_with_variability <- function(wind_speed_kmh, fuel_type = "grasslands", 
                                                     wind_direction = "E", variability_data = NULL) {
  wind_speed_ms <- wind_speed_kmh / 3.6
  
  # Base L/W ratio calculation
  base_ratios <- list(
    "grasslands" = 1.5,
    "chaparral" = 1.2,
    "pine_litter" = 1.0
  )
  
  base_ratio <- base_ratios[[fuel_type]]
  
  # Calculate base L/W ratio
  if (wind_speed_ms <= 1.0) {
    lw_ratio <- base_ratio
  } else if (wind_speed_ms <= 10.0) {
    lw_ratio <- base_ratio + (wind_speed_ms - 1.0) * 0.3
  } else {
    lw_ratio <- base_ratio + 9.0 * 0.3 + (wind_speed_ms - 10.0) * 0.05
  }
  lw_ratio <- min(lw_ratio, 8.0)
  
  # Calculate base opening angle
  wl_ratio <- 1 / lw_ratio
  base_opening_angle <- 2 * atan(wl_ratio / 2) * 180 / pi
  
  # Calculate φ angle (additive adjustment)
  phi_angle <- 0.0  # Default no adjustment
  
  if (!is.null(variability_data) && nrow(variability_data) > 0) {
    # Find variability for this wind direction
    variability_row <- variability_data[variability_data$Dominant_Wind_Direction_Compass == wind_direction, ]
    if (nrow(variability_row) > 0 && !is.na(variability_row$Average_Variability[1])) {
      avg_variability <- variability_row$Average_Variability[1]
      # Convert variability directly to φ angle
      # Higher variability = wider opening angle
      # φ angle scales with variability (max ~30 degrees for high variability)
      phi_angle <- (avg_variability / 180) * 30.0  # Scale factor to convert to degrees
    }
  }
  
  # Add φ angle to base opening angle
  final_opening_angle <- base_opening_angle + phi_angle
  
  # Ensure bounds
  final_opening_angle <- max(final_opening_angle, 15)
  final_opening_angle <- min(final_opening_angle, 150)
  
  return(list(
    opening_angle = final_opening_angle,
    base_angle = base_opening_angle,
    phi_angle = phi_angle,
    lw_ratio = lw_ratio
  ))
}

# Wedge length calculation
calculate_wedge_length <- function(wind_speed_kmh, minutes, fuel_type = "grasslands") {
  wind_speed_ms <- wind_speed_kmh / 3.6
  
  base_rates <- list(
    "grasslands" = 3.0,
    "chaparral" = 2.0,
    "pine_litter" = 1.5
  )
  
  base_rate <- base_rates[[fuel_type]]
  
  if (wind_speed_ms <= 1.0) {
    wind_factor <- 1.0
  } else {
    wind_factor <- 1.0 + 0.4 * sqrt(wind_speed_ms - 1.0)
  }
  
  head_rate_m_per_min <- base_rate * wind_factor
  length_km <- (head_rate_m_per_min * minutes) / 1000
  length_km <- max(length_km, 0.05)
  
  return(length_km)
}

# ==============================================================================
# SHINY USER INTERFACE
# ==============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Integrated Fire Spread & Wind Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fire Spread Model", tabName = "fire_model", icon = icon("fire")),
      menuItem("Wind Analysis", tabName = "wind_analysis", icon = icon("wind")),
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data_upload",
              fluidRow(
                box(width = 12, title = "Wind Data Upload", status = "primary", solidHeader = TRUE,
                    fileInput("wind_file", "Upload Wind Data CSV",
                              accept = c(".csv")),
                    p("Expected format: Date (DD/MM/YYYY), WDD (Wind Direction in degrees 0-360)"),
                    p("The system will handle 'NoData', 'OffScan', and other invalid values automatically."),
                    verbatimTextOutput("upload_status"),
                    conditionalPanel(
                      condition = "output.upload_status != null && output.upload_status != ''",
                      h4("Processing Summary:"),
                      verbatimTextOutput("processing_summary")
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Wind Variability Summary", status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("wind_variability_table")
                )
              )
      ),
      
      # Fire Model Tab
      tabItem(tabName = "fire_model",
              fluidRow(
                box(width = 4, title = "Parameters", status = "primary", solidHeader = TRUE,
                    h4("Location"),
                    p("Click and drag the pin on the map"),
                    verbatimTextOutput("coordinates"),
                    
                    h4("Fuel Type"),
                    selectInput("fuel_type", "Fuel Type:",
                                choices = list("Grasslands" = "grasslands",
                                               "Chaparral/Shrub" = "chaparral",
                                               "Pine Litter" = "pine_litter"),
                                selected = "grasslands"),
                    
                    h4("Wind Parameters"),
                    p("Select where the wind is coming FROM:"),
                    selectInput("wind_direction", "Wind Coming From:",
                                choices = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                                            "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"),
                                selected = "E"),
                    
                    sliderInput("wind_speed", "Wind Speed (km/h)", 
                                min = 1, max = 100, value = 20, step = 1),
                    
                    sliderInput("minutes", "Duration (minutes)", 
                                min = 30, max = 360, value = 60, step = 10),
                    
                    h4("Calculated Parameters"),
                    verbatimTextOutput("calculated_params"),
                    
                    br(),
                    actionButton("generate_wedge", "Generate Fire Wedge", 
                                 class = "btn-primary btn-block"),
                    br(),
                    downloadButton("download_kml", "Download Roads (KML)", class = "btn-block"),
                    downloadButton("download_csv", "Download Roads (CSV)", class = "btn-block")
                ),
                
                box(width = 8, title = "Fire Spread Visualization", status = "success", solidHeader = TRUE,
                    leafletOutput("map", height = 500)
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Analysis Summary", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("summary")
                )
              )
      ),
      
      # Wind Analysis Tab
      tabItem(tabName = "wind_analysis",
              fluidRow(
                box(width = 6, title = "Wind Direction Variability", status = "warning", solidHeader = TRUE,
                    plotlyOutput("variability_plot")
                ),
                box(width = 6, title = "φ Angle Addition", status = "info", solidHeader = TRUE,
                    plotlyOutput("phi_angle_plot")
                )
              ),
              fluidRow(
                box(width = 12, title = "Wind Variability by Direction", status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("variability_detailed_table")
                )
              )
      )
    )
  )
)

# ==============================================================================
# SHINY SERVER
# ==============================================================================

server <- function(input, output, session) {
  # Reactive values
  wind_data <- reactiveVal(NULL)
  wedge_data <- reactiveVal(NULL)
  roads_data <- reactiveVal(NULL)
  coords <- reactiveValues(lat = 40.64248688976646, lng = 22.994168348355085)
  
  # File upload handling
  observeEvent(input$wind_file, {
    if (!is.null(input$wind_file)) {
      result <- process_wind_data(input$wind_file$datapath)
      if ("error" %in% names(result)) {
        output$upload_status <- renderText(paste("ERROR:", result$error))
        wind_data(NULL)
      } else {
        wind_data(result)
        output$upload_status <- renderText("SUCCESS: Wind data processed successfully!")
        output$processing_summary <- renderText({
          paste("Raw data rows:", result$raw_data_rows,
                "\nDaily summaries:", nrow(result$daily_data),
                "\nWind directions found:", nrow(result$variability_summary),
                "\nDate range:", min(result$daily_data$Date), "to", max(result$daily_data$Date))
        })
      }
    }
  })
  
  # Wind variability table
  output$wind_variability_table <- DT::renderDataTable({
    data <- wind_data()
    if (!is.null(data) && !is.null(data$variability_summary)) {
      DT::datatable(data$variability_summary, 
                    options = list(pageLength = 16),
                    caption = "Average Wind Direction Variability by Compass Direction") %>%
        DT::formatRound(columns = c("Average_Variability"), digits = 2)
    }
  })
  
  # Detailed variability table
  output$variability_detailed_table <- DT::renderDataTable({
    data <- wind_data()
    if (!is.null(data) && !is.null(data$variability_summary)) {
      DT::datatable(data$variability_summary, 
                    options = list(pageLength = 16),
                    caption = "Wind Direction Analysis Results") %>%
        DT::formatRound(columns = "Average_Variability", digits = 2)
    }
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = coords$lng, lat = coords$lat, zoom = 16) %>%
      addMarkers(lng = coords$lng, lat = coords$lat, 
                 layerId = "origin_marker",
                 options = markerOptions(draggable = TRUE),
                 popup = "Drag me to change location!")
  })
  
  # Handle marker drag
  observeEvent(input$map_marker_dragend, {
    event <- input$map_marker_dragend
    if (!is.null(event$id) && event$id == "origin_marker") {
      coords$lat <- event$lat
      coords$lng <- event$lng
    }
  })
  
  # Display coordinates
  output$coordinates <- renderText({
    paste("Latitude:", round(coords$lat, 5), "\nLongitude:", round(coords$lng, 5))
  })
  
  # Calculate parameters with wind variability
  calculated_values <- reactive({
    variability_data <- NULL
    if (!is.null(wind_data()) && !is.null(wind_data()$variability_summary)) {
      variability_data <- wind_data()$variability_summary
    }
    
    angle_result <- calculate_opening_angle_with_variability(
      input$wind_speed, input$fuel_type, input$wind_direction, variability_data
    )
    
    wedge_length <- calculate_wedge_length(input$wind_speed, input$minutes, input$fuel_type)
    
    list(
      opening_angle = angle_result$opening_angle,
      base_angle = angle_result$base_angle,
      phi_angle = angle_result$phi_angle,
      lw_ratio = angle_result$lw_ratio,
      wedge_length = wedge_length
    )
  })
  
  # Display calculated parameters
  output$calculated_params <- renderText({
    calc <- calculated_values()
    fire_speed_m_per_min <- (calc$wedge_length * 1000) / input$minutes
    
    variability_info <- ""
    if (!is.null(wind_data()) && !is.null(wind_data()$variability_summary)) {
      variability_row <- wind_data()$variability_summary[
        wind_data()$variability_summary$Dominant_Wind_Direction_Compass == input$wind_direction, ]
      if (nrow(variability_row) > 0) {
        variability_info <- paste0("\nWind Variability: ", round(variability_row$Average_Variability[1], 1), "°")
      }
    }
    
    paste("Fuel Type:", input$fuel_type,
          "\nWind Coming From:", input$wind_direction,
          "\nWind Speed:", input$wind_speed, "km/h",
          variability_info,
          "\nBase Opening Angle:", round(calc$base_angle, 1), "°",
          "\nφ Angle Addition:", round(calc$phi_angle, 1), "°",
          "\nFinal Opening Angle:", round(calc$opening_angle, 1), "°",
          "\nLength/Width Ratio:", round(calc$lw_ratio, 2),
          "\nWedge Length:", round(calc$wedge_length, 3), "km",
          "\nFire Speed:", round(fire_speed_m_per_min, 2), "m/min")
  })
  
  # Generate wedge and query roads
  observeEvent(input$generate_wedge, {
    calc <- calculated_values()
    direction_degrees <- wind_direction_to_degrees(input$wind_direction)
    
    wedge <- create_wedge(
      center_lon = coords$lng,
      center_lat = coords$lat,
      direction = direction_degrees,
      opening_angle = calc$opening_angle,
      length_km = calc$wedge_length
    )
    
    wedge_data(wedge)
    roads <- get_roads_in_wedge(wedge)
    roads_data(roads)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(data = wedge, color = "red", fillColor = "orange", 
                  fillOpacity = 0.4, weight = 2, group = "Wedge") %>%
      addPolylines(data = roads, color = "blue", weight = 2,
                   popup = ~paste("Road:", name_clean, "<br>Type:", highway)) %>%
      addMarkers(lng = coords$lng, lat = coords$lat, 
                 layerId = "origin_marker",
                 options = markerOptions(draggable = TRUE),
                 popup = paste("Origin<br>Wind Coming From:", input$wind_direction, 
                               "<br>Speed:", input$wind_speed, "km/h"))
  })
  
  # Summary output
  output$summary <- renderPrint({
    wedge <- wedge_data()
    roads <- roads_data()
    if (is.null(wedge) || is.null(roads)) {
      return("Click 'Generate Fire Wedge' to begin analysis.")
    }
    
    calc <- calculated_values()
    cat("=== FIRE SPREAD ANALYSIS ===\n")
    cat("Fuel Type:", input$fuel_type, "\n")
    cat("Wind Coming From:", input$wind_direction, "(Fire spreads toward", 
        (wind_direction_to_degrees(input$wind_direction) + 180) %% 360, "° )\n")
    cat("Wind Speed:", input$wind_speed, "km/h\n")
    cat("Duration:", input$minutes, "minutes\n")
    
    if (!is.null(wind_data()) && !is.null(wind_data()$variability_summary)) {
      variability_row <- wind_data()$variability_summary[
        wind_data()$variability_summary$Dominant_Wind_Direction_Compass == input$wind_direction, ]
      if (nrow(variability_row) > 0) {
        cat("Wind Variability:", round(variability_row$Average_Variability[1], 1), "degrees\n")
        cat("φ Angle Addition:", round(calc$phi_angle, 1), "degrees\n")
      }
    }
    
    cat("Base Opening Angle:", round(calc$base_angle, 1), "degrees\n")
    cat("Final Opening Angle:", round(calc$opening_angle, 1), "degrees\n")
    cat("Length/Width Ratio:", round(calc$lw_ratio, 2), "\n")
    cat("Wedge Length:", round(calc$wedge_length, 3), "km\n")
    cat("Wedge Area:", round(wedge$area_km2, 3), "km²\n")
    cat("Fire Spread Rate:", round((calc$wedge_length * 1000) / input$minutes, 2), "m/min\n")
    cat("\n=== INFRASTRUCTURE IMPACT ===\n")
    cat("Roads in Fire Path:", nrow(roads), "\n")
    cat("Total Road Length at Risk:", round(sum(roads$length_km, na.rm = TRUE), 3), "km\n")
  })
  
  # Variability visualization
  output$variability_plot <- renderPlotly({
    data <- wind_data()
    if (!is.null(data) && !is.null(data$variability_summary)) {
      p <- plot_ly(data$variability_summary, 
                   x = ~Dominant_Wind_Direction_Compass, 
                   y = ~Average_Variability,
                   type = 'bar',
                   text = ~paste("Direction:", Dominant_Wind_Direction_Compass,
                                 "<br>Avg Variability", round(Average_Variability, 1), "°",
                                 "<br>Count:", Count),
                   hovertemplate = "%{text}<extra></extra>") %>%
        layout(title = "Wind Direction Variability",
               xaxis = list(title = "Wind Direction"),
               yaxis = list(title = "Average Variability (degrees)"))
      p
    }
  })
  
  # Phi factor visualization
   output$phi_factor_plot <- renderPlotly({
    data <- wind_data()
    if (!is.null(data)) {
      # Calculate phi factors for all directions
      phi_data <- data$variability_summary
      phi_data$Phi_Factor <- 1.0 + (phi_data$Average_Variability / 180) * 1.0
      
      p <- plot_ly(phi_data, 
                   x = ~Dominant_Wind_Direction_Compass, 
                   y = ~Phi_Factor,
                   type = 'bar',
                   text = ~paste("Direction:", Dominant_Wind_Direction_Compass,
                                 "<br>φ Factor:", round(Phi_Factor, 2),
                                 "<br>Opening Angle Multiplier"),
                   hovertemplate = "%{text}<extra></extra>") %>%
        layout(title = "Opening Angle Adjustment Factor (φ)",
               xaxis = list(title = "Wind Direction"),
               yaxis = list(title = "φ Factor"))
      p
    }
  })
  
  # Download handlers
  output$download_kml <- downloadHandler(
    filename = function() { "fire_spread_roads.kml" },
    content = function(file) {
      roads <- roads_data()
      if (is.null(roads) || nrow(roads) == 0) {
        file.create(file)
        return()
      }
      roads_kml <- st_zm(roads)
      roads_kml$Name <- roads_kml$name_clean
      st_write(roads_kml, file, driver = "KML", delete_dsn = TRUE)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() { "fire_spread_roads.csv" },
    content = function(file) {
      roads <- roads_data()
      if (is.null(roads) || nrow(roads) == 0) {
        write.csv(data.frame(), file)
        return()
      }
      df <- st_drop_geometry(roads)
      df <- df[, c("name_clean", "highway", "length_km")]
      colnames(df) <- c("Name", "Type", "Length_km")
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

# Run the integrated application
shinyApp(ui = ui, server = server)

# ==============================================================================
# STANDALONE ANALYSIS FUNCTIONS
# ==============================================================================

# Function to run analysis without Shiny (for command line use)
run_fire_analysis <- function(wind_csv_path, lat, lng, wind_direction, wind_speed, 
                              fuel_type = "grasslands", duration_min = 60) {
  
  # Process wind data
  wind_result <- process_wind_data(wind_csv_path)
  if ("error" %in% names(wind_result)) {
    stop(wind_result$error)
  }
  
  # Calculate fire spread parameters
  angle_result <- calculate_opening_angle_with_variability(
    wind_speed, fuel_type, wind_direction, wind_result$variability_summary
  )
  
  wedge_length <- calculate_wedge_length(wind_speed, duration_min, fuel_type)
  
  # Create wedge
  direction_degrees <- wind_direction_to_degrees(wind_direction)
  wedge <- create_wedge(lng, lat, direction_degrees, angle_result$opening_angle, wedge_length)
  
  # Get roads
  roads <- get_roads_in_wedge(wedge)
  
  # Return results
  list(
    wind_analysis = wind_result,
    fire_parameters = list(
      opening_angle = angle_result$opening_angle,
      base_angle = angle_result$base_angle,
      phi_factor = angle_result$phi_factor,
      wedge_length = wedge_length,
      wedge_area_km2 = wedge$area_km2
    ),
    roads_at_risk = nrow(roads),
    total_road_length_km = sum(roads$length_km, na.rm = TRUE)
  )
}

# Example usage (uncomment to test):
# result <- run_fire_analysis(
#   wind_csv_path = "diavata_air_quality_weather_data_2023csv.csv",
#   lat = 40.64248688976646,
#   lng = 22.994168348355085,
#   wind_direction = "E",
#   wind_speed = 25,
#   fuel_type = "grasslands",
#   duration_min = 60
# )