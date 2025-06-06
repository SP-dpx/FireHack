# FireHack
User Manual: Fire Impacted Road System Assessment Tool
1. Introduction

Welcome to the Fire Impacted Road System Assessment Tool. This application is designed to identify and visualize sections of the road network that may be impacted by a wildland fire, serving as a critical aid for civilian safety and emergency traffic management.

The tool simulates the potential spread area of a fire and then overlays this with OpenStreetMap road data to highlight roads that fall within the projected fire zone. The primary goal is to:

    Inform civilians about roads to avoid for their safety.
    Facilitate emergency responders by indicating routes that may be compromised and areas where traffic should be restricted to ensure clear access for fire trucks and other emergency vehicles.
    Enable rapid communication of these avoidance zones through KML file export, compatible with mapping platforms like Google Maps.

The application features:

    An interactive map for visualizing the impacted road network.
    Functionality to upload and analyze historical wind data to predict more accurate fire spread shapes.
    Detailed reports on impacted road segments.

2. System Requirements

    R installed on your system.
    Internet connection (for loading map tiles and OpenStreetMap data).

3. Installation and Setup

    Save the R Code: Save the provided R code into a file named fire_roads_tool.R (or any other .R extension).
    Install R Packages: The application automatically checks and installs necessary R packages (shiny, leaflet, sf, osmdata, dplyr, readr, shinyWidgets, geosphere, DT, plotly, shinydashboard). When you first run the app, it will prompt you to install these if they are not already present.
    Prepare Wind Data (Optional but Recommended): While the tool can run without it, providing wind data enhances the accuracy of the fire spread prediction. You will need a CSV file containing historical wind data. The application expects at least two columns:
        Date: Date of the observation (e.g., DD/MM/YYYY, YYYY-MM-DD, MM/DD/YYYY).
        WDD: Wind Direction in degrees (0-360). The application is robust to common non-numeric values like "NoData", "OffScan", "Samp<", and commas in numbers.

4. Running the Application

To start the application, open your R environment (e.g., RStudio) and run the fire_roads_tool.R file:
R

source("fire_roads_tool.R")

This will launch the Shiny application in your default web browser.
5. Application Layout

The application is organized into a dashboard with three main tabs:

    Fire Spread Model: Define fire parameters, visualize the potential fire impact zone on a map, and identify affected roads. This is your primary operational tab.
    Wind Analysis: (Optional) View plots and detailed tables of historical wind direction variability from your uploaded data, which informs the accuracy of the fire spread shape.
    Data Upload: Upload your wind data CSV file and see a summary of the processing.

6. Data Upload Tab

This is where you can upload historical wind data to improve the accuracy of fire spread prediction.

    Wind Data Upload:
        Click the "Browse..." button next to "Upload Wind Data CSV".
        Select your wind data CSV file.
        The application will automatically attempt to process the data.
        upload_status: Shows "SUCCESS" or "ERROR" messages indicating if the upload and initial processing were successful.
        processing_summary: Provides details on the number of rows processed, daily summaries created, and the date range of your data.
    Wind Variability Summary:
        Once data is successfully uploaded, this table will populate with the average wind direction variability for each dominant compass direction identified in your data. This variability is used in the Fire Spread Model to calculate the "φ Angle Addition".

7. Fire Spread Model Tab

This is the core tab for assessing road impacts.
7.1. Parameters Section

This section on the left-hand side allows you to define the fire scenario.

    Location:
        The map defaults to a specific location in Greece.
        You can drag the red marker on the map to set the estimated ignition point of the fire.
        The coordinates display will update with the Latitude and Longitude of the marker.
    Fuel Type:
        Select the type of fuel present in the fire area: "Grasslands", "Chaparral/Shrub", or "Pine Litter". This influences the fire's base spread rate and thus the size of the potential impact zone.
    Wind Parameters:
        Wind Coming From: Select the dominant direction from which the current wind is blowing (e.g., "E" for East). The fire is assumed to spread in the opposite direction.
        Wind Speed (km/h): Adjust the current wind speed using the slider (1 to 100 km/h).
        Duration (minutes): Set the projected duration of fire spread from the ignition point (30 to 360 minutes). This defines the maximum extent of the potential impact zone.
    Calculated Parameters:
        This section dynamically updates to show key calculated values based on your inputs:
            Fuel Type, Wind Direction, Wind Speed.
            Wind Variability: (If wind data has been uploaded) This will show the average variability for the selected Wind Coming From direction, derived from your historical data.
            Base Opening Angle: The theoretical fire spread angle (width of the fire front) without considering wind variability.
            φ Angle Addition: The additional angle (in degrees) added to the base opening angle due to wind variability (calculated from your uploaded historical wind data). This widens the potential impact zone.
            Final Opening Angle: The Base Opening Angle + φ Angle Addition, representing the total expected spread angle (width) of the fire's potential impact zone.
            Length/Width Ratio.
            Wedge Length (total distance fire is projected to spread in the primary direction).
            Fire Speed (average spread rate in meters per minute).
    Generate Fire Wedge:
        Click this button after setting your parameters. The application will:
            Calculate the potential fire impact zone (represented as a wedge shape) based on your inputs.
            Query OpenStreetMap data for roads that intersect with or are contained within this calculated fire impact zone.
            Display the fire impact zone and the identified impacted roads on the map.
    Download Buttons:
        Download Roads (KML): Downloads the identified impacted roads as a KML file. This file can be easily distributed and opened in common mapping applications like Google Earth or Google Maps, providing clear visual guidance on areas/roads to avoid.
        Download Roads (CSV): Downloads a CSV file containing tabular details of the affected roads (Road Name, Type, Length in km). This is useful for further analysis or record-keeping.

7.2. Fire Impact Visualization Map

    Displays the geographical context.
    Shows the draggable red marker for the fire's ignition point.
    Once "Generate Fire Wedge" is clicked:
        A red/orange wedge will appear, representing the simulated potential fire impact zone. This is the area within which roads are considered compromised.
        Blue lines will indicate roads that fall within or are intersected by this fire impact zone. These are the roads to be avoided.
        Hover over the roads to see their name and type in a popup.

7.3. Analysis Summary

    Provides a detailed text summary of the impact assessment, including:
        Input parameters of the fire scenario.
        Calculated fire spread characteristics (angles, lengths, speeds) that define the impact zone.
        Crucially, statistics on:
            Roads in Fire Path: The number of individual road segments identified as impacted.
            Total Road Length at Risk: The cumulative length (in km) of all road segments identified as impacted.

8. Wind Analysis Tab

This tab is for users who have uploaded historical wind data and wish to understand its characteristics.

    Wind Direction Variability Plot:
        A bar chart showing the average wind direction variability (in degrees) for each dominant wind direction recorded in your uploaded data. This plot helps in understanding the consistency of wind directions.
    φ Angle Addition Plot:
        A bar chart visualizing the calculated φ angle (in degrees) that is derived from the average wind variability for each dominant wind direction. This directly illustrates how much the wind's historical variability contributes to the widening of the projected fire impact zone.
    Wind Variability by Direction Table:
        A detailed table showing the average variability, the count of days, and the total number of observations for each dominant wind direction from your uploaded dataset.

9. Standalone Analysis (Advanced Users / Command Line)

The R script also includes a run_fire_analysis function that can be used directly from the R console or another R script without launching the Shiny app. This is useful for batch processing, automated reports, or integrating the core logic into other R workflows.
R

run_fire_analysis <- function(wind_csv_path, lat, lng, wind_direction, wind_speed,
                              fuel_type = "grasslands", duration_min = 60) {
    # ... (function definition as in the provided code)
}

# Example Usage:
# result <- run_fire_analysis(
#   wind_csv_path = "path/to/your/wind_data.csv", # Replace with actual path
#   lat = 40.64248688976646, # Example latitude
#   lng = 22.994168348355085, # Example longitude
#   wind_direction = "E",    # Example wind direction
#   wind_speed = 25,         # Example wind speed in km/h
#   fuel_type = "grasslands",# Example fuel type
#   duration_min = 60        # Example duration in minutes
# )
# print(result)

Parameters for run_fire_analysis:

    wind_csv_path: Path to your wind data CSV file.
    lat: Latitude of the fire ignition point.
    lng: Longitude of the fire ignition point.
    wind_direction: Dominant wind direction (e.g., "N", "E", "SW").
    wind_speed: Wind speed in km/h.
    fuel_type: Type of fuel ("grasslands", "chaparral", "pine_litter").
    duration_min: Projected duration of fire spread in minutes.

The function returns a list containing processed wind analysis, fire spread parameters (defining the impact zone), and road impact details.
10. Troubleshooting

    "WDD column not found" / "Date column not found": Ensure your CSV file has columns named WDD and Date (case-sensitive) and they contain valid data.
    "No valid data rows found after cleaning": This indicates issues with the Date or WDD values in your CSV. Check for consistency and correct formats.
    Map not loading / Roads not appearing: Ensure you have an active internet connection. OpenStreetMap data retrieval requires internet access.
    Application is slow: Processing large wind data files or querying large geographical areas for roads can be computationally intensive and may take time. Be patient.
    Error messages in R console: If the Shiny app crashes or shows an error, check the R console for detailed error messages. These messages are crucial for diagnosing and resolving issues.
