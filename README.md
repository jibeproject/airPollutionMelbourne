# airPollutionMelbourne
This repository was set up to contain code for preparing Melbourne data inputs required for air pollution model.

# Overview
There is one script, `monitoring site locations.R`, which assesses the distance from air quality monitoring sites to roads, finds the nearest network links (accessible by car) to the monitoring stations, and classifies the monitoring sites based on descriptions at https://uk-air.defra.gov.uk/networks/site-types .

# Input files
The code requires the following input files, which are available [to authorised users] at [*insert location when known*].  The code assumes that the input files are located in a `data` directory ("../data/") which sits beside the directory in which the script files are located .

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/processed/annual_pm_no2_vic.xlsx* | Location of air quality monitoring stations |
|*data/processed/edgesMelbourne.gpkg* |Edges and nodes making up the road network |
|*data/processed/region.sqlite* |Region: the study area for the JIBE project    |
|*data/processed/region_buffer.sqlite* |Region, buffered: the region, plus a buffer to limit edge effects |

# Output file
The code produces the following output file, which is saved to the `data/processed` directory.

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*monitoring site locations classified.csv* | Distance from monitoring site locations to roads, nearest network links (car-accessible), and site classification. |