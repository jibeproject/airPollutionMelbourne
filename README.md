# airPollutionMelbourne
This repository was set up to contain code for preparing Melbourne data inputs required for air pollution model.

# Overview
There are two scripts:

- `monitoring site locations.R`, which assesses the distance from air quality monitoring sites to roads, finds the nearest network links (accessible by car) to the monitoring stations, and classifies the monitoring sites based on descriptions at https://uk-air.defra.gov.uk/networks/site-types.
- `emission factors.R`, which calculates NO2 and particulate emission factors for Victoria using data from the COPERT Australia v1.3 tool and PIARC.

In addition to these scripts (described below), the repository contains the following validation documents:
- `compare emission factors.Rmd`, which compares Melbourne and Manchester emission factor values.
- `JIBE_AirPollution_Validation_Melb.Rmd`, which validates Melbourne model air pollution outputs against published values.

# `monitoring site locations.R`

## Input files
The code requires the following input files, which are available [to authorised users] at [*insert location when known*].  The code assumes that the input files are located in a `data` directory ("../data/") which sits beside the directory in which the script files are located .

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/processed/annual_pm_no2_vic.xlsx* | Location of air quality monitoring stations |
|*data/processed/edgesMelbourne.gpkg* |Edges and nodes making up the road network |
|*data/processed/region.sqlite* |Region: the study area for the JIBE project    |
|*data/processed/region_buffer.sqlite* |Region, buffered: the region, plus a buffer to limit edge effects |

## Output file
The code produces the following output file, which is saved to the `data/processed` directory.

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*monitoring site locations classified.csv* | Distance from monitoring site locations to roads, nearest network links (car-accessible), and site classification. |

# `emission factors.R`

## Input files
The code requires the following input files, which are available [to authorised users] at [*insert location when known*].  The code assumes that the input files are located in a `data` directory ("../data/") which sits beside the directory in which the script files are located .

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/processed/COPERT_outputs_vic.xls* | Emissions for Victoria, 2010 fleet, as calculated by the COPERT Australia v1.3 tool |
|*data/processed/COPERT_outputs_vic_supp.xlsx* | Emissions for Victoria, 2010 fleet, as calculated by the COPERT Australia v1.3 tool - supplementary information extracted from the tool but not exported in the main output file |
|*data/original/PIARC_tables_aust.xlsx* | Selected tables of Australian emissions, extracted from PIARC (World Road Association) 2012, Road Tunnels: Vehicle Emission and Air Demand for Ventilation https://www.piarc.org/en/log-in.htm?path=/ressources/publications/7/16655,WEB-2012R05-EN-revise.pdf|

## Output file
The code produces the following output files, which are saved to the `data/processed` directory.

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*emission factors hot.csv* | Hot emission factors                              |
|*emission factors cold start.csv* | Cold start emission factors                |

Note the acknowledgement requirement for COPERT Australia, see https://copert.emisia.com/copert-australia/download/: *Any published results using the tool will properly acknowledge the tool, and must display the following copyright statement: © Generated with COPERT Australia v.1.3, created and maintained by Emisia and Transport Energy/Emission Research (TER), 2025*