# Colorado EnviroScreen Shiny App - Spanish

View shiny app [here](https://teeo-cdphe.shinyapps.io/COEnviroScreen_Spanish/)

The Spanish language inplemention for the Colorado EnviroScreen web mapping application. For more information about the code base supporting this work please visit the project landing [page](https://geospatialcentroid.github.io/COEnviroScreen/).

The repository contains the data and code base for the Spanish language implemention for the Colorado EnviroScreen web mapping application. For more information about the code base supporting this application, please visit the Colorado EnviroScreen repositories <a href="https://geospatialcentroid.github.io/COEnviroScreen/" target="_blank">landing page.</a>

## About the Colorado EnviroScreen project

<a href="https://cdphe.colorado.gov/enviroscreen/" target="_blank">https://cdphe.colorado.gov/enviroscreen.</a>

The Colorado Department of Public Health and Environment (CDPHE) and a team at Colorado State University are gathering pre-launch feedback on an enhanced environmental health screening tool for Colorado. This interactive mapping tool is called Colorado EnviroScreen. The tool will launch in early summer.

Colorado EnviroScreen will enable users to identify disproportionately impacted (DI) communities based on the definition in Colorado’s Environmental Justice Act (HB21-1266) so that communities directly benefit from:

- Money and other resources. For example, the Environmental Justice Advisory Board at CDPHE will use EnviroScreen to determine where to distribute environmental justice grants created by the new law.
- Enhanced opportunities to participate in Air Quality Control Commission rulemaking and permitting decisions.
- Priority for emissions reductions during climate-related air quality rulemakings.
- Enhanced emissions monitoring and modeling for air pollution permits.
- Priority for enforcement and compliance initiatives under an agreement between CDPHE and the U.S. Environmental Protection Agency.



## Quick Start

### Running the Shiny Application
- Download the repo.
- open R project and open app.r
- run the script to render the app locally.
- We host on shinyapps.io, follow there documentation for depolying on there system.

### Gathering Input Datasets
- The data processing code base, if ran successfully will contain a folder call shinyContent. The material in this folder becomes the content for the `data/scores` folder in the shiny deployment. Note that some alteration to the app.r file may be need if you have changed the version number.
- Do to issues rendering special characters the app.r file renames all input parameters. Any adjustments to variable names should be made in the app.r file only.

### Branch Sctructure of the Repositories
The `main` branch of this repository is meant to reflect the current delopment of the EnviroScreen Application. Addational branchs are there for development purposes and will be merged or removed before to long.
