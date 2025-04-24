# Shiny-Calorie: A package for <ins>C</ins>alorimetric <ins>A</ins>nalysis to <ins>L</ins>everage <ins>O</ins>rderly data visualization using Shiny <ins>R</ins> 


<img src="https://github.com/stephanmg/Shiny-Calorieimetry/blob/data/shiny_logo.png?raw=true" align="left" width="70" height="70"> 
Shiny-Calorie is a Shiny/R web application for data and metadata integration, visualization and comprehensive statistical analysis of indirect Shiny-Calorieimetry data sets. Shiny-Calorie ingests data sets originating from common metabolic phenotyping platforms, i.e. Sable, TSE Systems and COSMED. Metadata is provided through a standardized metadata sheet (Excel) and as a fallback through the metadata headers in raw data sets. For more details please refer to the documentation for users from the section below or the supplementary material of the Shiny-Calorie preprint (see references below).

<br clear="left"/>
<hr/>

[![Docker.io](https://github.com/stephanmg/Shiny-Calorieimetry/actions/workflows/publish-docker.yml/badge.svg)](https://github.com/stephanmg/Shiny-Calorieimetry/actions/workflows/publish-docker.yml)
[![Docs](https://github.com/stephanmg/Shiny-Calorieimetry/actions/workflows/docs.yml/badge.svg?branch=develop)](https://github.com/stephanmg/Shiny-Calorieimetry/actions/workflows/docs.yml)
[![Tests](https://github.com/stephanmg/Shiny-Calorieimetry/actions/workflows/test-shiny.yml/badge.svg?branch=fix-annotation)](https://github.com/stephanmg/Shiny-Calorieimetry/actions/workflows/test-shiny.yml)
[![R](https://img.shields.io/badge/R%3E%3D-4.2.0-6666ff.svg)](https://cran.r-project.org/)
[![License](https://img.shields.io/badge/license-MIT-blue)]()
[![Code style](https://img.shields.io/badge/code%20style-black-000000.svg)]()

<hr/>


# For users

## Documentation
Documentation available through Github Pages: https://stephanmg.github.io/Shiny-Calorieimetry

The app is available on the following web sites: 
- [Shiny-Calorie in ShinyApps](https://Shiny-Calorieimetry.shinyapps.io/Shiny-Calorieimetry/) or 
- [Shiny-Calorie on on-premise Uni Bonn](https://shinys.iaas.uni-bonn.de/Calo).

Please refer also to the supplementary material from Shiny-Calorie preprint [1].

Tutorial videos are available on YouTube: [Tutorials](http://youtube.com/@Shiny-Calorie-APP).

## Standalone Desktop apps

One can use the Electron wrapper of the app which uses docker inside the Electron app: https://github.com/stephanmg/shiny-electron-wrapper

Releases are uploaded to Sciebo automatically here: https://uni-bonn.sciebo.de/s/0qDhG2Bu1VNkRli/

There you can download the nightly builds for Windows, OSX or Linux. Builds are automatically generated and artifacts deployed trough the `build` Github workflow.


## Metadata integration

One can use the Shiny/R metadata converter app: https://github.com/stephanmg/metadata-converter 
The metadata converter will generate a truncated metadata sheet compatible for reading into Shiny-Calorie.

Alternatively you can fill out the standardized metadata sheet [2] yourself in Excel or fall back to relying on the metadata header of raw data sets.

# For developers

## Option 1: DockerHub / Docker.io

1. Pull Shiny-Calorie image from remote
```
docker pull stephanmg/Shiny-Calorie
```
2. Start container
```
docker run --name Shiny-Calorie -it -p 1338:1338 stephanmg/Shiny-Calorie
```
3. Open browser and navigate to: http://localhost:1338/

Note that you can also build your own container from the `Dockerfile` provided with either `docker` or `podman`.

## Option 2: ShinyApps.io and on-premise Uni Bonn
The Shiny-Calorie application is also hosted on http://shinyapps.io/stephanmg/Shiny-Calorie
and deployed to our local infrastructure on http://shinys.iaas.uni-bonn.de/Shiny-Calorie

Note that on the Uni Bonn infrastructure, a small Kubernetes cluster is used for efficient multi-user usage, upscaling and load balancing.

## Option 3: Development installation
1. Clone *this* repository
2. Install dependencies with `Rscript -e "library(renv); renv::restore()"` from the current working directory
3. Run `Rscript startapp.R` from base directory (inside *this* respository)
4. Optionally if you wish to use load balancing, use the shell script `start_app_instances.sh` which will run 
(specified by the user) a variable number of app instances in parallel. 

An example Nginx configuration for load balancing is provided in `nginx_config.txt` for administrators.

Note for developers: After building documentation a folder `R` will be created in the root directory. Manually
delete this folder and do not add for tracking with Git, nor deploy, as this interferes with the Shiny app. 
We need to fake a proper R package structure in order to use the `roxygenise` functions, but do not need the
structure after docs have been generated.

## References

- [1] Add reference to Shiny-Calorie
- [2] Add reference to Metadata Sheet

The Shiny-Calorie image was created with the assistance of the AI tool DALL-E 2.
