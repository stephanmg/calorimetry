# CALOR: A package for <ins>C</ins>alorimetric <ins>A</ins>nalysis to <ins>L</ins>everage <ins>O</ins>rderly data visualization using Shiny <ins>R</ins> 


<img src="https://github.com/stephanmg/calorimetry/blob/data/shiny_logo.png?raw=true" align="left" width="50" height="70"> 
CALOR is a holistic web application for data integration, visualization and statistical analysis of indirect calorimetry measurements which can be acquired by various platforms during for indirect calorimetry experiments in the wet lab to the end of metabolic phenotyping. 

<br clear="left"/>
<hr/>

[![Shiny app deployment](https://github.com/stephanmg/calorimetry/actions/workflows/deploy-shiny.yml/badge.svg)](https://github.com/stephanmg/calorimetry/actions/workflows/deploy-shiny.yml)
[![Docker.io](https://github.com/stephanmg/calorimetry/actions/workflows/publish-docker.yml/badge.svg)](https://github.com/stephanmg/calorimetry/actions/workflows/publish-docker.yml)
[![Tests](https://github.com/stephanmg/calorimetry/actions/workflows/test-shiny.yml/badge.svg)](https://github.com/stephanmg/calorimetry/actions/workflows/test-shiny.yml)
[![R](https://img.shields.io/badge/R%3E%3D-4.2.0-6666ff.svg)](https://cran.r-project.org/)
[![License](https://img.shields.io/badge/license-MIT-blue)]()
[![Code style](https://img.shields.io/badge/code%20style-black-000000.svg)]()

<hr/>


# User manual

## Option 1: DockerHub / Docker.io

1. Pull CALOR image from remote
```
docker pull stephanmg/CALOR
```
2. Start container
```
docker run --name CALOR -it -p 1338:1338 stephanmg/CALOR
```
3. Open browser and navigate to: http://localhost:1338/

Note that you can also build your own container from the `Dockerfile` provided with either `docker` or `podman`.

## Option 2: ShinyApps.io 
The CALOR application is also hosted on http://shinyapps.io/stephanmg/CALOR
and deployed to our local infrastructure on http://shinys.iaas.uni-bonn.de/calo

## Option 3: Development installation
1. Clone *this* repository
2. Install dependencies with `Rscript -e "library(renv); renv::restore()"` from the current working directory
3. Run `Rscript startapp.R` from base directory (inside *this* respository)


## Demonstration of application
Visit the following website: [Shiny Calo App](https://calorimetry.shinyapps.io/calorimetry/)

# Latest changes

![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)![](https://github.com/stephanmg/calorimetry/blob/data/mybadge.svg?raw=true)