FROM ubuntu:20.04

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get -y dist-upgrade
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev cmake libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype-dev r-base libnlopt-dev libxml2-dev libarchive-dev libpq-dev

RUN groupadd --system app
RUN useradd --system -g app app

COPY . /home/app/
COPY inc/ /home/app/inc/
WORKDIR /home/app

RUN Rscript -e "install.packages(c('shiny', 'shinybusy', 'shinyjs', 'shinythemes', 'shinyFiles', 'shinyWidgets', 'shinyalert', 'shinyhelper', 'colourpicker', 'dplyr', 'writexl', 'readxl', 'tidyr', 'viridis', 'lubridate', 'zoo', 'fs', 'hash', 'tools', 'cicerone', 'tidyverse'))"
RUN Rscript -e "install.packages(c('data.table', 'plotly', 'ggplot2'))"
RUN Rscript -e "install.packages(c('doBy', 'stringr', 'broom', 'rstatix', 'emmeans', 'ggExtra'))"
# (ggpubr); stringr, ggExtra, emmeaans, doBy, stringr, broom, rstatix

EXPOSE 1338
CMD ["Rscript", "startapp.R"]
