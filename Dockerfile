FROM ubuntu:22.04

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get -y dist-upgrade
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev cmake libfontconfig1-dev libharfbuzz-dev libfribidi-dev libnlopt-dev libxml2-dev libarchive-dev libpq-dev libatlas-base-dev libeigen3-dev libfreetype6-dev libgeos-dev libgmp3-dev libhdf5-dev libjpeg-dev libpng-dev libtiff5-dev gnupg2 software-properties-common

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/'
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get install -y r-base

RUN groupadd --system app
RUN useradd --system -g app app

# for example data sets (assets)
ENV SHINY_DATA_FOLDER=/home/app/

COPY . /home/app/
COPY inc/ /home/app/inc/
WORKDIR /home/app

RUN Rscript -e "install.packages(c('shiny', 'shinybusy', 'shinyjs', 'shinythemes', 'shinyFiles', 'shinyWidgets', 'shinyalert', 'shinyhelper', 'colourpicker', 'dplyr', 'writexl', 'readxl', 'tidyr', 'viridis', 'lubridate', 'zoo', 'fs', 'hash', 'tools', 'cicerone', 'tidyverse'))"
RUN Rscript -e "install.packages(c('data.table', 'plotly', 'ggplot2'))"
RUN Rscript -e "install.packages(c('doBy', 'stringr', 'broom', 'rstatix', 'emmeans', 'ggExtra'))"
RUN Rscript -e "install.packages(c('patchwork'))"
RUN Rscript -e "install.packages(c('ggpubr'))"

EXPOSE 1338
CMD ["Rscript", "startapp.R"]
