FROM r-base

RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev

RUN addgroup --system app \
   && adduser --system --ingroup app app

COPY . /home/app/
WORKDIR /home/app
RUN Rscript -e "install.packages(c('hash', 'plotly', 'shinyFiles', 'MatrixModels', 'RcppArmadillo', 'RcppEigen', 'SparseM', 'abind', 'car', 'carData', 'conquer', 'cowplot', 'corrplot', 'ggpubr', 'RColorBrewer', 'tidyverse', 'cli', 'viridis', 'dplyr', 'colorspace', 'fansi', 'farver', 'ggplot2', 'gtable', 'isoband', 'labeling', 'munsell', 'shiny', 'shinyWidgets'))"

EXPOSE 1337
CMD ["Rscript", "startapp.R"]
