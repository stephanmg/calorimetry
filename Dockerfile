FROM r-base

RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev cmake libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev=2.13.0 libpng-dev libtiff5-dev libjpeg-dev

RUN addgroup --system app \
   && adduser --system --ingroup app app

COPY . /home/app/
COPY inc/ /home/app/inc/
WORKDIR /home/app

RUN Rscript -e "install.packages('versions'); library(versions); install.packages(c('Deriv', 'here', 'doBy', 'ragg', 'microbenchmark', 'patchwork', 'zoo','listenv', 'parallelly', 'future', 'globals', 'future.apply', 'progressr', 'SQUAREM', 'lava', 'prodlim', 'bit', 'ps', 'evaluate', 'highr', 'xfun', 'proxy', 'iterators', 'ipred', 'timeDate', 'rematch', 'bit64', 'prettyunits', 'processx',  'sys', 'cpp11', 'pkgconfig', 'numDeriv', 'knitr', 'sp', 'minqa', 'e1071', 'ModelMetrics', 'plyr', 'pROC', 'recipes', 'reshape2', 'backports', 'assertthat', 'blob', 'DBI', 'gargle', 'uuid', 'cellranger', 'ids', 'rematch2', 'utf8', 'vroom', 'tzdb', 'progress', 'callr', 'rmarkdown', 'selectr', 'stringi', 'jquerylib', 'rappdirs', 'askpass', 'scales', 'httr', 'magrittr', 'digest', 'viridisLite', 'base64enc', 'htmltools', 'htmlwidgets', 'tidyr', 'vctrs', 'tibble', 'lazyeval', 'crosstalk', 'purrr', 'data.table', 'promises', 'fs', 'Rcpp', 'pbkrtest', 'quantreg', 'maptools', 'lme4', 'matrixStats', 'caret', 'ggrepel', 'ggsci', 'ggsignif', 'gridExtra', 'glue', 'polynom', 'rstatix', 'broom', 'crayon', 'dbplyr', 'dtplyr', 'forcats', 'googledrive', 'googlesheets4', 'haven', 'hms', 'lubridate', 'modelr', 'pillar', 'readr', 'readxl', 'writexl', 'reprex', 'rstudioapi', 'rvest', 'stringr', 'xml2', 'ellipsis', 'generics', 'lifecycle', 'R6', 'withr', 'httpuv', 'mime', 'xtable', 'fontawesome', 'shinythemes', 'sourcetools', 'later', 'fastmap', 'commonmark', 'bslib', 'cachem', 'sass', 'curl', 'openssl', 'packrat', 'yaml', 'hash', 'plotly', 'shinyFiles', 'MatrixModels', 'RcppArmadillo', 'RcppEigen', 'SparseM', 'abind', 'car', 'carData', 'conquer', 'cowplot', 'corrplot', 'ggpubr', 'RColorBrewer', 'tidyverse', 'cli', 'viridis', 'dplyr', 'shinyalert', 'colorspace', 'fansi', 'farver', 'ggplot2', 'gtable', 'isoband', 'labeling', 'munsell', 'shiny', 'shinyWidgets', 'rsconnect', 'shinybusy', 'shinyjs', 'miniUI', 'ggExtra', 'colourpicker'))"

EXPOSE 1338
CMD ["Rscript", "startapp.R"]
