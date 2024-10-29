setwd(".")
library(roxygen2)

load_code_from_inc <- function(path) {
   env <- new.env(parent = globalenv())
   r_files <- list.files(file.path(path, "R"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
   for (file in r_files) {
      sys.source(file, envir = env)
   }
   env
}

env = load_code_from_inc(".")
ls(env)
roxygen2::roxygenise(load_code = load_code_from_inc, roclets = c("rd", "namespace"))
