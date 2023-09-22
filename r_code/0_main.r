## Main file for the project

## Install missing packages
cran_pkgs <- c("tidyverse", "here")
if(length(missing_pkgs <- setdiff(cran_pkgs, row.names(utils::installed.packages()))) > 0){
  
  message("Installing missing package(s): ",
          paste(missing_pkgs, collapse = ", "))
  
  utils::install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  
} else {
  
  message("All packages already installed")
  
}
rm(cran_pkgs, missing_pkgs)


## Load libraries ####

library(tidyverse)
