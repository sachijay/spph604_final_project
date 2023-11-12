## Main file for the project

## Install missing packages
cran_pkgs <- c("devtools", "tidyverse", "here", "labelled", "survey", "mice", "furrr", "gtsummary", "WeightedROC")
if(length(missing_pkgs <- setdiff(cran_pkgs, row.names(utils::installed.packages()))) > 0){
  
  message("Installing missing package(s): ",
          paste(missing_pkgs, collapse = ", "))
  
  utils::install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  
} else {
  
  message("All packages already installed")
  
}
rm(cran_pkgs, missing_pkgs)


## Install custom packages (UNCOMMENT IF NOT INSTALLED!!!!!!!!)

# devtools::install_github("sachijay/miscr")


## Load libraries ####

library(tidyverse)
library(miscr) ## This is a custom package installed from GitHub (commented above)
library(survey)
library(gtsummary)
