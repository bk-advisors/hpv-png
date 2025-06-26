# Checking if the packages you need are installed -- if not, it will install for you

packages <- c("tidyverse", "stringr", "censusapi", "sf", "tidycensus", "ggspatial", "tigris")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")
}

# Load libraries 

library(tidyverse)
library(sf)
library(ggspatial)
library(tigris)

# USA Map

us_states <- states(cb = TRUE, resolution = "20m")

glimpse(us_states)
