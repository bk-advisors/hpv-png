# PNG Shape File - Data Download and Setup

# Install Packages
# install.packages("remotes")
# remotes::install_github("dickoa/rhdx")

# Libraries 

library(rhdx)
library(tidyverse)
library(sf)

#Setup the link to HDX

set_rhdx_config(hdx_site = "prod")

# Admin - level 1
png_adm1 <- pull_dataset("cod-ab-png") %>%
  get_resource(1) %>%
  read_resource(layer = "png_admbnda_adm1_nso_20190508")

# Write csv
# write.csv(png_adm1, "./data/png_adm1.csv", row.names = FALSE)


# Admin - level 2
png_adm2 <- pull_dataset("cod-ab-png") %>%
  get_resource(1) %>%
  read_resource(layer = "png_admbnda_adm2_nso_20190508")

# Let's do a map for adm1 using ggplot
ggplot(png_adm1) +
  geom_sf()
