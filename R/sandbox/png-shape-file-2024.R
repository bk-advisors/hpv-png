# PNG Shape File - Data Download and Setup

# Install Packages
# install.packages("remotes")
# remotes::install_github("dickoa/rhdx")

# Install necessary packages if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, stringr, sf, ggplot2)


# Libraries 

library(rhdx)
library(tidyverse)
library(sf)
library(readxl)

# Part 1: Get the target population data by province from excel model

# Read raw population data from Excel
pop_data <- read_excel(
  "./data/png-hpv-pop-modelling-v1-20250606.xlsx",
  sheet = "nso_9to14_by_province_2021"
) %>%
  # Filter out the national total row
  filter(Prov_Name != "NATIONAL TOTAL") %>%
  # Clean and format province names
  mutate(
    Province = str_to_title(Prov_Name),  # Convert to title case (e.g., "CENTRAL" → "Central")
    pop2021 = median  # Use median population estimate
  ) %>%
  select(Province, pop2021)  # Keep relevant columns

# Preview the cleaned data
head(pop_data, 5)

# Handle name matching

pop_data <- pop_data %>%
  mutate(Province = case_when(
    Province == "Northern (Oro)" ~ "Northern",
    TRUE ~ Province
  ))

# Write csv
# write.csv(png_adm1, "./data/png_adm1.csv", row.names = FALSE)

# Part 2: Match the names

# Create province name mapping table
province_mapping <- data.frame(
  pop_data_name = c(
    "Central", "Central Bougainville", "Chimbu (Simbu)", "East New Britain",
    "East Sepik", "Eastern Highlands", "Enga", "Gulf", "Hela", "Jiwaka",
    "Madang", "Manus", "Milne Bay", "Morobe", "National Capital District",
    "New Ireland", "North Bougainville", "Northern", "South Bougainville",
    "Southern Highlands", "West New Britain", "West Sepik", "Western",
    "Western Highlands"
  ),
  shapefile_name = c(
    "Central Province", "Autonomous Region of Bougainville", "Chimbu (Simbu) Province",
    "East New Britain Province", "East Sepik Province", "Eastern Highlands Province",
    "Enga Province", "Gulf Province", "Hela Province", "Jiwaka Province",
    "Madang Province", "Manus Province", "Milne Bay Province", "Morobe Province",
    "National Capital District", "New Ireland Province", "Autonomous Region of Bougainville",
    "Northern (Oro) Province", "Autonomous Region of Bougainville", "Southern Highlands Province",
    "West New Britain Province", "West Sepik (Sandaun) Province", "Western Province",
    "Western Highlands Province"
  )
)

# Merge mapping with population data
pop_data_mapped <- pop_data %>%
  left_join(province_mapping, by = c("Province" = "pop_data_name"))

# Aggregate Bougainville provinces
pop_data_aggregated <- pop_data_mapped %>%
  group_by(shapefile_name) %>%
  summarize(pop2021 = sum(pop2021, na.rm = TRUE)) %>%
  rename(ADM1_EN = shapefile_name)


# Part 3 : Get the PNG shape file

#Setup the link to HDX

set_rhdx_config(hdx_site = "prod")

# Admin - level 1
png_adm1 <- pull_dataset("cod-ab-png") %>%
  get_resource(1) %>%
  read_resource(layer = "png_admbnda_adm1_nso_20190508")

# Merge pop data with shape file
png_adm1_pop <- png_adm1 %>%
  left_join(pop_data_aggregated, by = "ADM1_EN")

# Check for missing matches
anti_join(pop_data_aggregated, png_adm1_pop, by = "ADM1_EN")



# Let's do a map for adm1 using ggplot
ggplot(png_adm1_pop) +
  geom_sf() +
  theme_minimal()

# Fill map with population

ggplot(png_adm1_pop) +
  geom_sf(aes(fill = pop2021)) +
  scale_fill_viridis_c(name = "Population (9-14 Girls)") +
  labs(title = "HPV Vaccine Target Population Density in PNG by Province") +
  theme_minimal()
