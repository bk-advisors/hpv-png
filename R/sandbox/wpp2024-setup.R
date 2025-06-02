# WPP 2024 Data Download and Setup

# Install relevant packages

library(devtools)
options(timeout = 600)
install_github("PPgp/wpp2024")

# Load library

library(wpp2024)
library(tidyverse)

?popAge1dt

#Get data

data("popAge1dt") #Estimates

data("popprojAge1dt") #Projections

#Save it an new data frame


unwpp_data2024 <- popAge1dt

unwpp_proj <- popprojAge1dt

# PNG pop projections only data from 2025 to 2050 for 9 - 14 year olds

png_unwpp_proj9_14 <- unwpp_proj |> 
  filter(country_code == 598) |> 
  mutate(Year = as.numeric(year)) |> 
  filter(Year >= 2025) |> 
  filter(Year <= 2050) |> 
  filter(age >= 9) |> 
  filter(age <= 14) |> 
  select(country_code,name,year,age,popF,popF_low,popF_high)



# Write it to csv file
# write.csv(png_unwpp_proj9_14, file = "./data/png_unwpp_proj9_14.csv", row.names = FALSE)

# Read csv file
png_unwpp_proj9_14 <- read.csv("./data/png_unwpp_proj9_14.csv.csv")
