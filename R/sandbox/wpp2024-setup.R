# WPP 2024 Data Download and Setup

# Install relevant packages

library(devtools)
options(timeout = 600)
install_github("PPgp/wpp2024")

# Load library

library(wpp2024)
library(tidyverse)
library(highcharter)
library(scales)

?popAge1dt

#Get data

data("popAge1dt") #Estimates

data("popprojAge1dt") #Projections

#Save it an new data frame


unwpp_data2024 <- popAge1dt

unwpp_proj <- popprojAge1dt

# PNG estimates 2020 - 2023

png_unwpp_est9_14 <- unwpp_data2024 |> 
  filter(country_code == 598) |> 
  mutate(Year = as.numeric(year)) |> 
  filter(Year >= 2020) |> 
  filter(Year <= 2024) |> 
  filter(age >= 9) |> 
  filter(age <= 14) |> 
  select(country_code,name,year,age,popF)


# PNG pop projections only data from 2025 to 2050 for 9 - 14 year olds

png_unwpp_proj9_14 <- unwpp_proj |> 
  filter(country_code == 598) |> 
  mutate(Year = as.numeric(year)) |> 
  filter(Year >= 2024) |> 
  filter(Year <= 2050) |> 
  filter(age >= 9) |> 
  filter(age <= 14) |> 
  select(country_code,name,year,age,popF,popF_low,popF_high)



# Write it to csv file
write.csv(png_unwpp_proj9_14, file = "./data/png_unwpp_proj9_14_2024_2050.csv", row.names = FALSE)
write.csv(png_unwpp_est9_14, file = "./data/png_unwpp_est9_14_2020_2023.csv", row.names = FALSE)

# Read csv file
png_unwpp_proj9_14 <- read.csv("./data/png_unwpp_proj9_14.csv")


# Read the dataset
data <- png_unwpp_proj9_14

# Convert population values from thousands to absolute numbers and calculate annual totals
results <- data %>%
  mutate(across(c(popF, popF_low, popF_high), ~ .x * 1000)) %>%
  group_by(year) %>%
  summarise(
    Total_median = sum(popF),
    Total_low = sum(popF_low),
    Total_high = sum(popF_high)
  ) %>%
  arrange(year)

# View results
print(results)


# Visualizing the trends with ggplot
ggplot(results, aes(x = year)) +
  geom_line(aes(y = Total_median, color = "Median"), linewidth = 1) +
  geom_ribbon(aes(ymin = Total_low, ymax = Total_high, fill = "Uncertainty"), alpha = 0.3) +
  geom_point(aes(y = Total_median), color = "blue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Projected Population of Girls Aged 9-14 in Papua New Guinea",
       subtitle = "UN WPP Projections (2025-2050)",
       x = "Year",
       y = "Number of Girls",
       color = "Projection") +
  scale_fill_manual(name = "", values = c("Uncertainty" = "grey70")) +
  theme_minimal() +
  theme(legend.position = "bottom")

