library(tidyverse)
library(highcharter)
library(scales)

# Read and process UN WPP data
unwpp_data <- read.csv("./data/png_unwpp_est9_14_2020_2023.csv") 

# Convert population from thousands to absolute numbers
unwpp_processed <- unwpp_data %>%
  mutate(popF_abs = popF * 1000) %>%
  select(year, age, popF_abs) %>%
  group_by(year) %>%
  mutate(total = sum(popF_abs)) %>%
  ungroup()

# Create national summary
unwpp_national <- unwpp_processed %>%
  filter(year == 2021) %>%
  group_by(age) %>%
  summarise(popF_abs = first(popF_abs),
            .groups = "drop") %>%
  mutate(Source = "UN WPP")

# Write csv
# write.csv(unwpp_national, "./data/png_unwpp_9_to_14_pop_by_age_2021.csv", row.names = FALSE)


# Get WorldPop national data (from previous analysis)
  
# Read and process the data
data <- read.csv("./data/agesex_gamma_gaussian_prov.csv") 


# Calculate single-year estimates
worldpop_national <- data %>%
  filter(sex == "f", age %in% c("5 to 9", "10 to 14")) %>%
  mutate(
    # Apply 20% multiplier to 5-9 group for age 9
    total = ifelse(age == "5 to 9", total * 0.2, total),
    lower = ifelse(age == "5 to 9", lower * 0.2, lower),
    upper = ifelse(age == "5 to 9", upper * 0.2, upper),
    
    # Create single-year ages
    age_list = ifelse(age == "5 to 9", 
                      list(9), 
                      list(10:14))
  ) %>%
  unnest(age_list) %>%
  # Distribute 10-14 group evenly across 5 years
  group_by(Prov_Name, age) %>%
  mutate(
    n_years = ifelse(age == "5 to 9", 1, 5),
    total = total / n_years,
    lower = lower / n_years,
    upper = upper / n_years
  ) %>%
  ungroup() %>%
  select(Prov_Name, age_single = age_list, total)

# Write CSV
# write.csv(worldpop_national, "./data/png_9_to_14_pop_national.csv", row.names = FALSE)

# Create national summary
worldpop_national <- worldpop_national %>%
  group_by(age_single) %>%
  summarise(
    total_population = sum(total, na.rm = TRUE),
    num_provinces = n_distinct(Prov_Name),
    .groups = 'drop'
  ) %>%
  arrange(age_single) %>%
  mutate(Source = "NSO",
         age = age_single,
         popF_abs = total_population) |> 
  select(age,popF_abs,Source)
# Write csv
# write.csv(worldpop_national, "./data/png_nos_9_to_14_pop_by_age_2021.csv", row.names = FALSE)
