# Step 1: Load required libraries
library(readxl)
library(tidyverse)
library(janitor)

# Step 2: Define file path and load the data (skip first row with repeated headers)
file_path <- "./data/20250718_PNG Penta & MR data 2021-2024.xlsx"
raw_data <- read_excel(file_path, sheet = "2021-2024", skip = 1)

# Step 3: Clean column names
df <- clean_names(raw_data)

# Step 4: Rename key variables for clarity
df <- df %>%
  rename(
    year = year,
    province = province,
    estimated_births = estimated_births,
    dpt1 = dtp_hib_1st_dose,
    dpt3 = dtp_hib_3rd_dose
  )

# Step 5: Filter to include only 2024 data
df <- df %>%
  filter(year == 2024, !is.na(province))

# Step 6: Convert relevant columns to numeric
df <- df %>%
  mutate(
    estimated_births = as.numeric(estimated_births),
    dpt1 = as.numeric(dpt1),
    dpt3 = as.numeric(dpt3)
  )

# Step 7: Calculate surviving infants using IMR = 35.7 per 1000
df <- df %>%
  mutate(surviving_infants = estimated_births * (1 - 35.7 / 1000))

# Step 8: Group by province and summarise
prov_summary <- df %>%
  group_by(province) %>%
  summarise(
    estimated_births = sum(estimated_births, na.rm = TRUE),
    surviving_infants = sum(surviving_infants, na.rm = TRUE),
    dpt1 = sum(dpt1, na.rm = TRUE),
    dpt3 = sum(dpt3, na.rm = TRUE)
  )

# Step 9: Calculate coverage
prov_summary <- prov_summary %>%
  mutate(
    dpt1_coverage = round((dpt1 / surviving_infants) * 100, 1),
    dpt3_coverage = round((dpt3 / surviving_infants) * 100, 1)
  )

# Step 10: Final arrangement
prov_summary <- prov_summary %>%
  select(province, estimated_births, surviving_infants, dpt1, dpt1_coverage, dpt3, dpt3_coverage) %>%
  arrange(province)

# Step 11: View results
print(prov_summary)

# Step 12: Write to CSV file

write.csv(prov_summary, "./data/png_dpt_cov_2024_by_province.csv", row.names = FALSE)
