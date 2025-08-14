library(tidyverse)
library(readxl)


# -------------------- config ----------------------
xlsx_path <- "./data/png-pop-data-by-province-n-age.xlsx"

# Read raw data
# -------------------- read & segment --------------------
raw_data <- read_excel(xlsx_path, sheet = 1, col_names = FALSE)

#raw_data <- read_excel("png-pop-data-by-province-n-age.xlsx", 
                       #sheet = "Sheet1", col_names = FALSE)



# Modified processing function
process_province <- function(start_row, province_name, districts) {
  n_districts <- length(districts)
  n_years <- 10
  n_age_bands <- 17  # Fixed number of age bands
  
  # Calculate column ranges based on number of districts
  n_cols_per_sex <- n_districts * n_years
  total_cols <- 3:(2 + n_cols_per_sex)
  males_cols <- max(total_cols) + 2 + 1:n_cols_per_sex
  females_cols <- max(males_cols) + 2 + 1:n_cols_per_sex
  
  # Extract data blocks
  blocks <- list(
    total = raw_data[(start_row + 5):(start_row + 21), total_cols] %>% as.matrix(),
    males = raw_data[(start_row + 5):(start_row + 21), males_cols] %>% as.matrix(),
    females = raw_data[(start_row + 5):(start_row + 21), females_cols] %>% as.matrix()
  )
  
  # Create tidy dataset
  map2_dfr(blocks, names(blocks), ~ {
    df <- as_tibble(.x) 
    df$age_band <- raw_data[(start_row + 5):(start_row + 21), 1] %>% pull(1)
    
    df %>% 
      pivot_longer(
        cols = -age_band,
        names_to = "col_index",
        values_to = "population"
      ) %>% 
      mutate(
        # Create proper district/year sequencing
        district = rep(rep(districts, each = n_years), times = n_age_bands),
        year = rep(rep(2021:2030, times = n_districts), times = n_age_bands),
        sex = .y
      ) %>% 
      select(-col_index)
  }) %>% 
    mutate(province = province_name) %>% 
    filter(!is.na(population), 
           !age_band %in% c("Total", "80 +"),
           !is.na(age_band))
}

# Process provinces with correct district counts
western <- process_province(
  start_row = 1,
  province_name = "WESTERN PROVINCE",
  districts = c("Middle Fly", "North Fly", "South Fly")
)

gulf <- process_province(
  start_row = 28,
  province_name = "GULF PROVINCE",
  districts = c("Kerema", "Kikori")
)

# Combine and finalize
tidy_data <- bind_rows(western, gulf) %>% 
  select(province, district, sex, year, age_band, population) %>% 
  mutate(
    age_band = str_replace(age_band, " - ", "-"),
    population = as.numeric(population)
  ) %>% 
  arrange(province, district, sex, year, age_band)

# Verify results
glimpse(tidy_data)