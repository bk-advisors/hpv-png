library(tidyverse)
library(readxl)

# -------- THIS CODE WORKS ! JUST NEED TO ADD REMAINING 20 PROVINCES -----

# -------------------- config ----------------------
xlsx_path <- "./data/png-pop-data-by-province-n-age-v2.xlsx"

# Read raw data
# -------------------- read & segment --------------------
raw_data <- read_excel(xlsx_path, sheet = 1, col_names = FALSE)

# ---- helper: build column indices for one sheet layout ----
# Assumes:
#   - first_data_col is the FIRST column of the "Total" group for Year 2021
#   - per-year block = 1 age column + n_districts value columns (left→right)
#   - there is a constant gap (blank columns) between sex groups (default = 2)
build_group_cols <- function(first_data_col, n_years, n_districts, gap_between_groups = 2) {
  per_year_block <- n_districts + 1                # 1 age col + n_districts values
  block_width    <- n_years * per_year_block       # total columns (including the age col per year)
  
  cols_for_group <- function(start_col) {
    # For each year, skip the 1st (age) column, then take the n_districts value columns
    unlist(lapply(0:(n_years - 1), function(k) {
      s <- start_col + k * per_year_block          # start col of that year's block (age column)
      seq(s + 1, s + n_districts)                  # skip age col -> take value columns
    }))
  }
  
  start_total   <- first_data_col
  start_males   <- start_total + block_width + gap_between_groups
  start_females <- start_males + block_width + gap_between_groups
  
  list(
    total   = cols_for_group(start_total),
    males   = cols_for_group(start_males),
    females = cols_for_group(start_females)
  )
}

# ---- modified processor ----
process_province <- function(start_row, province_name, districts,
                             first_data_col = 3, n_years = 10, n_age_bands = 17, gap_between_groups = 2) {
  
  n_districts <- length(districts)
  
  # Build the exact column indices for each sex group (skipping per-year age columns)
  idx <- build_group_cols(first_data_col, n_years, n_districts, gap_between_groups)
  
  # Pull the 17 age-band labels from the first column in the province block
  age_band_vec <- raw_data[(start_row + 5):(start_row + 5 + n_age_bands - 1), 1] %>% pull(1)
  
  # Helper to turn one group's matrix into long tidy data with correct year/district
  to_long <- function(col_index_vec, sex_label) {
    # Extract the values block for this sex
    mat <- raw_data[(start_row + 5):(start_row + 5 + n_age_bands - 1), col_index_vec] %>% as.matrix()
    df  <- as_tibble(mat)
    # Name the columns with a fixed order marker so we can map year/district correctly
    names(df) <- sprintf("c%03d", seq_len(ncol(df)))
    df$age_band <- age_band_vec
    
    # Design table: map each column to (year, district) given the order we constructed:
    # Within each YEAR, we laid out district1 .. districtN
    k <- 0:(length(col_index_vec) - 1)
    design <- tibble(
      col_index = sprintf("c%03d", seq_along(col_index_vec)),
      year      = 2021 + (k %/% n_districts),                # year-major order
      district  = districts[(k %% n_districts) + 1]
    )
    
    df %>%
      pivot_longer(cols = starts_with("c"),
                   names_to = "col_index",
                   values_to = "population_raw") %>%
      left_join(design, by = "col_index") %>%
      transmute(
        province  = province_name,
        district,
        sex       = sex_label,
        year,
        age_band,
        population = suppressWarnings(readr::parse_number(as.character(population_raw)))
      ) %>%
      filter(!is.na(population))
  }
  
  bind_rows(
    to_long(idx$total,   "Total"),
    to_long(idx$males,   "Male"),
    to_long(idx$females, "Female")
  ) %>%
    # Clean & standardise
    mutate(
      age_band   = str_replace_all(age_band, "\\s*–\\s*", "-") %>% str_replace_all("\\s*-\\s*", "-") %>% str_squish(),
      sex        = recode(sex, "Total" = "All")
    ) %>%
    filter(!is.na(age_band),
           !str_detect(str_to_lower(age_band), "^total$")) %>%
    # Drop impossible or obviously wrong age labels
    filter(str_detect(age_band, "^\\d{1,3}(\\+|\\-\\d{1,3})$")) %>%
    arrange(province, district, sex, year, age_band)
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

central <- process_province(
  start_row = 55,
  province_name = "CENTRAL PROVINCE",
  districts = c("Abau", "Goilala","Kairuku-Hiri","Rigo")
)


ncd <- process_province(
  start_row = 82,
  province_name = "NATIONAL CAPITAL DISTRICT",
  districts = c("","NCD")
) # Code offsets the column by 1-year forward, so assignments are not correct - fix later

milne <- process_province(
  start_row = 110,
  province_name = "MILNE BAY PROVINCE",
  districts = c("Alotau","Esa'ala","Kiriwina-Goodenough","Samarai-Murua")
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

# Optional sanity checks
tidy_data %>% count(sex)
tidy_data %>% count(province, district)
tidy_data %>% filter(sex == "All", year == 2021) %>% group_by(province) %>% summarise(tot = sum(population, na.rm = TRUE))
