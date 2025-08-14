# =========================================================
# PNG NSO population workbook -> tidy (all 22 provinces)
# Output columns:
#   province, district, sex (All/Male/Female), year (2021–2030),
#   age_band (e.g., "10-14", "80+"), age_lower, age_upper, population
# =========================================================

# install.packages(c("readxl","tidyverse","janitor"))
library(readxl)
library(tidyverse)
library(janitor)
library(stringr)

# -------------- CONFIG ----------------
xlsx_path <- "./data/png-pop-data-by-province-n-age.xlsx"
year_min  <- 2021
year_max  <- 2030

# -------------- HELPERS ---------------

# Age-band recogniser: "0-4", "5 - 9", "80+"
is_age_band <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- ""
  y <- str_squish(y)
  y <- str_replace_all(y, "–", "-")
  str_detect(y, "^\\d{1,3}\\s*(\\+|\\s*-\\s*\\d{1,3})$")
}

# Parse "0-4" / "80+" to numeric bounds
parse_age_bounds <- function(age_band) {
  y <- age_band %>% as.character() %>% str_to_lower() %>% str_squish()
  y <- y %>% str_replace_all("–", "-") %>% str_replace_all("\\s+", "")
  lower <- suppressWarnings(as.integer(str_extract(y, "^\\d{1,3}")))
  upper <- ifelse(str_detect(y, "\\+$"),
                  Inf,
                  suppressWarnings(as.integer(str_extract(y, "(?<=-)\\d{1,3}$"))))
  tibble(age_lower = lower, age_upper = upper)
}

# Province name normaliser (fix common typos/variants)
normalise_province <- function(s) {
  u <- toupper(str_squish(as.character(s)))
  mapping <- c(
    "SOUTHERN ISLANDS PROVINCE" = "SOUTHERN HIGHLANDS PROVINCE",
    "EASTERN ISLANDS" = "EASTERN HIGHLANDS PROVINCE",
    "AUTONOMOUS REGION OF BUGAINVILLE" = "AUTONOMOUS REGION OF BOUGAINVILLE",
    "JUWAKA PROVINCE" = "JIWAKA PROVINCE",
    "WESTERN HIGHLANDS" = "WESTERN HIGHLANDS PROVINCE",
    "EAST SEPIK" = "EAST SEPIK PROVINCE",
    "WEST SEPIK" = "WEST SEPIK (SANDAUN) PROVINCE",
    "CHIMBU PROVINCE" = "SIMBU (CHIMBU) PROVINCE"
  )
  if (u %in% names(mapping)) return(mapping[[u]])
  if (u == "NATIONAL CAPITAL DISTRICT") return("NATIONAL CAPITAL DISTRICT")
  str_to_title(u)
}

# Heuristic: a row is a province header if col 2 has a label that is not age/sex/total,
# and the rest of the row has no meaningful numeric values
is_province_row <- function(row_vec) {
  lab <- row_vec[[2]] %>% as.character() %>% str_squish()
  if (is.na(lab) || lab == "") return(FALSE)
  if (toupper(lab) %in% c("TOTAL","MALE","MALES","FEMALE")) return(FALSE)
  if (is_age_band(lab)) return(FALSE)
  other <- suppressWarnings(sapply(row_vec[-(1:2)], function(v) !is.na(as.numeric(v))))
  has_numbers <- any(other, na.rm = TRUE)
  !has_numbers
}

# Extract tidy records from a single province "chunk"
extract_from_province_chunk <- function(chunk_df, prov_name_raw, year_min = 2021, year_max = 2030) {
  if (nrow(chunk_df) < 4) return(tibble())
  
  # 1) Find the header row where sex labels (TOTAL/MALE/FEMALE) appear
  hdr_candidates <- which(
    apply(chunk_df[, 2:ncol(chunk_df), drop = FALSE], 1, function(r)
      any(toupper(str_squish(as.character(r))) %in% c("TOTAL","MALE","MALES","FEMALE")))
  )
  if (length(hdr_candidates) == 0) return(tibble())
  hdr_rel <- hdr_candidates[1]
  dist_hdr_rel <- hdr_rel + 1
  if (dist_hdr_rel > nrow(chunk_df)) return(tibble())
  
  hdr_row  <- chunk_df[hdr_rel, , drop = FALSE]  %>% unlist() %>% as.character() %>% str_squish() %>% toupper()
  dist_row <- chunk_df[dist_hdr_rel, , drop = FALSE] %>% unlist() %>% as.character()
  
  # 2) Identify (sex, year) group starts across columns using the header row
  group_starts <- which(hdr_row %in% c("TOTAL","MALE","MALES","FEMALE"))
  get_year <- function(j) suppressWarnings(as.integer(chunk_df[[hdr_rel, j + 1]]))
  groups <- tibble(
    j   = group_starts,
    sex = recode(hdr_row[group_starts],
                 "TOTAL"="All", "MALE"="Male", "MALES"="Male", "FEMALE"="Female"),
    year = map_int(group_starts, get_year)
  ) %>%
    filter(!is.na(year), year >= year_min, year <= year_max)
  
  if (nrow(groups) == 0) return(tibble())
  
  prov_name <- normalise_province(prov_name_raw)
  
  # 3) For each group, read age column j and district value columns (j+2 .. next_group-1)
  out <- map_dfr(seq_len(nrow(groups)), function(g) {
    j <- groups$j[g]
    yr <- groups$year[g]
    sx <- groups$sex[g]
    j_next <- if (g < nrow(groups)) groups$j[g + 1] else ncol(chunk_df)
    
    # District value columns: skip the per-year age column (j), and the year column (j+1)
    val_cols <- seq.int(j + 2, j_next - 1)
    val_cols <- val_cols[val_cols >= 1 & val_cols <= ncol(chunk_df)]
    
    # If there are no district columns, treat it as province total (use the "year" column as value)
    if (length(val_cols) == 0) {
      val_cols <- j + 1
      dist_names <- prov_name
    } else {
      dist_names <- dist_row[val_cols]
      # If district header row is empty but only one value column, assume province total
      if (all(is.na(dist_names)) && length(val_cols) == 1) dist_names <- prov_name
    }
    
    # Age rows start below the district header, in the group's age-band column j
    search_rows <- seq.int(dist_hdr_rel + 1, nrow(chunk_df))
    if (length(search_rows) == 0) return(tibble())
    age_vec <- as.character(chunk_df[[j]])
    age_vec[is.na(age_vec)] <- ""
    
    is_age <- map_lgl(search_rows, ~ is_age_band(age_vec[.x]))
    if (!any(is_age, na.rm = TRUE)) return(tibble())
    
    # take the first contiguous block of age rows
    first_idx <- which(is_age)[1]
    first <- search_rows[first_idx]
    last  <- first
    repeat {
      nxt <- last + 1
      if (nxt > nrow(chunk_df)) break
      if (!is_age_band(age_vec[nxt])) break
      last <- nxt
    }
    age_rows <- seq.int(first, last)
    
    age_bands <- chunk_df[age_rows, j, drop = TRUE] %>% as.character() %>% str_squish() %>%
      str_replace_all("–", "-") %>% str_replace_all("\\s*-\\s*", "-") %>% str_replace_all("\\s*\\+\\s*$", "+")
    
    vals <- suppressWarnings(chunk_df[age_rows, val_cols, drop = FALSE])
    
    # Build long frame (one row per age × district)
    d <- as_tibble(vals, .name_repair = "minimal") %>%
      mutate(age_band = age_bands) %>%
      pivot_longer(cols = -age_band, names_to = "col_id", values_to = "population") %>%
      mutate(district = dist_names[match(col_id, names(vals))]) %>%
      transmute(
        province = prov_name,
        district = if_else(is.na(district) | str_squish(district) == "", prov_name, str_squish(district)),
        sex = sx,
        year = yr,
        age_band = age_band,
        population = suppressWarnings(readr::parse_number(as.character(population)))
      ) %>%
      filter(!is.na(population))
    
    d
  })
  
  out
}

# -------------- READ & SEGMENT --------------

raw <- read_excel(xlsx_path, sheet = 1, col_names = FALSE)

# Identify province block starts and ends
prov_starts <- which(apply(raw, 1, is_province_row))
if (length(prov_starts) == 0) stop("No province headers detected. Check file layout.")
prov_ends   <- c(prov_starts[-1] - 1, nrow(raw))

# -------------- EXTRACT ALL PROVINCES --------

tidy_all <- map_dfr(seq_along(prov_starts), function(i) {
  start <- prov_starts[i]
  end   <- prov_ends[i]
  prov  <- raw[start, 2, drop = TRUE] %>% as.character()
  chunk <- raw[start:end, ]
  extract_from_province_chunk(chunk, prov, year_min, year_max)
})

# -------------- CLEAN & ENRICH ---------------

tidy_all <- tidy_all %>%
  mutate(
    age_band = age_band %>% str_replace_all("\\s+", "") %>% str_replace_all("–", "-"),
    year     = as.integer(year),
    sex      = factor(sex, levels = c("All","Male","Female")),
    province = if_else(is.na(province) | province=="", NA_character_, province),
    district = if_else(is.na(district) | district=="", province, district)
  ) %>%
  filter(year >= year_min, year <= year_max) %>%
  filter(str_detect(age_band, "^\\d{1,3}(\\+|\\-\\d{1,3})$")) %>%   # keep only true age bands
  bind_cols(parse_age_bounds(.$age_band)) %>%
  filter(!is.na(population), population >= 0)

# Optional: tidy names (title case districts; keep NCD as-is)
tidy_all <- tidy_all %>%
  mutate(
    province = province, # already normalised in extractor
    district = if_else(province == "NATIONAL CAPITAL DISTRICT",
                       "National Capital District",
                       str_to_title(district))
  )

# -------------- OUTPUT & QA ------------------

readr::write_csv(tidy_all, "png_nso_population_tidy.csv")

cat("Saved: png_nso_population_tidy.csv\n")

# Quick QA
qa_summary <- tidy_all %>%
  summarise(
    rows = n(),
    year_range = paste0(min(year), "-", max(year)),
    n_provinces = n_distinct(province),
    n_districts = n_distinct(district),
    n_agebands  = n_distinct(age_band),
    sexes       = paste(levels(sex), collapse = ", ")
  )
print(qa_summary)

# Province-year totals (All sexes)
qa_totals <- tidy_all %>%
  filter(sex == "All") %>%
  group_by(province, year) %>%
  summarise(pop_total = sum(population, na.rm = TRUE), .groups = "drop") %>%
  arrange(province, year)
print(head(qa_totals, 20))

