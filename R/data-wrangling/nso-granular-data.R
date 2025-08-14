# -------------------- packages --------------------
# install.packages(c("readxl","tidyverse","janitor"))
library(readxl)
library(tidyverse)
library(janitor)
library(stringr)

# -------------------- config ----------------------
xlsx_path <- "./data/png-pop-data-by-province-n-age.xlsx"

# -------------------- helpers ---------------------

# Is an age band like "0-4", "5 - 9", "80+", "80 +"
is_age_band <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- ""              # <- NA-safe
  y <- str_squish(y)
  y <- str_replace_all(y, "–", "-")
  # matches "0-4", "5 - 9", "80+"
  str_detect(y, "^\\d{1,3}\\s*(\\+|\\s*-\\s*\\d{1,3})$")
}


# Parse "0-4" / "80+" to (lower, upper)
parse_age_bounds <- function(age_band) {
  y <- age_band %>% as.character() %>% str_to_lower() %>% str_squish()
  y <- y %>% str_replace_all("–", "-") %>% str_replace_all("\\s+", "")
  lower <- as.integer(str_extract(y, "^\\d{1,3}"))
  upper <- ifelse(str_detect(y, "\\+$"),
                  Inf,
                  as.integer(str_extract(y, "(?<=-)\\d{1,3}$")))
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
  # Default: title case for nicer display, keep exact well-known ones
  if (u == "NATIONAL CAPITAL DISTRICT") return("NATIONAL CAPITAL DISTRICT")
  str_to_title(u)
}

# Heuristic: a row is a province title if col2 has a non-age, non-total label
# and the rest of row is basically empty/non-numeric
is_province_row <- function(row_vec) {
  lab <- row_vec[[2]] %>% as.character() %>% str_squish()
  if (is.na(lab) || lab == "") return(FALSE)
  if (toupper(lab) %in% c("TOTAL","MALE","MALES","FEMALE")) return(FALSE)
  if (is_age_band(lab)) return(FALSE)
  
  other <- suppressWarnings(sapply(row_vec[-(1:2)], function(v) !is.na(as.numeric(v))))
  has_numbers <- any(other, na.rm = TRUE)   # <- add na.rm = TRUE
  !has_numbers
}


# Extract a tidy data.frame from one province "chunk"
extract_from_province_chunk <- function(chunk_df, prov_name_raw) {
  
  if (nrow(chunk_df) < 4) return(tibble())
  
  # 1) Find the header row: first row that contains one of Total/Male/Female (usually in col 2)
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
  
  # 2) Identify (sex, year) group starts across columns
  group_starts <- which(hdr_row %in% c("TOTAL","MALE","MALES","FEMALE"))
  # Keep groups that actually have a year immediately to the right
  get_year <- function(j) suppressWarnings(as.integer(chunk_df[[hdr_rel, j + 1]]))
  groups <- tibble(
    j   = group_starts,
    sex = recode(hdr_row[group_starts],
                 "TOTAL"="All", "MALE"="Male", "MALES"="Male", "FEMALE"="Female"),
    year = map_int(group_starts, get_year)
  ) %>% filter(!is.na(year))
  
  if (nrow(groups) == 0) return(tibble())
  
  prov_name <- normalise_province(prov_name_raw)
  
  # 3) For each group, read age column (col j) and district value columns (j+2 .. next_group-1)
  out <- map_dfr(seq_len(nrow(groups)), function(g) {
    j <- groups$j[g]
    yr <- groups$year[g]
    sx <- groups$sex[g]
    j_next <- if (g < nrow(groups)) groups$j[g + 1] else ncol(chunk_df)
    
    # District columns live to the right of year: j+2 .. j_next-1
    val_cols <- seq.int(j + 2, j_next - 1)
    val_cols <- val_cols[val_cols >= 1 & val_cols <= ncol(chunk_df)]
    if (length(val_cols) == 0) {
      # Province with no district columns: values sit in the single column j+1; use province name as "district"
      val_cols <- j + 1
      dist_names <- prov_name
    } else {
      # Use the district header row for names; if NA, fall back to province for single-column cases
      dist_names <- dist_row[val_cols]
      if (all(is.na(dist_names)) && length(val_cols) == 1) dist_names <- prov_name
    }
    
    # Age rows start two rows below header, in the group's age column j
    search_rows <- seq.int(dist_hdr_rel + 1, nrow(chunk_df))
    if (length(search_rows) == 0) return(tibble())
    
    age_vec <- as.character(chunk_df[[j]])
    age_vec[is.na(age_vec)] <- ""  # <- NA-safe for the helper
    
    is_age <- map_lgl(search_rows, ~ is_age_band(age_vec[.x]))
    
    # IMPORTANT: na.rm = TRUE, so NA doesn't poison the IF
    if (!any(is_age, na.rm = TRUE)) return(tibble())
    
    # first TRUE within search_rows
    first_idx <- which(is_age)[1]
    first <- search_rows[first_idx]
    
    # walk down while rows look like age bands (NA treated as FALSE)
    last <- first
    repeat {
      nxt <- last + 1
      if (nxt > nrow(chunk_df)) break
      if (!is_age_band(age_vec[nxt])) break
      last <- nxt
    }
    age_rows <- seq.int(first, last)
    
    
    age_bands <- chunk_df[age_rows, j, drop = TRUE] %>% as.character() %>% str_squish() %>%
      str_replace_all("–", "-") %>% str_replace_all("\\s*-\\s*", "-") %>% str_replace_all("\\s*\\+\\s*$", "+")
    
    vals <- chunk_df[age_rows, val_cols, drop = FALSE]
    
    # Build long frame
    d <- as_tibble(vals, .name_repair = "minimal") %>%
      mutate(age_band = age_bands) %>%
      pivot_longer(cols = -age_band, names_to = "col_id", values_to = "population") %>%
      mutate(district = dist_names[match(col_id, names(vals))]) %>%
      transmute(
        province = prov_name,
        district = if_else(is.na(district) | district == "", prov_name, district) %>% str_squish(),
        sex = sx,
        year = yr,
        age_band = age_band,
        population = suppressWarnings(as.numeric(population))
      ) %>%
      filter(!is.na(population))
    
    d
  })
  
  out
}

# -------------------- read & segment --------------------
raw <- read_excel(xlsx_path, sheet = 1, col_names = FALSE)




# Locate province starts & ends
prov_starts <- which(apply(raw, 1, is_province_row))
if (length(prov_starts) == 0) stop("No province headers detected. Check file layout.")
prov_ends   <- c(prov_starts[-1] - 1, nrow(raw))

# -------------------- smoke test --------------------

# Recompute starts/ends if you restarted your R session
prov_starts <- which(apply(raw, 1, is_province_row))
prov_ends   <- c(prov_starts[-1] - 1, nrow(raw))

# Try the first province chunk only
i <- 1
start <- prov_starts[i]
end   <- prov_ends[i]
prov  <- raw[start, 2, drop = TRUE] %>% as.character()
chunk <- raw[start:end, ]

tmp <- extract_from_province_chunk(chunk, prov)
dplyr::glimpse(tmp)


# -------------------- extract all -----------------------
tidy_all <- map_dfr(seq_along(prov_starts), function(i) {
  start <- prov_starts[i]
  end   <- prov_ends[i]
  prov  <- raw[start, 2, drop = TRUE] %>% as.character()
  chunk <- raw[start:end, ]
  extract_from_province_chunk(chunk, prov)
})

# -------------- CODE STOPS WORKING AFTER THIS------------
# -------------------- clean & enrich --------------------
tidy_all <- tidy_all %>%
  mutate(
    # standardise age band labels
    age_band = age_band %>% str_replace_all("\\s+", "") %>% str_replace_all("–","-"),
    # keep plausible years
    year = as.integer(year),
    # normalise sex labels
    sex = factor(sex, levels = c("All","Male","Female"))
  ) %>%
  filter(year >= 2021, year <= 2030) %>%
  # parse age bounds
  bind_cols(parse_age_bounds(.$age_band)) %>%
  # basic QA: drop weird rows
  filter(!is.na(population), population >= 0)

# -------------------- write output ---------------------
readr::write_csv(tidy_all, "png_nso_population_tidy.csv")

# -------------------- quick QA summaries ----------------
qa1 <- tidy_all %>%
  summarise(
    rows = n(),
    years = paste0(min(year, na.rm = TRUE), "-", max(year, na.rm = TRUE)),
    n_provinces = n_distinct(province),
    n_districts = n_distinct(district),
    n_agebands  = n_distinct(age_band),
    sexes       = paste(sort(unique(as.character(sex))), collapse = ", ")
  )
print(qa1)

# province-year totals (both sexes combined)
qa2 <- tidy_all %>%
  filter(sex == "All") %>%
  group_by(province, year) %>%
  summarise(pop_total = sum(population, na.rm = TRUE), .groups = "drop") %>%
  arrange(province, year)
print(head(qa2, 20))
