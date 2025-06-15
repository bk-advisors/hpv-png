# PNG Census Estimates 2021 - Data Download and Setup

# Install Packages
# install.packages("remotes")
# remotes::install_github("dickoa/rhdx")

# Libraries 

library(rhdx)
library(tidyverse)
library(highcharter)
library(scales)

# Read and process the data
data <- read.csv("./data/agesex_gamma_gaussian_prov.csv") 

# Calculate girls aged 9-14 (using 5-9 and 10-14 age groups)
results <- data %>%
  filter(sex == "f", age %in% c("5 to 9", "10 to 14")) %>%
  mutate(
    age_group = ifelse(age == "5 to 9", "9", "10-14"),
    across(c(total, lower, upper), ~ .x * ifelse(age == "5 to 9", 0.2, 1))
  ) %>%
  group_by(Prov_Name) %>%
  summarise(
    median = sum(total),
    low = sum(lower),
    high = sum(upper)
  ) %>%
  ungroup()

# Add national total
national <- results %>%
  summarise(
    median = sum(median),
    low = sum(low),
    high = sum(high)
  ) %>%
  mutate(Prov_Name = "NATIONAL TOTAL")

results <- bind_rows(results, national)

# Create interactive visualization
hc <- results %>%
  filter(Prov_Name != "NATIONAL TOTAL") %>%
  hchart("bar", hcaes(x = Prov_Name, y = median, low = low, high = high),
         name = "Girls Aged 9-14",
         color = "#5C7EAF") %>%
  hc_title(text = "Estimated Number of Girls Aged 9-14 by Province") %>%
  hc_subtitle(text = "WorldPop Modeled Estimates for Papua New Guinea (2020-2021)") %>%
  hc_xAxis(title = list(text = "Province")) %>%
  hc_yAxis(title = list(text = "Number of Girls"),
           labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 0); }"))) %>%
  hc_tooltip(
    formatter = JS("function() {
      return '<b>' + this.point.Prov_Name + '</b><br>' +
             'Median: <b>' + Highcharts.numberFormat(this.point.median, 0) + '</b><br>' +
             '95% Uncertainty Range: <b>' + Highcharts.numberFormat(this.point.low, 0) + 
             ' - ' + Highcharts.numberFormat(this.point.high, 0) + '</b>';
    }")
  ) %>%
  hc_plotOptions(
    series = list(
      dataLabels = list(
        enabled = TRUE,
        formatter = JS("function() { return Highcharts.numberFormat(this.y, 0); }")
      )
    )
  ) %>%
  hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "viewData")))) %>%
  hc_caption(text = "Note: Estimates include 20% of '5 to 9' cohort to approximate 9-year-olds") %>%
  hc_credits(enabled = TRUE, text = "Source: WorldPop & PNG National Statistical Office")

# Display the chart and results
print(hc)
print(results)

# Export results to CSV
write.csv(results, "./data/png_girls_9_14_nso_estimates.csv", row.names = FALSE)

# Breakdown by specific age group

# Read and process data
data <- read.csv("./data/agesex_gamma_gaussian_prov.csv") 


# Calculate single-year estimates
results <- data %>%
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
  select(Prov_Name, age_single = age_list, total, lower, upper)

# Create national summary
national <- results %>%
  group_by(age_single) %>%
  summarise(
    total = sum(total),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  mutate(Prov_Name = "NATIONAL TOTAL")

# Prepare provincial output
provincial_output <- results %>%
  select(Prov_Name, age_single, total, lower, upper) %>%
  arrange(Prov_Name, age_single)

# Combine national and provincial data
final_output <- bind_rows(
  provincial_output,
  national %>% select(Prov_Name, age_single, total, lower, upper)
)

# Create interactive visualization
hc <- results %>%
  hchart(
    "column", 
    hcaes(x = "age_single", y = total, group = Prov_Name),
    name = "Provincial Estimates"
  ) %>%
  hc_title(text = "Girls Aged 9-14 by Single Year of Age") %>%
  hc_subtitle(text = "WorldPop Modeled Estimates for Papua New Guinea (2020-2021)") %>%
  hc_xAxis(title = list(text = "Age"), 
           categories = 9:14) %>%
  hc_yAxis(title = list(text = "Number of Girls"),
           labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 0); }"))) %>%
  hc_tooltip(
    shared = TRUE,
    headerFormat = "<b>Age {point.key}</b><br>",
    pointFormatter = JS("function() {
      return this.series.name + ': <b>' + 
        Highcharts.numberFormat(this.y, 0) + 
        '</b><br>Uncertainty: ' + Highcharts.numberFormat(this.point.lower, 0) + 
        ' - ' + Highcharts.numberFormat(this.point.upper, 0);
    }")
  ) %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(enabled = FALSE),
      stacking = "normal"
    )
  ) %>%
  hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "viewData")))) %>%
  hc_caption(text = "Note: Age 9 estimated from 20% of '5-9' cohort; Ages 10-14 equally distributed from '10-14' cohort")

# Display and export results
print(hc)
print(final_output)
write.csv(final_output, "png_girls_9_14_single_year_estimates.csv", row.names = FALSE)
