library(tidyverse)

# Load and prepare data
pop_data <- read_csv("./data/png_unwpp_est9_14_2020_2023.csv") %>%
  mutate(age_group = "9-14")  # Create age group identifier

#write csv file for external reference
write.csv(pop_data, "./data/png_girls_9_14_unwpp_2020_2023.csv", row.names = FALSE)

# Calculate annual totals
annual_totals <- pop_data %>%
  group_by(year) %>%
  summarize(total_pop = sum(popF))

# Calculate growth rates
growth_rates <- annual_totals %>%
  arrange(year) %>%
  mutate(
    prev_total = lag(total_pop),
    growth_rate = (total_pop - prev_total) / prev_total
  ) %>%
  filter(!is.na(growth_rate))  # Remove NA for first year

# Calculate average growth rate
avg_growth_rate <- mean(growth_rates$growth_rate)

# Project future populations
projections <- tibble(
  year = 2024:2025,
  total_pop = annual_totals$total_pop[annual_totals$year == 2023] * 
    (1 + avg_growth_rate)^(1:2)
) %>%
  mutate(
    growth_rate = avg_growth_rate,
    projection_type = "Projected"
  )

# Combine actuals and projections
all_data <- annual_totals %>%
  mutate(
    growth_rate = c(NA, growth_rates$growth_rate),
    projection_type = "Actual"
  ) %>%
  bind_rows(projections)

# Print results
cat("Population Projection Analysis\n")
cat("==============================\n")
cat("Average Annual Growth Rate:", round(avg_growth_rate * 100, 2), "%\n\n")

cat("Yearly Projections:\n")
all_data %>%
  mutate(total_pop = round(total_pop)) %>%
  select(Year = year, Population = total_pop, 
         `Growth Rate` = growth_rate, Type = projection_type) %>%
  print()

# Visualization
ggplot(all_data, aes(x = year, y = total_pop)) +
  geom_line(aes(color = "Actual"), data = filter(all_data, projection_type == "Actual"), 
            linewidth = 1.5) +
  geom_line(aes(color = "Projected"), data = filter(all_data, projection_type == "Projected"), 
            linewidth = 1.5, linetype = "dashed") +
  geom_point(aes(fill = projection_type), shape = 21, size = 3) +
  geom_text(aes(label = round(total_pop)), vjust = -1, size = 4, fontface = "bold") +
  scale_color_manual(values = c("Actual" = "#1f77b4", "Projected" = "#ff7f0e")) +
  scale_fill_manual(values = c("Actual" = "#1f77b4", "Projected" = "#ff7f0e")) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "PNG Female Population Projection (Ages 9-14)",
       subtitle = paste("Based on average annual growth rate of", 
                        round(avg_growth_rate * 100, 2), "% (2020-2023)"),
       x = "Year",
       y = "Population",
       caption = "Source: UN World Population Prospects 2023") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12))
