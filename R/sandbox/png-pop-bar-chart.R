# Load required packages
library(tidyverse)
library(scales)

# Read raw population data from Excel
pop_data <- read_excel(
  "./data/png-hpv-pop-modelling-v1-20250606.xlsx",
  sheet = "nso_9to14_by_province_2021"
) %>%
  # Filter out the national total row
  filter(Prov_Name != "NATIONAL TOTAL") %>%
  # Clean and format province names
  mutate(
    Province = str_to_title(Prov_Name),  # Convert to title case (e.g., "CENTRAL" → "Central")
    pop2021 = median  # Use median population estimate
  ) %>%
  select(Province, pop2021)  # Keep relevant columns

# Preview the cleaned data
head(pop_data, 5)

# Handle name matching

pop_data <- pop_data %>%
  mutate(Province = case_when(
    Province == "Northern (Oro)" ~ "Northern",
    TRUE ~ Province
  ))


# Create province name mapping
province_mapping <- data.frame(
  pop_data_name = c(
    "Central", "Central Bougainville", "Chimbu (Simbu)", "East New Britain",
    "East Sepik", "Eastern Highlands", "Enga", "Gulf", "Hela", "Jiwaka",
    "Madang", "Manus", "Milne Bay", "Morobe", "National Capital District",
    "New Ireland", "North Bougainville", "Northern", "South Bougainville",
    "Southern Highlands", "West New Britain", "West Sepik", "Western",
    "Western Highlands"
  ),
  shapefile_name = c(
    "Central Province", "Autonomous Region of Bougainville", "Chimbu (Simbu) Province",
    "East New Britain Province", "East Sepik Province", "Eastern Highlands Province",
    "Enga Province", "Gulf Province", "Hela Province", "Jiwaka Province",
    "Madang Province", "Manus Province", "Milne Bay Province", "Morobe Province",
    "National Capital District", "New Ireland Province", "Autonomous Region of Bougainville",
    "Northern (Oro) Province", "Autonomous Region of Bougainville", "Southern Highlands Province",
    "West New Britain Province", "West Sepik (Sandauri) Province", "Western Province",
    "Western Highlands Province"
  )
)

# Prepare population data with simplified names
pop_data_plot <- pop_data %>%
  left_join(province_mapping, by = c("Province" = "pop_data_name")) %>%
  mutate(
    # Clean province names for display
    Province_clean = gsub(" Province$", "", shapefile_name),
    Province_clean = gsub(" \\(.*\\)", "", Province_clean),
    Province_clean = gsub(" Autonomous Region of", "", Province_clean),
    
    # Highlight max and min values
    highlight = case_when(
      pop2021 == max(pop2021) ~ "Highest",
      pop2021 == min(pop2021) ~ "Lowest",
      TRUE ~ "Normal"
    )
  ) %>%
  arrange(desc(pop2021))

# Aggregate Bougainville provinces
pop_data_plot_agg <- pop_data_plot %>%
  group_by(shapefile_name) %>%
  summarize(pop2021 = sum(pop2021, na.rm = TRUE)) %>%
  mutate(
    # Clean province names for display
    Province_clean = gsub(" Province$", "", shapefile_name),
    Province_clean = gsub(" \\(.*\\)", "", Province_clean),
    Province_clean = gsub(" Autonomous Region of", "", Province_clean),
    
    # Highlight max and min values
    highlight = case_when(
      pop2021 == max(pop2021) ~ "Highest",
      pop2021 == min(pop2021) ~ "Lowest",
      TRUE ~ "Normal"
    )
  ) %>%
  arrange(desc(pop2021))



# Create bar plot
ggplot(pop_data_plot_agg, 
       aes(x = reorder(Province_clean, pop2021), 
           y = pop2021, 
           fill = highlight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = comma(round(pop2021))), 
            hjust = -0.1, 
            size = 3) +
  scale_fill_manual(values = c("Highest" = "#E74C3C", 
                               "Lowest" = "#3498DB", 
                               "Normal" = "#7FB3D5")) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +  # Horizontal bars
  labs(title = "Population of Girls Aged 9-14 by Province (2021)",
       subtitle = "HPV Vaccine Target Group",
       x = "",
       y = "Population",
       caption = "Source: PNG National Statistical Office") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(margin = margin(b = 15)),
        axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank())
