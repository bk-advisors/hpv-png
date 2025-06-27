# Load required packages
library(tidyverse)
library(scales)

## Slide 1: In 2022, an estimated 348,709 women died from Cervical Cancer, globally. More than half of these deaths occurred in the Asia-Pacific region

# Read the dataset
data <- read_csv("./data/dataset-absolute-numbers-mort-females-in-2022-cervix-uteri.csv")

# Define Asia-Pacific countries (based on common geographical definitions)
asia_pacific_countries <- c(
  "AFG", "AUS", "BGD", "BTN", "BRN", "KHM", "CHN", "FJI", "IND", "IDN", 
  "JPN", "KAZ", "PRK", "KOR", "KGZ", "LAO", "MYS", "MDV", "MNG", "MMR", 
  "NPL", "NZL", "PAK", "PNG", "PHL", "SGP", "LKA", "TJK", "THA", "TLS", 
  "TKM", "UZB", "VUT", "VNM", "WSM"
)

# Calculate totals
global_total <- sum(data$Total, na.rm = TRUE)
asia_pacific_total <- sum(data$Total[data$`Alpha‑3 code` %in% asia_pacific_countries], na.rm = TRUE)
other_regions_total <- global_total - asia_pacific_total

# Create summary data for visualization
summary_data <- data.frame(
  Region = c("Asia-Pacific", "Other Regions"),
  Deaths = c(asia_pacific_total, other_regions_total),
  Percentage = c(
    round((asia_pacific_total / global_total) * 100, 1),
    round((other_regions_total / global_total) * 100, 1)
  )
)

# Print key statistics
cat("=== CERVICAL CANCER DEATHS IN 2022 ===\n")
cat("Global Total:", format(global_total, big.mark = ","), "deaths\n")
cat("Asia-Pacific:", format(asia_pacific_total, big.mark = ","), "deaths (", 
    summary_data$Percentage[1], "%)\n")
cat("Other Regions:", format(other_regions_total, big.mark = ","), "deaths (", 
    summary_data$Percentage[2], "%)\n")

# Create a professional pie chart
p1 <- ggplot(summary_data, aes(x = "", y = Deaths, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Asia-Pacific" = "#D32F2F", "Other Regions" = "#757575")) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "Global Cervical Cancer Deaths in 2022",
    subtitle = paste0("Total: ", format(global_total, big.mark = ","), " deaths worldwide")
  ) +
  geom_text(aes(label = paste0(Region, "\n", format(Deaths, big.mark = ","), 
                               " (", Percentage, "%)")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white")

# Create a bar chart alternative
p2 <- ggplot(summary_data, aes(x = reorder(Region, -Deaths), y = Deaths, fill = Region)) +
  geom_col(width = 0.6, color = "white", size = 1) +
  scale_fill_manual(values = c("Asia-Pacific" = "#E41A1C", "Other Regions" = "#377EB8")) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "Cervical Cancer Deaths by Region in 2022",
    subtitle = paste0("Total global deaths: ", format(global_total, big.mark = ",")),
    y = "Number of Deaths"
  ) +
  geom_text(aes(label = paste0(format(Deaths, big.mark = ","), " deaths\n(", 
                               Percentage, "%)")),
            vjust = -0.5, size = 4, fontface = "bold")

# Display both charts
print(p1)
print(p2)

# Highlight PNG specifically
png_deaths <- data$Total[data$`Alpha‑3 code` == "PNG"]
cat("\n=== PAPUA NEW GUINEA SPECIFIC DATA ===\n")
cat("PNG Cervical Cancer Deaths in 2022:", png_deaths, "deaths\n")
cat("PNG as % of Asia-Pacific total:", round((png_deaths / asia_pacific_total) * 100, 2), "%\n")
cat("PNG as % of global total:", round((png_deaths / global_total) * 100, 3), "%\n")

# Top 10 Asia-Pacific countries by deaths
asia_pacific_data <- data %>%
  filter(`Alpha‑3 code` %in% asia_pacific_countries) %>%
  arrange(desc(Total)) %>%
  head(10)

cat("\n=== TOP 10 ASIA-PACIFIC COUNTRIES BY CERVICAL CANCER DEATHS ===\n")
for(i in 1:nrow(asia_pacific_data)) {
  cat(i, ". ", asia_pacific_data$Label[i], ": ", 
      format(asia_pacific_data$Total[i], big.mark = ","), " deaths\n")
}

## Slide 2: Papua New Guinea, a low-middle-income country in the Asia Pacific region, has the second highest (age-standardized) cervical cancer mortality rate, at an estimated 19.9 per 100,000 women
# Load required packages

library(ggrepel)


# Create dataset from Table 1 (Page 36)
cervical_data <- tibble(
  country = c("Fiji", "Papua New Guinea", "Solomon Islands", "Vanuatu", "Myanmar", 
              "Indonesia", "Maldives", "India", "Guam", "Mongolia"),
  mortality = c(22.5, 19.9, 17.9, 13.6, 13.4, 13.2, 12.9, 11.2, 11.2, 9.6),
  incidence = c(35.0, 27.8, 26.2, 18.2, 21.4, 23.3, 22.6, 17.7, 19.0, 20.2)
) %>%
  mutate(
    highlight = if_else(country == "Papua New Guinea", "PNG", "Other"),
    # CORRECTED: Removed extra comma in paste0
    label = paste0(country, "\n", mortality, " (M), ", incidence, " (I)")
  )

# Create visualization
ggplot(cervical_data, aes(x = reorder(country, -mortality), 
                          y = mortality, 
                          fill = highlight)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(mortality)), 
            vjust = -0.5, 
            size = 4, 
            fontface = "bold") +
  scale_fill_manual(values = c("PNG" = "#E41A1C", "Other" = "#377EB8")) +
  scale_y_continuous(limits = c(0, 27), 
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Cervical Cancer Mortality in Asia-Pacific (2022)",
    subtitle = "Age-standardized mortality rate per 100,000 women",
    x = "",
    y = "Mortality Rate",
    caption = "Source: Ueda (2024) | Journal of Obstetrics and Gynaecology Research"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(5,0,20,0)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(0,15,0,0)),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0)
  )


## Slide 3: Vaccination is the most powerful tool to reduce cervical cancer deaths, and the HPV vaccine translates to about 17.4 deaths averted per 1,000 vaccinated

