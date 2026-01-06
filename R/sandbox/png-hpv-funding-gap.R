library(tidyverse)

# Updated data with new HPV budget value
funding_data <- data.frame(
  Category = factor(c("HPV Introduction Budget", "Government Commitment"), 
                    levels = c("HPV Introduction Budget", "Government Commitment")),
  Amount = c(38.6, 6.2),  # Updated HPV budget to 38.6M
  Description = c("Total Required", "Funded by PNG Government")
)

# Calculate the funding gap
funding_gap <- 38.6 - 6.2  # 32.4 million kina

# Create the bar chart with vertical gap annotation
ggplot(funding_data, aes(x = Category, y = Amount, fill = Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Amount, "M")), 
            vjust = -0.5, 
            size = 6,
            fontface = "bold",
            color = "black") +
  # Add vertical gap annotation
  annotate("segment",
           x = 1.5, xend = 1.5,
           y = 6.2, yend = 38.6,
           color = "#F18F01",
           size = 1.5,
           arrow = arrow(ends = "both", length = unit(0.3, "cm"))) +
  # Add gap label positioned vertically along the arrow
  annotate("text",
           x = 1.65,
           y = (6.2 + 38.6) / 2,  # Middle point of the arrow
           label = paste0("Gap:\n", funding_gap, "M"),
           color = "#F18F01",
           size = 5,
           fontface = "bold",
           hjust = 0,
           lineheight = 0.8) +
  scale_fill_manual(values = c("HPV Introduction Budget" = "#2E86AB",
                               "Government Allocation" = "#A23B72")) +
  labs(
    title = "HPV Vaccine Funding Analysis (2026)",
    subtitle = "Papua New Guinea (in million kina)",
    x = NULL,
    y = "Amount (Million Kina)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 20, 15, 15)  # Added right margin for gap label
  ) +
  ylim(0, 42)  # Adjusted to accommodate the larger value

# Alternative: Simplified version with cleaner vertical annotation
ggplot(funding_data, aes(x = Category, y = Amount, fill = Category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Amount, "M")), 
            vjust = -0.5, 
            size = 5.5,
            fontface = "bold") +
  # Vertical bracket for gap
  annotate("segment",
           x = 1.5, xend = 1.5,
           y = 6.2, yend = 38.6,
           color = "#F18F01",
           size = 1,
           lineend = "round") +
  # Arrow heads
  annotate("segment",
           x = 1.45, xend = 1.5,
           y = 6.2, yend = 6.2,
           color = "#F18F01",
           size = 1) +
  annotate("segment",
           x = 1.45, xend = 1.5,
           y = 38.6, yend = 38.6,
           color = "#F18F01",
           size = 1) +
  # Gap label
  annotate("text",
           x = 1.6,
           y = 22.4,  # Middle point (6.2 + 38.6)/2 = 22.4
           label = paste0("Funding Gap\n", funding_gap, "M"),
           color = "#F18F01",
           size = 5,
           fontface = "bold",
           hjust = 0,
           lineheight = 0.85) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
  labs(
    title = "HPVv Introduction Funding for 2026",
    subtitle = "Papua New Guinea (in million kina)",
    x = NULL,
    y = "Million Kina"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  ) +
  ylim(0, 42)

# Version with the gap annotation to the side
ggplot(funding_data, aes(x = Category, y = Amount, fill = Category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Amount, "M")), 
            vjust = -0.5, 
            size = 5.5,
            fontface = "bold") +
  # Add a vertical line connecting the two levels
  annotate("segment",
           x = 1.05, xend = 1.05,  # Just to the right of first bar
           y = 6.2, yend = 38.6,
           color = "#F18F01",
           size = 1.2,
           arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  # Gap label positioned beside the line
  annotate("text",
           x = 1.15,
           y = 22.4,
           label = paste0("Gap:\n", funding_gap, "M"),
           color = "#F18F01",
           size = 5,
           fontface = "bold",
           hjust = 0,
           lineheight = 0.8) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
  labs(
    title = "HPV Vaccine Funding Gap Analysis",
    subtitle = "2026 Budget Requirements - Papua New Guinea",
    x = NULL,
    y = "Million Kina"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  ) +
  ylim(0, 42)

# Console output
cat("\nHPV Vaccine Funding Analysis - Papua New Guinea (2026)\n")
cat("========================================================\n")
cat(sprintf("%-25s %10.1f million kina\n", "Total Budget Required:", 38.6))
cat(sprintf("%-25s %10.1f million kina\n", "Government Allocation:", 6.2))
cat(sprintf("%-25s %10.1f million kina\n", "Funding Gap:", funding_gap))
cat("========================================================\n")