library(tidyverse)
library(janitor)
library(showtext)
library(ggrepel)
library(scales)

# Add fonts for title and subtitle
font_add_google("Roboto Condensed", "font2")  # For subtitle
font_add_google("Lora", "font1")  # For title

# Enable showtext
showtext_auto()

# Define theme
custom_theme <- theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, family = "font1"),
    plot.subtitle = element_text(size = 12, color = "#333333", hjust = 0.5, family = "font2"),
    legend.position = "none",
    panel.border = element_rect(color = "darkgray", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.background = element_rect(fill = "white"),
    legend.margin = margin(t = 5, r = 10, b = 5, l = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "#999999"),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, family = "font2"),
    axis.title.x = element_text(margin = margin(7.5, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 7.5, 0, 0))
  )

# Read data and clean names
df <- read.csv("/Users/theodosiou/Documents/Projects/tidytuesday/2025/20250407_Medical/Timely_and_Effective_Care-State.csv") %>%
  as_tibble() %>%
  janitor::clean_names()

# Process data
data <- df %>%
  mutate(
    score = na_if(score, "Not Available") %>% as.double(),
    across(ends_with("_date"), lubridate::mdy)
  )

# Income data frame
income_df <- data.frame(
  full_state = c("Maryland", "District of Columbia", "Massachusetts", "New Jersey", "New Hampshire",
                 "California", "Hawaii", "Washington", "Connecticut", "Colorado", "Virginia", "Utah",
                 "Alaska", "Minnesota", "New York", "Rhode Island", "Vermont", "Illinois", "Oregon",
                 "Delaware", "Arizona", "Pennsylvania", "Wisconsin", "Texas", "Nebraska", "Georgia",
                 "North Dakota", "Idaho", "Nevada", "South Dakota", "Iowa", "Wyoming", "Maine", "Kansas",
                 "Michigan", "Montana", "Florida", "Indiana", "Ohio", "North Carolina", "Missouri",
                 "Tennessee", "South Carolina", "Oklahoma", "Kentucky", "New Mexico", "Alabama", "Arkansas",
                 "Louisiana", "West Virginia", "Mississippi"),
  Median_Income = c(90203, 90088, 89645, 89296, 88465, 84907, 84857, 84247, 83771, 82254, 80963, 79449,
                    77845, 77720, 74314, 74008, 72431, 72205, 71562, 71091, 69056, 68957, 67125, 66963,
                    66817, 66559, 66519, 66474, 66274, 66143, 65600, 65204, 64767, 64124, 63498, 63249,
                    63062, 62743, 62262, 61972, 61847, 59695, 59318, 55826, 55573, 53992, 53913, 52528,
                    52087, 51248, 48716),
  state = c("MD", "DC", "MA", "NJ", "NH", "CA", "HI", "WA", "CT", "CO", "VA", "UT", "AK", "MN", "NY",
            "RI", "VT", "IL", "OR", "DE", "AZ", "PA", "WI", "TX", "NE", "GA", "ND", "ID", "NV", "SD", "IA",
            "WY", "ME", "KS", "MI", "MT", "FL", "IN", "OH", "NC", "MO", "TN", "SC", "OK", "KY", "NM", "AL",
            "AR", "LA", "WV", "MS"),
  stringsAsFactors = FALSE
)

# Join data and income information
plot_data <- left_join(data, income_df, by = "state") %>%
  filter(complete.cases(.))

# Filter and prepare the data
plot_data <- plot_data %>%
  filter(measure_id == "OP_18b") %>%
  select(state, measure_id, score, Median_Income, full_state) %>%
  pivot_wider(names_from = measure_id, values_from = score)

# Calculate median values for quadrant division
median_x <- median(plot_data$OP_18b, na.rm = TRUE)
median_y <- median(plot_data$Median_Income, na.rm = TRUE)

# Assign quadrants based on median values
plot_data <- plot_data %>%
  mutate(quadrant = case_when(
    OP_18b >= median_x & Median_Income >= median_y ~ "Q1: Long ED waits, High median income",
    OP_18b < median_x & Median_Income >= median_y ~ "Q2: Efficient ED, High median income",
    OP_18b < median_x & Median_Income < median_y ~ "Q3: Efficient ED, Low median income",
    OP_18b >= median_x & Median_Income < median_y ~ "Q4: Long ED waits, Low median income",
    TRUE ~ NA_character_
  ))

# Select representative states for labeling
# Helper function for labeled points
get_label_states <- function(df, quadrant_prefix, order_col, n = 2, top = TRUE) {
  df %>%
    filter(startsWith(quadrant, quadrant_prefix)) %>%
    {
      if (top) slice_max(., {{ order_col }}, n = n)
      else     slice_min(., {{ order_col }}, n = n)
    } %>%
    mutate(label_state = state)
}

# Use helper with custom logic per quadrant
label_data <- bind_rows(
  get_label_states(plot_data, "Q1", OP_18b, n = 2, top = TRUE),         # Q1: Top ED Waits
  get_label_states(plot_data, "Q2", Median_Income, n = 2, top = TRUE),  # Q2: High Income
  get_label_states(plot_data, "Q3", Median_Income, n = 2, top = FALSE), # Q3: Low Income
  get_label_states(plot_data, "Q4", OP_18b, n = 2, top = TRUE)          # Q4: Top ED Waits
) %>%
  distinct(full_state)
# Add label column to main data
plot_data <- plot_data %>%
  mutate(
    label_state = ifelse(full_state %in% label_data$full_state, full_state, NA),
    label_y = Median_Income - 1000
  )

# Create the plot
ggplot(plot_data, aes(x = OP_18b, y = Median_Income)) +
  geom_vline(xintercept = median_x, color = "grey", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = median_y, color = "grey", linetype = "dashed", size = 0.5) +
  geom_point(shape = 21, color = "white", aes(fill = quadrant, size = OP_18b), alpha = 0.9) +
  geom_text_repel(aes(y = label_y, label = label_state, color = quadrant), size = 3, family = "font1", fontface = "bold",
                  nudge_x = 0.3, nudge_y = -4, hjust = 0.5, point.size = NA, segment.color = NA, box.padding = 0.1,
                  min.segment.length = 0, seed = 42, na.rm = TRUE) +
  scale_fill_manual(values = c("Q1: Long ED waits, High median income" = "#01A7C0",
                               "Q2: Efficient ED, High median income" = "#BABA21",
                               "Q3: Efficient ED, Low median income" = "#F19F00",
                               "Q4: Long ED waits, Low median income" = "#DB5152")) +
  scale_color_manual(values = c("Q1: Long ED waits, High median income" = "#01A7C0",
                                "Q2: Efficient ED, High median income" = "#BABA21",
                                "Q3: Efficient ED, Low median income" = "#F19F00",
                                "Q4: Long ED waits, Low median income" = "#DB5152")) +
  scale_size_continuous(range = c(4, 10)) +
  guides(size = "none") +
  labs(
    title = "Wealthier States Have Longer Emergency Department (ED) Wait Times",
    subtitle = "Quadrant analysis shows efficient EDs in both high and low income areas (Q2-top left, Q3-bottom left),\n with inefficiency across income levels (Q1-top right, Q4-bottom right)",
    x = "Average ED Wait (minutes)", y = "State Median Income ($)",
    caption = "Source: Centers for Medicare & Medicaid Services Data (USA) · Graphic: Loukas Theodosiou"
  ) +
  custom_theme +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linetype = "dashed", size = 1, color = "#333333") +
  scale_y_continuous(limits = c(45000, 95000), breaks = seq(45000, 95000, by = 10000), labels = label_number(scale = 1e-3, suffix = "k")) +
  scale_x_continuous(limits = c(100, 320), breaks = seq(100, 320, by = 40))

ggsave("ED_waits_by_income_nice_size.pdf",
       plot = last_plot(),
       width = 8.25,
       height = 6.25,
       units = "in",
       dpi = 300,
       device = cairo_pdf)
