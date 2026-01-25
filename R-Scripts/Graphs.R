analysis_data |>
  filter(set == "LEA", full_rarity == "C") |>
  summarise(average_price = mean(cur_price, na.rm = TRUE)) |>
  print(alpha_common_avg) #avg alpha common price

#Chart for avg price against print runs 
# 1. Prepare the summary data
# We include print_run in group_by so it's available for sorting the x-axis
analysis_data |>
  group_by(set_rarity, print_run) |>
  summarise(avg_price = mean(cur_price, na.rm = TRUE), .groups = "drop") |>

# 2. Create the graph
# The x-axis shows the label, but the position is determined by print_run
ggplot(aes(x = reorder(set_rarity, print_run), y = avg_price, fill = set_rarity)) +
  geom_col() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = "Average Price by Set and Rarity",
    subtitle = "Sorted by Print Run Size (Scarcity)",
    x = "Set & Rarity",
    y = "Average Price ($)"
  )



analysis_data |>
  mutate(
    # 1. Remove $ and , symbols
    # 2. Convert to numeric
    old_price = as.numeric(str_remove_all(old_price, "[\\$,]")),
    cur_price = as.numeric(str_remove_all(cur_price, "[\\$,]"))
  )|>
ggplot(aes(x = old_price, y = cur_price, color = rarity)) +
  # 1. Add the data points
  geom_point(alpha = 0.5) +
  
  # 2. Add a 45-degree reference line
  # If a point is above this line, the price went UP. Below means it went DOWN.
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  
  # 3. Apply log scales (Highly recommended for Magic prices)
  # This prevents expensive cards from squishing the cheap cards into the corner
  scale_x_log10(labels = scales::dollar) +
  scale_y_log10(labels = scales::dollar) +
  
  theme_minimal() +
  labs(
    title = "Price Evolution: Old Price vs. Current Price",
    subtitle = "Dashed line represents no change in value",
    x = "Old Price (Historical)",
    y = "Current Price (Market)",
    color = "Rarity"
  )


#Set and rarity percentage growth with benchmark
# 1. Clean data and calculate percentage increase
analysis_data |>
  mutate(
    # Convert to numeric by removing $ and ,
    old_num = as.numeric(str_remove_all(old_price, "[\\$,]")),
    cur_num = as.numeric(str_remove_all(cur_price, "[\\$,]")),
    # Calculate the percentage growth
    pct_increase = ((cur_num - old_num) / old_num) * 100
  ) |>

# 2. Summarize the average increase per set and rarity
  group_by(set_rarity) |>
  summarise(avg_growth = mean(pct_increase, na.rm = TRUE), .groups = "drop") |>

# 3. Create the bar chart
ggplot(aes(x = reorder(set_rarity, avg_growth), y = avg_growth)) +
  geom_col(aes(fill = avg_growth > 2250)) + # Colors bars differently if they beat the S&P
  geom_hline(yintercept = 2250, linetype = "dashed", color = "red", size = 1.2) +
  annotate("text", x = 1.5, y = 2300, label = "S&P 500 (+2,250%)", color = "red", fontface = "bold", hjust = 0) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#2ca25f", "FALSE" = "#636363")) +
  theme_minimal() +
  labs(
    title = "Average Growth vs. S&P 500 (1994-2026)",
    subtitle = "Green bars outperformed the stock market benchmark",
    x = "Set and Rarity",
    y = "Average Percentage Increase (%)",
    fill = "Beat the Market"
  )


summary_data <- analysis_data |>
  mutate(
    old_num = as.numeric(str_remove_all(old_price, "[\\$,]")),
    cur_num = as.numeric(str_remove_all(cur_price, "[\\$,]")),
    pct_increase = ((cur_num - old_num) / old_num) * 100
  ) |>
  # str_detect is safer; it looks for "2ED" regardless of case or extra spaces
  filter(str_detect(set, regex("2ED", ignore_case = TRUE))) |>
  # This catches "R", "Rare", "rare", etc.
  filter(str_detect(rarity, "rare")) |>
  filter(!is.na(pct_increase))

# 3. Only plot if we actually have data
if(nrow(summary_data) > 0) {
  
  summary_data <- summary_data |>
    mutate(performance = if_else(pct_increase > 2250, "Beat S&P", "Below S&P"))

  ggplot(summary_data, aes(x = performance, fill = performance)) +
    geom_bar() +
    stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) +
    scale_fill_manual(values = c("Beat S&P" = "#2ca25f", "Below S&P" = "#636363")) +
    theme_minimal() +
    labs(title = "Unlimited (2ED) Rares vs. S&P 500")
    
} else {
  print("No data matched the filter. Check 'unique(analysis_data$set)' to see the correct codes.")
}
