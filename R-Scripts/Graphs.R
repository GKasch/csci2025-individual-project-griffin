mtg_data <- read_csv("analysis_data.csv")

mtg_data |>
  filter(set == "LEA", full_rarity == "C") |>
  summarise(average_price = mean(cur_price, na.rm = TRUE)) |>
  print(mtg_data) #avg alpha common price

#Chart for avg price against print runs 
# 1. Prepare the summary data
# We include print_run in group_by so it's available for sorting the x-axis
mtg_data |>
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



mtg_data |>
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
mtg_data |>
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


summary_data <- mtg_data |>
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
  summary_data <- summary_data |>
    mutate(performance = if_else(pct_increase > 2250, "Beat S&P", "Below S&P"))

  ggplot(summary_data, aes(x = performance, fill = performance)) +
    geom_bar() +
    stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) +
    scale_fill_manual(values = c("Beat S&P" = "#2ca25f", "Below S&P" = "#636363")) +
    theme_minimal() +
    labs(title = "Unlimited (2ED) Rares vs. S&P 500")
    

library(tidyverse)

# 1. Load your data
mtg_appdata <- read_csv("analysis_data.csv")

# 2. Define your specific portfolio (Beta Rares)
target_edition <- "LEB"  # 'LEB' is the standard set code for Beta
target_rarity  <- "rare"

# 3. Filter for your portfolio
portfolio_data <- mtg_appdata %>%
  filter(
    set == target_edition,
    rarity == target_rarity,
    !is.na(pct_increase)
  )

# 4. Calculate Aggregate Stats
# This tells you the total "alpha" (how much better/worse you did than the S&P)
portfolio_summary <- portfolio_data %>%
  summarize(
    avg_pct_increase = mean(pct_increase),
    win_rate = mean(performance == "Beat S&P") * 100,
    card_count = n()
  )

print(portfolio_summary)

# 5. Visualize the Portfolio Makeup
ggplot(portfolio_data, aes(x = performance, fill = performance)) +
  geom_bar() +
  stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5, size = 6) +
  scale_fill_manual(values = c("Beat S&P" = "#2ca25f", "Below S&P" = "#d73027")) +
  theme_minimal() +
  labs(
    title = paste("Portfolio Analysis:", target_edition, target_rarity, "vs S&P 500"),
    subtitle = paste0("Average Return: ", round(portfolio_summary$avg_pct_increase, 1), "%"),
    caption = paste0("Based on ", portfolio_summary$card_count, " cards"),
    x = "Did the card outperform the S&P 500?",
    y = "Number of Cards"
  )




