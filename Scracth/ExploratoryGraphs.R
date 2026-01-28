mtg_data <- read_csv("analysis_data.csv")

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
beta_portfolio <- mtg_data %>%
  filter(
    set == "2ED", 
    rarity == "uncommon", 
    !is.na(pct_increase)
  )

# 2. Calculate the Portfolio Average Return
portfolio_avg_return <- mean(beta_portfolio$pct_increase, na.rm = TRUE)

# 3. Define your S&P 500 Benchmark 
# (Replace 250 with the actual S&P 500 return % for your specific date range)
sp500_return <- 2250 

# 4. Create a summary data frame for the chart
plot_comparison <- data.frame(
  Group = c("Beta Rares Portfolio", "S&P 500 Index"),
  Return = c(portfolio_avg_return, sp500_return)
)

# 5. Create the Chart
ggplot(plot_comparison, aes(x = Group, y = Return, fill = Group)) +
  geom_col(width = 0.5) +
  # Add percentage labels on top of the bars
  geom_text(aes(label = paste0(round(Return, 1), "%")), 
            vjust = -0.8, size = 6, fontface = "bold") +
  # Use your existing color scheme
  scale_fill_manual(values = c("Beta Rares Portfolio" = "#2ca25f", "S&P 500 Index" = "#636363")) +
  theme_minimal() +
  expand_limits(y = max(plot_comparison$Return) * 1.2) + # Add space for labels
  labs(
    title = "Investment Comparison: Beta Rares vs. S&P 500",
    subtitle = paste("Based on an average of", nrow(beta_portfolio), "unique cards"),
    y = "Total Return Percentage (%)",
    x = ""
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 12)
  )

library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# 1. Clean the data and filter for the specific card
# Replace "Benalish Hero" with any card name you want to track
target_card <- "Benalish Hero"

card_history <- mtg_data |>
  # Ensure prices are numeric
  mutate(
    cur_num = as.numeric(str_remove_all(cur_price, "[\\$,]")),
    print_run = as.numeric(print_run)
  ) |>
  # Filter for the specific card name
  filter(str_detect(card_name, regex(target_card, ignore_case = TRUE))) |>
  filter(!is.na(print_run), !is.na(cur_num))

# 2. Create the Scatter Plot
ggplot(card_history, aes(x = print_run, y = cur_num)) +
  # Draw a line connecting the printings to show the "trend"
  geom_path(color = "grey", linetype = "dotted") +
  
  # Large points for each printing
  geom_point(aes(color = set), size = 5) +
  
  # Label each point with the Set Name
  geom_text(aes(label = set), vjust = -1.2, fontface = "bold") +
  
  # Improve scales
  # Using log scale for Print Run is helpful if comparing Alpha (1,100) to Revised (Millions)
  scale_x_log10(labels = label_comma()) + 
  scale_y_continuous(labels = label_dollar()) +
  
  theme_minimal() +
  labs(
    title = paste("Price vs. Scarcity:", target_card),
    subtitle = "Relationship between total print run and current market price",
    x = "Total Print Run (Log Scale)",
    y = "Current Price ($)",
    color = "Expansion Set"
  )


# 1. Clean and filter the data for the target sets
bench_data <- mtg_data |>
  mutate(
    cur_num = as.numeric(str_remove_all(cur_price, "[\\$,]")),
    print_num = as.numeric(str_remove_all(as.character(print_run), "[,]"))
  ) |>
  filter(
    str_detect(card_name, regex("Benalish Hero", ignore_case = TRUE)),
    set %in% c("LEA", "LEB", "2ED", "3ED")
  )

# 2. Extract Alpha values to use as the denominator
lea_price <- bench_data |> filter(set == "LEA") |> pull(cur_num)
lea_print <- bench_data |> filter(set == "LEA") |> pull(print_num)

# 3. Calculate the multipliers relative to Alpha
# Print Run Multiplier (X): How many times more supply than Alpha?
# Price Multiplier (Y): What percentage of the Alpha price is this?
plot_data <- bench_data |>
  mutate(
    print_run_multiplier = print_num / lea_print,
    price_multiplier = cur_num / lea_price,
    expected_price_ratio = 1 / print_run_multiplier # The "Perfect Scarcity" prediction
  )

# 4. Create the Graph
ggplot(plot_data, aes(x = print_run_multiplier)) +
  # The "Perfect Inverse" Curve (What happens if price = 1/supply)
  stat_function(fun = function(x) 1/x, aes(color = "Predicted (1/Supply)"), 
                linetype = "dashed", size = 1) +
  
  # The Actual Data Points
  geom_point(aes(y = price_multiplier, fill = set), size = 6, shape = 21, color = "black") +
  geom_text(aes(y = price_multiplier, label = set), vjust = -1.5, fontface = "bold") +
  
  # Formatting
  scale_x_log10(breaks = c(1, 3, 15, 100, 300), labels = label_comma(suffix = "x")) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.1)) +
  scale_color_manual(values = c("Predicted (1/Supply)" = "red")) +
  theme_minimal() +
  labs(
    title = "Price vs. Supply: Benalish Hero (Alpha Benchmark)",
    subtitle = "Does price drop as fast as supply increases? (100% = LEA Price)",
    x = "Print Run Multiplier (vs. Alpha)",
    y = "Price as % of Alpha Price",
    color = "Model",
    fill = "Actual Data"
  )


library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# 1. First, extract the LEA Benchmark values (needed for the math)
lea_data <- mtg_data |> 
  filter(card_name == "Black Lotus", set == "LEA") |>
  mutate(
    price_num = as.numeric(gsub("[\\$,]", "", cur_price)),
    print_num = as.numeric(gsub(",", "", print_run))
  )

lea_price <- lea_data$price_num
lea_print <- lea_data$print_num

# 2. Prepare Comparison Data for the other sets
plot_data <- mtg_data |>
  filter(card_name == "Black Lotus", set %in% c("LEB", "2ED", "3ED")) |> # LEA is excluded here
  mutate(
    price_num = as.numeric(gsub("[\\$,]", "", cur_price)),
    print_num = as.numeric(gsub(",", "", print_run)),
    # Calculate performance relative to the LEA constants we saved
    relative_price = price_num / lea_price,
    relative_scarcity = lea_print / print_num
  ) |>
  select(set, relative_price, relative_scarcity) |>
  pivot_longer(cols = starts_with("relative"), names_to = "metric", values_to = "value")

# 3. Create the focused Graph
ggplot(plot_data, aes(x = factor(set, levels = c("LEB", "2ED", "3ED")), y = value, fill = metric)) +
  geom_col(position = "dodge") +
  
  # Formatting the Y-axis as percentage
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, by = 0.1)) +
  
  # Custom colors and labels
  scale_fill_manual(
    values = c("relative_price" = "#377eb8", "relative_scarcity" = "#e41a1c"),
    labels = c("Actual Price (% of Alpha)", "Expected Price based on Scarcity")
  ) +
  
  theme_minimal() +
  labs(
    title = "Benalish Hero: Market Value vs. Supply Logic",
    subtitle = "Calculated relative to LEA (Alpha) values. Red bars show where price 'should' be based on print run.",
    x = "Expansion Set",
    y = "Percentage of Alpha Benchmark",
  ) +
  theme(legend.position = "bottom")
