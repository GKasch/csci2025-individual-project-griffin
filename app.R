library(shiny)
library(bslib)
library(tidyverse)

mtg_appdata <- read_csv("analysis_data.csv")

ui <- page_fluid(
  titlePanel(" "),
  tabsetPanel(
    tabPanel(
      title = "Tab 1",
      selectInput(
        "Edition", 
        "Select Edition", 
        choices = c("All", unique(mtg_appdata$full_set)), 
        selected = unique(mtg_appdata$full_set)[1]
    ),
    selectInput(
      "Rarity", 
      "Select Rarity", 
      choices = c("All", unique(mtg_appdata$rarity)), 
      selected = unique(mtg_appdata$rarity)[2]
    ),
     plotOutput("selected")
    ),
    tabPanel(
      title = "Tab 2",
      selectInput(
        "Edition_tab2", 
        "Select Edition", 
        choices = c("All", unique(mtg_appdata$full_set)), 
        selected = unique(mtg_appdata$full_set)[1]
      ),
      selectInput(
        "Rarity_tab2", 
        "Select Rarity", 
        choices = c("All", unique(mtg_appdata$rarity)), 
        selected = unique(mtg_appdata$rarity)[2]
      ),
      plotOutput("selected_tab2")
    )
  )
)
server <- function(input, output, session) {

  output$selected <- renderPlot({
    rarity_label <- if(input$Rarity == "All") {
      "Full Set"
    } else {
      paste0(input$Rarity, "s")
    }
    plot_title <- paste(input$Edition, rarity_label, "vs S&P 500")
    mtg_appdata |>
      filter(
        (full_set == input$Edition | input$Edition == "All") & 
        (rarity == input$Rarity | input$Rarity == "All") &
          !is.na(pct_increase)) |>
      ggplot(aes(x=performance, fill=performance)) +
      geom_bar() +
      stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5) +
    scale_fill_manual(values = c("Beat S&P" = "#2ca25f", "Below S&P" = "#636363")) +
    theme_minimal() +
    labs(title = plot_title)
})
  
   
  
  
  
  output$selected_tab2 <- renderPlot({
    rarity_label_tab2 <- if(input$Rarity_tab2 == "All") {
      "Full Set"
      } else {
        paste0(input$Rarity_tab2, "s")
      }
    plot_title_tab2 <- paste("Investment Comparison:", input$Edition_tab2, rarity_label_tab2, "vs S&P 500")
   

    subset_mtg <- mtg_appdata |>
      filter(
        (full_set == input$Edition_tab2 | input$Edition_tab2 == "All") & 
        (rarity == input$Rarity_tab2 | input$Rarity_tab2 == "All")
      )

# 2. Calculate the Portfolio Average Return
total_cost <- sum(subset_mtg$old_price)
total_value <- sum(subset_mtg$cur_price)
portfolio_total_return <- ((total_value - total_cost) / total_cost) * 100

# 3. Define your S&P 500 Benchmark 
# (Replace 250 with the actual S&P 500 return % for your specific date range)
sp500_return <- 2250 

# 4. Create a summary data frame for the chart
plot_comparison <- data.frame(
  Group = c("MTG Portfolio", "S&P 500 Index"),
  Return = c(portfolio_total_return, sp500_return)
)

# 5. Create the Chart
ggplot(plot_comparison, aes(x = Group, y = Return, fill = Group)) +
  geom_col(width = 0.5) +
  # Add percentage labels on top of the bars
  geom_text(aes(label = paste0(round(Return, 0), "%")), 
            vjust = -0.8, size = 6, fontface = "bold") +
  # Use your existing color scheme
  scale_fill_manual(values = c("MTG Portfolio" = "#2ca25f", "S&P 500 Index" = "#636363")) +
  theme_minimal() +
  expand_limits(y = max(plot_comparison$Return) * 1.2) + # Add space for labels
  labs(
    title = plot_title_tab2,
    subtitle = paste("Based on an average of", nrow(subset_mtg), "unique cards"),
    y = "Total Return Percentage (%)",
    x = ""
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 12)
  )
})



}

shinyApp(ui, server)
  
      


