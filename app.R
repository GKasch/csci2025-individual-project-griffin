library(shiny)
library(bslib)
library(tidyverse)

mtg_appdata <- read_csv("analysis_data.csv")

ui <- page_fluid(
  selectInput(
    "Edition", 
    "Select Edition", 
    choices = c("All", unique(mtg_appdata$full_set)), 
    selected = unique(mtg_appdata$full_set)[1]),
  selectInput(
    "Rarity", 
    "Select Rarity", 
    choices = c("All", unique(mtg_appdata$rarity)), 
    selected = unique(mtg_appdata$rarity)[2]),
  plotOutput("selected")
)

server <- function(input, output, session) {

  output$selected <- renderPlot({

    rarity_label <- if(input$Rarity == "All") {
      "All Rarities"
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
})}

shinyApp(ui, server)
  
      


