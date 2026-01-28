library(shiny)
library(bslib)
library(tidyverse)
library(scales)

mtg_appdata <- read_csv("analysis_data.csv")

ui <- page_fluid(
  titlePanel("MTG against the S&P500"),
  tabsetPanel(
    tabPanel(
      title = "Number of Cards",
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
      title = "Portfolio Method",
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
      plotOutput("selected_tab2")),
    tabPanel(
      title = "Print Run Analysis",
      fluidRow(
        column(6,
          selectInput(
            "lea_card", 
            "Select Alpha Card", 
            choices = mtg_appdata |>
              filter(set == "LEA") |>
              pull(card_name) |>
              unique(), 
            selected = "Air Elemental"
          ),
          hr(),
          plotOutput("plot_alpha")
        ),
        column(6,
          selectInput(
            "leb_card", 
            "Select Beta Card", 
            choices = mtg_appdata |>
              filter(set == "LEB") |>
              pull(card_name) |>
              unique(),
            selected = "Air Elemental"
          ),
          hr(),
          plotOutput("plot_beta")
        )
      )
   )
  )
)
server <- function(input, output, session) {

  output$selected <- renderPlot({
    
    rarity_label <- if(input$Rarity == "All") {"Full Set"
    } else {paste0(input$Rarity, "s")}
    
    plot_title <- paste(input$Edition, rarity_label, "vs S&P 500")
    
    mtg_appdata |>
      filter(
        (full_set == input$Edition | input$Edition == "All") & 
        (rarity == input$Rarity | input$Rarity == "All") &
          !is.na(pct_increase)) |>
      ggplot(aes(x=performance, fill=performance)) +
        geom_bar() +
        stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.5, size = 5, fontface="bold") +
        scale_fill_manual(values = c("Beat S&P" = "#2ca25f", "Below S&P" = "#636363")) +
        theme_minimal() +
        labs(title = plot_title)+
      theme(
        legend.position="none",
        plot.title = element_text(size =18, face = "bold"),
        axis.text = element_text(size = 12))
})
  
  output$selected_tab2 <- renderPlot({
   
    rarity_label_tab2 <- if(input$Rarity_tab2 == "All") {"Full Set"
      } else { paste0(input$Rarity_tab2, "s")}
   
    plot_title_tab2 <- paste("Investment Comparison:", input$Edition_tab2, rarity_label_tab2, "vs S&P 500")

    subset_mtg <- mtg_appdata |>
      filter(
        (full_set == input$Edition_tab2 | input$Edition_tab2 == "All") & 
        (rarity == input$Rarity_tab2 | input$Rarity_tab2 == "All")
      )
    total_cost <- sum(subset_mtg$old_price)
    total_value <- sum(subset_mtg$cur_price)
    portfolio_total_return <- ((total_value - total_cost) / total_cost) * 100
    sp500_return <- 2250 

    plot_comparison <- data.frame(
      Group = c("MTG Portfolio", "S&P 500 Index"),
      Return = c(portfolio_total_return, sp500_return)
    )

    ggplot(plot_comparison, aes(x = Group, y = Return, fill = Group)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = paste0(round(Return, 0), "%")), 
            vjust = -0.8, size = 6, fontface = "bold") +
      scale_fill_manual(values = c("MTG Portfolio" = "#2ca25f", "S&P 500 Index" = "#636363")) +
      theme_minimal() +
      expand_limits(y = max(plot_comparison$Return) * 1.2) + 
      labs(
        title = plot_title_tab2,
        subtitle = paste("Based on an average of", nrow(subset_mtg), "unique cards"),
        y = "Total Return Percentage (%)",
        x = ""
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(size =18, face = "bold"),
        axis.text = element_text(size = 12)
      )
  })

  output$plot_alpha <- renderPlot({
    lea_data <- mtg_appdata |> 
      filter(card_name == input$lea_card, set == "LEA") |>
      mutate(
        price_num = as.numeric(gsub("[\\$,]", "", cur_price)),
        print_num = as.numeric(gsub(",", "", print_run))
      )

    lea_price <- lea_data$price_num
    lea_print <- lea_data$print_num

    plot_data <- mtg_appdata |>
      filter(card_name == input$lea_card, set %in% c("LEB", "2ED", "3ED")) |> 
      mutate(
        price_num = as.numeric(gsub("[\\$,]", "", cur_price)),
        print_num = as.numeric(gsub(",", "", print_run)),
        relative_price = price_num / lea_price,
        relative_scarcity = lea_print / print_num
      ) |>
      select(set, relative_price, relative_scarcity) |>
      pivot_longer(cols = starts_with("relative"), names_to = "metric", values_to = "value")


    ggplot(plot_data, aes(x = factor(set, levels = c("LEB", "2ED", "3ED")), y = value, fill = metric)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, by = 0.1)) +
      scale_fill_manual(
        values = c("relative_price" = "#377eb8", "relative_scarcity" = "#e41a1c"),
        labels = c("Actual Price (% of Alpha)", "Expected Price based on Scarcity")
      ) +
      theme_minimal() +
      labs(
        title = paste0(input$lea_card, ": Market Value vs. Supply Logic"),
        subtitle = "Calculated relative to LEA (Alpha) values. Red bars show where price 'should' be based on print run.",
        x = "Expansion Set",
        y = "Percentage of Alpha Benchmark",
      ) +
      theme(legend.position = "bottom",
        plot.title=element_text(size=18, face="bold"))
  })
  
  
  output$plot_beta <- renderPlot({
    leb_data <- mtg_appdata |> 
      filter(card_name == input$leb_card, set == "LEB") |>
      mutate(
        price_num = as.numeric(gsub("[\\$,]", "", cur_price)),
        print_num = as.numeric(gsub(",", "", print_run))
      )

    leb_price <- leb_data$price_num
    leb_print <- leb_data$print_num

    plot_data <- mtg_appdata |>
      filter(card_name == input$leb_card, set %in% c("2ED", "3ED")) |> 
      mutate(
        price_num = as.numeric(gsub("[\\$,]", "", cur_price)),
        print_num = as.numeric(gsub(",", "", print_run)),
        relative_price = price_num / leb_price,
        relative_scarcity = leb_print / print_num
      ) |>
      select(set, relative_price, relative_scarcity) |>
      pivot_longer(cols = starts_with("relative"), names_to = "metric", values_to = "value")


    ggplot(plot_data, aes(x = factor(set, levels = c("2ED", "3ED")), y = value, fill = metric)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, by = 0.1)) +
      scale_fill_manual(
        values = c("relative_price" = "#377eb8", "relative_scarcity" = "#e41a1c"),
        labels = c("Actual Price (% of Beta)", "Expected Price based on Scarcity")
      ) +
      theme_minimal() +
      labs(
        title = paste0(input$leb_card, ": Market Value vs. Supply Logic"),
        subtitle = "Calculated relative to LEB (Beta) values. Red bars show where price 'should' be based on print run.",
        x = "Expansion Set",
        y = "Percentage of Beta Benchmark",
      ) +
      theme(legend.position = "bottom",
        plot.title=element_text(size=18, face="bold"))
  })
}

shinyApp(ui, server)
  
      


