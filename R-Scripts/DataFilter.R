library(tidyverse)

test_data <- read_csv("Data/cardPrices.csv") |>
  filter(providerListing == "retail", #remove buylist data
    currency == "USD", #only US price
    gameAvailability == "paper") |> #remove online versions
    group_by(uuid) |>  
  summarize(cur_price = mean(price, na.rm=TRUE), #makes one row for each card with average price
    .groups = "drop") 
  


cards <- read_csv("Data/cards.csv") |>
  mutate(name = if_else(
    str_detect(number, "[a-zA-Z]"),
    str_glue("{name} ({toupper(str_extract(number, '[a-zA-Z]'))})"),   #adds variant to name for atq
    name)
  ) |>
  filter(setCode %in% c("LEA", "LEB", "2ED", "3ED", "ARN", "ATQ"), #filter for desired sets
    !str_detect(type, "Basic Land")) #remove basics

cards_full <- test_data |>
  inner_join(cards, by = "uuid") #join both
  
Scrye <- read_csv("Data/ScryeData.csv")

combined_data <- Scrye |>
  mutate(card_name = if_else(card_name == "Junœn Efreet", "Junún Efreet", card_name)) |> #fix accent reading issue
  full_join(cards_full, by = c("set" = "setCode", "card_name" = "name")) #combine data sets

print_run <- read_csv("Data/PrintRunData.csv")

full_data <- combined_data |>
  left_join(print_run, c("set"= "set", "full_rarity" = "rarity")) #add print run information

analysis_data <- full_data |>
  select(card_name, set, rarity, full_rarity, old_price, cur_price, print_run)|> #limit data frame to useful columns
  mutate(set_rarity = str_glue("{set} ({full_rarity})"))
  
