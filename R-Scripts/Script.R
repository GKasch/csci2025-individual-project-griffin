library(tidyverse)

# Install jsonlite if you don't already have it
install.packages("jsonlite")

# Load the library
library(jsonlite)

# Dummy file path to the JSON file
json_file_path <- "IndivProject/Data/AllPricesToday.json"

# Read the JSON file
json_data <- fromJSON(json_file_path)

# View the data
print(json_data)


test_data <- read_csv("IndivProject/Data/cardPrices.csv")

glimpse(test_data)


cards <- read_csv("IndivProject/Data/cards.csv")

test_data2 <- merge(
  test_data,
  cards[, c("uuid", "name", "setCode")],
  by = "uuid",
  all.x = TRUE
)

glimpse(test_data2)

test_data2 |>
  arrange(desc(uuid)) |>
  glimpse()

test_data2 |>
  filter(name=="Blue Elemental Blast") |>
ggplot(aes(x=setCode, y=price)) +
  geom_point()

json_data_big <- fromJSON("IndivProject/Data/AllPrices.JSON")

library(jsonlite)
library(readr)

