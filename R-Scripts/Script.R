


test_data <- read_csv("Data/cardPrices.csv")

glimpse(test_data)


cards <- read_csv("Data/cards.csv")

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




