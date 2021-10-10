library(ggplot2)

QCL %>% filter(element == "Stocks") %>% pull(item) %>% unique()
item_agg <- c("Poultry Birds", "Human", "Sheep and Goats", "Cattle", "Pigs", "Cats and dogs")
#item_agg <- c("Poultry Birds", "Human", "Sheep and Goats", "Cattle and Buffaloes", "Cats and dogs")

QCL %>% filter(element == "Stocks", item %in% item_agg) %>%
  Agg_reg(item, year) %>%
  mutate(value = if_else(item %in% c("Poultry Birds"), value, value /1000)) %>%
  bind_rows(
    OA %>% filter(area == "World", element == "Total Population - Both sexes", year >= 1973) %>%
      select(year, item, value) %>% mutate(item = "Human")
  ) %>% filter(year <=2020, year <= 2018) %>%
  mutate(value = value / 1000000) %>%
  bind_rows(
    data.frame(item = c(#"Cats and dogs (pets)",
      "Cats and dogs"),
      year = 2018, value = c(#0.844,
        1.500)) #Statista
  ) %>%
  mutate(item = factor(item, levels = item_agg)) -> df



df %>% filter(year == 2018) %>%
  ggplot() +
  geom_bar(aes(x = item, y = value,  fill = item), stat = "identity",
           color = "black", size = 1, alpha = 0.9 ) +
  scale_x_discrete(limits = rev(levels( df$item))) +
  scale_y_continuous(n.breaks = 6) +
  ggsci::scale_fill_npg(name = "") +
  labs(y = "Billion heads or persons",
       title = "Inventory: human and livestock\n2018") +
  theme_bw() + coord_flip() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(colour = "black", size = 20)) -> A

png("output/Inventory_2018.png",width = 2300, height = 2000, res = 300)
print(A)
dev.off()



df %>% filter(item != "Cats and dogs") %>%
  ggplot() +
  #facet_wrap(~item, scales = "fixed") +
  geom_line(aes(x = year, y = value, color = item), size = 1.5) +
  ggsci::scale_color_npg(name = "Human & livestock") +
  scale_y_continuous(n.breaks = 6) +
  labs(x = "Year", y = "Billion heads or persons",
       title = "Inventory: human and livestock, 1973 - 2018") +
  theme_bw() + coord_flip() +
  theme(#legend.position = "none",
        #axis.title.y = element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(colour = "black", size = 20)) -> A

png("output/Inventory_ts.png",width = 2300, height = 2000, res = 300)
print(A)
dev.off()

