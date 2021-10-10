#Neet to test r package data.tree


# There are 8 SUA items for maize and the processed food.
# Note that maize germ cake data are not available
# Maize germ cake is added to maize oil for primarizing
# That is, APE includes both maize oil and cake
# Need to remove Maize germ oil in FAO_ag_items_cal_SUA.csv

GCAM_crop <- "Corn"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

SUA_item <- FBS_SUA_item_mapping %>%
  filter(FBS_label %in% c("Maize and products (including white maize)", "Maize Germ Oil and products")) %>%
  pull(SUA_item) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

#Proc_primarize
bal %>%
  Proc_primarize_addfeed( c("Oil, maize"), feedshare = 0.5) %>%
  Proc_primarize(c("Germ, maize"), c("Oil, maize")) %>%
  Proc_primarize(c("Bran, maize", "Gluten, maize"), c("Feed and meal, gluten")) %>%
  Proc_primarize(c("Flour, maize"), c("Starch, maize", "Gluten, maize")) %>%
  Proc_primarize(c("Maize"), c("Flour, maize", "Germ, maize", "Bran, maize")) %>%
  mutate(item = replace(item, item == "Maize", "APE")) %>%
  bind_rows(bal %>% filter(item == "Maize")) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))



T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item == "Maize and products") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>% #filter(region == "USA") %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3

T2 %>% Reg_FAOtoGCAM %>% filter(element %in% c("Food","Loss",
                                               "Production", "Regional supply",
                                               "Regional demand", "Stock Variation",
                                               "Opening stocks", "Closing stocks")) %>%
  spread(element, value) %>% filter(item == "Maize") %>%
  mutate(StU = `Closing stocks`/ `Regional demand`, LtU = Loss /`Regional demand`) %>%
  select(region, item, year, LtU, StU) %>% gather(variable, value, StU, LtU)-> A


T2 %>% filter(item == "Maize") %>% Agg_reg(year, item, element) %>%
  filter(element %in% c("Food","Loss",
                                               "Production", "Regional supply",
                                               "Regional demand", "Stock Variation",
                                               "Opening stocks", "Closing stocks")) %>%
  spread(element, value) %>%
  mutate(StU = `Closing stocks`/ `Regional demand`, LtU = Loss /`Regional demand`) %>%
  select(item, year, LtU, StU) %>% gather(variable, value, StU, LtU)-> A


library(ggplot2)
A %>% #filter(year == 2018) %>%
  ggplot() + facet_grid(cols = vars(year), rows = vars(variable)) +
  geom_bar(aes(x = reorder(region, -value), y = value, fill = variable), stat = "identity") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

FBS %>% filter(item == "Maize and products", element %in% c('Production', 'Loss'))%>%
  Reg_FAOtoGCAM %>% spread(element, value) %>%
  mutate(LtP = Loss / Production) %>% filter(LtP < 0.5) ->A

A %>%
  ggplot() + facet_wrap(~region) +
  geom_line(aes(x = year, y = LtP, group = region), color = "red") +
  theme_bw()

