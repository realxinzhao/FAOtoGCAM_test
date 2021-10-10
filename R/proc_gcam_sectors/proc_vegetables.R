
# !!!check in prodstat  need to be consistent in mapping on both supply and demand sides
# "Melons, other (inc.cantaloupes)" is fruit
# "Maize, green" is in corn


GCAM_crop <- "Vegetables"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal


Vegetables_primary <- bal %>% Agg_reg(item, element) %>%
  filter(element == "Area harvested", value > 0) %>% pull(item) %>%
  union(c("Cassava leaves",
          "Coffee, substitutes containing coffee"))
Vegetables_processed <- setdiff(SUA_item, Vegetables_primary)


bal %>%
  Proc_primarize(c("Tomatoes"), c("Juice, tomato", "Tomatoes, paste", "Tomatoes, peeled")) %>%
  Proc_primarize(c("Maize, green"), c("Sweet corn frozen", "Sweet corn prep or preserved")) %>%
  Proc_primarize("Mushrooms and truffles", c("Mushrooms, dried", "Mushrooms, canned")) %>%
  mutate(item = replace(item,
                        item %in% setdiff(Vegetables_primary, c("Tomatoes", "Maize, green", "Mushrooms and truffles")),
                        "Vegetables")) %>%
  Agg_reg(area, year, item, element) %>%
  Proc_primarize("Vegetables", Vegetables_processed[grepl("egetab", Vegetables_processed)]) %>%
  mutate(item = "Vegetables") %>%
  Agg_reg(area, year, item, element) %>%
  mutate(item = "APE") %>% Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% Vegetables_primary) %>% mutate(item = GCAM_crop) %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


#tested putting all level1 processed products together
#
# bal %>%
#   mutate(item = replace(item, item %in% Vegetables_primary, "Vegetables")) %>%
#   Agg_reg(area, year, item, element) %>%
#   Proc_primarize("Vegetables", Vegetables_processed) %>%
#   mutate(item = "APE") %>%
#    Agg_reg(area, year, item, element) %>%
#   bind_rows(bal %>% filter(item %in% Vegetables_primary) %>% mutate(item = GCAM_crop) %>%
#               Agg_reg(area, year, item, element) ) -> T2

T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item %in% FBS_item) %>% mutate(item = "FBS") %>%
              Agg_reg(area, year, item, element) %>%
              mutate(element = gsub(" Quantity", "", element)) %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3

T3 %>% gather(item, value, -year, -element) %>% Agg_reg(element, item) %>% spread(item, value) %>%
  mutate(element = factor(element, levels = Bal_element_new)) %>% arrange(element)
