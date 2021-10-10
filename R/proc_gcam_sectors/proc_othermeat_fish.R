# Need to include fish from FBS

GCAM_crop <- "OtherMeat_Fish"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>%
  union(c())

#	NA is  "meat of other domestic camelids, fresh or chilled"  added  based on prodstat

SUA_item <- c("Fat, camels", "Fat, other camelids", "Oils, fats of animal nes",
              "Offals, horses", "Offals, edible, camels", "Offals nes",
              "Meat nes", "Meat, ass", "Meat, bird nes", "Meat, camel",
              "Meat, horse", "Meat, mule", "Meat, other rodents", "Meat, rabbit",
              "Meat, game", "Snails, not sea", "Meat nes, preparations",
              "Meat, other camelids") %>% #added
  setdiff(c("Degras",	"Grease incl. lanolin wool",	"Fat nes, prepared")) #not used



FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

OtherMeat_Fish_primary <- SUA_item


bal %>%
  mutate(item = replace(item, item %in% OtherMeat_Fish_primary,"APE")) %>%
  Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% OtherMeat_Fish_primary) %>%
              mutate(item = "OtherMeat_Fish_primary") %>%
              Agg_reg(area, year, item, element) ) -> T2

T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item %in% FBS_item) %>% mutate(item = "FBS") %>%
              Agg_reg(area, year, item, element) %>%
              mutate(element = gsub(" Quantity", "", element)) %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
