
GCAM_crop <- "SheepGoat"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>%
  union(c("Offals, sheep,edible", "Offals, edible, goats", "Fat, sheep", "Fat, goats", "Skins, goat, fresh"))

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

bal %>% filter(year == 2017) %>%  Agg_reg(item, year, element) %>% spread(item, value) -> A


SheepGoat_primary <- SUA_item

bal %>%
  mutate(item = replace(item, item %in% SheepGoat_primary,"APE")) %>%
  Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% SheepGoat_primary) %>%
              mutate(item = "SheepGoat_primary") %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item %in% "Mutton & Goat Meat") %>% mutate(item = "FBS") %>%
              Agg_reg(area, year, item, element) %>%
              mutate(element = gsub(" Quantity", "", element)) %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
