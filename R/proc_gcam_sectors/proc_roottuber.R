
GCAM_crop <- "RootTuber"
#*************
#* data 2014 to 2018
SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()
#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

RootTuber_primary <- c("Potatoes", "Sweet potatoes", "Cassava", "Yams",
                       "Taro (cocoyam)", "Yautia (cocoyam)", "Roots and tubers nes")
RootTuber_processed <- setdiff(SUA_item, RootTuber_primary)
bal %>%
  Proc_primarize(c("Potatoes"), c("Potatoes, frozen", "Flour, potatoes", "Starch, potatoes", "Tapioca, potatoes" )) %>%
  Proc_primarize("Cassava", c("Cassava dried", "Flour, cassava", "Starch, cassava", "Tapioca, cassava")) %>%
  Proc_primarize("Roots and tubers nes", c("Roots and tubers dried", "Flour, roots and tubers nes")) %>%
  mutate(item = replace(item, item %in% RootTuber_primary, "RootTuber_primary")) %>%
  Agg_reg(area, year, item, element) %>%
  mutate(item = "APE") %>%
  bind_rows(bal %>% filter(item %in% RootTuber_primary) %>% mutate(item = GCAM_crop) %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


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
  mutate(element = factor(element, levels = Bal_element_new))



