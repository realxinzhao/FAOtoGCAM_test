
GCAM_crop <- "Pork"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>% union(c("Fat, pig butcher", "Offals, pigs, edible",
                                          "Fat, pigs", "Lard", "Lard stearine oil"))

# related FBS items Offals, Edible and products and Fats, Animals, Raw and products


FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

setdiff(SUA_item, unique(Bal_new_all$item))

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal


Pork_primary <- c("Meat, pig", "Fat, pigs", "Offals, pigs, edible")

bal %>%
  Proc_primarize(c("Meat, pig"),
                 c("Meat, pork", "Fat, pig butcher", "Bacon and ham",
                   "Meat, pig sausages", "Meat, pig, preparations")) %>%
  Proc_primarize(c("Lard"), c("Lard stearine oil")) %>%
  Proc_primarize(c("Fat, pigs"), c("Lard")) %>%
  mutate(item = replace(item, item %in% Pork_primary,"APE")) %>%
  Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% Pork_primary) %>%
              mutate(item = "Pork_primary") %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))



T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item %in% c("Pigmeat")) %>% mutate(item = "FBS") %>%
              Agg_reg(area, year, item, element) %>%
              mutate(element = gsub(" Quantity", "", element)) %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3

# remaining processed is likely (processed) food uses
