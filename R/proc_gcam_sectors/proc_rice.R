
# There are 108 SUA items for paddy rice and the processed products
# Need to update  FAO_ag_items_cal_SUA.csv
# Note that Paddy to milled equivalent in PRODSTAT/FBSH has a 0.667 rate
GCAM_crop <- "Rice"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

SUA_item <- FBS_SUA_item_mapping %>%
  filter(FBS_label %in% c("Rice and products", "Ricebran Oil and products")) %>%
  pull(SUA_item) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

# Proc_primarize
# Feedshare of 80/94.5 is an average between China and India values (two major rice oil producers)
bal %>%
  Proc_primarize_addfeed( c("Oil, rice bran"), feedshare = 80/94.5) %>%
  Proc_primarize(c("Bran, rice"), c("Oil, rice bran")) %>%
  Proc_primarize(c("Rice, broken"), c("Gluten, rice", "Starch, rice", "Flour, rice")) %>%
  Proc_primarize(c("Rice, milled"), c("Rice, broken")) %>%
  Proc_primarize(c("Rice, husked"), c("Rice, milled/husked")) %>%
  Proc_primarize(c("Rice, paddy"), c("Bran, rice", "Rice, husked", "Rice, milled")) %>%
  mutate(item = replace(item, item == "Rice, paddy", "APE")) %>%
  bind_rows(bal %>% filter(item == "Rice, paddy")) %>%
  mutate(element = factor(element, levels = Bal_element_new)) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item == "Rice and products") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>% filter(region == "USA") %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
  spread(item, value) %>%
  mutate(element = factor(element, levels = Bal_element_new)) %>%
  arrange(year, element) -> T3


setdiff(unique(QCL$item), unique(Bal_new_all$item))

