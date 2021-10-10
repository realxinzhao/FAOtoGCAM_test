
GCAM_crop <- "Wheat"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

bal %>% Proc_primarize(
  c("Flour, wheat"),
  c("Gluten, wheat", "Starch, wheat", "Bread", "Pastry", "Macaroni",
    "Wafers", "Mixes and doughs"), agg_sink_items = T, keep_primary = F) %>%
  Proc_primarize(
    c("Wheat"),
    c("Food preparations, flour, malt extract", "Bulgur",
      "Cereals, breakfast", "Bran, wheat", "Germ, wheat", "Flour, wheat"),
    keep_primary = F
  ) %>%
  mutate(item = "APE") %>%
  bind_rows(bal %>% filter(item == "Wheat")) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item == "Wheat and products") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
