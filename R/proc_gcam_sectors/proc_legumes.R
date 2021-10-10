
#!!!note "Broad beans, horse beans, dry" in vegetable
#!!!note "Vetches" in FodderHerb
GCAM_crop <- "Legumes"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()
#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

Legumes_primary <- bal %>% Agg_reg(item, element) %>%
  filter(element == "Area harvested", value > 0) %>% pull(item)
Legumes_processed <- setdiff(SUA_item, Legumes_primary)

bal %>%
  mutate(item = replace(item, item %in% Legumes_primary, GCAM_crop)) %>%
  Agg_reg(area, year, item, element) %>%
  Proc_primarize(GCAM_crop, Legumes_processed) %>%
  mutate(item = "APE") %>%
  bind_rows(bal %>% filter(item %in% Legumes_primary) %>% mutate(item = GCAM_crop) %>%
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
  mutate(element = factor(element, levels = Bal_element_new)) %>% arrange(element)
