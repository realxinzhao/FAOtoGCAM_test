
GCAM_crop <- "OtherGrain"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

OtherGrain_primary <- c("Barley", "Rye", "Oats", "Millet", "Sorghum",
                        "Triticale", "Buckwheat", "Fonio", "Quinoa",
                        "Canary seed", "Grain, mixed", "Cereals nes")

OtherGrain_processed <- c("Malt", "Barley, pot",  "Bran, barley",
                          "Flour, rye", "Bran, rye", "Oats rolled", "Bran, oats",
                          "Flour, millet", "Bran, millet", "Flour, sorghum", "Bran, sorghum",
                          "Flour, buckwheat","Flour, fonio", "Flour, triticale", "Flour, mixed grain", "Flour, cereals",
                          "Bran, buckwheat", "Bran, fonio", "Bran, triticale", "Bran, mixed grains", "Bran, cereals nes",
                          "Cereal preparations nes")


bal %>%
  Proc_primarize(c("Malt"), c("Malt extract")) %>%
  Proc_primarize("Barley, pot", c("Barley, pearled", "Flour, barley and grits")) %>%
  mutate(item = replace(item, item %in% OtherGrain_primary, "OtherGrain")) %>%
  Agg_reg(area, year, item, element) %>%
  Proc_primarize("OtherGrain", OtherGrain_processed) %>%
  mutate(item = "APE") %>%
  bind_rows(bal %>% filter(item %in% OtherGrain_primary) %>% mutate(item = "OtherGrain") %>%
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
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T4
