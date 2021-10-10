
GCAM_crop <- "MiscCrop"
#*************
#* data 2014 to 2018
#* adding Cocoa, butter from oil crops here
SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>% union("Cocoa, butter") %>%
  union(c("Carobs", "Spices nes")) %>% #two not included in FBS but in SCL and QCL
  union(c("Peppermint", "Hops",
          "Pyrethrum, dried", "Tobacco, unmanufactured", #four not included in FBS & SCL but in QCL
          "Rubber, natural", "Gums, natural")) %>%  # Added rubber given the land implications
  setdiff("Tea, mate extracts") #no production small impacts

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal



bal %>% Agg_reg(year, item, element) %>% filter( year == 2017) %>% spread(item, value)-> A

MiscCrop_primary <- bal %>% Agg_reg(item, element) %>%
  filter(element == "Area harvested", value > 0) %>% pull(item)

MiscCrop_processed <- setdiff(SUA_item, MiscCrop_primary)

bal %>%
  Proc_primarize(c("Coffee, green"), c("Coffee, roasted", "Coffee, extracts")) %>%
  Proc_primarize(c("Cocoa, butter", "Cocoa, powder & cake"), c("Chocolate products nes")) %>%
  Proc_primarize("Cocoa, paste", c("Cocoa, butter", "Cocoa, powder & cake")) %>%
  Proc_primarize("Cocoa, beans", c("Cocoa, paste")) %>%
  mutate(item = GCAM_crop) %>%
  Agg_reg(area, year, item, element) %>%
  mutate(item = "APE") %>%
  bind_rows(bal %>% filter(item %in% MiscCrop_primary) %>% mutate(item = GCAM_crop) %>%
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
