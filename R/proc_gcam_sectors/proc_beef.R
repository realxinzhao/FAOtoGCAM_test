

GCAM_crop <- "Beef"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>% union(c("Fat, cattle butcher", "Fat, cattle", "Fat, buffaloes", "Tallow",
                                          "Offals, edible, cattle", "Offals, edible, buffaloes",
                                          "Meat, dried nes",	"Liver prep."))

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)


#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

Beef_primary <- c("Meat, cattle", "Meat, buffalo", "Fat, cattle", "Fat, buffaloes",
               "Offals, edible, cattle", "Offals, edible, buffaloes")

bal %>%
  Proc_primarize(c("Fat, cattle", "Fat, buffaloes"), c("Tallow")) %>%
  Proc_primarize(c("Offals, edible, cattle"), c("Meat, dried nes",	"Liver prep.")) %>%
  Proc_primarize(c("Meat, cattle"),
                 c("Meat, cattle, boneless (beef & veal)", "Fat, cattle butcher",
                   "Meat, beef, dried, salted, smoked", "Meat, extracts",
                   "Meat, beef and veal sausages", "Meat, beef, preparations",
                   "Meat, homogenized preparations")) %>%
  mutate(item = replace(item, item %in% Beef_primary, "APE")) %>%
  bind_rows(bal %>% filter(item %in% Beef_primary) %>%
              mutate(item = "Beef_primary") %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))

T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item == "Bovine Meat") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
