#*****************************************************************************
#SugarCrop
# "Sugar non-centrifugal" was not included in GCAM data (FAO_ag_items_cal_SUA),
# but is now added.
# "Sugar crops nes" is also added for consistency with production
# That is, all sugar crops and their processed products up to
# the refined sugar & confectionery level are included, but presented in
# aggregated primary equivalent.

GCAM_crop <- "SugarCrop"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

#NA here was 23511.01 and 23512

# "Sugar crops nes" only exists in few regions with relatively small production, but it is treated as primary sugarcrop here.
# Note that CPC, 23511.01 (cane sugar) & 23512 (beet sugar) are in 2351F, raw cane or beet sugar (centrifugal only).
# Only cane has non-centrifugal process
# "sugar confectionery" is treated as refined sugar with no loss.

SUA_item <- c(
  "Sugar cane", "Sugar beet", "Sugar crops nes",
  "Sugar non-centrifugal", #CPC 23511.02
  "Sugar refined",
  "Sugar Raw Centrifugal", #including both can and beet
  "Sugar confectionery",
  "Molasses") # Only Molasses from FBS item "Sweeteners, Other and products" is included here

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

unique(Bal_new_all$item)[grepl("sugar", unique(Bal_new_all$item), ignore.case = T)]
FBS_item[grepl("sugar", FBS_item, ignore.case = T)]


#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

#Proc_primarize
bal %>%
  mutate(item = replace(item, item %in% c("Sugar cane", "Sugar beet", "Sugar crops nes"), "SugarCrop")) %>%
  Agg_reg(area, year, item, element) %>%
  Proc_primarize_mvprod("Sugar confectionery") %>%
  Proc_primarize(c("Sugar Raw Centrifugal"),
                 c("Sugar refined", "Sugar confectionery")) %>%
  Proc_primarize(c("SugarCrop"),
                 c("Sugar non-centrifugal", "Sugar Raw Centrifugal", "Molasses"))  %>%
  mutate(item = replace(item, item == "SugarCrop", "APE")) %>%
  bind_rows(bal %>% filter(item %in% c("Sugar cane", "Sugar beet", "Sugar crops nes"))) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item == "Sugar (Raw Equivalent)") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3

T2 %>%
  Reg_FAOtoGCAM %>%
  group_by(element, item) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3


