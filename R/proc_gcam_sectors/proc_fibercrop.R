
GCAM_crop <- "FiberCrop"
#*************
#* data 2014 to 2018
SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>%
  union(c("Cake, cottonseed", "Seed cotton", "Cotton lint")) %>%
  union(c("Agave fibres nes", "Bastfibres, other", "Coir", "Fibre crops nes",
          "Flax fibre and tow", "Hemp tow waste", "Jute","Kapok fibre",
          "Manila fibre (abaca)", "Ramie", "Sisal"))
FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(FBS_label) %>% unique() %>% union(
    FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
      pull(FBSH_item) %>% unique()
  )
#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)

# Large variation in linter rate relative to cake
# China 2/55 India 2/50 USA 6.8/44 Pakistan 3/50
# othershare in 3.5 - 11% and using 5% here

bal %>%
  Proc_primarize_addother(c("Cake, cottonseed"), othershare = 0.05) %>%
  Proc_primarize(c("Cottonseed"), c("Oil, cottonseed", "Cake, cottonseed")) %>%
  Proc_primarize(c("Seed cotton"), c("Cottonseed", "Cotton lint")) %>%
  mutate(item = GCAM_crop) %>%
  Agg_reg(area, year, item, element) %>%
  mutate(item = "APE") %>%
  bind_rows(bal %>% filter(item == "Seed cotton") %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item %in% FBS_item) %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
