

#There are 3 FBS_items and 22 SUA_items

GCAM_crop <- "NutsSeeds"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>%
  union(
    FBS_SUA_item_mapping %>%
      filter(FBS_label  %in% c("Groundnuts (Shelled Eq) and products","Groundnut Oil and products")) %>%
      pull(SUA_item) %>% unique()
  ) %>% union(c("Cake, groundnuts"))

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(FBS_label) %>% unique() %>%
  union(c("Groundnuts (Shelled Eq) and products", "Groundnut Oil and products",
          "Groundnuts", "Groundnut Oil")) #FBS in FAO was inconsistent


#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

NutsSeeds_primary <- bal %>% Agg_reg(item, element) %>%
  filter(element == "Area harvested", value > 0) %>% pull(item)
NutsSeeds_processed <- setdiff(SUA_item, NutsSeeds_primary)

unique(bal$item)
bal %>%
  Proc_primarize(c("Groundnuts, shelled"), c("Groundnuts, prepared", "Peanut butter", "Oil, groundnut", "Cake, groundnuts")) %>%
  Proc_primarize(c("Groundnuts, with shell"), c("Groundnuts, shelled")) %>%
  Proc_primarize(c("Brazil nuts, shelled", "Cashew nuts, shelled", "Almonds shelled",
                   "Hazelnuts, shelled", "Walnuts, shelled", "Pistachios", "Nuts nes"),
                 c("Nuts, prepared (exc. groundnuts)")) %>%
  Proc_primarize(c("Brazil nuts, with shell"), c("Brazil nuts, shelled")) %>%
  Proc_primarize(c("Cashew nuts, with shell"), c("Cashew nuts, shelled")) %>%
  Proc_primarize(c("Almonds, with shell"), c("Almonds shelled")) %>%
  Proc_primarize(c("Hazelnuts, with shell"), c("Hazelnuts, shelled")) %>%
  Proc_primarize(c("Walnuts, with shell"), c("Walnuts, shelled")) %>%
  mutate(item = replace(item, item %in% c(NutsSeeds_primary), "APE")) %>%
  Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% NutsSeeds_primary) %>% mutate(item = "NutsSeeds_primary") %>%
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



