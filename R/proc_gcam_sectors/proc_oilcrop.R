
#There are 12 FBS_items and 50 SUA_items
#Coconut and products are included
#Groundnuts and products are included in nutseeds

# !!!"Poppy seed" and "Tallowtree seed" are under NutsSeeds in prodstat

GCAM_crop <- "OilCrop"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>% union(c("Cake, copra",
                                        "Cake, rapeseed", "Cake, mustard", "Cake, sesame seed",
                                        "Cake, sunflower")) %>%  #, "Cake, others" low data quality
  setdiff(c("Oil, hydrogenated", "Margarine, short", "Margarine, liquid", "Oil, boiled etc")) %>%
  union(
    FBS_SUA_item_mapping %>%
      filter(FBS_label  %in% c("Coconuts (including copra) and products", "Coconut Oil and products")) %>%
      pull(SUA_item) %>% unique()
  ) %>% setdiff("Cocoa, butter") #added in MiscCrop



FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(FBS_label) %>% unique() %>% union(c("Coconuts (including copra) and products","Coconut Oil and products"))

SUA_item1 <- FBS_SUA_item_mapping %>%
  filter(FBS_label  %in% c("Rape and Mustardseed and products", "Rape and Mustard Oil and products")) %>%
  pull(SUA_item) %>% unique() %>%
  union(c("Cake, rapeseed", "Cake, mustard"))

SUA_item2 <- FBS_SUA_item_mapping %>%
  filter(FBS_label  %in% c("Sesame seed and products", "Sesameseed Oil and products",
                           "Sunflower seed and products", "Sunflowerseed Oil and products",
                           "Coconuts (including copra) and products", "Coconut Oil and products")) %>%
  pull(SUA_item) %>% unique()  %>%
  union(c("Cake, sunflower", "Cake, copra", "Cake, sesame seed"))

SUA_item3 <- FBS_SUA_item_mapping %>%
  filter(FBS_label  %in% c("Olives (including preserved) and products", "Olive Oil and products")) %>%
  pull(SUA_item) %>% unique()

# Stillingia oil is toxic
SUA_item4 <- c("Castor oil seed", "Oil, castor beans", "Castor oil, hydrogenated (opal wax)",
               "Linseed", "Oil, linseed",
               "Tallowtree seed", "Vegetable tallow",
               "Karite nuts (sheanuts)", "Butter of karite nuts",
               "Kapok fruit", "Kapokseed in shell", "Kapokseed shelled", "Oil, kapok")
SUA_item5 <- FBS_SUA_item_mapping %>%
  filter(FBS_label  %in% c("Oilcrops, Other and products", "Oilcrops Oil, Other and products")) %>%
  pull(SUA_item) %>% unique()  %>%
  setdiff("Cocoa, butter") %>% setdiff(NA) %>% setdiff(SUA_item4) %>%
  setdiff(c("Oil, hydrogenated", "Margarine, short", "Margarine, liquid", "Oil, boiled etc"))
# NA is stillingia oil which is included in Tallowtree seed

setdiff(SUA_item, c(SUA_item1, SUA_item2, SUA_item3, SUA_item4, SUA_item5))


#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal



OilCrop_primary <- bal %>% Agg_reg(item, element) %>%
  filter(element == "Area harvested", value > 0) %>% pull(item)
OilCrop_processed <- setdiff(SUA_item, c(OilCrop_primary, NA))

unique(bal$item)
bal %>%
  Proc_primarize(c("Rapeseed"), c("Oil, rapeseed", "Cake, rapeseed")) %>%
  Proc_primarize(c("Mustard seed"), c("Oil, mustard", "Cake, mustard", "Flour, mustard")) %>%
  Proc_primarize(c("Sunflower seed"), c("Oil, sunflower", "Cake, sunflower")) %>%
  Proc_primarize(c("Copra"), c("Oil, coconut (copra)", "Cake, copra")) %>%
  Proc_primarize(c("Coconuts"), c("Coconuts, desiccated", "Copra")) %>%
  Proc_primarize(c("Sesame seed"), c("Oil, sesame", "Cake, sesame seed")) %>%
  Proc_primarize_addfeed( c("Oil, olive residues"), feedshare = 0.935) %>% #assuming a 6.5% olive oil rate
  Proc_primarize(c("Olives"), c("Olives preserved", "Oil, olive, virgin", "Oil, olive residues")) %>%
  Proc_primarize_addfeed( c("Oil, linseed"), feedshare = .63) %>%
  Proc_primarize(c("Linseed"), c("Oil, linseed")) %>%
  Proc_primarize(c("Oil, castor beans"), c("Castor oil, hydrogenated (opal wax)")) %>%
  Proc_primarize(c("Castor oil seed"), c("Oil, castor beans")) %>%
  Proc_primarize(c("Tallowtree seed"), c("Vegetable tallow")) %>%
  Proc_primarize(c("Karite nuts (sheanuts)"), c("Butter of karite nuts")) %>%
  Proc_primarize_addfeed( c("Oil, kapok"), feedshare = 0.8) %>%
  Proc_primarize(c("Kapokseed shelled"), c("Oil, kapok")) %>%
  Proc_primarize(c("Kapokseed in shell"), c("Kapokseed shelled")) %>%
  Proc_primarize(c("Kapok fruit"), c("Kapokseed in shell")) %>%
  Proc_primarize_addfeed( c("Oil, safflower", "Oil, poppy", "Oil, hempseed",  "Oil, jojoba",
                            "Oil, vegetable origin nes"), feedshare = 0.55) %>%
  mutate(item = replace(item, item %in% c("Safflower seed", "Poppy seed", "Melonseed",
                                          "Jojoba seed", "Hempseed","Oilseeds nes", "Tung nuts"),"Other oilseed")) %>%
  Agg_reg(area, year, item, element) %>%
  Proc_primarize("Other oilseed",
                 c("Flour, oilseeds", "Oil, safflower", "Oil, poppy", "Oil, hempseed",  "Oil, jojoba",
                   "Oil, vegetable origin nes", "Oil, tung nuts"))  %>%
  mutate(item = replace(item, item %in% c(OilCrop_primary, "Other oilseed"), "APE")) %>%
  Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% OilCrop_primary) %>% mutate(item = "OilCrop_primary") %>%
              Agg_reg(area, year, item, element) ) %>%
  bind_rows(bal %>% filter(item %in% OilCrop_processed[grepl("Oil,", OilCrop_processed)]) %>%
              mutate(item = "OilCrop_oil") %>%
              Agg_reg(area, year, item, element) ) %>%
  bind_rows(bal %>% filter(item %in% OilCrop_processed[grepl("Cake,", OilCrop_processed)]) %>%
              mutate(item = "OilCrop_cakemain") %>%
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




T2_palm %>%
  Reg_FAOtoGCAM(independent = "Malaysia") %>%
  mutate(item = replace(item, item %in% c("APE"), "APE_palmfruit")) %>%
  bind_rows(
    T2_soy %>%
      Reg_FAOtoGCAM(independent = "Malaysia") %>%
      mutate(item = replace(item, item %in% c("APE"), "APE_soybean"))
  ) %>%
  bind_rows(
    T2 %>%
      Reg_FAOtoGCAM(independent = "Malaysia") %>%
      mutate(item = replace(item, item %in% c("APE"), "APE_oilcrop"))
  ) -> oilcrops

write.csv(oilcrops, "output/oilcrops.csv")

oilcrops %>%
  group_by(element, item) %>%
  summarise(value = sum(value, na.rm = T)/5, .groups = "drop") %>%
  spread(item, value)-> oilcrops_mean
write.csv(oilcrops_mean, "output/oilcrops_mean1.csv")
