###############################################

GCAM_crop <- "PalmFruit"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  filter(FBS_label %in% c("Palm Oil and products", "Palmkernel Oil and products", "Palm kernels and products")) %>%
  pull(SUA_item) %>% unique() %>% setdiff(c("Fatty acids", "Fatty substance residues")) %>% union("Cake, palm kernel")

unique(Bal_new_all$item)[grepl("palm", unique(Bal_new_all$item), ignore.case = T)]
FBS_item[grepl("palm", FBS_item, ignore.case = T)]

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

#Proc_primarize

bal %>%
  Proc_primarize(c("Palm kernels"), c("Oil, palm kernel", "Cake, palm kernel")) %>%
  Proc_primarize(c("Oil palm fruit"), c("Palm kernels", "Oil, palm")) %>%
  mutate(item = replace(item, item == "Oil palm fruit", "APE")) %>%
  bind_rows(bal %>% filter(item %in% SUA_item)) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM(independent = "Malaysia") %>%
  group_by(element, item) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3

T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item == "Palm Oil") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS_oil") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  left_join(FBS %>% filter(item == "Palmkernel Oil") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS_kerneloil") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3


T2 %>%
  Reg_FAOtoGCAM %>%
  group_by(element, item) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
write.csv(T3, "palm.csv")
