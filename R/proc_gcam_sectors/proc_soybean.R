
#There are 6 SUA items for soybean and the processed food.
GCAM_crop <- "Soybean"
#*************
#* data 2014 to 2018
#*
SUA_item <- c(FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
                pull(SUA_item) %>% unique(), "Cake, soybeans")

unique(Bal_new_all$item)[grepl("Soy", unique(Bal_new_all$item), ignore.case = T)]
FBS_item[grepl("Soy", FBS_item, ignore.case = T)]


assertthat::assert_that(length(setdiff(SUA_item, unique(Bal_new_all$item))) == 0)


#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

bal %>%  Proc_primarize(c("Soybeans"),
                        c("Oil, soybean", "Cake, soybeans", "Soya sauce", "Soya paste",  "Soya curd")) %>%
  mutate(item = replace(item, item == "Soybeans", "APE")) %>%
  bind_rows(bal %>% filter(item %in% c("Soybeans", "Oil, soybean", "Cake, soybeans"))) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item == "Soyabeans") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  left_join(FBS %>% filter(item == "Soyabean Oil") %>%
              mutate(element = gsub(" Quantity", "", element)) %>% mutate(item = "FBS_oil") %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
