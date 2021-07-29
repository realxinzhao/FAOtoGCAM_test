#The code here aims to generate FAO FBS (new, 2014-) from the corresponding primary & processed files
#The mapping has been provided by FAO

unique(SC $element)
unique(SD $element) #cottonseed is seed..
unique(SL $element) #eggs in shell could be seed..
unique(SP $element)

# All new SUA files process
SC %>% bind_rows(SD) %>% bind_rows(SL) %>% bind_rows(SP) %>%
  FAO_ctry_remap(.colname = "area") -> SUA_new

###############################################
GCAM_crop <- "Dairy"
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique() -> FBS_item
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(SUA_item) %>% unique() -> SUA_item



###############################################
GCAM_crop <- "Beef"
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique() -> FBS_item
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(SUA_item) %>% unique() -> SUA_item

SUA_new %>% filter(`element code`>5000) %>% filter(item %in% c(SUA_item)) %>%
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  select(region = area, year, item, element, value) %>%
  spread(element, value, fill = 0) %>%  gather(element, value, -region, -year, -item) %>%
  spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element) -> bal

bal$element <- factor(bal$element,levels =
                        c("Opening stocks", "Production", "Export Quantity", "Import Quantity", "Stock Variation",
                          "Food", "Feed", "Seed", "Processed", "Other uses (non-food)", "Tourist consumption",
                          "Loss", "Residuals"))

bal %>% filter(region == "United States of America",
  year == 2015) %>% spread(item, value)
bal %>%  Proc_primarize(c("Meat, cattle", "Meat, buffalo"),
                        c("Meat, beef, dried, salted, smoked", "Meat, extracts",
                          "Meat, cattle, boneless (beef & veal)", "Meat, beef, preparations",
                          "Meat, beef and veal sausages", "Meat, homogenized preparations")
                        ) -> beef

.df = bal
source_item = c("Meat, cattle", "Meat, buffalo")
sink_item =  c("Meat, beef, dried, salted, smoked", "Meat, extracts",
               "Meat, cattle, boneless (beef & veal)", "Meat, beef, preparations",
               "Meat, beef and veal sausages", "Meat, homogenized preparations")



beef %>% group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  rename(APE = value) %>% #aggregated primary equivalent
  left_join(bal %>%  filter(item %in%  c("Meat, cattle", "Meat, buffalo")) %>%
              group_by(region, year, element) %>% summarise(value = sum(value)) %>%
              mutate(item = "Bovine") %>%
              spread(item, value)) %>%
  mutate(APE = if_else(element == "Production", Bovine, APE)) %>%
  gather(item, value, APE, Bovine) %>%
  left_join(AGLU_ctry_Unique %>% select(region = FAO_country, iso)) %>%
  left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID)) %>%
  left_join(GCAM_region_names %>% select(GCAM_region_ID, reg = region)) %>%
  group_by(reg, item, year, element) %>% summarise(value = sum(value), .groups = "drop") -> A


A %>% filter(reg == "USA", year == 2014) %>%
  group_by(item, reg, year, element) %>%
  summarise(value = sum(value)/1000000, .groups = "drop") %>%
  spread(item, value) %>% write.csv("0USbeef.csv")



###############################################
GCAM_crop <- "Soybean"
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique() -> FBS_item
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(SUA_item) %>% unique() -> SUA_item

SUA_new %>% filter(`element code`>5000) %>% filter(item %in% c(SUA_item)) %>%
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  select(region = area, year, item, element, value) %>%
  spread(element, value, fill = 0) %>%  gather(element, value, -region, -year, -item) %>%
  spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element) -> bal

bal$element <- factor(bal$element,levels =
                        c("Opening stocks", "Production", "Export Quantity", "Import Quantity", "Stock Variation",
                          "Food", "Feed", "Seed", "Processed", "Other uses (non-food)", "Tourist consumption",
                          "Loss", "Residuals"))

bal %>% filter(year == 2015) %>% spread(item, value)
bal %>%  Proc_primarize_oil(c("Soybeans"),
                            c("Oil, soybean", "Soya sauce", "Soya paste",  "Soya curd"),
                            oil_item = "Oil, soybean",
                            oilshare = 0.19/0.99) -> soybean


soybean %>% group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  rename(APE = value) %>% #aggregated primary equivalent
  left_join(bal %>%  filter(item == "Soybeans") %>% spread(item, value)) %>%
  mutate(APE = if_else(element == "Production", `Soybeans`, APE)) %>%
  gather(item, value, APE, `Soybeans`) %>%
  left_join(AGLU_ctry_Unique %>% select(region = FAO_country, iso)) %>%
  left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID)) %>%
  left_join(GCAM_region_names %>% select(GCAM_region_ID, reg = region)) %>%
  group_by(reg, item, year, element) %>% summarise(value = sum(value), .groups = "drop") -> A


A %>% filter(reg == "USA", year == 2014) %>%
  group_by(item, reg, year, element) %>%
  summarise(value = sum(value)/1000000, .groups = "drop") %>%
  spread(item, value) %>% write.csv("0USsoy.csv")




###############################################
GCAM_crop <- "PalmFruit"

FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique() -> FBS_item
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  filter(FBS_label %in% c("Palm Oil and products", "Palmkernel Oil and products", "Palm kernels and products")) %>%
  pull(SUA_item) %>% unique() -> SUA_item

SUA_new %>% filter(`element code`>5000) %>% filter(item %in% c(SUA_item)) %>%  #,  area == "Indonesia"
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  select(region = area, year, item, element, value) %>%
  spread(element, value, fill = 0) %>%  gather(element, value, -region, -year, -item) %>%
  spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element) -> bal

bal$element <- factor(bal$element,levels =
                        c("Opening stocks", "Production", "Export Quantity", "Import Quantity", "Stock Variation",
                          "Food", "Feed", "Seed", "Processed", "Other uses (non-food)", "Tourist consumption",
                          "Loss", "Residuals"))

bal %>%
  Proc_primarize(c("Oil palm fruit"), c("Oil, palm", "Palm kernels")) %>%
  Proc_primarize_oil(c("Palm kernels"), c("Oil, palm kernel"), oilshare = 0.5) %>%
  Proc_primarize(c("Oil, palm", "Oil, palm kernel"), c("Fatty acids", "Fatty substance residues")) -> palmfruit

palmfruit %>% group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  rename(APE = value) %>% #aggregated primary equivalent
  left_join(bal %>%  filter(item == "Oil palm fruit") %>% spread(item, value)) %>%
  mutate(APE = if_else(element == "Production", `Oil palm fruit`, APE)) %>%
  gather(item, value, APE, `Oil palm fruit`) %>%
  left_join(AGLU_ctry_Unique %>% select(region = FAO_country, iso)) %>%
  left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID)) %>%
  left_join(GCAM_region_names %>% select(GCAM_region_ID, reg = region)) %>%
  group_by(reg, item, year, element) %>% summarise(value = sum(value), .groups = "drop") -> A


A %>% filter(reg == "Indonesia", year == 2014) %>%
  group_by(item, reg, year, element) %>%
  summarise(value = sum(value)/1000000, .groups = "drop") %>%
  spread(item, value) %>% write.csv("0Indopalm.csv")

A %>% group_by(item, element) %>% summarise(value = sum(value)/1000000, .groups = "drop") %>%
  spread(item, value)-> world



#############
GCAM_crop <- "Corn"

FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique() -> FBS_item
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(SUA_item) %>% unique() -> SUA_item

SUA_new %>% filter(`element code`>5000) %>% filter(item %in% c(SUA_item)) %>%  #, area == "United States of America"
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  select(region = area, year, item, element, value) %>%
  spread(element, value, fill = 0) %>%  gather(element, value, -region, -year, -item) %>%
  spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element) -> bal

bal %>% Proc_primarize(c("Maize"), c("Flour, maize", "Germ, maize", "Bran, maize")) %>%
  Proc_primarize(c("Flour, maize"), c("Starch, maize", "Gluten, maize")) %>%
  Proc_primarize(c("Bran, maize", "Gluten, maize"), c("Feed and meal, gluten")) %>%
  Proc_primarize_oil(c("Germ, maize"), c("Oil, maize"), oilshare = 0.5) -> maize

maize %>% group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  rename(APE = value) %>% #aggregated primary equivalent
  left_join(bal %>%  filter(item == "Maize") %>% spread(item, value)) %>%
  mutate(APE = if_else(element == "Production", Maize, APE)) %>%
  gather(item, value, APE, Maize) %>%
  left_join(AGLU_ctry_Unique %>% select(region = FAO_country, iso)) %>%
  left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID)) %>%
  left_join(GCAM_region_names %>% select(GCAM_region_ID, reg = region)) %>%
  group_by(reg, item, year, element) %>% summarise(value = sum(value), .groups = "drop") -> A

A %>% group_by(item, reg, element) %>% summarise(value = sum(value)/1000000, .groups = "drop")

A %>% group_by(item, element) %>% summarise(value = sum(value)/1000000, .groups = "drop") %>%
  spread(item, value)-> world




GCAM_crop <- "Wheat"

FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique() -> FBS_item
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(SUA_item) %>% unique() -> SUA_item


SUA_new %>% filter(`element code`>5000) %>% filter(item %in% c(SUA_item)) %>%
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  select(region = area, year, item, element, value) %>%
  spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element) -> bal

bal %>% Proc_primarize(
  c("Wheat"),
  c("Food preparations, flour, malt extract", "Bulgur",
    "Cereals, breakfast", "Bran, wheat", "Germ, wheat", "Flour, wheat")) %>%
  Proc_primarize(
    c("Flour, wheat"),
    c("Gluten, wheat", "Starch, wheat", "Bread", "Pastry", "Macaroni",
      "Wafers", "Mixes and doughs")) -> wheat




A %>%  filter(year == 2015) %>% mutate(value = value /1000) %>%
  spread(item,value)

A %>% group_by(region, year, element) %>%
  summarise(value = sum(value)/1000) %>% filter(year == 2015)






##########################################################################
source_item = c("Maize")
sink_item = c("Flour, maize", "Germ, maize", "Bran, maize")

bal %>% filter(element == "Production",
  item %in% sink_item) %>%
  group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  bind_rows(
    bal %>% filter(element == "Processed",
                   item %in% source_item) %>%
      group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop")
  ) %>% spread(element, value, fill = 0) %>%
  mutate(extraction_rate = Production / Processed) %>%
  select(region, year, extraction_rate) %>% right_join(bal) %>%
  mutate(value = if_else(item %in% sink_item, value / extraction_rate, value),
         value = if_else(item %in% source_item & element == "Processed", 0, value)) %>%
  select(-extraction_rate) -> bal1


source_item = c("Flour, maize")
sink_item = c("Starch, maize", "Gluten, maize")

bal1 %>% filter(element == "Production",
               item %in% sink_item) %>%
  group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  bind_rows(
    bal1 %>% filter(element == "Processed",
                   item %in% source_item) %>%
      group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop")
  ) %>% spread(element, value, fill = 0) %>%
  mutate(extraction_rate = Production / Processed) %>%
  select(region, year, extraction_rate) %>% right_join(bal1) %>%
  mutate(value = if_else(item %in% sink_item, value / extraction_rate, value),
         value = if_else(item %in% source_item & element == "Processed", 0, value)) %>%
  select(-extraction_rate) -> bal2


source_item = c("Bran, maize", "Gluten, maize")
sink_item = c("Feed and meal, gluten")

bal2 %>% filter(element == "Production",
                item %in% sink_item) %>%
  group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  bind_rows(
    bal2 %>% filter(element == "Processed",
                    item %in% source_item) %>%
      group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop")
  ) %>% spread(element, value, fill = 0) %>%
  mutate(extraction_rate = Production / Processed) %>%
  select(region, year, extraction_rate) %>% right_join(bal2) %>%
  mutate(value = if_else(item %in% sink_item, value / extraction_rate, value),
         value = if_else(item %in% source_item & element == "Processed", 0, value)) %>%
  select(-extraction_rate) -> bal3


source_item = c("Germ, maize")
sink_item = c("Oil, maize")

oilshare = 0.5 #oil / (oil + cake)

bal3 %>% filter(element == "Production",
                item %in% sink_item) %>%
  group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  bind_rows(
    bal3 %>% filter(element == "Processed",
                    item %in% source_item) %>%
      group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop")
  ) %>% spread(element, value, fill = 0) %>%
  mutate(oilshare = oilshare,
         cakeprod =  Production * (1 / oilshare - 1),
         extraction_rate = (Production + cakeprod) / Processed ) %>%
  select(region, year, extraction_rate, cakeprod) %>% right_join(bal3) %>%
  mutate(value = if_else(item %in% sink_item & element == "Production", value + cakeprod, value),
         value = if_else(item %in% sink_item & element == "Feed", value + cakeprod, value),
         value = if_else(item %in% sink_item, value / extraction_rate, value),
         value = if_else(item %in% source_item & element == "Processed", 0, value)) %>%
  select(-extraction_rate, -cakeprod) -> bal4

bal4 %>% filter(item %in% sink_item, year == 2015)


bal %>% left_join(Maizelevel) %>%
  group_by(region, year, level, element) %>% summarise(value = sum(value)) %>%
  ungroup()  -> A


A %>% filter((element %in% c("Production") & level == "1") | (element == "Processed" & level == "0")) %>%
  mutate(level = "extrate1") %>% bind_rows(
    A %>% filter(element == "Production" & level == 2) %>%
      mutate(level = "extrate2")
  )  %>% bind_rows(
    bal %>% filter(element %in% c("Processed") & item == "Flour, maize") %>% select(-item) %>% mutate(level = "extrate2")
  ) %>%
  spread(element, value) %>%
  mutate(extraction_rate =  Production / Processed,
         extraction_rate = if_else(is.na(extraction_rate)| extraction_rate == 0, 1, extraction_rate)) %>%
  select(-Processed, -Production) %>% spread(level, extraction_rate) %>%
  right_join(A) -> C


C %>% filter(level != 100) %>%
  mutate(value = if_else(level == 1, value / extrate1,
                         if_else(level == 2, value / extrate1 / extrate2, value))) %>%
  group_by(region, year, element) %>% summarise(value = sum(value)) %>%
  spread(element, value, fill = 0) %>% left_join(
    C %>% filter(level == 0, element %in%  c("Opening stocks", "Stock Variation","Production", "Feed")) %>%
      select(region, year, element, value) %>% spread(element, value, fill = 0) %>%
      transmute(region, year, Production_primary = Production,
                `Stock variation_primary`= `Stock Variation`,
                `Closing stock_primary` = `Opening stocks` + `Stock Variation`,
                Feed_primary = Feed)
  ) %>% mutate(Processed = Processed - (Production - Production_primary),
               Production = Production_primary,
               `Closing stocks` = `Opening stocks` + `Stock Variation`,
               `Regional supply` = `Opening stocks` + Production - `Export Quantity` + `Import Quantity`,
               `Regional demand` = `Closing stocks` + Feed + Food + Loss + Processed + Seed +
                 Residuals + `Other uses (non-food)`,  # + `Tourist consumption`
               diff = `Regional supply` - `Regional demand`) %>%
  gather(element, value, -region, -year) -> Good

Good %>% filter(year == 2015)


unique(A$area)

setdiff(
  unique(A$area),
  unique(AGLU_ctry$FAO_country)
)



setdiff(unique(SUA_new$area), unique(FAO_ctry$Country))

setdiff(unique(SUA_old$area), unique(FAO_ctry$Country))

setdiff(unique(FBS$area), unique(FBSH$area)); setdiff( unique(FBSH$area), unique(FBS$area))

setdiff(unique(FAO_ctry$Country), unique(SUA_new$area) )


unique(SUA_new$area)
# All old SUA files process
BC %>% bind_rows(BL) -> SUA_old

unique(SUA_old$area)

setdiff(unique(SUA_new$area), unique(SUA_old$area)); setdiff(unique(SUA_old$area), unique(SUA_new$area))
  filter(`element code`>5000)


unique(FBS $element)
unique(FBSH $element)

setdiff(unique(FBS $element),unique(FBSH $element)); setdiff(unique(FBSH $element),unique(FBS $element))
setdiff(unique(FBS $item),unique(FBSH $item)); setdiff(unique(FBSH $item),unique(FBS $item))




SC %>% filter(year %in% yr, `element code`>5000,
              item %in% SUA_item, area %in% reg,) %>%
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  select(region = area, year, item,  element, value) %>% bind_rows(
  SD %>% filter(year %in% yr, `element code` > 5000,
                item %in% SUA_item, area %in% reg) %>%
    mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
    select(region = area, year, item, element, value)
) %>%   spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element)  -> bal




SL
             item %in% SUA_item, area %in% reg,)

setdiff(unique(FBS$item), unique(FBSH$item));  setdiff(unique(FBSH$item), unique(FBS$item))
setdiff(unique(FBS$`item code`), unique(FBSH$`item code`));  setdiff(unique(FBSH$`item code`), unique(FBS$`item code`))


setdiff(
c(unique(FAO_ag_items_cal_SUA$item), unique(FAO_an_items_cal_SUA$item)), unique(FBSH$item))
setdiff(unique(FBSH$item),
  c(unique(FAO_ag_items_cal_SUA$item), unique(FAO_an_items_cal_SUA$item)))


###################################
GCAM_crop <- "Corn"
GCAM_crop <- "Wheat"

FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique() -> FBS_item
FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(SUA_item) %>% unique() -> SUA_item

reg = "United States of America"
yr = 2015

reg = unique(SC$area)
yr = unique(FBS$year)

FBS %>% filter(year %in% yr, item %in% FBS_item, `element code`>5000, area %in% reg) %>%
  mutate(element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  select(region = area, year, item, element, value)  %>% bind_rows(
    SC %>% filter(year %in% yr, item %in% SUA_item, area %in% reg, `element code`>5000) %>%
      mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
      select(region = area, year, item,  element, value)
  ) %>% bind_rows(
    SD %>% filter(year %in% yr, `element code` > 5000,
                  item %in% SUA_item, area %in% reg) %>%
      mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
      select(region = area, year, item, element, value, )
  ) %>%   spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element)  -> bal

bal %>% left_join(data.frame(item = c(FBS_item, SUA_item), level = c(100, 0, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2) )) %>%
  group_by(region, year, level, element) %>% summarise(value = sum(value)) %>% ungroup() -> A


A %>% filter((element %in% c("Production") & level == 1) | (element == "Processed" & level == 0)) %>%
  mutate(level = "extrate1") %>% bind_rows(
    A %>% filter((element %in% c("Processed") & level == 1)|(element == "Production" & level == 2)) %>%
      mutate(level = "extrate2")
  )  %>% spread(element, value) %>%
  mutate(extraction_rate =  Production / Processed,
         extraction_rate = if_else(is.na(extraction_rate)| extraction_rate == 0, 1, extraction_rate)) %>%
  select(-Processed, -Production) %>% spread(level, extraction_rate) %>%
  right_join(A) -> C


C %>% filter(level != 100) %>%
  mutate(value = if_else(level == 1, value / extrate1,
                         if_else(level == 2, value / extrate1 / extrate2, value))) %>%
  group_by(region, year, element) %>% summarise(value = sum(value)) %>%
  spread(element, value, fill = 0) %>% left_join(
    C %>% filter(level == 0, element %in%  c("Opening stocks", "Stock Variation","Production", "Feed")) %>%
      select(region, year, element, value) %>% spread(element, value, fill = 0) %>%
      transmute(region, year, Production_primary = Production,
                `Stock variation_primary`= `Stock Variation`,
                `Closing stock_primary` = `Opening stocks` + `Stock Variation`,
                Feed_primary = Feed)
  ) %>% mutate(Processed = Processed - (Production - Production_primary),
               Production = Production_primary,
               `Closing stocks` = `Opening stocks` + `Stock Variation`,
               `Regional supply` = `Opening stocks` + Production - `Export Quantity` + `Import Quantity`,
               `Regional demand` = `Closing stocks` + Feed + Food + Loss + Processed + Seed +
               Residuals + `Tourist consumption` + `Other uses (non-food)`,
               diff = `Regional supply` - `Regional demand`) %>%
  gather(element, value, -region, -year) -> Good

unique(Good$element)

Good %>% filter(is.na(value) ==F) %>%
  group_by(year, element) %>% summarise(value = sum(value)) %>% spread(year, value)



#%>% spread(item, value, fill = 0)

bal$element <- factor(bal$element,levels =
                        c("Opening stocks", "Production", "Export Quantity", "Import Quantity", "Stock Variation",
                          "Domestic supply quantity", "Food", "Feed", "Seed", "Processed", "Other uses (non-food)",
                          "Loss", "Residuals"))

bal %>% gather(variable, value, -element, -year) -> A
write.csv(bal %>% arrange(element), "usWheat_2015_update.csv")



#************************************
#*FBSH: Food balance sheet old 1961-2013
code = "FBSH"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> FBSH
code = "FBS"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> FBS



FAO_item <- readr::read_csv("data_raw/FAOSTAT_meta/FAOSTAT_data_item_6-7-2021.csv") %>%
  select(datasetcode = "Domain Code", domain = "Domain",
         itemcode = "Item Code", item = "Item")





FAO_an_items_cal_SUA <- readr::read_csv("input/gcam-core_6_7_2021/aglu/FAO/FAO_an_items_cal_SUA.csv", comment = "#")
FAO_ag_items_cal_SUA <- readr::read_csv("input/gcam-core_6_7_2021/aglu/FAO/FAO_ag_items_cal_SUA.csv", comment = "#")

GCAM_FAO_ag_item <- readr::read_csv("input/gcam-core_6_7_2021/aglu/FAO/FAO_ag_items_PRODSTAT.csv", comment = "#")
GCAM_FAO_an_item <- readr::read_csv("input/gcam-core_6_7_2021/aglu/FAO/FAO_an_items_PRODSTAT.csv", comment = "#")

code <- c("PD", "PP",  "FO", "QC", "QL", "QA", "QD", "QP", "BC", "BL",
          "SC", "SD", "SL", "SP", "FBS", "FBSH", "TM")  #



fao_metadata %>% filter(datasetcode %in% code) %>%
  select(1, 2, 7, 10, 11, 12) %>%
  mutate(filelocation = gsub("http://fenixservices.fao.org/faostat/static/bulkdownloads/", "", filelocation)) ->
  FAO_key_metadata
# Check existing data
data_folder <- "data_raw/"

Local_rawdata_info(data_folder) -> local_metadata



#************************************


"FAO_ag_Feed_t_SUA"
"FAO_ag_Food_t_SUA"
"FAO_ag_Exp_t_SUA"
"FAO_ag_Imp_t_SUA"



#"BC", "BL"
FAO_item %>% filter(datasetcode %in% c("BC"), itemcode < 2900) -> FAO_item_BC  #exclude aggregated items
FAO_item %>% filter(datasetcode %in% c("BL"), itemcode < 2900) -> FAO_item_BL  #exclude aggregated items
FAO_item %>% filter(datasetcode %in% c("FBS"), itemcode < 2900, itemcode != 2501) -> FAO_item_FBS  #exclude aggregated items & population

#"SC", "SD", "SL", "SP"

FAO_item %>% filter(datasetcode %in% c("SC")) -> FAO_item_SC
FAO_item %>% filter(datasetcode %in% c("SD")) -> FAO_item_SD
FAO_item %>% filter(datasetcode %in% c("SL")) -> FAO_item_SL
FAO_item %>% filter(datasetcode %in% c("SP")) -> FAO_item_SP


setdiff(unique(FAO_ag_items_cal_SUA$item), FAO_item_BC$item); setdiff(FAO_item_BC$item, unique(FAO_ag_items_cal_SUA$item))
setdiff(unique(FAO_an_items_cal_SUA$item), FAO_item_BL$item); setdiff(FAO_item_BL$item, unique(FAO_an_items_cal_SUA$item))

setdiff(unique(BC$item), FAO_item_BC$item); setdiff(FAO_item_BC$item, unique(BC$item))
setdiff(unique(BL$item), FAO_item_BL$item); setdiff(FAO_item_BL$item, unique(BL$item))
# "Rice and products" in FAO_item_BC  "Rice (Milled Equivalent)" in FAO_ag_items_cal_SUA & BC
#BL, FAO_an_items_cal_SUA & FAO_item_BL consistent

setdiff(unique(BC$item), FAO_ag_items_cal_SUA$item); setdiff(FAO_ag_items_cal_SUA$item, unique(BC$item))
setdiff(unique(BL$item), FAO_an_items_cal_SUA$item); setdiff(FAO_an_items_cal_SUA$item, unique(BL$item))

setdiff(unique(FBS$item), unique(FBSH$item));setdiff(unique(FBSH$item), unique(FBS$item))
setdiff(unique(FBS$item), unique(BC$item));setdiff(unique(BC$item), unique(FBS$item))

setdiff(unique(FBS$item), FAO_ag_items_cal_SUA$item); setdiff(FAO_ag_items_cal_SUA$item, unique(FBS$item))
intersect(c(FAO_ag_items_cal_SUA$item, FAO_an_items_cal_SUA$item), unique(FBS$item))

setdiff(
  c(FAO_ag_items_cal_SUA$item, FAO_an_items_cal_SUA$item), #131 items all from BC and BL
  c(unique(BC$item), unique(BL$item)) )

intersect( c(unique(BC$item), unique(BL$item)), FAO_item_FBS$item)   #99 items in FBS




c(FAO_ag_items_cal_SUA %>%  filter(!is.na(GCAM_commodity)) %>% pull(item),
  FAO_an_items_cal_SUA %>%  filter(!is.na(GCAM_commodity)) %>% pull(item)) -> GCAM_SUA  #106 ag & an commodities

c(FAO_item_BC$item, FAO_item_BL$item) -> FAO_SUA_old  #132 items & only 106 were used in GCAM (avoid double counting)
setdiff(GCAM_SUA, FAO_SUA_old)   #all GCAM_SUA from FAO_SUA_old items in FBS

setdiff(GCAM_SUA, FAO_item_FBS$item)   #21 items not in FBS
intersect(GCAM_SUA, FAO_item_FBS$item)   #85 items in FBS

setdiff(FAO_SUA_old, FAO_item_FBS$item); setdiff(FAO_item_FBS$item, FAO_SUA_old)      #99 FAO_SUA_old items in FBS




FAO_item %>% filter(datasetcode %in% c("FBSH"), itemcode < 2900, itemcode != 2501) -> FAO_item_FBSH  #exclude aggregated items & population

setdiff(FAO_SUA_old, FAO_item_FBSH$item); setdiff(FAO_item_FBSH$item, FAO_SUA_old)      #99 FAO_SUA_old items in FBS

####################################################
# BC & BL are consistent with FBSH

intersect(unique(FAO_item_FBSH$item), unique(BC$item)) -> BC_FBSH_item

#"Wheat and products"

FBSH %>% filter(year == 2013, item %in%  BC_FBSH_item, `element code` > 5000#, area == "United States of America"
                ) %>%
  select(area, item, year, element, FBSH = value)  %>% full_join(
    BC %>% filter(year == 2013, item %in%  BC_FBSH_item#, area == "United States of America"
                  ) %>%
      mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
      transmute(area, item, year, element, BC = value/1000)
  ) %>% mutate(diff = FBSH - BC) -> A



BC %>% filter(grepl("maize", item, ignore.case = T),
  year == 2013, area == "United States of America"
) %>%
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  transmute(area, item, year, element,`element code`, BC = value/1000) %>% spread(item, BC) %>% arrange(`element code`) %>%
  write.csv("wheat2010.csv")


intersect(unique(FAO_item_FBSH$item), unique(BL$item)) -> BL_FBSH_item

FBSH %>% filter(year == 2013, item %in%  BL_FBSH_item, `element code` > 5000#, area == "United States of America"
) %>%
  select(area, item, year, element, FBSH = value)  %>% full_join(
    BL %>% filter(year == 2013, item %in%  BL_FBSH_item#, area == "United States of America"
    ) %>%
      mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
      transmute(area, item, year, element, BL = value/1000)
  ) %>% mutate(diff = FBSH - BL) -> A


##########################################################################

reg = "United States of America"
yr = 2015
FBS_item = "Wheat and products"
SC_item = "Wheat"
SD_grepl_item = " wheat|bread|pastry|bulgur|doughs|cereals|Food preparations|wafer|Macaroni"

FBS_item = FAO_item_FBS %>%
  filter(grepl("soy", item, ignore.case = T)) %>% pull(item)
SC_item = FAO_item_SC %>%
  filter(grepl("soy", item, ignore.case = T)) %>% pull(item)
SD_grepl_item = FAO_item_SD %>%
  filter(grepl(" wheat|bread|pastry|bulgur|doughs|cereals|Food preparations|wafer|Macaroni", item, ignore.case = T)) %>% pull(item)

FBS %>% filter(year == yr, item %in% FBS_item, `element code`>5000,
               area == reg) %>%
  mutate(element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  select(item, year, element, value) %>% spread(item, value) %>% full_join(
    SC %>% filter(year == yr, item %in% SC_item, area == reg) %>%
      select(item, year, element, value) %>% spread(item, value)
  ) %>% full_join(
    SD %>% filter(year == yr, `element code`>5000,
                  item %in% c(SD_grepl_item),
                  area == reg) %>%
      mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
      select(item, year, element, value, ) %>% spread(item, value)
  ) -> A


write.csv(A, "uswheat_2015_update.csv")

readr::read_csv("data_raw/psd_alldata_csv/psd_alldata.csv") ->psd


##########################################################################

#************************************
#*SUA old
#*SC:  SUA crops
code = "SC"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> SC

code = "SD"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> SD

code = "SL"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> SL

code = "SP"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> SP

code = "FBSH"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> FBSH

code = "QC"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> QC

code = "QA"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> QA

code = "QD"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> QD

code = "QL"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> QL

code = "QP"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> QP

unique(SC$item)
unique(SD$item)

unique(SD$element)
SD %>% filter(element %in% c("Opening stocks", "Production", "Stock Variation")) %>%
  filter(area == "United States of America") %>% select(-`element code`, -flag) %>%
  spread(element, value) %>%
  #filter(grepl("Oil", item)) %>%
  mutate(r1 = round(`Opening stocks` /Production *100, 0),
         r2 = round(`Stock Variation` /Production *100, 0))-> A



unique(SL$element)
SL %>% filter(element %in% c("Opening stocks", "Production", "Stock Variation")) %>%
  filter(area == "United States of America") %>% select(-`element code`, -flag) %>%
  spread(element, value) %>%
  mutate(r1 = round(`Opening stocks` /Production *100, 0),
         r2 = round(`Stock Variation` /Production *100, 0))-> A

SP %>% filter(element %in% c("Opening stocks", "Production", "Stock Variation")) %>%
  filter(area == "United States of America") %>% select(-`element code`, -flag) %>%
  spread(element, value) %>%
  mutate(r1 = round(`Opening stocks` /Production *100, 0),
         r2 = round(`Stock Variation` /Production *100, 0))-> A


intersect(
  unique(SC$item),
  unique(GCAM_FAO_ag_item$item)
)

intersect(
  unique(SL$item),
  unique(FAO_an_items_cal_SUA$item)
)

intersect(
  unique(SP$item),
  unique(FAO_an_items_cal_SUA$item)
)

intersect(
  unique(SL$item),
  unique(GCAM_FAO_an_item$item)
)

intersect(
  unique(SP$item),
  unique(GCAM_FAO_an_item$item)
)


intersect(
  unique(SP$item),
  unique(BL$item)
)

intersect(
  unique(BL$item),
  unique(SL$item)
)

code = "BL"; assign(code, get_faostat_bulk(code = code, data_folder = data_folder, download = download))
#The difference only includes aggregated items
setdiff(unique(BC$item), unique(FAO_ag_items_cal_SUA$item))

setdiff(unique(BL$item), unique(FAO_an_items_cal_SUA$item))

intersect(
  unique(BC$item),
  unique(FAO_ag_items_cal_SUA$item)
)





unique(BC$element)
unique(BL$element)




#************************************
#*SUA old
#*BL:  SUA Commodity Balances - Livestock and Fish Primary Equivalent
code = "BL"; assign(code, get_faostat_bulk(code = code, data_folder = data_folder, download = download))
unique(BL$unit)

gcam_dataset = "FAO_an_Food_t_SUA"
BL %>%
  filter(item %in% unique(FAO_an_items_cal_SUA$item)) %>%
  filter(element == "Food supply quantity (tonnes)") %>%
  select(countries = area, country.codes = `area code`,
         item, item.codes = `item code`, element, element.codes = `element code`,
         year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap() -> FAO_an_Food_t_SUA




output_csv_data(gcam_dataset,
                col_type_nonyear = "cicici",
                title = "FAO livestock food consumption by country.item.year",
                unit = "tonnes", code = code)

"FAO_an_Food_t_SUA"
"FAO_an_Exp_t_SUA"
"FAO_an_Imp_t_SUA"
"FAO_an_Prod_t_SUA"


#************************************
#*SUA old
#*BC:  SUA Commodity Balances - Crops Primary Equivalent
#*
code = "BC"; assign(code, get_faostat_bulk(code = code, data_folder = data_folder, download = download))

"FAO_ag_Feed_t_SUA"
"FAO_ag_Food_t_SUA"
"FAO_ag_Exp_t_SUA"
"FAO_ag_Imp_t_SUA"

gcam_dataset = "FAO_ag_Food_t_SUA"

unique(get(code)[, "element"])
FAO_ag_Food_t_SUA <-
assign(gcam_dataset,
       get(code) %>%
         filter(item %in% unique(FAO_ag_items_cal_SUA$item)) %>%
          filter(element == "Food supply quantity (tonnes)") %>%
          select(countries = area, country.codes = `area code`,
                 item, item.codes = `item code`, element, element.codes = `element code`,
                 year, value) %>%
          spread(year, value) %>%
          FAO_ctry_remap()
       )

BC %>%
  filter(item %in% unique(FAO_ag_items_cal_SUA$item)) %>%
  filter(element == "Food supply quantity (tonnes)") %>%
  select(countries = area, country.codes = `area code`,
         item, item.codes = `item code`, element, element.codes = `element code`,
         year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap() -> FAO_ag_Food_t_SUA


output_csv_data(gcam_dataset,
                col_type_nonyear = "cicici",
                title = "FAO agricultural food consumption by country.item.year",
                unit = "tonnes", code = code)


#************************************










unique(SUA_FS_crop$element)
unique(SUA_bal_crop$element)




setdiff(unique(FAO_ag_items_cal_SUA$item),
        unique(SUA_bal_crop$item))

setdiff(unique(SUA_bal_crop$item), unique(FAO_ag_items_cal_SUA$item))
intersect(unique(SUA_bal_crop$item), unique(FAO_ag_items_cal_SUA$item))

setdiff(unique(SUA_bal_an$item), unique(FAO_an_items_cal_SUA$item))
intersect(unique(FAO_an_items_cal_SUA$item), unique(SUA_bal_an$item))



intersect(unique(FBS$item),
          unique(FBSH$item)
)
setdiff(unique(FBS$item), unique(FBSH$item))
setdiff(unique(FBSH$item), unique(FBS$item))


intersect(unique(FAO_ag_items_cal_SUA$item),
        unique(FBSH$item)
        )

intersect(unique(FAO_an_items_cal_SUA$item),
        unique(FBSH$item)
)


code = "FBS"; assign(code, get_faostat_bulk(code = code, data_folder = data_folder, download = download))


