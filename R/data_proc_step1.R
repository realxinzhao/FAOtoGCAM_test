library(dplyr)
library(tidyr)
source("R/fn.faostat.R")
source("R/fn.dataproc.R")

#**************************
#*read raw_data
#*Preprocess raw data to:
#*use producing regions (largest meaningful set);
#*bilateral trade balance;
#*Clear accent in names & correct FAO regions nameing inconsistencies & remove aggregated regions
#*Keep useful subset of column and years
#*update item naming
#*Combine old FBSH & CB

#Production (303 items)
QCL <- get_faostat_bulk(code = "QCL", data_folder = data_folder)
#OA <- get_faostat_bulk(code = "OA", data_folder = data_folder)
#QV <- get_faostat_bulk(code = "QV", data_folder = data_folder)
#PP <- get_faostat_bulk(code = "PP", data_folder = data_folder)
#QV %>% filter(area_code < 350) %>% distinct(area) %>% pull() -> QV_reg
#PP %>% filter(area_code < 350) %>% distinct(area) %>% pull() -> PP_reg
#Bilateral trade: (426 items)
TM <- get_faostat_bulk(code = "TM", data_folder = data_folder)
#TCL <- get_faostat_bulk(code = "TCL", data_folder = data_folder) #there was inconsistency in FAO file naming
#Trade: (479 items)
TCL <- read_faostat_bulk(file.path(data_folder, basename("Trade_Crops_Livestock_E_All_Data_(Normalized).zip")))
#SUA (451 items)
SCL <- get_faostat_bulk(code = "SCL", data_folder = data_folder)
FBSH <- get_faostat_bulk(code = "FBSH", data_folder = data_folder)
CB <- get_faostat_bulk(code = "CB", data_folder = data_folder)
FBS <- get_faostat_bulk(code = "FBS", data_folder = data_folder)

#****************************
QCL %>% filter(area_code < 350) %>% distinct(area) %>% pull() -> QCL_reg
SCL %>% filter(area_code < 350) %>% distinct(area) %>% pull() -> SCL_reg
FBSH %>% filter(area_code < 350) %>% distinct(area) %>% pull() -> FBSH_reg
CB %>% filter(area_code < 350) %>% distinct(area) %>% pull() -> CB_reg
FBS %>% filter(area_code < 350) %>% distinct(area) %>% pull() -> FBS_reg
FBSH %>% filter(area_code < 350, year == 2013) %>% distinct(area) %>% pull() -> FBSH_reg1
setdiff(FBS_reg, FBSH_reg1); setdiff(FBSH_reg1, FBS_reg)


QCL %>% filter(`area_code` < 350, year > 2013) %>% distinct(area) %>% pull() -> QCL_reg1
TCL %>% filter(area_code < 350) %>% distinct(area) %>% filter(grepl("intra-trade", area) == F) %>% pull() -> TCL_reg
TM %>% filter(`reporter country code` < 350) %>% distinct(`reporter_countries`) %>% pull() -> TM_reporter
TM %>% filter(`partner country code` < 350) %>% distinct(`partner_countries`) %>% pull() -> TM_partner

TCL_reg %>% intersect(QCL_reg) %>%
  intersect(TM_partner) %>% #intersect(TM_reporter) %>%
  intersect(SCL_reg) %>%
  intersect(FBSH_reg) %>% intersect(CB_reg) -> reg_intersect #169 regions and + 3 when fixes FBSH.
#No major concerns on region coverage. Only small & unimportant regions/islands are not included.
#Thus, no major adjustments or interpolations are made across regions.
#There could be more concerns on items.

#**************************
#*Production and area
QCL %>% filter(year %in% Hist_Year, area_code < 350) %>%
  select(area, item, element, year, value) %>%
  rm_accent("item", "area") -> QCL1
saveRDS(QCL1, file.path(data_folder_proc,"QCL.rds"))
rm(QCL, QCL1)

#**************************
#*Trade data: quantity and value
#*FAO has better quality bilateral data since 1992, covering most SUA items
setdiff(TM_partner,QCL_reg); setdiff(TM_reporter,QCL_reg)
#TM has few (12) unimportant regions (unspecified area or islands), which are removed
#Hist_Year_Bilateral

TM %>%
  filter(year %in% Hist_Year_Bilateral,
         `partner_countries` %in% QCL_reg,
         `reporter_countries` %in% QCL_reg) -> TM1

TM1 %>% filter(element %in% c("Export Quantity")) %>% spread(element, value) %>%
  select(exporter = `reporter_countries`,
         importer = `partner_countries`, item, year, expflow = `Export Quantity`) %>%
  full_join(
    TM1 %>% filter(element %in% c("Import Quantity")) %>% spread(element, value)%>%
      select(importer = `reporter_countries`,
             exporter = `partner_countries`, item, year, impflow = `Import Quantity`),
    by = c("exporter", "importer", "item", "year")
  )  %>% replace_na(list(expflow = 0, impflow = 0)) %>%
  transmute(area = importer, year, item, source = exporter,
            value = if_else(expflow == 0, impflow, expflow)) -> TM2

#need to fix transportation cost
TM1 %>% filter(element %in% c("Export Value")) %>% spread(element, value) %>%
  select(exporter = `reporter_countries`,
         importer = `partner_countries`, item, year, expflow = `Export Value`) %>%
  full_join(
    TM1 %>% filter(element %in% c("Import Value")) %>% spread(element, value)%>%
      select(importer = `reporter_countries`,
             exporter = `partner_countries`, item, year, impflow = `Import Value`)
  )  %>% replace_na(list(expflow = 0, impflow = 0)) %>%
  transmute(area = importer, year, item, source = exporter,
            value = if_else(expflow == 0, impflow, expflow)) -> TM3

TM2 %>% mutate(element = "Import Quantity") %>%
  bind_rows(TM3 %>% mutate(element = "Import Value") ) %>%
  rm_accent("item", "area", "source") -> TM4

saveRDS(TM4, file.path(data_folder_proc,"TM.rds"))
rm(TM, TM1, TM2, TM3, TM4)
#*******
#*Gross trade
setdiff(TCL_reg, QCL_reg); setdiff(QCL_reg, TCL_reg)
#TCL has fewer regions (8 unimportant small regions); ignored
#*Hist_Year is used here and refined later

TCL %>%
  filter(year %in% Hist_Year,
         area %in% QCL_reg) %>%
  select(area, year, item, element, value) %>%
  rm_accent("item", "area") -> TCL1

saveRDS(TCL1, file.path(data_folder_proc,"TCL.rds"))
rm(TCL, TCL1)

TCL %>%
  filter(item %in% intersect(unique(TCL$item), unique(TM$item)),
         year >= 1992) %>%
  filter(grepl("Value", element) == F) %>%
  group_by(year, item, element) %>%
  summarise(value = sum(value,na.rm = T), .groups = "drop") %>%
  spread(element, value) %>%
  left_join(
    TM %>%
      filter(item %in% intersect(unique(TCL$item), unique(TM$item))) %>%
      group_by(year, item, element) %>%
      summarise(value = sum(value,na.rm = T), .groups = "drop") %>%
      filter(grepl("Value", element) == F) %>%
      mutate(element = paste0(element, "TM")) %>%
      spread(element, value)
  ) -> A


#**************************
#SUA
setdiff(SCL_reg,QCL_reg1); setdiff(QCL_reg1, SCL_reg)
# SCL has 24 fewer regions, mostly unimportant

SCL %>%
  filter(year %in% Hist_Year, area %in% QCL_reg) %>%
  #remove day items but keep year items of calories, proteins, and fats
  filter(!grepl("day", element)) %>%
  #use a consistent name set of elements
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food"),
         element = replace(element, element == "Other uses (non-food)", "Other uses")) %>%
  select(area, year, item, element, value) %>%
  rm_accent("item", "area") -> SCL1

saveRDS(SCL1, file.path(data_folder_proc,"SCL.rds"))
rm(SCL, SCL1)

#**************************
#FBSH & CB
setdiff(FBSH_reg, CB_reg); setdiff(CB_reg, FBSH_reg)
setdiff(QCL_reg, CB_reg); setdiff(CB_reg, QCL_reg)
#fix the three old region names in FBSH
FAO_rename <- data.frame(
  updated = c("Eswatini",
              "United Kingdom of Great Britain and Northern Ireland",
              "North Macedonia"),
  area = c("Swaziland",
           "United Kingdom",
           "The former Yugoslav Republic of Macedonia") )
FBSH %>%
  filter(year %in% Hist_Year, area %in% QCL_reg) %>%
  left_join(FAO_rename) %>%
  mutate(area = if_else(is.na(updated) == F, updated, area)) %>%
  bind_rows(
    CB %>% filter(year %in% Hist_Year, area %in% QCL_reg) %>%
      mutate(value = value / 1000) %>%
      filter(!item %in% intersect(unique(FBSH$item), unique(CB$item)))
  ) %>%
  mutate(element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed"),
         element = replace(element, element == "Other uses (non-food)", "Other uses"),
         element = replace(element, element == "Food supply quantity (tonnes)", "Food")) %>%
  filter(item_code < 2900, item_code != 2501) %>%  #remove aggregated
  select(area, year, item, element, value) %>%
  rm_accent("item", "area") -> FBSH1
saveRDS(FBSH1, file.path(data_folder_proc,"FBSH.rds"))
rm(FBSH, FBSH1)
#**************************

setdiff(FBS_reg, QCL_reg); setdiff(QCL_reg, FBS_reg)
unique(FBS$element)

FBS %>%
  filter(year %in% Hist_Year, area %in% QCL_reg) %>%
  filter(!grepl("capita", element)) %>%
  mutate(element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed"),
         element = replace(element, element == "Other uses (non-food)", "Other uses")) %>%
  filter(item_code < 2900, item_code != 2501) %>%  #remove aggregated
  select(area, year, item, element, value) %>%
  rm_accent("item", "area") -> FBS1
saveRDS(FBS1, file.path(data_folder_proc,"FBS.rds"))
rm(FBS, FBS1)

