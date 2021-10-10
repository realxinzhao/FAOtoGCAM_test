# Mannually downloaded data see data_raw/FAOSTAT_meta


FAO_ag_items_cal_SUA <- readr::read_csv("input/crop-remap_6_8_2021/aglu/FAO/FAO_ag_items_cal_SUA.csv", comment = "#")
#Maize germ oil was removed; included in maize products

FAO_an_items_cal_SUA <- readr::read_csv("input/crop-remap_6_8_2021/aglu/FAO/FAO_an_items_cal_SUA.csv", comment = "#")


#need to manually download the mapping from FAOSTAT under definition
FBS_SUA_item <- readxl::read_excel("data_raw/FAOSTAT_meta/FBS and SUA list_6-7-2021.xlsx")
names(FBS_SUA_item) <- c("FAO_FBS_code", "FBS_label", "CPC_code", "SUA_label")
FBS_SUA_item$FAO_FBS_code <- as.integer(FBS_SUA_item$FAO_FBS_code)

while (any(is.na(FBS_SUA_item$FBS_label))) {
  FBS_SUA_item %>%
    mutate(FBS_label = if_else(is.na(FBS_label), lag(FBS_label), FBS_label),
           FAO_FBS_code = if_else(is.na(FAO_FBS_code), lag(FAO_FBS_code), FAO_FBS_code)) -> FBS_SUA_item
}
#need to manually download those data set for extracting CPC code mapping to items
CPC_SUA <- readr::read_csv("data_raw/FAOSTAT_meta/SD_2015_CPC_6-9-2021.csv") %>%
  bind_rows(readr::read_csv("data_raw/FAOSTAT_meta/SP_2015_CPC_6-9-2021.csv")) %>%
  bind_rows(readr::read_csv("data_raw/FAOSTAT_meta/SC_2015_CPC_6-9-2021.csv")) %>%
  bind_rows(readr::read_csv("data_raw/FAOSTAT_meta/SL_2015_CPC_6-9-2021.csv")) %>%
  select(CPC_code = `Item Code (CPC)`, SUA_item = Item, datasetcode = `Domain Code`) %>%
  distinct() %>% mutate(CPC_code = toupper(CPC_code))



FBS_SUA_item %>% left_join(CPC_SUA %>% rm_accent("SUA_item") %>%
                             mutate(SUA_item = replace(SUA_item, SUA_item == "MatAC", "Mate")),
                           by = "CPC_code") %>% left_join(
  FBSH %>%
    bind_rows(CB %>%
                filter(!item %in% intersect(unique(FBSH$item), unique(CB$item)))) %>%
    select(FBSH_item = item, FAO_FBS_code = `item_code`) %>% distinct() %>%
    filter(FAO_FBS_code < 2900, FAO_FBS_code != 2501),   #exclude aggregated items & population
  by = "FAO_FBS_code"
) %>% left_join(
  FAO_ag_items_cal_SUA %>% select(FBSH_item = item, GCAM_commodity) %>%
    bind_rows(FAO_an_items_cal_SUA %>% select(FBSH_item = item, GCAM_commodity)), by = "FBSH_item"
) -> FBS_SUA_item_mapping



