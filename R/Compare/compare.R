#Compare new results with GCAM 5.3 on balance


list.files("input/gcam-5.3-output")

GCAM5p3_bal_ag<- readr::read_csv("input/gcam-5.3-output/L109.ag_ALL_Mt_R_C_Y.csv",
                      col_types = NULL,comment = "#")

GCAM5p3_bal_an<- readr::read_csv("input/gcam-5.3-output/L109.an_ALL_Mt_R_C_Y.csv",
                              col_types = NULL,comment = "#")

GCAM5p3_trade <- readr::read_csv("input/gcam-5.3-output/L1091.GrossTrade_Mt_R_C_Y.csv",
                      col_types = NULL,comment = "#")

GCAM5p3_bal_ag %>% filter(year == 2015) %>%
  left_join(GCAM5p3_trade) %>% filter(!is.na(GrossExp_Mt)) %>%
  left_join(GCAM_region_names %>% select(GCAM_region_ID, region), by = "GCAM_region_ID") %>%
  select(-GCAM_region_ID) %>%
  gather(element_old, value, -GCAM_commodity, -year, -region) %>% bind_rows(
    GCAM5p3_bal_an %>% filter(year == 2015) %>%
      left_join(GCAM5p3_trade) %>% filter(!is.na(GrossExp_Mt)) %>%
      left_join(GCAM_region_names %>% select(GCAM_region_ID, region), by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) %>%
      gather(element_old, value, -GCAM_commodity, -year, -region)
  ) %>% left_join(
    data.frame(
      element_old = c("Prod_Mt", "Food_Mt", "Feed_Mt", "Biofuels_Mt", "OtherUses_Mt", "GrossExp_Mt", "GrossImp_Mt"),
      element = c("Production",  "Food", "Feed", "Others", "Others", "Export", "Import")
    )
  ) %>% filter(!is.na(element)) %>%
  select(region, item = GCAM_commodity, year, element, value )->
  GCAM5p3_bal






c("Area harvested",
  "Opening stocks", "Production", "Import",
  "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
  "Residuals", "Regional supply", "Regional demand", "Stock Variation") -> Bal_element_new


SSA <- function(df){
  df %>%
    left_join(AGLU_ctry_Unique %>% select(area = FAO_country, iso), by = "area") %>%
    left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
    left_join(GCAM_region_names %>% select(GCAM_region_ID, region), by = "GCAM_region_ID") %>%
    group_by(region, element, item) %>% #filter(region == "USA") %>%
    summarise(value = sum(value, na.rm = T)/5, .groups = "drop")
}


GCAM_item <- FBS_SUA_item_mapping %>% pull(GCAM_commodity) %>% unique()
GCAM_item <- GCAM_item %>% setdiff(c("OtherMeat_Fish", NA))
GCAM_item <- GCAM_item %>%
  setdiff(c("OtherMeat_Fish", "Beef", "Pork", "Poultry",
            "SheepGoat", "Dairy", NA))
GCAM_item <- c("Wheat", "Corn", "SugarCrop", "Rice")

lapply(GCAM_item, function(item){
  SSA(get(item))}) %>% bind_rows() ->
  df_SSA

saveRDS(df_SSA, file.path(out_dir, "Intermediate", "df_SSA.rds") )
readRDS(file.path(out_dir, "Intermediate", "df_SSA.rds") ) -> df_SSA

element = c("Production",  "Food", "Feed", "Others", "Others", "Export", "Import")

df_SSA %>% mutate(value = value / 1000) %>%
  filter(!element %in% c("Area harvested", "Opening stocks", "Closing stocks",
                                   "Regional supply", "Regional demand")) %>%
  mutate(element = as.character(element),
         element = if_else(element %in% c("Production",  "Food", "Feed", "Others", "Others", "Export", "Import"),
                           element, "Others")) %>%
  mutate(item = as.character(item),
         item = if_else(item == "Soybean", "OilCrop", item),
         item = if_else(item %in% c("Fruits", "NutsSeeds", "Legumes", "Fruits", "Vegetables"),
                        "MiscCrop", item)) %>%
  Agg_reg(region, element, item) ->
  NewFAO_bal



unique(NewFAO_bal$item) %>%
  setdiff(unique(GCAM5p3_bal$item))
unique(GCAM5p3_bal$item) %>% setdiff(unique(NewFAO_bal$item))


NewFAO_bal %>% Agg_reg(item, element) %>% rename(New = value) %>%
  left_join(GCAM5p3_bal %>% Agg_reg(item, element) %>% rename(Old = value)) %>%
  replace_na(list(Old = 0)) %>%
  gather(data, value, New, Old)->
  compare

library(ggplot2)
library(ggsci)

unique(compare$item)
compare %>% filter(item %in% c("Corn", "Wheat", "OtherGrain", "FiberCrop",
                               "OilCrop", "Rice","RootTuber", "PalmFruit")) %>%
  mutate(value = if_else(element %in% c("Production", "Import"),
                         -value, value)) %>%
  ggplot() +
  facet_wrap(~item, nrow = 1) +
  geom_bar(aes(x = data, y = value, fill = element), color = "black", alpha = 0.8,
           stat="identity", position = "stack") +
  #scale_fill_npg(name = "SUA") +
  scale_fill_rickandmorty(name = "SUA") +
  labs(x = "FAO items in primary equivalent",
       y = "Supply (-) and demand (+), Mt",
       title = "FAO primary equivalent 14-18 mean (New) vs. GCAM 2015 5-yr mean (Old)") +
  theme_bw() +
  theme(#legend.position = "none",
    axis.title.x = element_blank(),
    text = element_text(size = 20),
    axis.text = element_text(colour = "black", size = 20),
    axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) -> A; A

png("output/compare1.png",width = 5000, height = 2200, res = 300)
print(A)
dev.off()

compare %>% filter(item %in% c("MiscCrop", "SugarCrop")) %>%
  mutate(value = if_else(element %in% c("Production", "Import"),
                         -value, value)) %>%
  ggplot() +
  facet_wrap(~item, nrow = 1) +
  geom_bar(aes(x = data, y = value, fill = element), color = "black", alpha = 0.8,
           stat="identity", position = "stack") +
  #scale_fill_npg(name = "SUA") +
  scale_fill_rickandmorty(name = "SUA") +
  labs(x = "FAO items in primary equivalent",
       y = "Supply (-) and demand (+), Mt",
       title = "FAO (New) vs. GCAM (Old)") +
  theme_bw() +
  theme(#legend.position = "none",
    axis.title.x = element_blank(),
    text = element_text(size = 20),
    axis.text = element_text(colour = "black", size = 20),
    axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) -> A; A

png("output/compare2.png",width = 2000, height = 2200, res = 300)
print(A)
dev.off()

#, "RootTuber", "FiberCrop"
#,"Dairy"
#"Beef", "Pork", "Poultry", "SheepGoat"
compare %>% filter(item %in% c("Dairy")) %>%
  mutate(value = if_else(element %in% c("Production", "Import"),
                         -value, value)) %>%
  ggplot() +
  facet_wrap(~item, nrow = 1) +
  geom_bar(aes(x = data, y = value, fill = element), color = "black", alpha = 0.8,
           stat="identity", position = "stack") +
  #scale_fill_npg(name = "SUA") +
  scale_fill_rickandmorty(name = "SUA") +
  labs(x = "FAO items in primary equivalent",
       y = "Supply (-) and demand (+), Mt",
       title = "FAO (New) vs. GCAM (Old)") +
  theme_bw() +
  theme(#legend.position = "none",
    axis.title.x = element_blank(),
    text = element_text(size = 20),
    axis.text = element_text(colour = "black", size = 20),
    axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) -> A; A

png("output/compare3.png",width = 4000, height = 2200, res = 300)
print(A)
dev.off()
png("output/compare4.png",width = 1800, height = 2200, res = 300)
print(A)
dev.off()

