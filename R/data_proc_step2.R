SUA_bal_adjust <- function(.df){
  .df %>%
    mutate(element = factor(element,levels = SLC_element_new),
           value = value / 1000) %>%  # unit: 1000 tonnes or ha
    replace_na(list(value = 0)) %>%
    spread(element, value) %>%
    #Merge tourist consumption into food
    mutate(Food = Food + `Tourist consumption`) %>%
    select(- `Tourist consumption`) %>%
    #Maintain stock balance across time; SCL data (2014-) quality was high
    #Calculate closing stock, when negative, shift up stocks in all years.
    group_by(area, item) %>% arrange(-year, item) %>%
    mutate(cumSV = cumsum(`Stock Variation`) - first(`Stock Variation`),
           `Opening stocks1` = first(`Opening stocks`) - cumSV) %>%
    select(-cumSV, -`Opening stocks`) %>%
    rename(`Opening stocks` = `Opening stocks1`) %>%
    mutate(`Closing stocks` = `Opening stocks` + `Stock Variation`) %>%
    mutate(Stockshifter = if_else(`Closing stocks` < 0, abs(`Closing stocks`), 0)) %>%
    mutate(Stockshifter = if_else(`Opening stocks` < 0 & `Opening stocks` < `Closing stocks`,
                                  abs(`Opening stocks`), Stockshifter)) %>%
    mutate(Stockshifter = max(Stockshifter),
           `Opening stocks` = `Opening stocks` + Stockshifter,
           `Closing stocks` = `Opening stocks` + `Stock Variation`) %>%
    select(-Stockshifter) %>%
    ungroup() %>%
    #Correct negative processed where applicable
    #For few regions (e.g., Congo), move residual to food so SUA data was not exist
    mutate(Processed = if_else(Processed < 0, 0, Processed),
           Food = ifelse(Production >0 & Food == 0 & Feed == 0 & Processed == 0 & Seed == 0 & `Other uses` == 0,
                         Production + Import - Export + `Stock Variation`, Food),
           Food = ifelse(Food < 0, 0, Food)) %>%
    #Check regional supply, demand, and residue
    mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
           `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
           Residuals = `Regional supply` -  `Regional demand`) %>%  #%>% filter(abs(Residuals1 - Residuals) > 1)
    gather(element, value, -area, -item, -year) %>%
    mutate(element = factor(element, levels = Bal_element_new))
}

#Fn adjusting gross trade in all regions to be consistent with average (world export and import)
Gross_trade_adjust <- function(.df, min_trade_prod_ratio = 0.01){
  .df %>% left_join(
    .df %>%
      group_by(item, element, year) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      spread(element, value) %>%
      group_by(item) %>%
      filter(sum(Production) != 0) %>%
      mutate(Expratio = sum(Export) / sum(Production),
             Impratio = sum(Import) / sum(Production)) %>%
      #remove zero world producing items
      filter(sum(Production) != 0) %>%
      #using average gross trade value to calculate trade adjusters
      mutate(Exportadjuster = (Export + Import)/ 2 / Export,
             Importadjuster = (Export + Import)/ 2 / Import) %>%
      #If trade share over production is smaller than 1% for the whole period, setting to zero
      #mainly due to data quality concerns. And those items are not very important
      #Those trade adjusters will be applied to all regions
      mutate(Exportadjuster = if_else(Impratio < min_trade_prod_ratio & Expratio < min_trade_prod_ratio, 0 , Exportadjuster),
             Importadjuster = if_else(Impratio < min_trade_prod_ratio & Expratio < min_trade_prod_ratio, 0 , Importadjuster)) %>%
      select(item, year, Exportadjuster, Importadjuster),
    by = c("year", "item")
  ) %>%
    mutate(value = case_when(
      element %in% c("Export") ~ value * Exportadjuster,
      element %in% c("Import") ~ value * Importadjuster,
      TRUE ~ value)) %>%
    select(-Exportadjuster, -Importadjuster)
}



#*************************************
#*Load rds data
gsub(".rds","", list.files(data_folder_proc)) -> datasets_rds

#Load rds files
lapply(datasets_rds, function(f){
  assign(f, readRDS(file.path(data_folder_proc, paste0(f, ".rds"))),
         envir = .GlobalEnv)
})

#unique items
lapply(datasets_rds, function(f){
  assign(paste0(f, "_item"), get(f) %>% distinct(item) %>% pull(),
         envir = .GlobalEnv)
})
f = "QCL"
assign(paste0(f, "_reg"), get(f) %>% distinct(area) %>% pull(),
       envir = .GlobalEnv)

# TM_item is essentially a subset of TCL_item
#TCL has more items; the intersection is 417 items
setdiff(TCL_item, TM_item);setdiff(TM_item, TCL_item)

SCL_item %>%
  intersect(TCL_item) %>% intersect(TM_item) %>%
  intersect(QCL_item) #%>% intersect(FBSH_item)
setdiff(FBS_item, FBSH_item) # There was no CB (no food uses) in new FBS

#**********************************************************************

#Start with maize for 2014 - 2018

c("Area harvested", "Opening stocks", "Production",
  "Export", "Import", "Stock Variation",
  "Food", "Feed", "Seed", "Processed", "Other uses",
  "Tourist consumption", "Loss", "Residuals") -> SLC_element_new

c("Area harvested",
  "Opening stocks", "Production", "Import",
  "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
  "Residuals", "Regional supply", "Regional demand", "Stock Variation") -> Bal_element_new

SCL_item  %>% intersect(TM_item) %>% intersect(QCL_item) -> SUA_tier1_item
#173 good items with best data
#QCL_item has fewer data than SCL_item
#TM_item is essentially a subset of TCL_item

SCL_item %>% setdiff(SUA_tier1_item) %>% intersect(TM_item) -> SUA_tier2_item
SUA_tier2_item %>% intersect(QCL_item) #no production / area
#Use TM info but not QCL

SCL_item %>%
  setdiff(SUA_tier1_item) %>%
  setdiff(SUA_tier2_item) %>%
  intersect(QCL_item) -> SUA_tier3_item

SUA_tier3_item %>% intersect(TCL_item)
TM_item %>% setdiff(TCL_item)
TCL_item %>% setdiff(TM_item)

SCL_item %>%
  setdiff(SUA_tier1_item) %>%
  setdiff(SUA_tier2_item) %>%
  setdiff(SUA_tier3_item) -> SUA_tier4_item

#********************************
#SUA_tier1_item & SUA_tier2_item

#create full template and fill in data
expand.grid(area = QCL_reg, year = Hist_Year_SUA_new,
            item = SUA_tier1_item,
            element = SLC_element_new) %>%
  left_join(
  QCL %>%
    filter(item %in% SUA_tier1_item,
           element %in% c("Area harvested", "Production")) %>%
    select(area, item, element, year, QCL = value),
  by = c("area", "item", "element", "year") ) %>%
  left_join(
    TM %>%
      filter(item %in% SUA_tier1_item,
             element %in% c("Import Quantity")) %>%
      mutate(element = "Import") %>%
      group_by(area, item, element, year) %>%
      summarise(TCL = sum(value, na.rm = T), .groups = "drop") %>%
      bind_rows(
        TM %>%
          filter(item %in% SUA_tier1_item,
                 element %in% c("Import Quantity")) %>%
          mutate(element = "Export") %>%
          group_by(area = source, item, element, year) %>%
          summarise(TCL = sum(value, na.rm = T), .groups = "drop") ),
    by = c("area", "item", "element", "year")  ) %>%
  left_join(
    SCL %>% filter(item %in% SUA_tier1_item) %>%
      mutate(element = gsub(" Quantity", "", element)) %>%
      rename(SCL = value) ,
    by = c("area", "item", "element", "year") ) %>%
  mutate(value = case_when(
    element %in% c("Area harvested", "Production") ~ QCL, #prod in QCL is used and not overwritten
    element %in% c("Export", "Import") ~ TCL,
    element %in% SLC_element_new ~ SCL) ) %>%
  select(-QCL, -TCL, -SCL) %>%
  #bind SUA_tier2_item
  bind_rows(
    expand.grid(area = QCL_reg, year = Hist_Year_SUA_new,
                item = SUA_tier2_item,
                element = SLC_element_new) %>%
      left_join(
        TM %>%
          filter(item %in% SUA_tier2_item,
                 element %in% c("Import Quantity")) %>%
          mutate(element = "Import") %>%
          group_by(area, item, element, year) %>%
          summarise(TCL = sum(value, na.rm = T), .groups = "drop") %>%
          bind_rows(
            TM %>%
              filter(item %in% SUA_tier2_item,
                     element %in% c("Import Quantity")) %>%
              mutate(element = "Export") %>%
              group_by(area = source, item, element, year) %>%
              summarise(TCL = sum(value, na.rm = T), .groups = "drop") ),
        by = c("area", "item", "element", "year")  ) %>%
      left_join(
        SCL %>% filter(item %in% SUA_tier2_item) %>%
          mutate(element = gsub(" Quantity", "", element)) %>%
          rename(SCL = value) ,
        by = c("area", "item", "element", "year") ) %>%
      mutate(value = case_when(
        element %in% c("Area harvested") ~ 0, # Non QCL items here; not overwritten later
        element %in% c("Export", "Import") ~ TCL,
        element %in% SLC_element_new ~ SCL) ) %>%
      select(-TCL, -SCL)
  ) -> A

Bal_new_tier1_2 <- SUA_bal_adjust(A)
assert_FBS_balance(.df = Bal_new_tier1_2)
saveRDS(Bal_new_tier1_2, file.path(data_folder_proc, "Bal_new_tier1_2.rds") )

#********************************
#SUA_tier3_item

expand.grid(area = QCL_reg, year = Hist_Year_SUA_new,
            item = SUA_tier3_item,
            element = SLC_element_new) %>%
  left_join(
    SCL %>% filter(item %in% SUA_tier3_item) %>%
      mutate(element = gsub(" Quantity", "", element)),
    by = c("area", "item", "element", "year") ) %>%
  mutate(value = if_else(is.na(value) & element == "Stock Variation", 0, value)) %>%
  group_by(area, item, element) %>%
  #fill in NA across years
  fill(value) %>% fill(value, .direction = "up") %>%
  Gross_trade_adjust(min_trade_prod_ratio = 0.01) %>%
  rename(SCL = value) %>%
  left_join(
    QCL %>%
      filter(item %in% SUA_tier3_item,
             element %in% c("Area harvested", "Production")) %>%
      select(area, item, element, year, QCL = value),
    by = c("area", "item", "element", "year")
  ) %>%
  mutate(value = case_when(
    element %in% c("Area harvested", "Production") ~ QCL, #prod in QCL is used and not overwritten
    element %in% SLC_element_new ~ SCL) ) %>%
  select(-QCL, -SCL) -> A
Bal_new_tier3 <- SUA_bal_adjust(A)
assert_FBS_balance(.df = Bal_new_tier3)
saveRDS(Bal_new_tier3, file.path(data_folder_proc, "Bal_new_tier3.rds") )

#********************************
#SUA_tier4_item

expand.grid(area = QCL_reg, year = Hist_Year_SUA_new,
            item = SUA_tier4_item,
            element = SLC_element_new) %>%
  left_join(
    SCL %>% filter(item %in% SUA_tier4_item) %>%
      mutate(element = gsub(" Quantity", "", element)),
    by = c("area", "item", "element", "year") ) %>%
  mutate(value = if_else(is.na(value) & element == "Stock Variation", 0, value)) %>%
  group_by(area, item, element) %>%
  #fill in NA
  fill(value) %>% fill(value, .direction = "up") %>%
  Gross_trade_adjust(min_trade_prod_ratio = 0.01) -> A
Bal_new_tier4 <- SUA_bal_adjust(A)
assert_FBS_balance(.df = Bal_new_tier4)
saveRDS(Bal_new_tier4, file.path(data_folder_proc, "Bal_new_tier4.rds") )


#********************************

Bal_new_1_4 <-
  Bal_new_tier1_2 %>%
  bind_rows(Bal_new_tier3) %>%
  bind_rows(Bal_new_tier4)

#***************************************************************
#For key oil crops, cake data are not currently provided by FAO in new FBS
#We interpolate the cake extraction rate relative to oil using 2011 - 2013 data in FBSH
#World average is used for missing regions
#We focus on cakes with bilateral trade information first
#The items are made SUA_tier5_item

#Bal_new_tier5

TM_item[grepl("cake", TM_item, ignore.case = T)] %>% intersect(SCL_item_Cake)

FBSH_item[grepl("cake", FBSH_item, ignore.case = T)]
FBSH_item[grepl("oil", FBSH_item, ignore.case = T)]
SCL_item[grepl("Oil", SCL_item, ignore.case = T)]
c("Cottonseed Cake", "Sesameseed Cake", "Sunflowerseed Cake", "Copra Cake",
"Groundnut Cake", "Palmkernel Cake", "Soyabean Cake","Rape and Mustard Cake",
"Rape and Mustard Cake", "Oilseed Cakes, Other") -> FBSH_item_cake

c("Cottonseed Oil", "Sesameseed Oil", "Sunflowerseed Oil", "Coconut Oil",
"Groundnut Oil", "Palmkernel Oil", "Soyabean Oil", "Rape and Mustard Oil",
"Rape and Mustard Oil", "Oilcrops Oil, Other") -> FBSH_item_oil

c("Oil, cottonseed", "Oil, sesame", "Oil, sunflower", "Oil, coconut (copra)",
"Oil, groundnut", "Oil, palm kernel", "Oil, soybean",
"Oil, rapeseed", "Oil, mustard", "Oil, vegetable origin nes") -> SCL_item_oil

c("Cake, cottonseed", "Cake, sesame seed", "Cake, sunflower", "Cake, copra",
  "Cake, groundnuts", "Cake, palm kernel", "Cake, soybeans",
  "Cake, rapeseed", "Cake, mustard", "Cake, others") -> SCL_item_cake

setdiff(SCL_item_oil, unique(Bal_new_1_4$item))
setdiff(SCL_item_oil, TM_item)

SUA_tier5_item = SCL_item_cake
SUA_tier5_item_oil = SCL_item_oil

lapply(seq(1:10), function(n){
  #average cake to oil rate in 2011-2013 is used
  FBSH %>% filter(year %in% seq(2011, 2013),
                  item %in% c(FBSH_item_cake[n], FBSH_item_oil[n])) -> .df

  .df %>%
    filter(element == "Production" ) %>% Agg_reg(area, item, element) %>%
    spread(item, value) %>%
    mutate(cake_rate = get(FBSH_item_cake[n]) / get(FBSH_item_oil[n])) %>%
    left_join(
      .df %>%
        filter(element == "Production") %>%  spread(item, value) %>%
        filter( get(FBSH_item_cake[n]) * get(FBSH_item_oil[n]) > 0) %>%
        gather(item, value, -area, -year, -element) %>%
        Agg_reg(item, element)%>%
        spread(item, value) %>%
        mutate(cake_rate_world = get(FBSH_item_cake[n]) / get(FBSH_item_oil[n])) %>%
        select(element, cake_rate_world), by = "element"
    ) %>%
    mutate(cake_rate = if_else(cake_rate == 0 | is.finite(cake_rate) == F,
                               cake_rate_world, cake_rate)) %>%
    select(area, cake_rate, cake_rate_world) %>%
    mutate(item = SCL_item_oil[n], cake_item = SCL_item_cake[n]) ->
    .df1
  return(.df1)

}) %>% bind_rows() -> cakerate



expand.grid(area = QCL_reg, year = Hist_Year_SUA_new,
            item = SUA_tier5_item,
            element = SLC_element_new) %>%
  left_join(
    TM %>%
      filter(item %in% SUA_tier5_item,
             element %in% c("Import Quantity")) %>%
      mutate(element = "Import") %>%
      group_by(area, item, element, year) %>%
      summarise(TCL = sum(value, na.rm = T), .groups = "drop") %>%
      bind_rows(
        TM %>%
          filter(item %in% SUA_tier5_item,
                 element %in% c("Import Quantity")) %>%
          mutate(element = "Export") %>%
          group_by(area = source, item, element, year) %>%
          summarise(TCL = sum(value, na.rm = T), .groups = "drop") ),
    by = c("area", "item", "element", "year")  ) %>%
  left_join(
    Bal_new_1_4 %>% filter(item %in% SUA_tier5_item_oil,
                       element == "Production") %>%
      mutate(value = value * 1000) %>%  #convert units back
      left_join(cakerate, by = c("area", "item")) %>%
      group_by(item) %>% fill(cake_item, .direction = "updown") %>%
      fill(cake_rate_world, .direction = "updown") %>%
      mutate(cake_rate = if_else(is.na(cake_rate), cake_rate_world, cake_rate)) %>%
      transmute(area, year, item = cake_item, element, QCL = value * cake_rate),
    by = c("area", "year", "item", "element")) %>%
  mutate(value = case_when(
    element %in% c("Export", "Import") ~ TCL,
    element %in% c("Production") ~ QCL,
    element %in% SLC_element_new ~ 0) ) %>%
  select(-TCL, -QCL) %>%
  spread(element, value) %>%
  mutate(Feed = if_else(Production + Import - Export > 0,
                        Production + Import - Export, 0)) %>%
  gather(element, value, -area, -year, -item) -> A

Bal_new_tier5 <- SUA_bal_adjust(A)
assert_FBS_balance(.df = Bal_new_tier5)
saveRDS(Bal_new_tier5, file.path(data_folder_proc, "Bal_new_tier5.rds") )



Bal_new_1_5 <-
  Bal_new_1_4 %>%
  bind_rows(Bal_new_tier5)
unique(Bal_new_1_5$item)


SUA_tier1_item %>%
  union(SUA_tier2_item) %>%
  union(SUA_tier3_item) %>%
  union(SUA_tier4_item) %>%
  union(SUA_tier5_item) -> SUA_tier1_5_item

# 461 items in total in 1-5 tiers



QCL %>% group_by(item, element) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
  #filter(element == "Area harvested", value > 0) %>%
  filter(!item %in% SUA_tier1_5_item) %>%
  filter(!grepl("Total|Primary|Equivalent|Oilcrops|\\&|All|and Buffalo|and Ghee|and Goat|Poultry Birds|Meat, Poultry", item)) %>%
  pull(item) %>% unique() %>%
  setdiff(c("Rice, paddy (rice milled equivalent)",
            "Eggs, other bird, in shell (number)",
            "Eggs, hen, in shell (number)")) ->  #removing rice here as no deamnd info
  SUA_tier6_item
# 50 items with 15 have harvested area
# SUA_tier6_item includes items included in QCL but not in SCL
# E.g., Seed cotton was an important one that was omitted by FAO in SCL
# There are 15 area items: Tobacco, rubber, Seed cotton, peppermint, pyrethrum, hops, and other fibre crops
# Kapok fruit (not here) is in oilcrop
# !!!Rubber was not included in GCAM prodstat
# Gums, nature is not important (small production)

QCL %>% filter(element == "Stocks") %>%
  filter(!item %in% c("Cattle and Buffaloes", "Poultry Birds", "Sheep and Goats" )) %>%
  pull(item) %>% unique   -> animals

SUA_tier6_item %>% setdiff(animals) -> SUA_tier6_item_nonanimal
# 31 items



# Assumption is no storage
intersect(SUA_tier6_item_nonanimal, TM_item)
intersect(SUA_tier6_item_nonanimal, TCL_item)
setdiff(SUA_tier6_item_nonanimal, TM_item)
setdiff(SUA_tier6_item_nonanimal, TCL_item)

# 12 items are not (significantly) traded (assumption)
# The only use is other uses for fibre crops, Tobacco, rubber
# but food used for peppermint, pyrethrum, hops

c("Skins, sheep, fresh", "Hides, buffalo, fresh") # in TCL not TM

setdiff(SUA_tier6_item_nonanimal,
        c("Seed cotton",
          "Silk-worm cocoons, reelable",
          "Meat, other camelids",
          "Hops", "Pyrethrum, dried", "Peppermint")) -> SUA_tier6_item_nonanimal_otheruse


expand.grid(area = QCL_reg, year = Hist_Year_SUA_new,
            item = SUA_tier6_item_nonanimal,
            element = SLC_element_new) %>%
  left_join(
    QCL %>%
      filter(item %in% SUA_tier6_item_nonanimal,
             element %in% c("Area harvested", "Production")) %>%
      select(area, item, element, year, QCL = value),
    by = c("area", "item", "element", "year") ) %>%
  left_join(
    TM %>%
      filter(item %in% SUA_tier6_item_nonanimal,
             element %in% c("Import Quantity")) %>%
      mutate(element = "Import") %>%
      group_by(area, item, element, year) %>%
      summarise(TCL = sum(value, na.rm = T), .groups = "drop") %>%
      bind_rows(
        TM %>%
          filter(item %in% SUA_tier6_item_nonanimal,
                 element %in% c("Import Quantity")) %>%
          mutate(element = "Export") %>%
          group_by(area = source, item, element, year) %>%
          summarise(TCL = sum(value, na.rm = T), .groups = "drop") ),
    by = c("area", "item", "element", "year")  ) %>%
  left_join(
    TCL %>%
      filter(item %in% SUA_tier6_item_nonanimal,
             element %in% c("Import Quantity")) %>%
      mutate(element = "Import") %>%
      bind_rows(
        TCL %>%
          filter(item %in% SUA_tier6_item_nonanimal,
                 element %in% c("Export Quantity")) %>%
          mutate(element = "Export") ) %>%
      select(area, item, element, year, TCL_gross = value)
     ) %>%
  mutate(TCL = if_else(is.na(TCL), TCL_gross, TCL)) %>%
  mutate(value = case_when(
    element %in% c("Area harvested", "Production") ~ QCL, #prod in QCL is used and not overwritten
    element %in% c("Export", "Import") ~ TCL,
    element %in% SLC_element_new ~ 0) ) %>%
  select(-QCL, -TCL, -TCL_gross) %>%
  group_by(area, item, element) %>%
  #fill in NA across years
  fill(value) %>% fill(value, .direction = "up") %>%
  Gross_trade_adjust(min_trade_prod_ratio = 0.01) %>%
  replace_na(list(value = 0)) %>%
  spread(element, value) %>%
  mutate(Processed = ifelse(item %in% c("Seed cotton",
                                        "Silk-worm cocoons, reelable") & Production + Import - Export > 0,
                            Production + Import - Export, 0),
         Food = ifelse(item %in% c("Meat, other camelids",
                                   "Hops", "Pyrethrum, dried", "Peppermint") & Production + Import - Export > 0,
                            Production + Import - Export, 0),
         `Other uses` = ifelse(item %in% SUA_tier6_item_nonanimal_otheruse & Production + Import - Export > 0,
                               Production + Import - Export, 0)) %>%
  gather(element, value, -area, -year, -item) -> A

Bal_new_tier6 <- SUA_bal_adjust(A)
assert_FBS_balance(.df = Bal_new_tier6)
saveRDS(Bal_new_tier6, file.path(data_folder_proc, "Bal_new_tier6.rds") )

SUA_tier1_5_item %>%
  union(SUA_tier6_item_nonanimal) -> SUA_tier1_6_item



Bal_new_all <-
  Bal_new_1_5 %>%
  bind_rows(Bal_new_tier6)
unique(Bal_new_all$item)
saveRDS(Bal_new_all, file.path(out_dir, "Intermediate", "Bal_new_all.rds") )
saveRDS(Bal_new_all, file.path(data_folder_proc, "Bal_new_all.rds") )

setdiff(QCL_item, unique(Bal_new_all$item)) #animals and aggregates

Bal_new_all %>%
  group_by(year, item, element) %>% summarise(value = sum(value), .groups = "drop") %>%
  #spread(element, value) %>% mutate(Yield = Production/`Area harvested`) %>% gather(element, value, -item, -year) %>%
  spread(year, value)-> Bal_new_world

#######################################################



QCL %>% filter(element == "Area harvested") %>% #filter(area == "Canada") %>%
  Agg_reg(item, year) %>% mutate(value = value / 1000000) %>%
  filter(!grepl("Total|Primary|Equivalent|Oilcrops|\\&|All", item)) %>%
  filter(year %in% c(1995, 2018)) %>%
  Agg_reg(year) %>%
  spread(year, value) %>%
  filter(`2018` >1) %>%
  mutate(diff = `2018` - `1995`, diff1 = `2018`/ `1995` ) %>%
  arrange(diff) -> A
