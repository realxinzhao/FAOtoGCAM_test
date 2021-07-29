
#7-25-2021
# FAOSTAT had another updates to aggregate some datasets
# E.g., SC, SD, SL, SP are aggregated into SCL (SUA dataset),
# as reflected in fao_metadata_7-25-2021

#*********************************
#*The code here aims to generate FAO FBS (new, 2014-)
#*from the corresponding primary & processed files (SUA)
#*The mapping has been provided by FAO

#FBS_SUA_item_mapping is the processed mapping

unique(FBS_SUA_item_mapping$GCAM_commodity)
#There are 21 GCAM crops on the demand side, as mapped from FAO FBS
#The data processing is performed for each GCAM crop

"Wheat",
"Corn",
"Rice",
"Soybean"
"PalmFruit"
"SugarCrop"
"OilCrop"
"OtherGrain"                "RootTuber"           "Legumes"
[8] "NutsSeeds"              "OilCrop"        "FiberCrop"                       "Vegetables"
[15] "Fruits"         "MiscCrop"       "Beef"           "SheepGoat"      "Pork"           "Poultry"        "OtherMeat_Fish"
[22] "Dairy"



unique(SCL$element)

#############



A %>% group_by(year, item, element) %>% summarise(value = sum(value)/1000000, .groups = "drop") %>%
  spread(item, value) %>% select(-Maize)-> world


world$element <- factor(world$element, levels =
                        c("Opening stocks", "Production", "Export Quantity", "Import Quantity", "Stock Variation",
                          "Food", "Feed", "Seed", "Processed", "Other uses (non-food)",
                          "Loss", "Residuals", "Tourist consumption"))


write.csv(world %>% spread(year, APE) %>%
            arrange(element), "cornworld2015update.csv")








SCL %>% filter(year == 2015) %>% filter(item == "Sugar crops nes")



fao_metadata <- FAOsearch()

data_folder <- "data_raw"
out_dir <- "output"
download <- T
dir.create(data_folder)
dir.create("output")

code = "SCL"
assign(code, get_faostat_bulk(code = code, data_folder = data_folder, download = download))

metadata <- FAOsearch(code = code)
basename(metadata$filelocation)

SP <- read_faostat_bulk(file.path(data_folder, "SUA_LivestockProcessed_E_All_Data_(Normalized).zip"))
SC %>% bind_rows(SD) %>% bind_rows(SL) %>% bind_rows(SP)-> SCL_old

rm(SCL_old)
