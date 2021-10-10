
library(dplyr)
library(assertthat)
#library(ggalluvial)
library(tidyr)
source("R/fn.faostat.R")
source("R/fn.dataproc.R")

#source("R/FAO_Check.R")



fao_metadata <- FAOsearch()
data_folder <- "data_raw"
data_folder_proc <- "data_raw/Proc_rds"
out_dir <- "output"

FBSH <- get_faostat_bulk(code = "FBSH", data_folder = data_folder)
CB <- get_faostat_bulk(code = "CB", data_folder = data_folder)
unique(FBSH$element)
unique(CB$element)
source("R/FBSinfo.R")

Hist_Year <- seq(1973, 2018)
Hist_Year_Bilateral <- seq(1992, 2018); Hist_Year_Gross <- seq(1973, 1991)
Hist_Year_SUA_new <- seq(2014, 2018)





#current GCAM mappings
AGLU_ctry <- readr::read_csv("input/crop-remap_6_8_2021/aglu/AGLU_ctry.csv", comment = "#")
AGLU_ctry_Unique <-distinct(AGLU_ctry,FAO_country,.keep_all = TRUE)

GCAM_FAO_item <- readr::read_csv("input/crop-remap_6_8_2021/aglu/FAO/FAO_ag_items_PRODSTAT.csv", comment = "#")

iso_GCAM_regID <- readr::read_csv("input/crop-remap_6_8_2021/common/iso_GCAM_regID.csv", comment = "#")
GCAM_region_names <- readr::read_csv("input/crop-remap_6_8_2021/common/GCAM_region_names.csv", comment = "#")

setdiff( unique(GCAM_FAO_item$item), QCL_item)



c("Area harvested", "Opening stocks", "Production",
  "Export", "Import", "Stock Variation",
  "Food", "Feed", "Seed", "Processed", "Other uses",
  "Tourist consumption", "Loss", "Residuals") -> SLC_element_new

c("Area harvested",
  "Opening stocks", "Production", "Import",
  "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
  "Residuals", "Regional supply", "Regional demand", "Stock Variation") -> Bal_element_new

setdiff(Bal_element_new,
unique(FBS$element))







c("Opening stocks", "Production", "Export Quantity", "Import Quantity", "Stock Variation",
  "Food", "Feed", "Seed", "Processed", "Other uses", "Tourist consumption",
  "Loss", "Residuals") -> Bal_element_SUA_new

unique(FBSH$element)
c("Production", "Export Quantity", "Import Quantity", "Stock Variation",
  "Food", "Feed", "Seed", "Processed", "Other uses", "Loss") ->
  Bal_element_FBS_old

bal_APE_element <-
  c("Opening stocks", "Production", "Export Quantity", "Import Quantity", "Stock Variation",
    "Food", "Feed", "Seed", "Processed", "Other uses", "Loss", "Residuals", "Closing stocks")
