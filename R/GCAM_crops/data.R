library(dplyr)
library(tidyr)
library(assertthat)
#library(ggalluvial)
source("R/fn.faostat.R")
source("R/fn.dataproc.R")
source("R/FAO_Check.R")

fao_metadata <- FAOsearch()
data_folder <- "data_raw"
out_dir <- "output"

#SUA (451 items)
SCL <- get_faostat_bulk(code = "SCL", data_folder = data_folder)
#Production (303 items)
QCL <- get_faostat_bulk(code = "QCL", data_folder = data_folder)
#TCL <- get_faostat_bulk(code = "TCL", data_folder = data_folder)
#Trade: (479 items)
TCL <- read_faostat_bulk(file.path(data_folder, basename("Trade_Crops_Livestock_E_All_Data_(Normalized).zip")))
#Bilateral trade: (426 items)
TM <- get_faostat_bulk(code = "TM", data_folder = data_folder)

unique(TM$`reporter countries`)
unique(TM$`partner countries`)
unique(TCL$area)  unique(QCL$area)
unique(QCL$item) %>% intersect(unique(TCL$item))
unique(QCL$item) %>% setdiff(unique(TCL$item)); unique(TCL$item) %>% setdiff(unique(QCL$item))

unique(TM$item) %>% intersect(unique(TCL$item))
unique(TM$item) %>% setdiff(unique(TCL$item)); unique(TCL$item) %>% setdiff(unique(TM$item))

unique(QCL$item)
unique(QCL$item) %>% intersect(unique(TM$item))
unique(SCL$item) %>% setdiff(unique(QCL$item))
unique(QCL$item) %>% setdiff(unique(SCL$item))

unique(CB$item) %>% intersect(unique(QCL$item))



SUA_new <- SCL %>%
  FAO_ctry_remap(.colname = "area") %>%
  mutate(element = replace(element, element == "Food supply quantity (tonnes)", "Food"),
         element = replace(element, element == "Other uses (non-food)", "Other uses")) %>%
  select(region = area, year, item, element, value) %>%
  spread(element, value, fill = 0) %>%  gather(element, value, -region, -year, -item) %>%
  spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element)

FBSH <- get_faostat_bulk(code = "FBSH", data_folder = data_folder)
CB <- get_faostat_bulk(code = "CB", data_folder = data_folder)
unique(FBSH$element)
unique(CB$element)
source("R/FBSinfo.R")

FBS_old <- FBSH %>%
  bind_rows(CB %>% mutate(value = value / 1000) %>%
              filter(!item %in% intersect(unique(FBSH$item), unique(CB$item)))) %>%
  FAO_ctry_remap(.colname = "area") %>%
  mutate(element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  filter(`item code` < 2900, `item code` != 2501) %>%  #remove aggregated
  select(region = area, year, item, element, value) %>%
  spread(element, value, fill = 0) %>%  gather(element, value, -region, -year, -item) %>%
  spread(item, value, fill = 0) %>% gather(item, value, -region, -year, -element)


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





Proc_Corn()-> bal_APE
Proc_SugarCrop()
Proc_PalmFruit() -> bal_APE
#Problems with Australia... because extraction factor for import!!!





library(ggplot2)
library(ggsci)
bal_APE$element <- factor(bal_APE$element, bal_APE_element)

bal_APE %>% mutate(element = factor(element,levels = bal_APE_element)) %>%
  filter(!element %in% c("Opening stocks", "Closing stocks")) %>%
  filter(year == 2012, #region == "USA", #year == 2018,
         item == "APE") %>%
  mutate(value = value /1000) %>%
  mutate(value = if_else(element %in% c("Opening stocks", "Production", "Import Quantity"),
                         value, -value)) %>%
  filter(element %in% bal_APE_element) -> data1

data1 %>% filter(element == "Production") %>% arrange(-value) %>% pull(region) -> xorder
data1 %>%
  ggplot() + theme_bw() +
  geom_bar(aes(x = reorder(region, value), y = value, fill = element, group = region),
           stat = "identity", position = "identity") +
  scale_fill_npg(name = "Element") +
  scale_x_discrete(limits = xorder) +
  labs(x = "Year", y = "Mt") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept = 0)
