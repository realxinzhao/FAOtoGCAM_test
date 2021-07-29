library(dplyr)
library(tidyr)
library(assertthat)
source("R/fn.faostat.R")
source("R/fn.dataproc.R")
source("R/FAO_Check.R")

fao_metadata <- FAOsearch()
data_folder <- "data_raw"
out_dir <- "output"

SCL <- get_faostat_bulk(code = "SCL", data_folder = data_folder)
QCL <- get_faostat_bulk(code = "QCL", data_folder = data_folder)

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





Proc_Corn()
Proc_SugarCrop()
Proc_PalmFruit() -> bal_APE
#Problems with Australia... because extraction factor for import!!!





library(ggplot2)
library(ggsci)
bal_APE$element <- factor(bal_APE$element, bal_APE_element)

bal_APE %>% mutate(element = factor(element,levels = bal_APE_element)) %>%
  filter(!element %in% c("Opening stocks", "Closing stocks")) %>%
  filter(year == 2015, #region == "USA", #year == 2018,
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
