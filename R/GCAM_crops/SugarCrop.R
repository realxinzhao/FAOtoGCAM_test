#*****************************************************************************
#SugarCrop
#SugarCrop
# "Sugar non-centrifugal" was not included in GCAM data (FAO_ag_items_cal_SUA),
# but is now added.
# "Sugar crops nes" is also added for consistency with production
# That is, all sugar crops and their processed products up to
# the refined sugar & confectionery level are included, but presented in
# aggregated primary equivalent.

Proc_SugarCrop <- function(){

GCAM_crop <- "SugarCrop"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

#NA here was 23511.01 and 23512

# "Sugar crops nes" only exists in few regions with relatively small production, but it is treated as primary sugarcrop here.
# Note that CPC, 23511.01 (cane sugar) & 23512 (beet sugar) are in 2351F, raw cane or beet sugar (centrifugal only).
# Only cane has non-centrifugal process
# "sugar confectionery" is treated as refined sugar with no loss.

SUA_item <- c(
  "Sugar cane", "Sugar beet", "Sugar crops nes",
  "Sugar non-centrifugal", #CPC 23511.02
  "Sugar refined",
  "Sugar Raw Centrifugal", #including both can and beet
  "Sugar confectionery")

#Filter by relevant items
SUA_new %>% filter(item %in% c(SUA_item), element %in% Bal_element_SUA_new) -> bal

#Proc_primarize
bal %>%
  Proc_primarize(c("Sugar cane", "Sugar beet", "Sugar crops nes"),
                 c("Sugar non-centrifugal", "Sugar Raw Centrifugal")) %>%
  Proc_primarize_self("Sugar confectionery") %>%
  Proc_primarize(c("Sugar Raw Centrifugal"),
                 c("Sugar refined", "Sugar confectionery")) -> bal_PE

#Aggregate to Primary equivalent and keep Primary
#Note that during the primarizing, the implied stock carry over may become imbalance
#due to potentially different extraction rates across periods.
#The discrepancy should be small.
#In fact, only need stock variations and opening stock of one period to calculate
#stock in all periods, assuming no losses there.
#So the opening stock of the most recent year is used.
#And stocks in other periods are ignored. Also unit in tonnes.
#The adjustments are done later.
#Map to GCAM regions
bal_PE %>% Proc_primarize_aggregate(Primary_crop = c("Sugar cane", "Sugar beet", "Sugar crops nes")) -> bal_APE_new

#*************
#* data 1961 to 2013
FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique()

FBS_old %>% filter(item %in% FBS_item, element %in% Bal_element_FBS_old) -> bal
#Proc_primarize
bal %>% Proc_primarize(c("Sugar cane", "Sugar beet"),
                       c("Sugar non-centrifugal", "Sugar (Raw Equivalent)")) -> bal_PE  #primary equivalent

#Aggregate to Primary equivalent and keep Primary: Maize and product
#Map to GCAM regions
bal_PE %>% Proc_primarize_aggregate(Primary_crop = c("Sugar cane", "Sugar beet"))-> bal_APE_old
#22 (1992 - 2013) * 32 * 2 * 10 #Europe_Eastern and Russia exist after 1992
#*************
intersect(Bal_element_new, Bal_element_FBSH)
#Combine data
#Move tourist consumption to food; adjust unit
#
bal_APE_new %>% mutate(value = value / 1000) %>%  # unit: 1000 tonnes
  spread(element, value) %>%
  mutate(Food = Food + `Tourist consumption`) %>%
  select(- `Tourist consumption`) %>%
  mutate(`Closing stocks` = `Opening stocks` + `Stock Variation`,
         `Regional supply` = Production - `Export Quantity` + `Import Quantity`,
         `Regional demand` = Feed + Food + Loss + Processed + Seed + `Stock Variation` + `Other uses`,
         Residuals = `Regional supply` -  `Regional demand`) %>%
  gather(element, value, -region, -item, -year) %>%
  bind_rows(
    bal_APE_old %>%
      spread(element, value) %>%
      mutate(`Stock Variation` = -`Stock Variation`,
             `Regional supply` = Production - `Export Quantity` + `Import Quantity`,
             `Regional demand` = Feed + Food + Loss + Processed + Seed + `Stock Variation` + `Other uses`,
             Residuals = `Regional supply` -  `Regional demand`) %>%
      gather(element, value, -region, -item, -year)
  ) -> A

#check missing
A %>% filter(year >1973) %>%
  filter(!element %in% c("Opening stocks", "Closing stocks")) %>%
  spread(year, value, fill = NA) %>% gather(year, value, -region, -item, -element) %>%
  filter(!(year < 1992 & region %in% c("Russia", "Europe_Eastern") )) %>%
  filter(is.na(value))

A %>% filter(year >1973) %>%
  spread(element, value) %>% arrange(-year, item) %>%
  group_by(region, item) %>%
  mutate(cumSV = cumsum(`Stock Variation`) - first(`Stock Variation`)) %>%
  mutate(`Opening stocks1` = first(`Opening stocks`) - cumSV ) %>%
  select(-cumSV, -`Opening stocks`) %>%
  rename(`Opening stocks` = `Opening stocks1`) %>%
  mutate(`Closing stocks` = `Opening stocks` + `Stock Variation`) %>%
  gather(element, value, -region, -item, -year) -> bal_APE
#*****************************************************************************
return(bal_APE %>% mutate(sector = GCAM_crop))
}
