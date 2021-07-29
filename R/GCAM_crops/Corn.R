#*****************************************************************************
#Corn
# FBS or GCAM: "Maize and products" and "Maize Germ Oil"

Proc_Corn <- function(){

  GCAM_crop <- "Corn"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique()

#Filter by relevant items
SUA_new %>% filter(item %in% c(SUA_item), element %in% Bal_element_SUA_new) -> bal

#Proc_primarize
bal %>%
  Proc_primarize(c("Maize"), c("Flour, maize", "Germ, maize", "Bran, maize")) %>%
  Proc_primarize(c("Flour, maize"), c("Starch, maize", "Gluten, maize")) %>%
  Proc_primarize(c("Bran, maize", "Gluten, maize"), c("Feed and meal, gluten")) %>%
  Proc_primarize_oil(c("Germ, maize"), c("Oil, maize"), oilshare = 0.5) -> bal_PE  #primary equivalent

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
bal_PE %>% Proc_primarize_aggregate(Primary_crop = "Maize")-> bal_APE_new
unique(bal_APE_new$element)
#13 * 32 * 5 * 2
#*************
#* data 1961 to 2013
FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBSH_item) %>% unique()

FBS_old %>% filter(item %in% FBS_item, element %in% Bal_element_FBS_old) -> bal
#Proc_primarize
bal %>% Proc_primarize_self("Maize Germ Oil") -> bal_PE  #primary equivalent
#Aggregate to Primary equivalent and keep Primary: Maize and product
#Map to GCAM regions
bal_PE %>% Proc_primarize_aggregate(Primary_crop = "Maize and products")-> bal_APE_old
#22 (1992 - 2013) * 32 * 2 * 10 #Europe_Eastern and Russia exist after 1992
#*************
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

