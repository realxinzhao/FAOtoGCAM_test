#*****************************************************************************
#PalmFruit
# FBS or GCAM: "Palm Oil and products", "Palmkernel Oil and products", "Palm kernels and products"
# Coconuts are not included here

Proc_PalmFruit <- function(){

  GCAM_crop <- "PalmFruit"
  #*************
  #* data 2014 to 2018

  SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
    filter(FBS_label %in% c("Palm Oil and products", "Palmkernel Oil and products", "Palm kernels and products")) %>%
    pull(SUA_item) %>% unique()

  #Filter by relevant items
  SUA_new %>% filter(item %in% c(SUA_item), element %in% Bal_element_SUA_new) -> bal

  #Proc_primarize
  bal %>%
    Proc_primarize(c("Oil palm fruit"), c("Oil, palm", "Palm kernels")) %>%
    Proc_primarize_oil(c("Palm kernels"), c("Oil, palm kernel"), oilshare = 0.5) %>%
    Proc_primarize(c("Oil, palm", "Oil, palm kernel"),
                   c("Fatty acids", "Fatty substance residues")) -> bal_PE  #primary equivalent

  bal_PE %>% Proc_primarize_aggregate(Primary_crop = "Oil palm fruit")-> bal_APE_new
  unique(bal_APE_new$element)

  #*************
  #* data 1961 to 2013
  FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
    pull(FBSH_item) %>% unique()
  FBS_item <- c("Palm kernels", "Palmkernel Oil", "Palmkernel Cake", "Palm Oil", "Oil palm fruit")

  #"Oil palm fruit" is add from production dataset (QCL) for both production and processed

  FBS_old %>% filter(item %in% FBS_item, element %in% Bal_element_FBS_old) %>%
    spread(item, value) %>%
    left_join(
      QCL %>% filter(item == "Oil palm fruit", element == "Production")%>%
        mutate(value = value / 1000) %>%
        spread(element, value) %>% mutate(Processed = Production) %>%
        gather(element, value, c("Production", "Processed")) %>%
        FAO_ctry_remap(.colname = "area") %>%
        select(region = area, year, element, item, value) %>%
        spread(item, value), by = c("region", "year", "element")
    ) %>% replace_na(list(`Oil palm fruit` = 0)) %>%
    gather(item, value, -region, -year, -element)-> bal

  #Proc_primarize
  bal %>%
    Proc_primarize(c("Oil palm fruit"), c("Palm Oil", "Palm kernels")) %>%
    Proc_primarize(c("Palm kernels"), c("Palmkernel Oil", "Palmkernel Cake")) -> bal_PE

  #Map to GCAM regions
  bal_PE %>% Proc_primarize_aggregate(Primary_crop = "Oil palm fruit")-> bal_APE_old
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

