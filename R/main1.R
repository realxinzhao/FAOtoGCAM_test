#Additional data download to process
#FAO PRODSTAT
download = T

GCAM_FAO <- list("FAO_GDP_deflators",
                 "FAO_ag_an_ProducerPrice",
                 "FAO_BilateralTrade",
                 "FAO_For_Exp_m3_USD_FORESTAT",
                 "FAO_For_Imp_m3_FORESTAT",
                 "FAO_For_Prod_m3_FORESTAT",
                 "FAO_For_Exp_m3_FORESTAT",
                 "FAO_ag_Prod_t_PRODSTAT",
                 "FAO_ag_HA_ha_PRODSTAT",
                 "FAO_an_Prod_t_PRODSTAT")
FAO_domain_code = c("PD", "PP", "TM", rep("FO",4), rep("QC",2), "QL")
data_map <- data.frame(name = GCAM_FAO %>% unlist(), FAO_domain_code)







#************************************
#*Ag crop economics
#*QC:  FAO_ag_Prod_t_PRODSTAT

gcam_dataset = "FAO_ag_Prod_t_PRODSTAT"
code = lookup(gcam_dataset, data_map, "name", "FAO_domain_code")

get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> QC


unique(QC$element)
unique(QC$`element code`)

FAO_ag_Prod_t_PRODSTAT <- QC %>%
  filter(element == "Production") %>%
  select(countries = area,`country codes` = `area code`,
         item, `item codes` = `item code`, element, `element codes` = `element code`,
         year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap()

output_csv_data(gcam_dataset,
                col_type_nonyear = "cicici",
                title = "FAO agricultural production by country.year",
                unit = "tonnes")

#************************************
#*QC:  FAO_ag_Prod_t_PRODSTAT
gcam_dataset = "FAO_ag_HA_ha_PRODSTAT"
code = lookup(gcam_dataset, data_map, "name", "FAO_domain_code")

get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> QC

FAO_ag_HA_ha_PRODSTAT <- QC %>%
  filter(element == "Area harvested") %>%
  select(countries = area,`country codes` = `area code`,
         item, `item codes` = `item code`, element, `element codes` = `element code`,
         year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap()

output_csv_data(gcam_dataset,
                col_type_nonyear = "cicici",
                title = "FAO agricultural harvested area by country.year",
                unit = "ha")

#************************************
#*QL:  FAO_an_Prod_t_PRODSTAT
gcam_dataset = "FAO_an_Prod_t_PRODSTAT"
code = lookup(gcam_dataset, data_map, "name", "FAO_domain_code")

assign(code, get_faostat_bulk(code = code, data_folder = data_folder, download = download))
unique(get(code)[, "element"])
FAO_an_Prod_t_PRODSTAT <- get(code) %>%
  filter(element == "Production") %>%
  select(countries = area,`country codes` = `area code`,
         item, `item codes` = `item code`, element, `element codes` = `element code`,
         year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap()

output_csv_data(gcam_dataset,
                col_type_nonyear = "cicici",
                title = "FAO animal product output by country.year",
                unit = "tonnes")

get(code) %>%
  filter(element == "Production") -> A
unique(A$item)

#************************************








