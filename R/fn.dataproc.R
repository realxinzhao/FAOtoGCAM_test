#************************************
lookup <- function(.lookupvalue, .lookup_df, .lookup_col, .target_col){

  assert_that(is.character(.lookupvalue))
  assert_that(is.data.frame(.lookup_df))
  assert_that(.lookup_col %in% colnames(.lookup_df))
  assert_that(.target_col %in% colnames(.lookup_df))

  .lookup_df[grep(paste0("^",.lookupvalue,"$"),
                  .lookup_df[, .lookup_col]), .target_col]
}


output_csv_data <- function(gcam_dataset, col_type_nonyear, title, unit, code){

  if (!missing(code)) {code = code} else {
    code = lookup(gcam_dataset, data_map, "name", "FAO_domain_code")
  }

  col_type = paste0(col_type_nonyear, paste0(rep("n",FAO_tbl_summary(gcam_dataset)["nyear"]), collapse = ""))
  cmnts <- c(
    paste0("File: ", gcam_dataset, ".csv"),
    paste0("Title: ", title),
    paste0("Units: ", unit),
    paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")"),
    paste0("Date of last update: ", Sys.Date()),
    paste0("Column types: ",col_type) ,
    "----------"
  )
  fqfn <- file.path(out_dir, paste0(gcam_dataset, ".csv"))
  suppressWarnings(file.remove(fqfn))
  cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
  readr::write_csv(get(gcam_dataset), fqfn, append = TRUE, col_names = TRUE, na = "")
}


#############################


#extract info of data (e.g., zip files) in a folder

Local_rawdata_info <- function(data_folder = "data_raw/"){          #folder including data files

  file.info(dir(data_folder, full.names = T)) %>%
    tibble::rownames_to_column(var = "filelocation") %>%
    filter(isdir == F,
           grepl("zip$", filelocation)) %>%
    transmute(filelocation = gsub(data_folder, "", filelocation),
              ctime = as.Date(ctime), mtime = as.Date(mtime) ) %>%
    left_join(fao_metadata %>%
                mutate(filelocation = gsub("http://fenixservices.fao.org/faostat/static/bulkdownloads/", "", filelocation)),
              by = "filelocation") %>%
    transmute(datasetcode, datasetname,  FAOupdate = dateupdate, Localupdate = mtime,
              needupdate = FAOupdate > Localupdate, filesize)

}


################################

Proc_primarize <- function(.df, source_item, sink_item){

  .df %>% filter(element == "Production",
                 item %in% sink_item) %>%
    group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
    bind_rows(
      .df %>% filter(element == "Processed",
                     item %in% source_item) %>%
        group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop")
    ) %>% spread(element, value, fill = 0) %>%
    mutate(extraction_rate = if_else(is.na(Production / Processed), 1, Production / Processed)) %>%
    mutate(extraction_rate = if_else(extraction_rate == 0, 1, extraction_rate)) %>%
    select(region, year, extraction_rate) %>% right_join(.df) %>%
    mutate(value = if_else(item %in% sink_item, value / extraction_rate, value),
           value = if_else(item %in% source_item & element == "Processed", 0, value)) %>%
    select(-extraction_rate)
}

Proc_primarize_self <- function(.df, source_item){
.df %>% filter(element == "Production", item == source_item) %>%
  select(region, year, item, prod = value) %>%
  right_join(.df, by = c("region", "year", "item")) %>%
  mutate(value = if_else(item %in% source_item & element %in% c("Processed", "Production"),
                         value - prod, value)) %>%
  select(-prod)
}

#oilshare oil / (oil + cake). When cake data is missing, add those into feed in oil using oilshare
Proc_primarize_oil <- function(.df, source_item, sink_item, oil_item = NULL, oilshare){

  if (is.null(oil_item) == F) {assertthat::assert_that(oil_item %in%  sink_item)} else
    if (is.null(oil_item) & length(sink_item) == 1) {oil_item = sink_item} else
    {stop("Need to specify oil_item!")}


  .df %>% filter(element == "Production",
                 item %in% sink_item) %>%
    group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
    bind_rows(
      .df %>% filter(element == "Production",
                     item %in% oil_item) %>%
        group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop") %>%
        mutate(element = "oilprod")
    ) %>%
    bind_rows(
      .df %>% filter(element == "Processed",
                     item %in% source_item) %>%
        group_by(region, year, element) %>% summarise(value = sum(value), .groups = "drop")
    ) %>% spread(element, value, fill = 0) %>%
    mutate(cakeprod =  oilprod * (1 / oilshare - 1),
           extraction_rate = if_else(is.na((oilprod + cakeprod) / Processed), 1,
                                     (oilprod + cakeprod) / Processed)) %>%
    mutate(extraction_rate = if_else(extraction_rate == 0, 1, extraction_rate)) %>%
    select(region, year, extraction_rate, cakeprod)  %>% right_join(.df) %>%
    mutate(value = if_else(item %in% oil_item & element == "Production", value + cakeprod, value),
           value = if_else(item %in% oil_item & element == "Feed", value + cakeprod, value),
           value = if_else(item %in% sink_item, value / extraction_rate, value),
           value = if_else(item %in% source_item & element == "Processed", 0, value)) %>%
    select(-extraction_rate, -cakeprod)
}


Proc_primarize_aggregate <- function(.df, Primary_crop){

  .df %>% group_by(region, year, element) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    rename(APE = value) %>% #aggregated primary equivalent
    left_join(.df %>% filter(item %in% Primary_crop) %>%
                group_by(region, year, element) %>%
                summarise(value = sum(value), .groups = "drop") %>%
                rename(Primary = value), by = c("region", "year", "element")) %>%
    mutate(APE = if_else(element == "Production", Primary, APE)) %>%
    gather(item, value, APE, Primary) %>%
    left_join(AGLU_ctry_Unique %>% select(region = FAO_country, iso), by = "region") %>%
    left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
    left_join(GCAM_region_names %>% select(GCAM_region_ID, reg = region), by = "GCAM_region_ID") %>%
    group_by(region = reg, item, year, element) %>% summarise(value = sum(value), .groups = "drop")
}
