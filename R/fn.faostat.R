# Functions start with FF indicate FAO Functions
#Some functions are duplicated or adapted from FAOSTAT package

FF_metadata <- function (code = NULL, dataset = NULL, topic = NULL, latest = FALSE, full = TRUE)
{
  FAOxml <- XML::xmlParse("http://fenixservices.fao.org/faostat/static/bulkdownloads/datasets_E.xml")
  metadata <- XML::xmlToDataFrame(FAOxml, stringsAsFactors = FALSE)
  names(metadata) <- tolower(gsub("\\.", "_", names(metadata)))
  if (!is.null(code)) {
    metadata <- metadata[code == metadata[, "datasetcode"],
    ]
  }
  if (!is.null(dataset)) {
    metadata <- metadata[grep(dataset, metadata[, "datasetname"],
                              ignore.case = TRUE), ]
  }
  if (!is.null(topic)) {
    metadata <- metadata[grep(topic, metadata[, "topic"],
                              ignore.case = TRUE), ]
  }
  if (latest == TRUE) {
    metadata <- metadata[order(metadata$DateUpdate, decreasing = TRUE),
    ]
  }
  if (full == FALSE) {
    return(metadata[, c("datasetcode", "datasetname",
                        "dateupdate")])
  }
  if (full == TRUE) {
    return(metadata)
  }
  else (return("Invalid query"))
}

# download the latest FF_metadata
# compare it with the last one

FF_metadata_update <- function(
  DATASETCODE  = NA,
  LOG_INDICATOR = "fao_metadata",
  DATA_FOLDER = "data_raw/FAOSTAT_meta/log/"){

  assertthat::assert_that(is.na(DATASETCODE)|is.character(DATASETCODE))
  # Add check path later [ToDo]


  # Download and save the latest metadata
  readr::write_csv(FF_metadata(),
                   file.path(DATA_FOLDER,
                             paste0(LOG_INDICATOR, "_", Sys.Date(),".csv")))

  # Read the lastest two metadata files in a list
  file.info(dir(DATA_FOLDER , full.names = T)) %>%
    tibble::rownames_to_column(var = "filelocation") %>%
    filter(isdir == F,
           grepl(LOG_INDICATOR, filelocation)) %>%
    transmute(filelocation, ctime = as.Date(ctime), mtime = as.Date(mtime) ) %>%
    arrange(desc(mtime)) %>%
    head(2) ->
    fao_metadata0

  fao_metadata0 %>%
    pull(filelocation) %>%
    lapply(readr::read_csv, col_types = readr::cols())->
    fao_metadata1

  # Filter DATASETCODE of interest
  if (!is.na(DATASETCODE)) {
    lapply(fao_metadata1, function(.df){
      .df %>% filter(datasetcode %in% DATASETCODE )
    }) -> fao_metadata1
  }

  # Setdiff new and old
  setdiff(fao_metadata1[[1]], fao_metadata1[[2]]) -> fao_metadata_diff

  if (nrow(fao_metadata_diff) == 0) {
    print(paste0( "No FAO data updates since ", fao_metadata0 %>% pull(mtime) %>% last()))
  } else {
    print("Dataset modified or added:")
    return(fao_metadata_diff)
  }
}


#extract info of data (e.g., zip files) in a folder
#folder including data files

FF_rawdata_info <- function(DATA_FOLDER = "data_raw/"){

  file.info(dir(DATA_FOLDER, full.names = T)) %>%
    tibble::rownames_to_column(var = "filelocation") %>%
    # Filter zip dir to ensure FAO files
    filter(isdir == F,
           grepl("zip$", filelocation)) %>%
    transmute(filelocation = gsub(DATA_FOLDER, "", filelocation),
              ctime = as.Date(ctime), mtime = as.Date(mtime)) %>%
    # Join the latest metadata
    # Note that FAO raw data had a typo (missing space) in Trade_CropsLivestock_E_All_Data_(Normalized).zip
    # Temporary fix here
    left_join(FF_metadata() %>%
                mutate(filelocation = gsub("http://fenixservices.fao.org/faostat/static/bulkdownloads/", "", filelocation)) %>%
                mutate(filelocation = replace(filelocation,
                                              filelocation == "Trade_CropsLivestock_E_All_Data_(Normalized).zip",
                                              "Trade_Crops_Livestock_E_All_Data_(Normalized).zip")),
              by = "filelocation") %>%
    transmute(datasetcode, datasetname,  FAOupdate = dateupdate, Localupdate = mtime,
              needupdate = FAOupdate > Localupdate, filesize)

}



download_faostat_bulk <- function(code, data_folder){
  metadata <- FAOsearch(code = code)
  url_bulk = metadata$filelocation
  file_name <- basename(url_bulk)
  download.file(url_bulk, file.path(data_folder, file_name))
}

#replace . or space with _
read_faostat_bulk <- function(zip_file_name){
  csv_file_name <- gsub(".zip$", ".csv", basename(zip_file_name))
  df <- readr::read_csv(unz(zip_file_name, csv_file_name), col_types = NULL)
  names(df) <- tolower(gsub("\\.| ", "_", names(df)))
  return(df)
}

#' get_faostat_bulk

#' @param code FAO data set code
#' @param data_folder Directory of raw data
#' @param download logical; Default = F; if T download latest data
#'
#' @return
#' @export

get_faostat_bulk <- function (code, data_folder, download = F){
  assert_that(is.character(code))
  assert_that(is.character(data_folder))
  assert_that(is.logical(download))

  metadata <- FAOsearch(code = code)

  if (isTRUE(download)) {
    download_faostat_bulk(code = code, data_folder = data_folder)
  } else {
    assert_that(file.exists(file.path(data_folder, basename(metadata$filelocation))))
  }

  output <- read_faostat_bulk(file.path(data_folder, basename(metadata$filelocation)))
  return(output)
}


