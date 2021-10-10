
readRDS( file.path(out_dir, "Intermediate", "Bal_new_all.rds") ) -> Bal_new_all

#Aggregate to Primary equivalent and keep Primary
#Note that during the primarizing, the implied stock carry over may become imbalance
#due to potentially different extraction rates across periods.
#Thus, only a world average extraction rate is used
#residue will be adjusted later
#using world average extraction rates
Proc_primarize <- function(.df, source_item, sink_item,
                           agg_sink_items = T, keep_primary = F){
  assert_that(is.logical(agg_sink_items))
  assert_that(is.logical(keep_primary))
  .df %>%
    #Prepare data to calculate world average extraction rates
    filter((element == "Production" & item %in% sink_item) |
             (element == "Processed" & item %in% source_item)) %>%
    group_by(area, year, element) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    group_by(element) %>% mutate(value = sum(value)) %>%
    spread(element, value) %>%
    mutate(extraction_rate = if_else(is.na(Production / Processed), 1, Production / Processed)) %>%
    mutate(extraction_rate = if_else(extraction_rate == 0, 1, extraction_rate)) %>%
    select(area, year, extraction_rate) %>%
    #join to original dataframe to adjust sink items using the extraction rate
    right_join(.df, by = c("area", "year")) %>%
    mutate(value = if_else(item %in% sink_item, value / extraction_rate, value)) %>%
    select(-extraction_rate) -> .df1

  if (agg_sink_items == F) {
    return(.df1)
  } else
    if (agg_sink_items == T) {
      #when agg_sink_items is true, choose whether to keep primary item

      #sink_items are aggregated into "Processed"
      .df1 %>% mutate(item = replace(item, item %in% sink_item, "Processed")) %>%
        group_by(area, year, item, element) %>%
        summarise(value = sum(value), .groups = "drop") -> .df2

      if (keep_primary == T) {
        return(.df2)
      } else{
        #Merge sink items into source items  fn: Proc_primarize_mvprod
        #Note that using world average extraction rate leads to negative processed values
        #This needs to be adjusted later with residues
        .df2 %>% filter(element == "Production", item == "Processed") %>%
          select(area, year, item, prod = value) %>%
          right_join(.df2, by = c("area", "year", "item")) %>%
          mutate(value = if_else(item %in% c("Processed") & element %in% c("Processed", "Production"),
                                 value - prod, value)) %>%
          select(-prod) -> .df3

        if (length(source_item) == 1) {
          #aggregate to PE
          .df3 %>% mutate(item = replace(item, item %in% "Processed", source_item)) %>%
            group_by(area, year, item, element) %>%
            summarise(value = sum(value), .groups = "drop") -> .df4

        } else {
          #Note that with multiple source items, sinks are aggregated into sources based on average processed shares across sources
          #Prepare data to calculate world average source share
          .df3 %>% filter(element == "Processed", item %in% source_item)  %>%
            group_by(item) %>% mutate(value = sum(value)) %>% ungroup() %>%
            group_by(area, year) %>%
            mutate(share = value/sum(value)) %>% ungroup() %>%
            select(-element, - value) %>%
            full_join(.df3 %>% filter(item == "Processed") %>%
                        select(-item), by = c("area", "year")) %>%
            mutate(value = share * value) %>% select(-share) %>%
            bind_rows(.df3 %>% filter(item != "Processed")) %>%
            group_by(area, year, item, element) %>%
            summarise(value = sum(value), .groups = "drop") -> .df4
        }
        .df4 %>%
          spread(element, value) %>%
        #Check regional supply, demand, and residue
          mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
               `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
               Residuals = `Regional supply` -  `Regional demand`) %>%  #%>% filter(abs(Residuals1 - Residuals) > 1)
          gather(element, value, -area, -item, -year) %>%
          mutate(element = factor(element, levels = Bal_element_new)) -> .df5

        return(.df5)

      }
    }
}
#Move prod to cancel out processed before aggregation
Proc_primarize_mvprod <- function(.df, source_item){
  .df %>% filter(element == "Production", item == source_item) %>%
    select(area, year, item, prod = value) %>%
    right_join(.df, by = c("area", "year", "item")) %>%
    mutate(value = if_else(item %in% source_item & element %in% c("Processed", "Production"),
                           value - prod, value)) %>%
    select(-prod)
}

#Adding a same volume to feed and production based on feed share
#feedshare = cake / (oil + cake), where oil is source_item
Proc_primarize_addfeed <- function(.df, source_item, feedshare){

  assertthat::assert_that(feedshare <=1 & feedshare >= 0)
  .df %>% filter(element == "Production", item %in% source_item) %>%
    select(area, year, item, prod = value) %>%
    right_join(.df, by = c("area", "year", "item")) %>%
    mutate(value = if_else(item %in% source_item & element %in% c("Feed", "Production"),
                           value + (prod / feedshare - prod), value)) %>%
    select(-prod)
}

#Adding a same volume to others and production based on othershare
#othershare = other / (prod + other), where oil is source_item
Proc_primarize_addother <- function(.df, source_item, othershare){

  assertthat::assert_that(othershare <=1 & othershare >= 0)
  .df %>% filter(element == "Production", item %in% source_item) %>%
    select(area, year, item, prod = value) %>%
    right_join(.df, by = c("area", "year", "item")) %>%
    mutate(value = if_else(item %in% source_item & element %in% c("Other uses", "Production"),
                           value + (prod / othershare - prod), value)) %>%
    select(-prod)
}


#Map to GCAM regions
Reg_FAOtoGCAM <- function(.df, independent = NULL){
  .df %>%
    left_join(AGLU_ctry_Unique %>% select(area = FAO_country, iso), by = "area") %>%
    left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
    left_join(GCAM_region_names %>% select(GCAM_region_ID, region), by = "GCAM_region_ID") ->
    .df1

  if (is.null(independent)) {
    .df1 %>%
      group_by(region, item, year, element) %>%
      summarise(value = sum(value), .groups = "drop") -> .df2
    return(.df2) } else {

    assertthat::assert_that(
      all(intersect(independent,
                    AGLU_ctry_Unique %>% pull(FAO_country) %>% unique()) == independent))

      .df1 %>%
        mutate(region = if_else(area %in% independent, area, region)) %>%
        group_by(region, item, year, element) %>%
        summarise(value = sum(value), .groups = "drop") -> .df3
      return(.df3)
  }

}

unique(FBS_SUA_item_mapping$GCAM_commodity)


###############################################

