
GCAM_crop <- "Poultry"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>%
  union(c("Fat, poultry, rendered", "Fat, poultry",
          "Offals, liver chicken", "Offals, liver geese", "Offals, liver duck", "Offals, liver turkeys"))

FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique()

setdiff(SUA_item, unique(Bal_new_all$item))

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal

bal %>% filter(year == 2017) %>%  Agg_reg(item, year, element) %>% spread(item, value) -> A


Poultry_primary <- c("Eggs, hen, in shell", "Eggs, other bird, in shell",
                     "Meat, chicken", "Meat, duck",	"Meat, goose and guinea fowl","Meat, turkey",
                     "Offals, liver geese", "Offals, liver duck", "Offals, liver turkeys")

bal %>%
  Proc_primarize(c("Eggs, hen, in shell"),
                 c("Eggs, liquid", "Eggs, dried", "Egg albumine")) %>%
  Proc_primarize(c("Fat, poultry"), c("Fat, poultry, rendered")) %>%
  Proc_primarize(c("Meat, chicken"), c("Meat, chicken, canned", "Fat, liver prepared (foie gras)",
                                       "Offals, liver chicken", "Fat, poultry")) %>%
  mutate(item = replace(item, item %in% Poultry_primary,"APE")) %>%
  Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% Poultry_primary) %>%
              mutate(item = "Poultry_primary") %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item %in% c("Eggs", "Poultry Meat")) %>% mutate(item = "FBS") %>%
              Agg_reg(area, year, item, element) %>%
              mutate(element = gsub(" Quantity", "", element)) %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3
