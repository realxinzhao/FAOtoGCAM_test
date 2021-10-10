
GCAM_crop <- "Dairy"
#*************
#* data 2014 to 2018

SUA_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>%
  pull(SUA_item) %>% unique() %>% setdiff(c("Milk, reconstituted", "Cheese, processed")) #all zeros or not needed

# related FBS items Offals, Edible and products and Fats, Animals, Raw and products


FBS_item <- FBS_SUA_item_mapping %>% filter(GCAM_commodity %in% c(GCAM_crop)) %>% pull(FBS_label) %>% unique() %>%
  union(c("Milk - Excluding Butter", "Cream", "Butter, Ghee"))

setdiff(SUA_item, unique(Bal_new_all$item))

#Filter by relevant items
Bal_new_all %>% filter(item %in% c(SUA_item)) -> bal


bal %>% filter(year == 2017, #area == "United States of America",
               item %in% c("Milk, skimmed cow", "Buttermilk, curdled, acidified milk")) %>%
  Agg_reg(area,item, year, element) %>% spread(item, value) -> A



Dairy_primary <- c("Milk, whole fresh buffalo", "Milk, whole fresh goat", "Milk, whole fresh sheep",
                  "Milk, whole fresh camel", "Ice cream and edible ice",
                  "Milk, whole fresh cow", "Milk, products of natural constituents nes")

bal %>%
  Proc_primarize(c("Milk, whole fresh buffalo"),
                 c("Ghee, buffalo milk",	"Butter, buffalo milk",	"Milk, skimmed buffalo",	"Cheese, buffalo milk")) %>%
  Proc_primarize(c("Milk, whole fresh goat"),
                 c("Milk, skimmed goat",	"Cheese, goat milk",	"Butter, goat milk")) %>%
  Proc_primarize(c("Milk, whole fresh sheep"),
                 c("Milk, skimmed sheep",	"Butter and ghee, sheep milk", "Cheese, sheep milk")) %>%
  Proc_primarize(c("Whey, fresh"),
                 c("Whey, cheese",	"Whey, condensed",	"Whey, dry")) %>%
  mutate(item = replace(item, item %in% c("Milk, skimmed cow", "Buttermilk, curdled, acidified milk"), "Skim Buttermilk")) %>%
  Agg_reg(area, year, item, element) %>%
  Proc_primarize(c("Skim Buttermilk"),
                 c("Cheese, skimmed cow milk",	"Milk, skimmed condensed",
                   "Milk, skimmed dried",	"Milk, skimmed evaporated",	"Milk, dry buttermilk",
                   "Casein")) %>%  #Whey is added in an upper nest
  Proc_primarize(c("Milk, whole fresh cow"),
                 c("Ghee, butteroil of cow milk",	"Butter, cow milk", "Cream fresh",
                   "Yoghurt, concentrated or not", "Yoghurt",
                   "Milk, whole condensed",	"Milk, whole dried",	"Milk, whole evaporated",
                   "Cheese, whole cow milk", "Skim Buttermilk", "Whey, fresh" )) %>%
  mutate(item = replace(item, item %in% Dairy_primary,"APE")) %>%
  Agg_reg(area, year, item, element) %>%
  bind_rows(bal %>% filter(item %in% Dairy_primary) %>%
              mutate(item = "Dairy_primary") %>%
              Agg_reg(area, year, item, element) ) -> T2

assign(paste0(GCAM_crop), T2 %>%
         filter(item == "APE") %>% mutate(item = GCAM_crop))


T2 %>%
  Reg_FAOtoGCAM %>% spread(item, value) %>%
  left_join(FBS %>% filter(item %in% FBS_item) %>% mutate(item = "FBS") %>%
              Agg_reg(area, year, item, element) %>%
              mutate(element = gsub(" Quantity", "", element)) %>%
              Reg_FAOtoGCAM %>% spread(item, value), by = c("region", "year", "element") ) %>%
  gather(item, value, -region, -year, -element) %>%
  group_by(year, element, item) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% spread(item, value)-> T3

# remaining processed is likely (processed) food uses
