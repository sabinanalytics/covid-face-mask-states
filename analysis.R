library(tidyverse)
library(lubridate)
library(rvest)
library(magrittr)
options(tibble.width = Inf)

## r_t model data from Kevin Systrom and Thomas Vladeck
r_t_dat <- read_csv("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv")

state_abbr <- read_html("https://www.infoplease.com/us/postal-information/state-abbreviations-and-state-postal-codes") %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  extract2(1) %>% 
  rename(state = 1, 
         abbr = 2, 
         post_code = 3)

mask_files <- list.files("state_requirements")

mask_data <- NULL
for( i in 1:length(mask_files)){
  mask_data <- read_csv(paste0("state_requirements/",mask_files[i])) %>% 
    mutate(data_date = str_replace_all(mask_files[i], "\\.|csv","") %>% 
                        str_replace_all("_", " ") %>% 
                        mdy()
           ) %>% 
    bind_rows(mask_data, . )
}
colnames(mask_data) <- colnames(mask_data) %>% 
  str_trim() %>% 
  str_replace_all("[[:punct:]]", "") %>% 
  str_replace_all("[[:space:]]", "_") %>% 
  str_to_lower()
  

mask_data$Name %>% table()


dat <- r_t_dat %>% 
  left_join(state_abbr, by = c("region" = "Postal Code")) %>% 
  left_join(mask_data, by = c("State/District" = "Name")) %>% 
  mutate(mask_requirement = case_when(masks_required == "Entire State" ~ 'All',
                                      masks_required == "Entire Territory" ~ 'All',
                                      masks_required == "Parts of State" ~ 'Part',
                                      TRUE ~ "None"
                                      ),
         
         )

