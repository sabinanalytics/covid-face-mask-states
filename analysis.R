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
  left_join(state_abbr, by = c("region" = "post_code")) %>% 
  left_join(mask_data, by = c("state" = "name")) %>% 
  mutate(mask_requirement = case_when(masks_required == "Entire State" ~ 'All',
                                      masks_required == "Entire Territory" ~ 'All',
                                      masks_required == "Parts of State" ~ 'Part',
                                      TRUE ~ "None"
                                      ),
         law_date = mdy(law_date)
         ) %>% 
  group_by(state) %>% 
  mutate(days_since_mask = time_length(date - law_date, unit = "day"),
         r_t_change_since_law = median - median[date == law_date]) %>% 
  ungroup() 



median_mask_date <- dat %>% 
  group_by(state) %>% 
  slice(1) %>% 
  filter(mask_requirement == "All") %>% 
  pull(law_date) %>% 
  median()


## plot change in r_t as function of time for states with mandatory mask requirements
mask_states <- dat %>% 
  filter(mask_requirement == "All",
         date >= ymd("2020-04-01")) %>% 
  ggplot(aes(x = days_since_mask, y = r_t_change_since_law)) + 
  facet_wrap(~ state) + 
  geom_path() + 
  theme_bw() + 
  xlab("Days Since Mask Law") + ylab("Change R_t") + 
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14)
  )

ggsave("plots/mask_states.png", mask_states)




## plot change in r_t as function of time for states with mandatory mask requirements
all_states_plot <- dat %>% 
  filter(date >= ymd("2020-04-01")) %>% 
  ggplot(aes(x = date, y = median, color = mask_requirement, group = state)) + 
  # facet_wrap(~ state) + 
  geom_path(size = 0.5, linetype = 2) + 
  geom_smooth(aes(x = date, y = median, group = mask_requirement, color = mask_requirement)) + 
  geom_vline(xintercept = median_mask_date, linetype = 3, size = 2) +
  annotate("text", label = "Median Mask Law Date", x = median_mask_date + days(10), y = 1.5) + 
  annotate("text", label = "Source: rt.live & masks4all.co", x = ymd("2020-06-01"), y = 0.6) + 
  theme_bw() + 
  xlab("Date") + ylab("Estimate of R_t") + 
  scale_color_discrete("Scope of Mask Law in State") + 
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        legend.position = "bottom", 
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
        )


ggsave("plots/all_states.png", all_states_plot)

