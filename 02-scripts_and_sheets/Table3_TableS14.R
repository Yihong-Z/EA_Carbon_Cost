library(tidyverse)

# Table 3.c1: Avoided emission ----------------------------------------------
emission=read.csv(here::here("01-data",
                              '2020_emission_plot.csv')) 

# Total emission
emission %>% 
  group_by(treatment) %>% 
  dplyr::summarise(mean=mean(carbon),
                   N_plot=n(),
                   se=sd(carbon)/N_plot^0.5) %>% 
  write.csv(here::here("03-results",
                       '2020_wildfire_emission_se.csv'))

#Table 3.c2: P-torch, P-mort summary------------------

potf=read.csv(here::here("01-data","FVS_PotFire.csv"))%>% 
  filter(StandID!="0340-00115")


potf %>% group_by(treatment) %>% 
  mutate(PTorch_Sev=PTorch_Sev*100) %>%
  dplyr::summarise(mean_ptorch=mean(PTorch_Sev),
            mean_pmort=mean(Mortality_BA_Sev),
            N_plot=n(),
            se_ptorch=sd(PTorch_Sev)/N_plot^0.5,
            se_pmort=sd(Mortality_BA_Sev)/N_plot^0.5) %>% 
  write.csv(here::here("03-results",
                       '2020_potfire_metric.csv'))

# Table 3.c3-4: Post-fire large-tree/fire-resistant mortality-----------------

# % Large tree dead

size_metric=read.csv(here::here("01-data",
                                '2020_large_tree_mortality_by_plot.csv')) 

size_summary=
  size_metric %>% 
  group_by(treatment) %>%
  dplyr::summarise(N_plot=n(),
            mean_dead_large_in_total_live_large=mean(dead_large_in_total_live_large),
            se_dead_large_in_total_live_large=sd(dead_large_in_total_live_large)/N_plot^0.5)

size_summary %>% 
  write.csv(here::here("03-results",'2020_large_tree_mortality.csv'))

# % Wildfire-resistant dead

fire_metric=read.csv(here::here("01-data",
                                '2020_fire_resist_mortality_by_plot.csv'))
fire_summary=
  fire_metric %>%
  group_by(treatment) %>%
  dplyr::summarise(N_plot=n(),
            mean_dead_fire_resist_in_total_live_fire_resist=mean(dead_fire_resist_in_total_live_fire_resist),
            se_dead_fire_resist_in_total_live_fire_resist=sd(dead_fire_resist_in_total_live_fire_resist)/N_plot^0.5)

fire_summary %>% write.csv(here::here("03-results",
                                      '2020_fire_resist_mortality.csv'))


# Table S14 ---------------------------------------------------------------
# Post-wildfire large tree carbon
plot_size=read.csv(here::here("01-data",
                              '2020_large_survive_by_plot.csv'))

size_survive=plot_size %>% 
  group_by(treatment) %>% 
  dplyr::summarise(mean_survive=mean(total_survive),
            mean_survive_large=mean(survive_large),
            N_plot=n(),
            se_survive=sd(total_survive)/N_plot^0.5,
            se_survive_large=sd(survive_large)/N_plot^0.5) %>% 
  select(-N_plot)

# Post-wildfire fire-resistant species carbon
plot_fire=read.csv(here::here("01-data",
                              '2020_fire_resist_survive_by_plot.csv'))

fire_survive=plot_fire %>% 
  group_by(treatment) %>% 
  dplyr::summarise(mean_survive=mean(total_survive),
            mean_survive_fire_resist=mean(total_survive_fire_resist),
            N_plot=n(),
            se_survive=sd(total_survive)/N_plot^0.5,
            se_survive_fire_resist=sd(total_survive_fire_resist)/N_plot^0.5)

survive_summary=
  left_join(size_survive,fire_survive,by="treatment") %>% 
  select(treatment,
         mean_survive_fire_resist,se_survive_fire_resist,
         mean_survive_large,se_survive_large) %>% 
  ungroup()

write.csv(survive_summary,here::here("03-results",
                                     '2020_survive_summary.csv'),
          row.names = FALSE)


