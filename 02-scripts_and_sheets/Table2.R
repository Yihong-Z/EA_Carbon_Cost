library(tidyverse)


# About -------------------------------------------------------------------

# Create Table 2


# Table 2.c2 FOFEM prescribed fire emission (Rx emission factors)----------------------------------------------------------

emission_plot=read.csv(here::here("01-data",
                                   'FOFEM_emission(by_plot)_Rx.csv')) 

emission_summary=
  emission_plot %>% 
  group_by(treatment) %>%
  dplyr::summarise(mean_emission=mean(total_emission),
            N_plot=n(),
            se_emission=sd(total_emission)/N_plot^0.5)


# Write summary 
write.csv(emission_summary,here::here("03-results",
                                      'FOFEM_emission_summary(by_plot)_Rx.csv'))

# Table 2.c3 Total wood removal -------------------------------------------------
total_by_plot=read.csv(here::here("01-data",
                                   'wood_removal_sum_2002_2018(mill_update)_by_plot.csv'))

# Value for column2
summary=
  total_by_plot %>%
  group_by(treatment) %>%
  dplyr::summarise(N_plot=n(),
            mean_wood_removal=mean(wood_removal),
            se_wood_removal=sd(wood_removal)/N_plot^0.5)
summary

write.csv(summary,here::here("03-results",
                             'wood_removal_sum_2002_2018(mill_update).csv'))

# Table 2.c1,4,5: Get net change --------------------------------------------------

# Here, the plot number needs to be consistent in 2001 and 2020
# also need to remove 0340-00115 as it did not have post_7 measurement for calculating emission

tree_plot=read.csv(here::here("01-data",
                              'Carbon_pool_pre_post_combined.csv')) %>% 
  filter(plot_id!="0340-00115")

harvest_plot=read.csv(here::here("01-data",
                                 'wood_removal_sum_2002_2018(mill_update)_by_plot.csv')) 

emission_plot=read.csv(here::here("01-data",
                                  'FOFEM_emission(by_plot)_Rx.csv'))

# Net change & NEP in Control
control=
  tree_plot %>% filter(treatment=="control") %>% 
  dplyr::select(plot_id,timestep,all_carbon) %>% 
  pivot_wider(names_from = timestep,values_from = all_carbon) %>% 
  mutate(net_change=`post_18`-`pre_treatment`,
         NEP=net_change/19) %>% 
  dplyr::summarise(N_plot=n(),
                   mean_net_change=mean(net_change),
                   se_net_change=sd(net_change)/N_plot^0.5,
                   mean_NEP=mean(NEP),
                   se_NEP=sd(NEP)/N_plot^0.5)
control

# Net change & NEP in Burn
tree_plot_burn=
  tree_plot %>% filter(treatment=="burn") %>% 
  dplyr::select(plot_id,timestep,all_carbon) %>% 
  pivot_wider(names_from = timestep,values_from = all_carbon) %>% 
  mutate(net_change=`post_18`-`pre_treatment`)

# Value in Total NECB
tree_plot_burn %>%
  dplyr::summarise(N_plot=n(),
                   mean_net_change=mean(net_change),
                   se_net_change=sd(net_change)/N_plot^0.5)

emission_plot_burn=emission_plot %>% filter(treatment=="Burn")

burn=
  tree_plot_burn %>% 
  left_join(emission_plot_burn,by="plot_id") %>% 
  mutate(net_change=net_change+total_emission,
         NEP=net_change/19) %>% 
  dplyr::summarise(N_plot=n(),
                   mean_net_change=mean(net_change),
                   se_net_change=sd(net_change)/N_plot^0.5,
                   mean_NEP=mean(NEP),
                   se_NEP=sd(NEP)/N_plot^0.5)
burn

# Net change & NEP in Mech
tree_plot_mech=tree_plot %>% filter(treatment=="mech") %>% 
  dplyr::select(plot_id,timestep,all_carbon) %>% 
  pivot_wider(names_from = timestep,values_from = all_carbon) %>% 
  mutate(net_change=`post_18`-`pre_treatment`)

# Value in Total NECB
tree_plot_mech %>% 
  dplyr::summarise(N_plot=n(),
                   mean_net_change=mean(net_change),
                   se_net_change=sd(net_change)/N_plot^0.5)

harvest_plot_mech=harvest_plot %>% filter(treatment=="mech")

mech=
  tree_plot_mech %>% left_join(harvest_plot_mech,by="plot_id") %>% 
  mutate(net_change=net_change+wood_removal,
         NEP=net_change/19) %>% 
  dplyr::summarise(N_plot=n(),
                   mean_net_change=mean(net_change),
                   se_net_change=sd(net_change)/N_plot^0.5,
                   mean_NEP=mean(NEP),
                   se_NEP=sd(NEP)/N_plot^0.5)
mech

#Net change & NEP in MechBurn
tree_plot_mechburn=
  tree_plot %>% filter(treatment=="mechburn") %>% 
  dplyr::select(plot_id,timestep,all_carbon) %>% 
  pivot_wider(names_from = timestep,values_from = all_carbon) %>% 
  mutate(net_change=`post_18`-`pre_treatment`)

# Value in Total NECB
tree_plot_mechburn %>%
  dplyr::summarise(N_plot=n(),
                   mean_net_change=mean(net_change),
                   se_net_change=sd(net_change)/N_plot^0.5)

emission_plot_mechburn=emission_plot %>% filter(treatment=="MechBurn")

harvest_plot_mechburn=harvest_plot %>% filter(treatment=="mechburn")

mechburn=
  tree_plot_mechburn %>% 
  left_join(harvest_plot_mechburn,by="plot_id") %>% 
  left_join(emission_plot_mechburn,by="plot_id") %>%
  mutate(net_change=net_change+wood_removal+total_emission,
         NEP=net_change/19) %>% 
  dplyr::summarise(N_plot=n(),
                   mean_net_change=mean(net_change),
                   se_net_change=sd(net_change)/N_plot^0.5,
                   mean_NEP=mean(NEP),
                   se_NEP=sd(NEP)/N_plot^0.5)

bind_rows(control,burn,mech,mechburn) %>% 
  mutate(treatment=c("Control","Burn","Mech","MechBurn")) %>%
  write.csv(here::here("03-results",
                       'net_change.csv'),
            row.names = FALSE)



