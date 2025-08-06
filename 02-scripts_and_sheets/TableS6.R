#### setup #####################################################################

library(here)
library(tidyverse)
library(rlang)
library(ggplot2)

# About---------------------------------------------------------------------

# Create Table S6


# Load data ---------------------------------------------------------------

all <- read.csv(here::here("01-data",
                         'Carbon_pool_pre_post_combined.csv'))

# Includes data for plots exist in both pre and post treatment

# Table S6: Carbon summary -------------------------------------------------

summary=all %>%
  group_by(treatment,timestep) %>% 
  dplyr::summarise(N_plot=n(),
                   mean_tree_Live=mean(tree_carbon_Live),
                   mean_tree_Snag=mean(tree_carbon_Snag),
                   mean_understory=mean(understory_carbon),
                   mean_duff=mean(duff_carbon),
                   mean_litter=mean(litter_carbon),
                   mean_fines=mean(fines_carbon),
                   mean_coarse=mean(coarse_carbon),
                   mean_soil=mean(soil_carbon),
                   mean_all_carbon_measured=mean(all_carbon),
                   se_tree_Live_per=sd(tree_carbon_Live)/N_plot^0.5,
                   se_tree_Snag_per=sd(tree_carbon_Snag)/N_plot^0.5,
                   se_understory_per=sd(understory_carbon)/N_plot^0.5,
                   se_duff_per=sd(duff_carbon)/N_plot^0.5,
                   se_litter_per=sd(litter_carbon)/N_plot^0.5,
                   se_fines_per=sd(fines_carbon)/N_plot^0.5,
                   se_coarse_per=sd(coarse_carbon)/N_plot^0.5,
                   se_soil_per=sd(soil_carbon)/N_plot^0.5,
                   se_all_carbon_measured=sd(all_carbon)/N_plot^0.5
  )%>%
  t() %>%
  as.data.frame()
summary

names(summary)=c("burn_post_18","burn_pre_treatment","control_post_18","control_pre_treatment",
                 "mech_post_18","mech_pre_treatment","mechburn_post_18","mechburn_pre_treatment")

summary=summary[-2:-1,]
rownames(summary)=c("N","tree_Live","tree_Snag","understory","duff","litter","fines","coarse","soil","all_carbon_measured",
                    "se_tree_live","se_tree_snag","se_understory","se_duff","se_litter","se_fines","se_coarse","se_soil",
                    "se_all_carbon_measured")

summary

## Write table 

write.csv(summary,here::here("03-results",
                              "TableS6_carbon_summary.csv"))
