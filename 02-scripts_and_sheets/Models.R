#### setup #####################################################################
library(here)
library(tidyverse)
library(rlang)
library(ggplot2)
library(readxl)
library(stringr)
library(lme4)
library(sjstats)
library(lmerTest)
library(DHARMa)
library(TMB)
library(glmmTMB)

# library(tab)
# library(nlme)
# library(mgcv)
# library(statmod)
# library(ptmixed)
# library(bbmle)
# library(zoib)
# library(MuMIn)
# library(dplyr)


# About -------------------------------------------------------------------

# Stats analysis on total carbon stocks, %fire-resistant carbon, live veg and surface fuel recovery rates


# 1.Total Carbon: Model ---------------------------------------------------

all=readRDS(here::here("01-data","fig1a_totalcarbon.rds"))

lm <- lmer(sqrt(all_carbon) ~ treatment_factor + timestep_factor + 
                              treatment_factor*timestep_factor+(1|comp), data = all)
summary(lm)

# Save model for later figure 
saveRDS(lm,here::here("03-results","fig1a_totalcarbon_lm.rds"))

# 2. %Large Tree Carbon: Model ---------------------------------------------

large_percentage=
  readRDS(here::here("01-data","fig2_largepercentage.rds"))

large_percentege_remove=
  large_percentage %>% 
  mutate(large_percentage_01=ifelse(large_percentage_01==1,0.999999999,large_percentage_01))

fit_beta_1 <- glmmTMB(large_percentage_01 ~ treatment_factor + timestep_factor + treatment_factor*timestep_factor+(1|comp), 
                      data = large_percentege_remove,
                      ziformula=~timestep_factor,
                      dispformula=~timestep_factor*treatment_factor,
                      family=beta_family(link="probit"))
summary(fit_beta_1)
testZeroInflation(fit_beta_1)#ok
testDispersion(fit_beta_1)# ok
hist(simulateResiduals(fit_beta_1))
testUniformity(fit_beta_1)

saveRDS(fit_beta_1,here::here("03-results","fig2_largepercentage_beta.RDS"))


# 3. fire-resistant carbon percentage -------------------------------------
plot_fire_percentage=
  readRDS(here::here("01-data","fig2a_firepercentage.RDS"))

fire_percentege_remove=
  plot_fire_percentage %>% 
  mutate(fire_percentage_1=ifelse(fire_percentage_01==1,0.999999999,fire_percentage_01))
fire_percentege_remove$fire_percentage_1

fit_beta_2 <- glmmTMB(fire_percentage_1 ~ treatment_factor + timestep_factor + treatment_factor*timestep_factor+(1|comp), 
                      data = fire_percentege_remove,ziformula=~treatment_factor,family=beta_family)
summary(fit_beta_2)
testZeroInflation(fit_beta_2)#ok
testDispersion(fit_beta_2)#pass
hist(simulateResiduals(fit_beta_2))#have outliers
testUniformity(fit_beta_2)#OK
plot(simulateResiduals(fit_beta_2))

saveRDS(fit_beta_2,here::here("03-results",
                              "fig3_firetolerantn_beta.rds"))


# 4. Recovery ----------------------------------------------------------------
recovery=read.csv(here::here('01-data',
                             'recovery_burn_control.csv'))

# Burn1: live veg recovery rate -------------------------------------------
recovery_burn_1=
  recovery %>% filter(period=="Burn_1") %>% 
  mutate(treatment_factor=as.factor(treatment))

recovery_burn_1$treatment_factor=relevel(recovery_burn_1$treatment_factor,ref="control")

## Fit model

lm=lmer(live_vegetation_recovery_rate~ 1+treatment_factor+live_vegetation_carbon+
           live_veg_disturbance+(1|comp),
         data=recovery_burn_1)
summary(lm)

saveRDS(lm,here::here("03-results","fig4_veg_recover_lme1.rds"))

# Burn2: live veg recovery rate -------------------------------------------
recovery_burn_2=
  recovery %>% filter(period=="Burn_2") %>% 
  mutate(treatment_factor=as.factor(treatment))

recovery_burn_2$treatment_factor=relevel(recovery_burn_2$treatment_factor,ref="control")


lm2=lmer(live_vegetation_recovery_rate~ 1+treatment_factor+live_vegetation_carbon+
           live_veg_disturbance+(1|comp),
         data=recovery_burn_2)
summary(lm2)

saveRDS(lm2,here::here("03-results","fig4_veg_recover_lme2.rds"))

# Burn3: live veg recovery rate -------------------------------------------
recovery_burn_3=
  recovery %>% filter(period=="Burn_3") %>% #&(treatment=="control"|treatment=="control"))
  mutate(treatment_factor=as.factor(treatment))

recovery_burn_3$treatment_factor=relevel(recovery_burn_3$treatment_factor,ref="control")

lm3=lm(live_vegetation_recovery_rate~ 1+treatment_factor+live_vegetation_carbon+
           live_veg_disturbance,
         data=recovery_burn_3)
summary(lm3)

saveRDS(lm3,here::here("03-results","fig4_veg_recover_lme3.rds"))

# read in surface fuel ----------------------------------------------------

recovery=read.csv(here::here('01-data',
                             'recovery_surface_fuels_burn_control.csv'))


# Burn1: surface fuel recovery rate -------------------------------------------
recovery_burn_1=
  recovery %>% filter(period=="Burn_1") %>% 
  mutate(treatment_factor=as.factor(treatment))

recovery_burn_1$treatment_factor=relevel(recovery_burn_1$treatment_factor,ref="control")

#LME
lm1=lmer(fuels_recovery_rate~ 1+treatment_factor+surface_carbon+
           fuels_disturbance+(1|comp),
         data=recovery_burn_1)
summary(lm1)
hist(residuals(lm1))
plot(lm1) 

saveRDS(lm1,here::here("03-results","fig4_surface_recover_lme1.rds"))

# Burn2: surface fuel recovery rate -------------------------------------------
recovery_burn_2=
  recovery %>% filter(period=="Burn_2") %>% 
  mutate(treatment_factor=as.factor(treatment))

recovery_burn_2$treatment_factor=relevel(recovery_burn_2$treatment_factor,ref="control")

#LME

lm2=lm(fuels_recovery_rate~ 1+treatment_factor+surface_carbon+
           fuels_disturbance,
         data=recovery_burn_2)
summary(lm2)
hist(residuals(lm2),20)
plot(lm2) #with outliers


saveRDS(lm2,here::here("03-results","fig4_surface_recover_lme2.rds"))

# Burn3: surface fuel recovery rate -------------------------------------------
recovery_burn_3=
  recovery %>% filter(period=="Burn_3") %>% 
  mutate(treatment_factor=as.factor(treatment))

recovery_burn_3$treatment_factor=relevel(recovery_burn_3$treatment_factor,ref="control")

#LME
lm3=lm(fuels_recovery_rate~ 1+treatment_factor+surface_carbon+
           fuels_disturbance,
         data=recovery_burn_3)
summary(lm3)
hist(residuals(lm3))
plot(lm3) #good

saveRDS(lm3,here::here("03-results","fig4_surface_recover_lme3.rds"))
