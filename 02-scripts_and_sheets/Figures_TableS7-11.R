#### setup #####################################################################
library(here)
library(tidyverse)
library(dplyr)
library(plyr)
library(rlang)
library(ggplot2)
library(grid)
library(gridExtra)
library(DHARMa)
library(TMB)
library(glmmTMB)

library(lme4)
library(sjstats)
library(lmerTest)

library(tab)
library(nlme)
library(mgcv)
library(statmod)

library(rjags)
library(MuMIn)

library(scales)
library(RColorBrewer)

# About -------------------------------------------------------------------

# This script is to make figures for the paper.

rm(list=ls())


# Format setting ----------------------------------------------------------
font_size=list(
  theme(legend.text=element_text(size=16),
        strip.text=element_text(size=16),
        axis.title = element_text(size=16),
        axis.text = element_text(size= 16,color="black"),
        title=element_text(size=16),
        text=element_text(size=16,family="serif"))
)

sjPlot::set_theme(theme.font="serif")


# Figure 1. Change in total measured carbon --------------------------------
# Figure 1a

all=readRDS(here::here("01-data", "fig1a_totalcarbon.rds"))

fig1<-ggplot(data=all,aes(x=treatment_factor,y=all_carbon,fill=timestep_factor))+
  geom_boxplot(width=0.6)+
  stat_summary(fun=base::mean, geom="point", shape=18, 
               size=3,position=position_dodge(0.6),
               aes(group=timestep_factor)) +
  theme_bw()+
  theme(legend.justification = c(0.98,0.98),
        legend.position=c(0.98,0.98),
        legend.title=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank())+
  labs(y="Total measured carbon (MgC/ha)",x="Treatment",
       size=18)+
  scale_x_discrete(labels=c("Control","Fire","Mech","Mech+Fire"))+
  scale_fill_manual(values = c("#8FBC94","#548687"), 
                     name="Timestep", 
                    labels=c("2001","2020"),
                    guide = guide_legend(reverse = TRUE))+
  font_size


# ggsave(fig1,
#        filename=here::here("05-results","figures","fig1a_totalcarbon_T.pdf"),
#        width=6.66,height=3.81,dpi=600,device = "pdf")

# Obtain mean and sd
a=all %>% 
  dplyr::group_by(treatment,timestep) %>% 
  dplyr::summarise(N_plot=n(),
                   mean=mean(all_carbon),
                   se=sd(all_carbon)/sqrt(n()))
a

## Figure 1b 
lm=readRDS(here::here("03-results", "fig1a_totalcarbon_lm.rds"))
summary(lm)

fig2=sjPlot::plot_model(lm, 
                   axis.labels=c(rev(c(
                     "Control","Fire","Mech",
                     "Mech+Fire"))),
                   terms=c("timestep_factorpost_18",
                           "treatment_factorburn:timestep_factorpost_18",
                           "treatment_factormech:timestep_factorpost_18",
                           "treatment_factormechburn:timestep_factorpost_18"),
                   group.terms = c("Baseline change","Treatment effect",
                                   "Treatment effect","Treatment effect"),
                   show.values=TRUE, show.p=FALSE,
                   colors=c("#3FB8AFFF","#3FB8AFFF"),
                   vline.color="#636363",
                   value.offset = 0.35,
                   value.size = 5,
                   value.font="serif",
                   dot.size=4)+
  sjPlot::theme_sjplot(base_family="serif")+
  aes(shape=group)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.justification = c(0.80,0.98),
        legend.position=c(0.3,1),
        legend.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  labs(y="Effect Size",x=" ", title="",size=18)+
  font_size
fig2

# ggsave(fig2,filename=here::here("05-results","figures","fig1b_totalcarbon_lm.pdf"),
#        width=7.04,height=3.75,dpi=600,device = "pdf")

grid.arrange(fig1,fig2,nrow=2)

## Table S7. Model of total measured carbon--------------------------------------------------------
sjPlot::tab_model(lm, 
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", 
                                 "Treatment:Fire", "Treatment:Mech", "Treatment:Mech+Fire",
                                 "Treatment:Control x Time:2020",
                                 "Treatment:Fire x Time:2020",
                                 "Treatment:Mech x Time:2020",
                                 "Treatment:Mech+Fire x Time:2020"),
                  dv.labels= "Response variable: Sqrt(total measured carbon)")


# Figure 2. Live veg and surface fuel recovery ----------------------------
# Figure 2a. Live veg recovery
recovery=read.csv(here::here('01-data',
                             'recovery_burn_control.csv')) %>% 
  select(plot_id,treatment,comp,rate=live_vegetation_recovery_rate,period) %>%
  mutate(type="Live vegetation recovery",
         treatment_factor=as.factor(treatment))

recovery$treatment_factor <- factor(recovery$treatment, 
                                    levels=c('control', 'burn'))
recovery %>% 
  dplyr::group_by(period,treatment_factor) %>% 
  dplyr::summarise(N_plot=n(),
                   mean=mean(rate),
                   se=sd(rate)/sqrt(n()))

relevel(recovery$treatment_factor,ref="control")

mean=ddply(recovery, c("treatment_factor","period","type"), 
           summarise, mean=mean(rate))
mean

median=ddply(recovery, c("treatment_factor","period","type"), 
             summarise, median=median(rate))
median  

period.labs <- c("1st Fire Recovery",
                 "2nd Fire Recovery",
                 "3rd Fire Recovery") 
names(period.labs) <- c("Burn_1", "Burn_2","Burn_3")

fig7=ggplot(data=recovery,
            aes(x=rate,
                fill=treatment_factor))+
  geom_density(alpha=0.5,linewidth=0.1)+
  ylim(c(0,0.18))+
  facet_wrap(.~period,nrow=3,
             labeller=labeller(period=period.labs),
             #scales="free_x"
  )+
  geom_vline(data=mean%>% filter(treatment_factor=="control"&(period=="Burn_1"|period=="Burn_2"|period=="Burn_3")),
             aes(xintercept=mean,color=treatment_factor
             ),alpha=0.7,   
             linetype=1, linewidth=1)+
  geom_vline(data=mean%>% filter(treatment_factor=="burn"&(period=="Burn_1"|period=="Burn_2"|period=="Burn_3")),
             aes(xintercept=mean,color=treatment_factor),   
             linetype=6, linewidth=1)+
  theme_bw()+
  theme(#legend.justification = c(0.97,1.00),
    legend.position="bottom",
    legend.background=element_blank(),
    legend.key.size = unit(5,"mm"),
    legend.direction="horizontal",
    strip.background = element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor = element_blank(),
    panel.border=element_blank() ,
    axis.line=element_line(color="black") 
  )+
  labs(y="Density",
       x="Live vegetation recovery and accumulation rate \n (MgC/ha/yr)",
       size=12,fill="Treatment")+
  scale_fill_manual(values=c("#548687","#33CC00"),
                    labels=c("Control","Fire"),
                    name="")+
  scale_color_manual(values=c("#FD8D3C","#F16913"),
                     labels=c("Control","Fire"),
                     name="")+
  font_size

fig7

# ggsave(fig7,filename=here::here("05-results","figures","fig2a_liveveg_recovery_T.pdf"),
#        width=5,height=8,dpi=600,device="pdf")

#Figure 2b. Surface fuels recovery
surface_fuels=read.csv(here::here('01-data',
                                  'recovery_surface_fuels_burn_control.csv'))%>% 
  select(plot_id,treatment,comp,rate=fuels_recovery_rate,period) %>%
  mutate(type="Surface fuel recovery") %>% 
  mutate(treatment_factor=as.factor(treatment))

period.labs <- c("1st Fire Recovery",
                 "2nd Fire Recovery",
                 "3rd Fire Recovery") 
names(period.labs) <- c("Burn_1", "Burn_2","Burn_3")

mean=ddply(surface_fuels, c("treatment_factor","period","type"), 
           summarise, mean=mean(rate))
mean$treatment_factor <- factor(mean$treatment, 
                                levels=c('control', 'burn'))
mean

median=ddply(surface_fuels, c("treatment_factor","period","type"), 
             summarise, median=median(rate))
median  

surface_fuels %>% 
  dplyr::group_by(period,treatment_factor) %>% 
  dplyr::summarise(mean=mean(rate),
                   se=sd(rate)/sqrt(n()),
                   N_plot=n())

surface_fuels$treatment_factor <- factor(surface_fuels$treatment, 
                                         levels=c('control', 'burn'))
relevel(surface_fuels$treatment_factor,ref="control")

fig8b=ggplot(data=surface_fuels,
             aes(x=rate,
                 fill=treatment_factor))+
  geom_density(alpha=0.5,linewidth=0.1)+
  ylim(c(0,0.6))+
  facet_wrap(~period,nrow=3,
             labeller=labeller(period=period.labs),
             scales="free_y"
  )+
  geom_vline(data=mean%>% filter(treatment_factor=="control"&
                                   (period=="Burn_1"|period=="Burn_2"|
                                      period=="Burn_3")),
             aes(xintercept=mean,color=treatment_factor),   
             alpha=0.7,
             linetype=1, linewidth=1)+
  geom_vline(data=mean%>% filter(treatment_factor=="burn"&(period=="Burn_1"|period=="Burn_2"|period=="Burn_3")),
             aes(xintercept=mean,color=treatment_factor),   
             alpha=0.9,
             linetype=6, linewidth=1)+
  theme_bw()+
  theme(
    legend.position="bottom",
    legend.background=element_blank(),
    legend.key.size = unit(5,"mm"),
    legend.direction="horizontal",
    title=element_text(size=12),
    strip.background = element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor = element_blank(),
    panel.border=element_blank() ,
    axis.line=element_line(color="black") 
  )+
  labs(y="Density",
       x="Surface fuel accumulation and recovery rate\n(MgC/ha/yr)",
       size=12,fill="Treatment")+
  scale_fill_manual(values=c("#FDDBC7","#B2182B"),
                    labels=c("Control","Fire"),
                    name="")+
  scale_color_manual(values=c("#1BB6AFFF","#088BBEFF"),
                     labels=c("Control","Fire"),
                     name="")+
  font_size

# ggsave(fig8b,filename=here::here("05-results","figures","fig2b1_surfacefuels_recovery_T.pdf"),
#        width=5,height=8,dpi=600,device="pdf")
# ggsave(fig8a,filename=here::here("05-results","figures","fig2b2_surfacefuels_recovery_T.pdf"),
#        width=5,height=8,dpi=600,device="pdf")

## Table S8. Model of live veg recovery -----------------------------------------------

lm1=readRDS(here::here("03-results","fig4_veg_recover_lme1.rds"))
sjPlot::plot_model(lm1,show.values=TRUE,show.p=TRUE)
summary(lm1)

lm2=readRDS(here::here("03-results","fig4_veg_recover_lme2.rds"))
sjPlot::plot_model(lm2,show.values=TRUE,show.p=TRUE)
summary(lm2)

lm3=readRDS(here::here("03-results","fig4_veg_recover_lme3.rds"))
sjPlot::plot_model(lm3,show.values=TRUE,show.p=TRUE)
summary(lm3)

sjPlot::tab_model(lm1,lm2,lm3,show.intercept=TRUE,
                  pred.labels=c( "(Intercept)",
                                 "Treatment:Fire", 
                                 "Pre-treatment live vegetation carbon",
                                 "Disturbance intensity"),
                  dv.labels=c("1st Fire Recovery",
                              "2nd Fire Recovery",
                              "3rd Fire Recovery"))


## Table S9. Model of surface fuel recovery -------------------------------------------

lm1=readRDS(here::here("03-results","fig4_surface_recover_lme1.rds"))
sjPlot::plot_model(lm1,show.values=TRUE,show.p=TRUE)
summary(lm1)

lm2=readRDS(here::here("03-results","fig4_surface_recover_lme2.rds"))
sjPlot::plot_model(lm2,show.values=TRUE,show.p=TRUE)
summary(lm2)

lm3=readRDS(here::here("03-results","fig4_surface_recover_lme3.rds"))
sjPlot::plot_model(lm3,show.values=TRUE,show.p=TRUE)
summary(lm3)

sjPlot::tab_model(lm1,lm2,lm3,
                  show.intercept=TRUE,
                  pred.labels=c( "(Intercept)",
                                 "Treatment:Fire", 
                                 "Pre-treatment surface fuel carbon",
                                 "Disturbance intensity"),
                  dv.labels=c("1st Fire Recovery",
                              "2nd Fire Recovery",
                              "3rd Fire Recovery"))

sjPlot::tab_model(lm2,lm3,
                  show.intercept=TRUE,
                  pred.labels=c( "(Intercept)",
                                 "Treatment:Fire", 
                                 "Pre-treatment surface fuel carbon",
                                 "Disturbance intensity"),
                  dv.labels=c("2nd Fire Recovery",
                              "3rd Fire Recovery"))

# Figure 3. Large tree percentage -----------------------------------------

# Figure 3a
plot_large_percentage=
  readRDS(here::here("01-data", "fig2_largepercentage.rds"))

rm(mean)

fig3=ggplot(data=plot_large_percentage,aes(x=treatment_factor,
                                      y=large_percentage,
                                      fill=timestep_factor))+
  geom_boxplot(width=0.6,alpha=0,show.legend = FALSE,aes(color=timestep_factor))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.3,
                                              dodge.width = 0.6,
                                              jitter.height = 1),
              aes(color=timestep_factor),alpha=0.7,
              size=2.1)+
  stat_summary(fun.y=base::mean, geom="point", shape=18,
               size=2.5,position=position_dodge(0.6),
               show.legend = FALSE, 
               aes(group=timestep_factor)) +
  scale_color_manual(values = c("#8FBC94","#548687"), 
                     name="", 
                     labels=c("2001","2020"),
                     guide=guide_legend(nrow=1))+
  theme_bw()+
  theme(legend.justification = c(0.01,1.01),
        legend.position=c(0.01,1.06),
        legend.box="horizontal",
        legend.title=element_text(size=12),
        legend.text=element_text(size=14),
        legend.background=element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size= 14,color="black"),
        title=element_text(size=14),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank())+
  labs(y="Large live tree carbon percentage (%)",x="Treatment",
       size=16)+
  scale_x_discrete(labels=c("Control","Fire","Mech","Mech+Fire"))+
  guides(fill=FALSE)+
  font_size

#Values in text
plot_large_percentage %>% 
  filter(large_percentage>0) %>% 
  dplyr::group_by(treatment,timestep) %>% 
  dplyr::summarise(N_plot=n(),
                   mean=mean(large_percentage),
                   se=sd(large_percentage)/sqrt(n())) 

plot_large_percentage %>% 
  dplyr::group_by(treatment,timestep) %>% 
  dplyr::summarise(N_plot=n(),
                   mean=mean(large_percentage),
                   se=sd(large_percentage)/sqrt(n())) 

# ggsave(fig3,filename=here::here("05-results","figures","fig3a_wildfirecarbon_T.pdf"),
#        width=6.66,height=3.81,dpi=600,device = "pdf")

# Figure 3b

fit_beta_1=readRDS(here::here("03-results","fig2_largepercentage_beta.rds"))

fit_beta_1$modelInfo

# Note: if plotting the model object leads to warning like “Could not recover model data from environment. ”,
# Just run the model code in Models.R again to get the model object.

fig3b=sjPlot::plot_model(fit_beta_1,pred.type="fe",
                   terms=c("timestep_factorpost_18",
                           "treatment_factorburn:timestep_factorpost_18",
                           "treatment_factormech:timestep_factorpost_18",
                           "treatment_factormechburn:timestep_factorpost_18"),
                   colors=c(#"#d8b365",
                            "#3FB8AFFF"),
                   group.terms = c("Baseline change","Treatment effect",
                                   "Treatment effect","Treatment effect",
                                   "Treatment effect","Treatment effect"),
                     #c(1,1,1,2,2,2,2,1,1,1,1,1,1,1,1),
                   axis.labels=c(rev(c( #"Fire", "Mech", "Mech+Fire",
                                        "Control","Fire","Mech",
                                        "Mech+Fire"))),
                   show.values=TRUE,
                   show.p=FALSE,
                   #show.intercept=TRUE,
                   show.zeroinf = FALSE,
                   value.size=5,value.offset=0.3,
                   vline.color="#636363",
                   title="") +
  theme_bw()+
  #sjPlot::theme_sjplot()+
  #sjPlot::scale_color_sjplot("simply")+
  aes(shape=group)+
  theme(legend.title=element_blank(),
        #legend.position="top",
        legend.justification = c(0.98,0.98),
        legend.position=c(0.99,1),
        legend.text=element_text(size=14),
        legend.background=element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size= 14,color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  labs(y="Odds ratio")+
  font_size

# ggsave(fig3b,filename=here::here("02-data","05-figures","fig3b_wildfirecarbon_lm.pdf"),
#        width=7.04,height=10,dpi=600,device = "pdf")


# Table S10. Model of large tree percentage -------------------------------------------------------
summary(fit_beta_1)
sjPlot::tab_model(fit_beta_1, 
                  show.re.var= TRUE, 
                  auto.label = FALSE,
                  show.intercept = TRUE,
                  # transform=FALSE,
                  pred.labels =c("(Intercept)", 
                                 "Treatment:Fire", "Treatment:Mech", "Treatment:Mech+Fire",
                                 "Treatment:Control x Time:2020",
                                 "Treatment:Fire x Time:2020",
                                 "Treatment:Mech x Time:2020",
                                 "Treatment:Mech+Fire x Time:2020",
                                 "(Intercept)",
                                 "Treatment:Fire", "Treatment:Mech", "Treatment:Mech+Fire",
                                 "Treatment:Control x Time:2020",
                                 "Treatment:Fire x Time:2020",
                                 "Treatment:Mech x Time:2020",
                                 "Treatment:Mech+Fire x Time:2020"),
                  dv.labels= "Response variable: Large live tree carbon percentage")

# Figure 4. Wildfire-resistant carbon -------------------------------------
# Figure 4a

plot_fire_percentage=readRDS(here::here("01-data","fig2a_firepercentage.RDS"))

fig5=ggplot(data=plot_fire_percentage,aes(x=treatment_factor,
                                      y=fire_percentage,
                                      fill=timestep_factor))+
  geom_boxplot(width=0.6,alpha=0,show.legend = FALSE,aes(color=timestep_factor))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.3,
                                              dodge.width = 0.6,
                                              jitter.height = 1),
              aes(color=timestep_factor),alpha=0.7,
              size=2.1)+
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=2.5,position=position_dodge(0.6),
               show.legend = FALSE, 
               aes(group=timestep_factor)) +
  scale_color_manual(values = c("#8FBC94","#548687"), 
                     name="", 
                     labels=c("2001","2020"),
                     guide=guide_legend(nrow=1))+
  theme_bw()+
  theme(legend.justification = c(0.01,1.01),
        legend.position=c(0.00,1.09),
        legend.box="horizontal",
        legend.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank())+
  labs(y="Fire-resistant carbon percentage (%)",x="Treatment",
      size=16)+
  scale_x_discrete(labels=c("Control","Fire","Mech","Mech+Fire"))+
  guides(fill=FALSE)+
  font_size
fig5
# 
# ggsave(fig5,
#        filename=here::here("05-results","figures","fig4a_firepercentage_T.pdf"),
#        width=6.66,height=4,dpi=600,device="pdf")

plot_fire_percentage %>% 
  filter(fire_percentage>0) %>% 
  dplyr::group_by(treatment,timestep) %>% 
  dplyr::summarise(N_plot=n(),
                   mean=mean(fire_percentage),
                   se=sd(fire_percentage)/sqrt(n())) 

plot_fire_percentage %>% 
  dplyr::group_by(treatment,timestep) %>% 
  dplyr::summarise(N_plot=n(),
                   mean=mean(fire_percentage),
                   se=sd(fire_percentage)/sqrt(n())) 

# Figure 4b
dev.off()
fit_beta_2=readRDS(here::here("03-results",
                              "fig3_firetolerantn_beta.rds"))

# Note: if plotting the model object leads to warning like “Could not recover model data from environment. ”,
# Just run the model code in Models.R again to get the model object.

fig6=sjPlot::plot_model(fit_beta_2,
                   terms=c("timestep_factorpost_18",
                           "treatment_factorburn:timestep_factorpost_18",
                           "treatment_factormech:timestep_factorpost_18",
                           "treatment_factormechburn:timestep_factorpost_18"), 
                   colors=c(#"#d8b365",
                     "#3FB8AFFF","#3FB8AFFF"),
                   group.terms = c("Baseline change","Treatment effect",
                                   "Treatment effect","Treatment effect"
                                   #"Treatment effect","Treatment effect"
                                   ),
                   axis.labels=c(rev(c( #"Fire", "Mech", "Mech+Fire",
                                        "Control","Fire","Mech",
                                        "Mech+Fire"))),
                   show.values=TRUE,
                   show.p=FALSE,
                   #show.intercept=TRUE,
                   show.zeroinf = FALSE,
                   value.size=5,value.offset=0.3,
                   dot.size=4,
                   vline.color="#636363",
                   title="") +
  theme_bw()+
  aes(shape=group)+
  theme(legend.title=element_blank(),
        legend.justification = c(0.98,0.98),
        legend.position=c(0.99,1),
        legend.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  labs(y="Odds ratio")+
  font_size

# ggsave(fig6,filename=here::here("05-results","figures","fig4b_firepercentage_lm.pdf"),
#        width=7.04,height=3.75,dpi=600,device = "pdf")


# Table S11. Model of wildfire-resistant carbon ---------------------------

sjPlot::tab_model(fit_beta_2,show.intercept=TRUE,show.re.var= TRUE,
                  show.r2=TRUE,digits=3,
                  show.zeroinf = TRUE,
                  pred.labels=c( "(Intercept)", 
                                 "Treatment:Fire", "Treatment:Mech", "Treatment:Mech+Fire",
                                 "Treatment:Control x Time:2020",
                                 "Treatment:Fire x Time:2020",
                                 "Treatment:Mech x Time:2020",
                                 "Treatment:Mech+Fire x Time:2020",
                                 "(Intercept)"),
                  dv.labels= "Response variable: Species-specific fire resistant carbon percentage"
                  )






