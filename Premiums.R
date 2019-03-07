
library(readxl) # read exlce
library(dplyr) # For table manipulations
library(plyr) # For table manipulations
library(reshape2) # For melt function to chane data frames to parse into ggplot functions
library(ggplot2) # For Nice plots
library(tidyr) # spread function


# General functions call
source("functions.R")
# GLM - parameters

# Call the GLM.R script, estimates number and AAC for current automoblies. Also
source("GLM.R")

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)

source("Base.R")

Base$BI_pv_prem <- Base$AC_BI_PV/Base$Exposure
Base$PD_pv_prem <- Base$AC_PD_PV/Base$Exposure
Base$COM_pv_prem <- Base$AC_COM_PV/Base$Exposure
Base$COL_pv_prem <- Base$AC_COL_PV/Base$Exposure
Base$PI_pv_prem <- Base$AC_PI_PV/Base$Exposure
Base$MR_pv_prem <- Base$AC_MR_PV/Base$Exposure
Base$CR_pv_prem <- Base$AC_CR_PV/Base$Exposure
Base$IS_pv_prem <- Base$AC_IS_PV/Base$Exposure

premiums.t.pv <- Base[, c("time", "RiskClass", "Type", "Autonomy", 
                          "BI_pv_prem","PD_pv_prem", "COM_pv_prem", "COL_pv_prem",
                          "PI_pv_prem", "MR_pv_prem", "CR_pv_prem", "IS_pv_prem")]

premiums.t.pv <- aggregate(. ~ time + RiskClass + Type + Autonomy, data = premiums.t.pv, FUN = sum )

tmp <- premiums.t.pv[, !(names(premiums.t.pv) %in% c( "RiskClass", "Type"))]
tmp <- aggregate(. ~ time + Autonomy, data = tmp, FUN = mean)
tmp.melt <- melt(tmp, id.vars = c("time", "Autonomy"))

library(latex2exp)
ggplot() + 
  geom_line(data = tmp.melt, mapping = aes(x = time, y = value, color = variable), size = 1.1) +
  facet_wrap(. ~ Autonomy, scales = "fixed")+
  theme_bw(base_size = 16) +
  scale_color_manual(values = c("BI_pv_prem" = "#D9E1E2", "PD_pv_prem" = "#BBDDE6"  ,"COM_pv_prem" = "#71B2C9", "COL_pv_prem" = "#4E87A0","PI_pv_prem" = "#072B31", "MR_pv_prem" = "#D45D00", "CR_pv_prem" = "#FDBE87", "IS_pv_prem" = "#F68D2E"),
                    labels = c("BI_pv_prem" = "BI", "PD_pv_prem" = "PD","COM_pv_prem" =  "COM", "COL_pv_prem" =  "COL","PI_pv_prem" =  "PI","MR_pv_prem" =  "MR","CR_pv_prem" =  "CR","IS_pv_prem" =  "IS"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)), 
      #  legend.position = c(0.85, 0.2) ,
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(0.3,0.8,0.3,1), "cm")) + 
  scale_y_continuous(expand = c(0,0),
                     name = TeX("Premiums in,  $\\hat{C}$"),
                     breaks = seq(0, 8, by = 1)*100,
                     labels = seq(0, 8, by = 1)*100,
                     limits = c(0, 800)) +
  scale_x_continuous(expand = c(0,0), 
                     name = "Year",
                     breaks = seq(2010, 2030, by = 5),
                     labels = seq(2010, 2030, by = 5),
                     limit = c(2009,2030.25)) +
  ggtitle("Average Discounted Premiums for Risk Classes and Types")

#rm(list=ls())





