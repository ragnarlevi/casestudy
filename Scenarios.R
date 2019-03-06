
library(readxl) # read exlce
library(dplyr) # For table manipulations
library(plyr) # For table manipulations
library(reshape2) # For melt function to chane data frames to parse into ggplot functions
library(ggplot2) # For Nice plots



# General functions call
source("functions.R")
# GLM - parameters

# Call the GLM.R script, estimates number and AAC for current automoblies. Also
source("GLM.R")



time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_Autonomous_Vehicle_Percentage_Best.R")
Auto_Exposure_pct_Best$Scenario <- "Autonomous % Best shock"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_Autonomous_Vehicle_Percentage_Worst.R")
Auto_Exposure_pct_Worst$Scenario <- "Autonomous % Worst shock"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_multiplier_best.R")
Auto_Multiplier_Best$Scenario <- "Multipliers Best"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_multiplier_worst.R")
Auto_Multiplier_Worst$Scenario <- "Multipliers Worst"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_new_coverge_Best.R")
Auto_Coverage_Best$Scenario <- "Coverage Best"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_new_coverge_Worst.R")
Auto_Coverage_Worst$Scenario <- "Coverage Worst"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source(file = "Auto_safelife_marketshare_Best.R")
Auto_safelife_ms_Best$Scenario <- "Safelife Marketshare Best"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_safelife_marketshare_Worst.R")
Auto_Safelife_ms_Worst$Scenario <- "Safelife Marketshare Worst"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Base.R")
Base$Scenario <- "Base"

max(tmp.all$time[tmp.all$Scenario == "Safelife Marketshare Worst"])

history <- Base[Base$time <= 2019, ]

all <- rbind(Auto_Coverage_Best[Auto_Coverage_Best$time >= 2019, ],
             Auto_Coverage_Worst[Auto_Coverage_Worst$time >= 2019, ],
             Auto_Exposure_pct_Best[Auto_Exposure_pct_Best$time >= 2019, ],
             Auto_Exposure_pct_Worst[Auto_Exposure_pct_Worst$time >= 2019, ],
             Auto_Multiplier_Best[Auto_Multiplier_Best$time >= 2019, ],
             Auto_Multiplier_Worst[Auto_Multiplier_Worst$time >= 2019, ],
             Auto_safelife_ms_Best[Auto_safelife_ms_Best$time >= 2019,],
             Auto_Safelife_ms_Worst[Auto_Safelife_ms_Worst$time >= 2019,],
             Base[Base$time >= 2019,])

tmp.history <- history[, c("time", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]
tmp.all <- all[, c("time", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]


tmp.history <- cbind(tmp.history[, c("time", "Scenario")], rowSums(tmp.history[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))
tmp.all <- cbind(tmp.all[, c("time", "Scenario")], rowSums(tmp.all[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))

names(tmp.history) <- c("time", "Scenario", "Pure Premium")
names(tmp.all) <- c("time", "Scenario", "Pure Premium")


tmp.all <- aggregate(. ~ time + Scenario, data = tmp.all, FUN = sum)
tmp.history <- aggregate(. ~ time + Scenario, data = tmp.history, FUN = sum)


ggplot() + 
  geom_line(data = tmp.history, mapping = aes(x = time, y = `Pure Premium`, color = Scenario)) +
  geom_line(data = tmp.all, mapping = aes(x = time, y = `Pure Premium`, color = Scenario)) +
  theme_bw() 
  



