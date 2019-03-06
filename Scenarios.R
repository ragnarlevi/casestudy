
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
source("Base.R")


time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_Autonomous_Vehicle_Percentage_Best.R")
Auto_Exposure_pct_Best$Scenario <- "Autonomous % of Carbia"
Auto_Exposure_pct_Best$Case <- "Best"
Base_Exp_pct <- Base
Base_Exp_pct$Scenario <-  "Autonomous % of Carbia"
Base_Exp_pct$Case <- "Base"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_Autonomous_Vehicle_Percentage_Worst.R")
Auto_Exposure_pct_Worst$Scenario <- "Autonomous % of Carbia"
Auto_Exposure_pct_Worst$Case <- "Worst"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_multiplier_best.R")
Auto_Multiplier_Best$Scenario <- "Multipliers"
Auto_Multiplier_Best$Case <- "Best"
Base_mult <- Base
Base_mult$Scenario <-  "Multipliers"
Base_mult$Case <- "Base"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_multiplier_worst.R")
Auto_Multiplier_Worst$Scenario <- "Multipliers"
Auto_Multiplier_Worst$Case <- "Worst"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_new_coverge_Best.R")
Auto_Coverage_Best$Scenario <- "Coverage"
Auto_Coverage_Best$Case <- "Best"
Base_Coverage <- Base
Base_Coverage$Scenario <-  "Coverage"
Base_Coverage$Case <- "Base"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_new_coverge_Worst.R")
Auto_Coverage_Worst$Scenario <- "Coverage"
Auto_Coverage_Worst$Case <- "Worst"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source(file = "Auto_safelife_marketshare_Best.R")
Auto_safelife_ms_Best$Scenario <- "Safelife Marketshare"
Auto_safelife_ms_Best$Case <- "Best"
Base_safelife_ms <- Base
Base_safelife_ms$Scenario <-  "Safelife Marketshare"
Base_safelife_ms$Case <- "Base"

time.frame <- 0:11
start.year <- 2019
t <- seq(min(time.frame), max(time.frame), by = 0.25)
source("Auto_safelife_marketshare_Worst.R")
Auto_Safelife_ms_Worst$Scenario <- "Safelife Marketshare"
Auto_Safelife_ms_Worst$Case <- "Worst"



history <- rbind(Base_safelife_ms[Base_safelife_ms$time <= 2019, ],
                 Base_Coverage[Base_Coverage$time <= 2019, ],
                 Base_Exp_pct[Base_Exp_pct$time <= 2019, ],
                 Base_mult[Base_mult$time <= 2019, ])

all <- rbind(Auto_Coverage_Best[Auto_Coverage_Best$time >= 2019, ],
             Auto_Coverage_Worst[Auto_Coverage_Worst$time >= 2019, ],
             Auto_Exposure_pct_Best[Auto_Exposure_pct_Best$time >= 2019, ],
             Auto_Exposure_pct_Worst[Auto_Exposure_pct_Worst$time >= 2019, ],
             Auto_Multiplier_Best[Auto_Multiplier_Best$time >= 2019, ],
             Auto_Multiplier_Worst[Auto_Multiplier_Worst$time >= 2019, ],
             Auto_safelife_ms_Best[Auto_safelife_ms_Best$time >= 2019,],
             Auto_Safelife_ms_Worst[Auto_Safelife_ms_Worst$time >= 2019,],
             Base_safelife_ms[Base_safelife_ms$time >= 2019, ],
             Base_Coverage[Base_Coverage$time >= 2019, ],
             Base_Exp_pct[Base_Exp_pct$time >= 2019, ],
             Base_mult[Base_mult$time >= 2019, ])

tmp.history <- history[, c("time", "Case", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]
tmp.all <- all[, c("time", "Case", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]


tmp.history <- cbind(tmp.history[, c("time", "Case", "Scenario")], rowSums(tmp.history[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))
tmp.all <- cbind(tmp.all[, c("time", "Case", "Scenario")], rowSums(tmp.all[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))

names(tmp.history) <- c("time", "Case", "Scenario", "Pure Premium")
names(tmp.all) <- c("time", "Case", "Scenario", "Pure Premium")


tmp.all <- aggregate(. ~ time + Scenario+ Case, data = tmp.all, FUN = sum)
tmp.history <- aggregate(. ~ time + Scenario + Case, data = tmp.history, FUN = sum)


ggplot() + 
  geom_line(data = tmp.history, mapping = aes(x = time, y = `Pure Premium`, color = Case)) +
  geom_line(data = tmp.all, mapping = aes(x = time, y = `Pure Premium`, color = Case)) +
  theme_bw()  + 
  facet_wrap(. ~ Scenario)
  



