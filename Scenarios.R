  
  library(readxl) # read exel
  library(dplyr) # For table/df manipulations
  library(plyr) # For table/df manipulations
  library(reshape2) # For melt function to change data frames to parse into ggplot functions
  library(ggplot2) # For Nice plots 
  library(tidyr) # spread function
  library(latex2exp) # to use latex in plots
  library(xlsx) # used to write to excel
  
  
  # General functions call
  source("functions.R")
  # GLM - parameters
  
  # Call the GLM.R script, estimates number and AAC for current automoblies. Also
  source("GLM.R")
  
  time.frame <- 0:11
  start.year <- 2019
  t <- seq(min(time.frame), max(time.frame), by = 0.25)
  sl.enter.year = 2020; sl.enter.qtr = 1
  
  # Base is the base case
  source("Base.R")
  # we want to plot the Base Case, Base is the name of the data frame wit all the info, A folder caleld graph is needed in the
  # working directory!
  # View(Base[Base$Exposure == 0, c("time", "Exposure", "Autonomy")])
  plot.main(predict.df = Base)
  
  # Exposure percentage, of autonomy in afater 2030
  end.year <- max(Base$time)
  sum(Base$Exposure[Base$Autonomy != "A0" & Base$time == end.year])/sum(Base$Exposure[Base$time == end.year])
  
  
  # We want to safe the Scenarios so we can plot them later. The following is not the best way but we don't want to make to many changes
  # to the code at this stage so we do it the brute force way. The following Scripts (and the Base.R) create data frames with the shock
  # info. We will keep the shocks in data frames and we make a column specifying the shok
  
  # Create the shock data frames. 
  
  # safelife marketshare data frame
  safelife.ms <- safelife.market.share
  safelife.ms$Case <- "Base"
  
  # Carbia Autonomous vehicle percentage of Exposure for personal
  carb.p.exp.pct <- carb.personal.pct
  carb.p.exp.pct$Case <- "Base"
  
  
  # Carbia Autonomous vehicle percentage of Exposure for commercial
  carb.c.exp.pct <- carb.commercial.pct
  carb.c.exp.pct$Case <- "Base"
  
  
  
  
  
  
  source("Auto_Autonomous_Vehicle_Percentage_Best.R")
  Auto_Exposure_pct_Best$Scenario <- "Autonomous % of Carbia"
  Auto_Exposure_pct_Best$Case <- "Upward"
  Base_Exp_pct <- Base
  Base_Exp_pct$Scenario <-  "Autonomous % of Carbia"
  Base_Exp_pct$Case <- "Base"
  
  # Here we have the upward shock for Autonomous vehicle percentage of Exposure
  carb.commercial.pct$Case <- "Upward"
  carb.c.exp.pct <- rbind(carb.c.exp.pct, carb.commercial.pct)
  carb.personal.pct$Case <- "Upward"
  carb.p.exp.pct <- rbind(carb.p.exp.pct, carb.personal.pct)
  
  # Exposure percentage, of autonomy in end.year
  tmp <- Auto_Exposure_pct_Best
  sum(tmp$Exposure[tmp$Autonomy != "A0" & tmp$time == end.year])/sum(tmp$Exposure[tmp$time == end.year])

  source("Auto_Autonomous_Vehicle_Percentage_Worst.R")
  Auto_Exposure_pct_Worst$Scenario <- "Autonomous % of Carbia"
  Auto_Exposure_pct_Worst$Case <- "Downward"
  
  # Here we have the Downward shock for Autonomous vehicle percentage of Exposure
  carb.commercial.pct$Case <- "Downward"
  carb.c.exp.pct <- rbind(carb.c.exp.pct, carb.commercial.pct)
  carb.personal.pct$Case <- "Downward"
  carb.p.exp.pct <- rbind(carb.p.exp.pct, carb.personal.pct)

  # Exposure percentage, of autonomy in end.year
  tmp <- Auto_Exposure_pct_Worst
  sum(tmp$Exposure[tmp$Autonomy != "A0" & tmp$time == end.year])/sum(tmp$Exposure[tmp$time == end.year])
  
  
  source("Auto_multiplier_best.R")
  Auto_Multiplier_Best$Scenario <- "Frequency and amount"
  Auto_Multiplier_Best$Case <- "Upward"
  Base_mult <- Base
  Base_mult$Scenario <- "Frequency and amount"
  Base_mult$Case <- "Base"
  

  source("Auto_multiplier_worst.R")
  Auto_Multiplier_Worst$Scenario <- "Frequency and amount"
  Auto_Multiplier_Worst$Case <- "Downward"
  
  source("Auto_new_coverge_Best.R")
  Auto_Coverage_Best$Scenario <- "Coverage"
  Auto_Coverage_Best$Case <- "Upward"
  Base_Coverage <- Base
  Base_Coverage$Scenario <-  "Coverage"
  Base_Coverage$Case <- "Base"
  

  source("Auto_new_coverge_Worst.R")
  Auto_Coverage_Worst$Scenario <- "Coverage"
  Auto_Coverage_Worst$Case <- "Downward"
  

  source(file = "Auto_safelife_marketshare_Best.R")
  Auto_safelife_ms_Best$Scenario <- "Safelife Marketshare"
  Auto_safelife_ms_Best$Case <- "Upward"
  Base_safelife_ms <- Base
  Base_safelife_ms$Scenario <-  "Safelife Marketshare"
  Base_safelife_ms$Case <- "Base"
  
  # Here we have the upward shock for marketshare
  safelife.market.share$Case <- "Upward"
  safelife.ms <- rbind(safelife.ms, safelife.market.share)

  tmp <- Auto_safelife_ms_Best
  sum(tmp$Exposure[tmp$Autonomy != "A0" & tmp$time == end.year])/sum(tmp$Exposure[tmp$time == end.year])
  
  
  source("Auto_safelife_marketshare_Worst.R")
  Auto_Safelife_ms_Worst$Scenario <- "Safelife Marketshare"
  Auto_Safelife_ms_Worst$Case <- "Downward"
  # Here we have the upward shock for marketshare
  safelife.market.share$Case <- "Downward"
  safelife.ms <- rbind(safelife.ms, safelife.market.share)
  
  tmp <- Auto_Safelife_ms_Worst
  sum(tmp$Exposure[tmp$Autonomy != "A0" & tmp$time == end.year])/sum(tmp$Exposure[tmp$time == end.year])
  
  
  source("Combine_Best.R")
  combine_Best$Scenario <- "Combined"
  combine_Best$Case <- "Upward"
  Base_combined <- Base
  Base_combined$Scenario <- "Combined"
  Base_combined$Case <- "Base"
  
  source("Combine_Worst.R")
  combine_Worst$Scenario <- "Combined"
  combine_Worst$Case <- "Downward"
  
  
  
  
  history <- rbind(Base_safelife_ms[Base_safelife_ms$time <= 2019, ],
                   Base_Coverage[Base_Coverage$time <= 2019, ],
                   Base_Exp_pct[Base_Exp_pct$time <= 2019, ],
                   Base_mult[Base_mult$time <= 2019, ],
                   Base_combined[Base_combined$time <= 2019, ])
  
  all <- rbind(Auto_Coverage_Best[Auto_Coverage_Best$time >= 2019, ],
               Auto_Coverage_Worst[Auto_Coverage_Worst$time >= 2019, ],
               Auto_Exposure_pct_Best[Auto_Exposure_pct_Best$time >= 2019, ],
               Auto_Exposure_pct_Worst[Auto_Exposure_pct_Worst$time >= 2019, ],
               Auto_Multiplier_Best[Auto_Multiplier_Best$time >= 2019, ],
               Auto_Multiplier_Worst[Auto_Multiplier_Worst$time >= 2019, ],
               Auto_safelife_ms_Best[Auto_safelife_ms_Best$time >= 2019,],
               Auto_Safelife_ms_Worst[Auto_Safelife_ms_Worst$time >= 2019,],
               combine_Best[combine_Best$time >= 2019, ],
               combine_Worst[combine_Worst$time >= 2019, ],
               Base_safelife_ms[Base_safelife_ms$time >= 2019, ],
               Base_Coverage[Base_Coverage$time >= 2019, ],
               Base_Exp_pct[Base_Exp_pct$time >= 2019, ],
               Base_mult[Base_mult$time >= 2019, ],
               Base_combined[Base_combined$time >= 2019, ])
  
  
  plot.scenarios(all = all, 
                 history = history, 
                 safelife.ms = safelife.ms, 
                 carb.c.exp.pct = carb.c.exp.pct, 
                 carb.p.exp.pct = carb.p.exp.pct, 
                 A2.hit.time = 3)
  
  
  
  
  # Write premiums
  
  premiums <- Base[, c("RiskClass", "Type", "Autonomy", "BI_pv_prem", "PD_pv_prem", "COM_pv_prem", "COL_pv_prem", "PI_pv_prem", "MR_pv_prem", "CR_pv_prem", "IS_pv_prem")]
  # take the mean
  
  premiums <- aggregate(. ~ RiskClass + Type + Autonomy, data = premiums, FUN = mean)
  
  write.xlsx(x = premiums, file = "premiums.xlsx", sheetName = "Sheet1", 
             col.names = TRUE, row.names = F, append = FALSE)
  
  
  
  # Long term
  
  time.frame <- 0:21
  start.year <- 2019
  t <- seq(min(time.frame), max(time.frame), by = 0.25)
  
  # Base is the base case
  source("Base.R")
  
  
  # Then plot long run
  tmp <- Base[, names(Base) %in% c("time", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  tmp <- aggregate(. ~ time , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time"))
  
  
  totalclaim.longrun <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5))) + 
    labs(fill = "Coverage")+
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IR"))
 # totalclaim.longrun
  ggsave("graphs/total_claim_longrun.png", device = "png",plot = totalclaim.longrun, width = 60, height = 20, units = "cm")
  
  
  
  
    