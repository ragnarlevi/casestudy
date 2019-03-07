  
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

  source("Auto_Autonomous_Vehicle_Percentage_Best.R")
  Auto_Exposure_pct_Best$Scenario <- "Autonomous % of Carbia"
  Auto_Exposure_pct_Best$Case <- "Best"
  Base_Exp_pct <- Base
  Base_Exp_pct$Scenario <-  "Autonomous % of Carbia"
  Base_Exp_pct$Case <- "Base"
  

  source("Auto_Autonomous_Vehicle_Percentage_Worst.R")
  Auto_Exposure_pct_Worst$Scenario <- "Autonomous % of Carbia"
  Auto_Exposure_pct_Worst$Case <- "Worst"

  source("Auto_multiplier_best.R")
  Auto_Multiplier_Best$Scenario <- "Multipliers"
  Auto_Multiplier_Best$Case <- "Best"
  Base_mult <- Base
  Base_mult$Scenario <-  "Multipliers"
  Base_mult$Case <- "Base"
  

  source("Auto_multiplier_worst.R")
  Auto_Multiplier_Worst$Scenario <- "Multipliers"
  Auto_Multiplier_Worst$Case <- "Worst"
  
  source("Auto_new_coverge_Best.R")
  Auto_Coverage_Best$Scenario <- "Coverage"
  Auto_Coverage_Best$Case <- "Best"
  Base_Coverage <- Base
  Base_Coverage$Scenario <-  "Coverage"
  Base_Coverage$Case <- "Base"
  

  source("Auto_new_coverge_Worst.R")
  Auto_Coverage_Worst$Scenario <- "Coverage"
  Auto_Coverage_Worst$Case <- "Worst"
  

  source(file = "Auto_safelife_marketshare_Best.R")
  Auto_safelife_ms_Best$Scenario <- "Safelife Marketshare"
  Auto_safelife_ms_Best$Case <- "Best"
  Base_safelife_ms <- Base
  Base_safelife_ms$Scenario <-  "Safelife Marketshare"
  Base_safelife_ms$Case <- "Base"
  
  
  source("Auto_safelife_marketshare_Worst.R")
  Auto_Safelife_ms_Worst$Scenario <- "Safelife Marketshare"
  Auto_Safelife_ms_Worst$Case <- "Worst"
  
  source("Combine_Best.R")
  combine_Best$Scenario <- "Combined"
  combine_Best$Case <- "Best"
  Base_combined <- Base
  Base_combined$Scenario <- "Combined"
  Base_combined$Case <- "Base"
  
  source("Combine_Worst.R")
  combine_Worst$Scenario <- "Combined"
  combine_Worst$Case <- "Worst"
  
  
  
  
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
  
  tmp.history.pv <- history[, c("time", "Case", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]
  tmp.all.pv <- all[, c("time", "Case", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]
  
  
  tmp.history.pv <- cbind(tmp.history.pv[, c("time", "Case", "Scenario")], rowSums(tmp.history.pv[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))
  tmp.all.pv <- cbind(tmp.all.pv[, c("time", "Case", "Scenario")], rowSums(tmp.all.pv[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))
  
  names(tmp.history.pv) <- c("time", "Case", "Scenario", "PurePremium")
  names(tmp.all.pv) <- c("time", "Case", "Scenario", "PurePremium")
  
  
  tmp.all.pv <- aggregate(. ~ time + Scenario+ Case, data = tmp.all.pv, FUN = sum)
  tmp.history.pv <- aggregate(. ~ time + Scenario + Case, data = tmp.history.pv, FUN = sum)
  
  tmp.pv <- rbind(tmp.all.pv, tmp.history.pv)
  
  tmp.all.pv$Scenario <- factor(tmp.all.pv$Scenario,
                             levels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "Combined"),
                             ordered = T)
  tmp.all.pv <- tmp.all.pv[order(tmp.all.pv$Scenario),]
  
  tmp.history.pv$Scenario <- factor(tmp.history.pv$Scenario,
                             levels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "Combined"),
                             ordered = T)
  tmp.history.pv <- tmp.history.pv[order(tmp.history.pv$Scenario),]
  
  
 p <-  ggplot() + 
    geom_ribbon(data = dcast(tmp.all.pv, time + Scenario ~  Case, value.var = "PurePremium"), mapping = aes(x = time, ymin = Worst,  ymax = Best), alpha = 0.8, fill = "#6191B4")+
    geom_line(data = tmp.history.pv, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1 ) +
    geom_line(data = tmp.all.pv, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1) +
    theme_bw(base_size = 16) +
    scale_color_manual(values = c("Base" = "#004680", "Best" = "#4B82A8", "Worst" = "#4B82A8"))+
  theme(plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
        axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
        panel.spacing = unit(2, "lines")) + 
    scale_y_continuous(expand = c(0,0), 
                       name = "Pure Premium in billions", 
                       breaks = seq(0, 2.2, by = 0.2)*10^(9),
                       labels = seq(0, 2.2, by = 0.2),
                       limits = c(0, 2200000000)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year",
                       breaks = seq(2010, 2030, by = 5),
                       labels = seq(2010, 2030, by = 5),
                       limit = c(2009,2030)) +
    ggtitle("Scenarios of discounted pure premiums")+ 
    facet_wrap(. ~ Scenario )
  
  
 tmp.all.pv$facet_fill_color <- c("red", "green", "blue", "yellow", "orange")[tmp.all.pv$Scenario]
  
  
  dummy <- p
  dummy$layers <- NULL
  dummy <- dummy + geom_rect(data=tmp.all.pv, xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
                             aes(fill = facet_fill_color))
  
  library(gtable)
  library(grid)
  library(gridExtra)
  g1 <- ggplotGrob(p)
  g2 <- ggplotGrob(dummy)
  
  gtable_select <- function (x, ...) 
  {
    matches <- c(...)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    x
  }
  
  panels <- grepl(pattern="panel", g2$layout$name)
  strips <- grepl(pattern="strip-right", g2$layout$name)
  g2$grobs[strips] <- replicate(sum(strips), nullGrob(), simplify = FALSE)
  g2$layout$l[panels] <- g2$layout$l[panels] + 1
  g2$layout$r[panels] <- g2$layout$r[panels] + 2
  
  new_strips <- gtable_select(g2, panels | strips)
  grid.newpage()
  grid.draw(new_strips)
  
  gtable_stack <- function(g1, g2){
    g1$grobs <- c(g1$grobs, g2$grobs)
    g1$layout <- rbind(g1$layout, g2$layout)
    g1
  }
  ## ideally you'd remove the old strips, for now they're just covered
  new_plot <- gtable_stack(g1, new_strips)
  grid.newpage()
  grid.draw(new_plot)
    
  
  
  tmp.history <- history[, c("time", "Case", "Scenario", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "AC_MR", "AC_CR", "AC_IS")]
  tmp.all <- all[, c("time", "Case", "Scenario", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "AC_MR", "AC_CR", "AC_IS")]
  
  
  tmp.history <- cbind(tmp.history[, c("time", "Case", "Scenario")], rowSums(tmp.history[, c("AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "AC_MR", "AC_CR", "AC_IS")]))
  tmp.all <- cbind(tmp.all[, c("time", "Case", "Scenario")], rowSums(tmp.all[, c("AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "AC_MR", "AC_CR", "AC_IS")]))
  
  names(tmp.history) <- c("time", "Case", "Scenario", "PurePremium")
  names(tmp.all) <- c("time", "Case", "Scenario", "PurePremium")
  
  
  tmp.all <- aggregate(. ~ time + Scenario+ Case, data = tmp.all, FUN = sum)
  tmp.history <- aggregate(. ~ time + Scenario + Case, data = tmp.history, FUN = sum)
  
  tmp <- rbind(tmp.all, tmp.history)
  
  tmp.all$Scenario <- factor(tmp.all$Scenario,
                             levels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "Combined"),
                             ordered = T)
  tmp.all <- tmp.all[order(tmp.all$Scenario),]
  
  tmp.history$Scenario <- factor(tmp.history$Scenario,
                                 levels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "Combined"),
                                 ordered = T)
  tmp.history <- tmp.history[order(tmp.history$Scenario),]
  
  
  ggplot() + 
    geom_ribbon(data = dcast(tmp.all, time + Scenario ~  Case, value.var="PurePremium"), mapping = aes(x = time, ymin = Worst,  ymax = Best), alpha = 0.8, fill = "#6191B4")+
    geom_line(data = tmp.history, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1 ) +
    geom_line(data = tmp.all, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1) +
    theme_bw(base_size = 16) +
    scale_color_manual(values = c("Base" = "#004680", "Best" = "#4B82A8", "Worst" = "#4B82A8"))+
    theme(plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(2, "lines")) + 
    scale_y_continuous(expand = c(0,0), 
                       name = "Pure Premium in billions", 
                       breaks = seq(0, 2.8, by = 0.2)*10^(9),
                       labels = seq(0, 2.8, by = 0.2),
                       limits = c(0, 2800000000)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year",
                       breaks = seq(2010, 2030, by = 5),
                       labels = seq(2010, 2030, by = 5),
                       limit = c(2009,2030)) +
    ggtitle("Scenarios of pure premiums")+ 
    facet_wrap(. ~ Scenario )
  

  
  # Define autonomy 
  # 
  # tmp.all.aut <- all[all$Autonomy != "A0", c("time", "Case", "Scenario","Autonomy", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]
  # 
  # tmp.all.aut$Autonomy <- "A1 and A2"
  # 
  # tmp.all.aut <- cbind(tmp.all.aut[, c("time", "Case", "Scenario", "Autonomy")], rowSums(tmp.all.aut[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))
  # 
  # names(tmp.all.aut) <- c("time", "Case", "Scenario", "Autonomy", "PurePremium")
  # 
  # tmp.all.aut <- aggregate(. ~ time + Scenario + Case+ Autonomy, data = tmp.all.aut, FUN = sum)
  # 
  # 
  # tmp.all.aut$Scenario <- factor(tmp.all.aut$Scenario,
  #                            levels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "Combined"),
  #                            ordered = T)
  # tmp.all.aut <- tmp.all.aut[order(tmp.all.aut$Scenario),]
  # 
  # ggplot() + 
  #   geom_ribbon(data = dcast(tmp.all, time + Scenario ~  Case, value.var="PurePremium"), mapping = aes(x = time, ymin = Worst,  ymax = Best), alpha = 0.8, fill = "#6191B4")+
  #   geom_line(data = tmp.history, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1 ) +
  #   geom_line(data = tmp.all, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1) +
  #   theme_bw(base_size = 16) +
  #   scale_color_manual(values = c("Base" = "#004680", "Best" = "#4B82A8", "Worst" = "#4B82A8"))+
  #   theme(plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
  #         axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
  #         axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
  #         panel.spacing = unit(2, "lines")) + 
  #   scale_y_continuous(expand = c(0,0), 
  #                      name = "Pure Premium in billions", 
  #                      breaks = seq(0, 2.2, by = 0.2)*10^(9),
  #                      labels = seq(0, 2.2, by = 0.2),
  #                      limits = c(0, 2200000000)) +
  #   scale_x_continuous(expand = c(0,0), 
  #                      name = "Year",
  #                      breaks = seq(2010, 2030, by = 5),
  #                      labels = seq(2010, 2030, by = 5),
  #                      limit = c(2009,2030)) +
  #   ggtitle("Claim frequency")+ 
  #   facet_wrap(. ~ Scenario ) +
  #   geom_ribbon(data = dcast(tmp.all.aut, time + Scenario ~  Case, value.var="PurePremium"), mapping = aes(x = time, ymin = Worst,  ymax = Best), alpha = 0.8, fill = "#6191B4")+
  #   geom_line(data = tmp.all.aut, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1) 
  #   
  # 