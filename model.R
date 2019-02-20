
# Libraries ----
library(readxl) # read exlce
library(dplyr) # For table manipulations
library(reshape2) # For melt function to chane data frames to parse into ggplot functions
library(ggplot2) # For Nice plots
library(deSolve)  # To solve differential equations
library(svDialogs) # to prompt  user
# Promp user to set wd
user <- dlgInput("Who is using?", Sys.info()["user"])$res

switch(EXPR = user, 
       ragna = setwd("C:/Users/ragna/OneDrive/Documents/ISEG/Actuarial Science Courses @ ISEG/case study"))



# Define the time frame
time.frame <- 0:20

# General functions call
source("functions.R")
# GLM - parameters

# Call the GLM.R script, estimates number and AAC for current automoblies. Also
source("GLM.R")


# Premiums for each autonomu class
source("autonomyClaimAmounts.R")
# estimates for A0 from the glm model
estimates.A0 <- A0initialParam(glm.list = glm, refine = "rd") # See functions.R
claims.estimates <- autonomyPremium(estimates.A0 = estimates.A0, type = "same", time.frame = time.frame, start.year = 2019)
  
# Proportions of Risk Classes within autonomy classes
source("RiskClassProp.R")
df.main <- autocar
df.main$Autonomy <- "A0"
df.main$AutonomyProp <- 1
df.main$prop <- 1
prop <- riskClassProp(type = "same", df.main, exposure.year = 2018, time.frame = time.frame, start.year = 2019)
# sum(prop$prop[prop$Type == "Personal" & prop$Qtr == 1 & prop$Autonomy == "A0"])

# Autonomy Proportions
source("autonomyProportions.R")
# initial value

# Personal autonomy development
A0.personal <- sum(df.main$Exposure[df.main$Year == 2018 & df.main$Qtr == 4 & df.main$Type == "Personal"])
# initial vector has to have the right names
init.personal <- c(A0 = A0.personal , A1 = 0, A2 = 0, A3 = 0, A4 = 0, A5 = 0)

autonomyChange <- list()
autonomyChange$Personal <- autonomyRate(time.frame = time.frame, func = func.personal, param = param.personal, init = init.personal, delay = delay.personal)
#autonomyChange$Personal$plot

# Personal autonomy development
A0.commercial <- sum(df.main$Exposure[df.main$Year == 2018 & df.main$Qtr == 4 & df.main$Type == "Commercial"])
# initial vector has to have the right names
init.commercial <- c(A0 = A0.commercial , A1 = 0, A2 = 0, A3 = 0, A4 = 0, A5 = 0)

autonomyChange$Commercial <- autonomyRate(time.frame = time.frame, func = func.commercial, param = param.commercial, init = init.commercial, delay = delay.commercial)
#autonomyChange$Commercial$plot

# Now we basically have everythin we can put it into one data frame

# we need to make sure the data types are the same within every data frame when we join them
claims.estimates$Qtr <- as.numeric(claims.estimates$Qtr)
claims.estimates$RiskClass <- as.character(claims.estimates$RiskClass)
prop$Qtr <- as.numeric(prop$Qtr)

predict.df <- full_join(claims.estimates, prop, by = c("Qtr","RiskClass", "Type", "Autonomy", "Year"))



  

#sum(is.na(predict.df))

# Then add the autonomy exposure. Like the data frame is set up It has to be done with loop/lapply, unfortunately
# Start by initalizing columns
# t is what we will loop over, by = 0.25 because the 0.25 is quarter, we actually need to start at time 0.25 which is year 2019 Qtr 1
tmp <- autonomyChange$Personal$out$time

predict.df$Exposure <- 0

for(i in 2:length(tmp)){

  time <- tmp[i]
  
  year <- 2018 + ceiling(time)
  qtr <- (time * 4) %% 4
  if(qtr == 0){ qtr <-  4}
 # print(paste("Year ", year, " ;Qtr ", qtr))
  
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Personal" & predict.df$Autonomy == "A0"] <- autonomyChange$Personal$out$A0[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Personal" & predict.df$Autonomy == "A1"] <- autonomyChange$Personal$out$A1[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Personal" & predict.df$Autonomy == "A2"] <- autonomyChange$Personal$out$A2[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Personal" & predict.df$Autonomy == "A3"] <- autonomyChange$Personal$out$A3[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Personal" & predict.df$Autonomy == "A4"] <- autonomyChange$Personal$out$A4[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Personal" & predict.df$Autonomy == "A5"] <- autonomyChange$Personal$out$A5[i]
  
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Commercial" & predict.df$Autonomy == "A0"] <- autonomyChange$Commercial$out$A0[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Commercial" & predict.df$Autonomy == "A1"] <- autonomyChange$Commercial$out$A1[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Commercial" & predict.df$Autonomy == "A2"] <- autonomyChange$Commercial$out$A2[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Commercial" & predict.df$Autonomy == "A3"] <- autonomyChange$Commercial$out$A3[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Commercial" & predict.df$Autonomy == "A4"] <- autonomyChange$Commercial$out$A4[i]
  predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == "Commercial" & predict.df$Autonomy == "A5"] <- autonomyChange$Commercial$out$A5[i]
  
}

# We need to adjust the exposure by the prop column
predict.df$Exposure <- predict.df$Exposure*predict.df$prop



# #----- Estimate Number and AAC -
# methodForEstimation <- "multiply"
# glm.models <- glm$rd

# predict.df <- aut.prem(glm.list = glm.models, type = methodForEstimation, predict.df = predict.df)


# ----- Make the original data ready to rbind() with theprediction data frame -----
# We want to bind the known data together with the new, but we need to make sure their columns are the same
names(predict.df)

df.main <- autocar[, names(autocar) %in% c("Year", "Qtr", "RiskClass", "Type", "Exposure", "NC_BI",
                                            "NC_PD", "NC_COM", "NC_COL", "NC_PI", "AAC_BI", "AAC_PD",
                                            "AAC_COM", "AAC_COL", "AAC_PI")]
# Find the proportion
df.main <- findProportions(df.main = df.main)

df.main$Autonomy <- "A0"

# rearrange so they have the same data types
df.main <- df.main[,names(predict.df)]
df.main$Qtr <- as.numeric(df.main$Qtr)

# Finally combine the data.frames

predict.df <- rbind(df.main, predict.df)

# add time

predict.df$time <- predict.df$Year + 2.5*as.numeric(predict.df$Qtr)/10





# Plot ----
# Autonomy exposure evolution plot
plots <- list()
tmp <- predict.df[, names(predict.df) %in% c("time", "Exposure", "Autonomy")]
tmp <- aggregate(formula = . ~ time + Autonomy, data = tmp, FUN = sum)
plots$Autonomy.evolution <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure, color = Autonomy))


plots$Autonomy.evolution


time.frame <- 0:20
lvls <- c("A0","A1", "A2")




# Define exposure growth

exp.growth.personal <- list(
  A0 = function(t){
    500*exp(-0.1*(t-7.5)^2)
  },
  A1 = function(t){
    500*exp(-0.1*(t-7.5)^2)
  },
  A2 = function(t){
    500*exp(-0.1*(t-7.5)^2)
  }
)

func.personal <- function(time, state, parameters, exp.growth) {
  
  with(as.list(c(state, parameters)), {
    
    # change = Flow In - FlowOut
    dA0 <- -A0toA1(time)*A0 - A0toA2(time)*A0 + exp.growth$A0(time)
    dA1 <- A0toA1(time)*A0 - A1toA2(time)*A1 + exp.growth$A1(time)
    dA2 <- A0toA2(time)*A0 + A1toA2(time)*A1 + exp.growth$A2(time)
    
    return(list(c(dA0, dA1, dA2)))
  })
}

param.personal <- list(
  A0toA1 = function(t){
    0.07*exp(-t)
  },
  A0toA2 = function(t){
    0.07*exp(-t)
  },
  A1toA2 = function(t){
    #plot(t, 0.001*(t-7.5)^2)
    1-0.01*(t-7.5)^2
  }
)

exp.growth.commercial <- exp.growth.personal
func.commercial <- func.personal
param.commercial <- param.personal


safeline.prop.aut <- list(Personal = data.frame(time = seq(min(time.frame), max(time.frame) + 1, by = 0.25),
                                                A0 = rep(0.34, length(time.frame)*4 +1 ),
                                                A1 = rep(1, length(time.frame)*4+1),
                                                A2 = rep(1, length(time.frame)*4+1)),
                        Commercial = data.frame(time = seq(min(time.frame), max(time.frame) + 1, by = 0.25),
                                                A0 = rep(0.34, length(time.frame)*4+1),
                                                A1 = rep(1, length(time.frame)*4+1),
                                                A2 = rep(1, length(time.frame)*4+1)))


