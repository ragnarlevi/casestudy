#---- Most recent exposure the same

growth.naive <- function(Year = NULL, propAutonomy = NULL, propRC = NULL, Type = NULL, RiskClass = NULL, Coverage = NULL, acRate =NULL, nRate = NULL, df.main = NULL, Qtr = NULL){
  # t
  
  return(df.main$Exposure[df.main$Qtr == Qtr & df.main$RiskClass == RiskClass & df.main$Type == Type & df.main$Year == Year])
}
  

# ----- Exposure Growth

CurrentTimeData <- df.main[df.main$Year == max(df.main$Year),]
totalExposureQtr1 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 1])
totalExposureQtr2 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 2])
totalExposureQtr3 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 3])
totalExposureQtr4 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 4])
currentYear <- max(df.main$Year)

# First we define cases

# Type
df.1 <- data.frame(Type = unique(df.main$Type))

# Risk Classes
df.2 <- data.frame(RiskClass = unique(df.main$RiskClass))
# quarters
df.3 <- data.frame(Qtr = c(1,2,3,4))
# Autonomy
df.4 <- data.frame(Autonomy = c("A0", "A1", "A2", "A3", "A4", "A5"))

cases <- merge(df.1, df.2)
cases <- merge(cases, df.3)
cases <- merge(cases, df.4)


# Find initial proportions of Risk Classes
prop <- CurrentTimeData[, names(CurrentTimeData) %in% c("Qtr", "Exposure", "RiskClass", "Type", "Autonomy")]
prop$prop <- 0
prop$prop[prop$Qtr == 1] <- prop$Exposure[prop$Qtr == 1]/totalExposureQtr1
prop$prop[prop$Qtr == 2] <- prop$Exposure[prop$Qtr == 2]/totalExposureQtr2
prop$prop[prop$Qtr == 3] <- prop$Exposure[prop$Qtr == 3]/totalExposureQtr3
prop$prop[prop$Qtr == 4] <- prop$Exposure[prop$Qtr == 4]/totalExposureQtr4

# finally we reomve the Exposure Column and add year
prop <- prop[, !(names(prop) %in% c("Exposure"))]
# check if sum is 1
sum(prop$prop[prop$Qtr == 4])

prop$AutonomyProp <- 1

tmp.1 <- prop
for( i in c("A1", "A2", "A3", "A4", "A5")){
  tmp.11 <- tmp.1
  tmp.11$Autonomy <- i
  tmp.11$AutonomyProp <- 0
  prop <- rbind(prop,tmp.11)
  
}

dim(prop)
27*2*6*4






## ---- Scenario 1: Only marker share is growing everyting else is constant ----
# First General Growth rate of the company for given time frame
GeneralGrowthRate <- 1 + log(1+ time.frame/100)

r <- lapply(X = time.frame, FUN = function(x, GeneralGrowthRate, prop, CurrentTimeData ){
  # x is time the time frame
  # GeneralGrowthRate is the growthRate for yeach year
  # prop is the InitialProportions
  # CurrentTimeData is the data Frame for year 2018
  
  
  # the year we are working in, just take the first index as the year is the same
  year <- CurrentTimeData$Year[1] + x
  
  
  # Calculate initial exposure
  totalExposureQtr1 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 1])
  totalExposureQtr2 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 2])
  totalExposureQtr3 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 3])
  totalExposureQtr4 <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == 4])
  # adjust for the growth rate for each quarter, for current year
  totalExposureQtr1 <- totalExposureQtr1*GeneralGrowthRate[x]
  totalExposureQtr2 <- totalExposureQtr2*GeneralGrowthRate[x]
  totalExposureQtr3 <- totalExposureQtr3*GeneralGrowthRate[x]
  totalExposureQtr4 <- totalExposureQtr4*GeneralGrowthRate[x]
  
  # Next we use this years proportions to get the exposure 
  prop$Exposure <- 0
  prop$Exposure[prop$Qtr == 1] <- totalExposureQtr1*prop$prop[prop$Qtr == 1]
  prop$Exposure[prop$Qtr == 2] <- totalExposureQtr2*prop$prop[prop$Qtr == 2]
  prop$Exposure[prop$Qtr == 3] <- totalExposureQtr3*prop$prop[prop$Qtr == 3]
  prop$Exposure[prop$Qtr == 4] <- totalExposureQtr4*prop$prop[prop$Qtr == 4]
  
  # Finally We combine the data frames with the new data
  # Current Time Data columns we want to inclide
  
  CTDnames <- c("Qtr", "RiskClass", "Type", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI", 
                "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "Autonomy", "Year")
 
  
  
  
  CurrentTimeData <- full_join(CurrentTimeData[, names(CurrentTimeData) %in% CTDnames], 
                               prop[, !(names(prop) %in% "prop")], by = c("Qtr", "RiskClass", "Type", "Autonomy"))
  CurrentTimeData$Year <- year
  

  return(CurrentTimeData)
  
}, GeneralGrowthRate = GeneralGrowthRate, prop = prop, CurrentTimeData = CurrentTimeData)

exposure.s1 <- do.call(what = rbind, args = r)

### ---- Scenario 2: Autonomous Levels change----
GeneralGrowthRate <- 1 + log(1+ time.frame/100)

aRate <- autonomyRate(time.frame = c(0,time.frame))
# add year and qtr
aRate$Year <- ceiling(aRate$time + currentYear )
aRate$Qtr <- 1
aRate$Qtr[aRate$time %% 1 == 0.5] <- 2
aRate$Qtr[aRate$time %% 1 == 0.75] <- 3
aRate$Qtr[aRate$time %% 1 == 0] <- 4
aRate

r <- lapply(X = time.frame, FUN = function(x, GeneralGrowthRate, prop, CurrentTimeData, aRate ){
  # x is time the time frame
  # GeneralGrowthRate is the growthRate for yeach year
  # prop is the InitialProportions
  # CurrentTimeData is the data Frame for year 2018
  # aRate is the outonomy proportion change
  
  
  # the year we are working in, just take the first index as the year is the same
  year <- CurrentTimeData$Year[1] + x
  
  # Exposure
  prop$Exposure <- 0
  totalExposure <- list()
  for(i in 1:4){
    totalExposure[[paste("Qtr", i, sep = "")]] <- sum(CurrentTimeData$Exposure[CurrentTimeData$Qtr == i])*GeneralGrowthRate[x]
    prop$Exposure[prop$Qtr == i] <-   totalExposure[[paste("Qtr", i, sep = "")]]
  }
  
  prop$AutonomyProp <- 0
  for(i in c("A0", "A1", "A2" ,"A3", "A4", "A5")){
    
    for(j in 1:4){
      prop$AutonomyProp[prop$Autonomy == i & prop$Qtr == j] <- aRate[aRate$Qtr == j & aRate$Year ==year, names(aRate) %in% i]
    }
    
  }
 
  
  # Then we finally adjust the exposure
  prop$Exposure <- prop$prop*prop$AutonomyProp*prop$Exposure
  

  
  # Finally We combine the data frames with the new data
  # Current Time Data columns we want to inclide
  
  CTDnames <- c("Qtr", "RiskClass", "Type", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI", 
                "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "Autonomy", "Year")
  
  
  
  
  CurrentTimeData <- full_join(CurrentTimeData[, names(CurrentTimeData) %in% CTDnames], 
                               prop, by = c("Qtr", "RiskClass", "Type", "Autonomy"))
  
  CurrentTimeData$Year <- year
  
  
  return(CurrentTimeData)
  
}, GeneralGrowthRate = GeneralGrowthRate, prop = prop, CurrentTimeData = CurrentTimeData, aRate = aRate)

exposure.s2 <- do.call(what = rbind, args = r)
View(exposure.s2[bool, ])

growth <- function(Year, Type, RiskClass, Qtr, exposure, Autonomy){
  
  
  
  # Expsosure Growth, data frame with Rows
  
  
  
  bool <- exposure$Year == Year & exposure$Qtr == Qtr & exposure$RiskClass == RiskClass & exposure$Type == Type &
    exposure$Autonomy == Autonomy
  randomFactor <- 1
  
  return(exposure$Exposure[bool]*randomFactor)
}




