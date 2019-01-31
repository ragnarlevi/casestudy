# load packages
require(tidyverse)
require(readxl)
require(GGally)
require(reshape2)
# set wd
getwd()
setwd()

# read data, skip = Minimum number of rows to skip before reading anything, be it column names or data
autoclaims <- read_excel("2019-student-research-case-study-data.xlsx", skip = 9)
head(autoclaims)

dimension <- dim(autoclaims)
dimension

names(autoclaims)
# Change the names

# temporary names vector
tmp <- c("Year", "Qtr", "RiskClass", "Type", "CarYearsOfExposure", "NoBodilyInjury", "NoPropertyDamage",
         "NoComprehensive", "NoCollision", "NoPersonalInjury", "AmountBodilyInjury", "AmountPropertyDamage",
         "AmountComprehensive", "AmountCollision", "AmountPersonalInjury")

variables <- c("CarYearsOfExposure", "NoBodilyInjury", "NoPropertyDamage",
"NoComprehensive", "NoCollision", "NoPersonalInjury", "AmountBodilyInjury", "AmountPropertyDamage",
"AmountComprehensive", "AmountCollision", "AmountPersonalInjury")
names(autoclaims) <- tmp
head(autoclaims)

# add year + qtr
autoclaims$YearQtr <- autoclaims$Year + 2.5*autoclaims$Qtr/10 

# Total Exposure for each qtr
autoclaims$TotalExposure <- 0
# let's just do a brute force for loop
for( i in levels(factor(autoclaims$Year))){
  for(j in levels(factor(autoclaims$Qtr))){
    
    autoclaims$TotalExposure[autoclaims$Year == i & autoclaims$Qtr == j] <- sum(autoclaims$CarYearsOfExposure[autoclaims$Year == i & autoclaims$Qtr == j] )
    
  }
}
# then we normalize
autoclaims$ExposureNormalize <- autoclaims$CarYearsOfExposure/autoclaims$TotalExposure

View(autoclaims[autoclaims$RiskClass == "SML" & autoclaims$Type == "Personal",])


# lets make three plots for each vehicle size 

ggplot(data = autoclaims[grepl("^S", x = autoclaims$RiskClass) & autoclaims$Type  == "Personal", , drop = F]) + geom_line(aes( x = YearQtr, y = ExposureNormalize , color = RiskClass))

ggplot(data = autoclaims[grepl("^M", x = autoclaims$RiskClass) & autoclaims$Type  == "Personal", , drop = F]) + geom_line(aes( x = YearQtr, y = ExposureNormalize , color = RiskClass))

ggplot(data = autoclaims[grepl("^L", x = autoclaims$RiskClass) & autoclaims$Type  == "Personal", , drop = F]) + geom_line(aes( x = YearQtr, y = ExposureNormalize , color = RiskClass))

# Amount comprehensive

# Super spike
melt.autoclaims <- melt(autoclaims, id = c("Year", "Qtr", "YearQtr", "Type", "RiskClass"))
ggplot(data = melt.autoclaims[ melt.autoclaims$Type  == "Personal" & !(melt.autoclaims$variable %in% c("TotalExposure", "ExposureNormalize"))
                               ,  , drop = F]) + geom_line(aes( x = YearQtr, y = value , color = RiskClass))+ facet_wrap( ~ variable, scales = "free")


# But if we aggregate The Risk classes
autoclaims.Qtrly <- aggregate(. ~ Year + Qtr+ YearQtr, autoclaims[, !(names(autoclaims) %in% c("Type", "RiskClass") )], FUN = sum)

melt.autoclaims.Qtrly <- melt(autoclaims.Qtrly, id = c("Year", "Qtr", "YearQtr"))
ggplot(data = melt.autoclaims.Qtrly[ !(melt.autoclaims.Qtrly$variable %in% c("TotalExposure", "ExposureNormalize", "NoPropertyDamage",
                                                                             "NoComprehensive", "NoCollision", "NoBodilyInjury", "NoPersonalInjury", "CarYearsOfExposure")), ]) + geom_line(aes( x = YearQtr, y = value, color = variable )) 

ggplot(data = melt.autoclaims.Qtrly[ !(melt.autoclaims.Qtrly$variable %in% c("TotalExposure", "ExposureNormalize", "AmountPropertyDamage",
                                                                             "AmountComprehensive", "AmountCollision", "AmountBodilyInjury", "AmountPersonalInjury")), ]) + geom_line(aes( x = YearQtr, y = value, color = variable )) 


ggplot(data = melt.autoclaims.Qtrly[ !(melt.autoclaims.Qtrly$variable %in% c("TotalExposure", "ExposureNormalize", "AmountPropertyDamage",
                                                                             "AmountComprehensive", "AmountCollision", "AmountBodilyInjury", "AmountPersonalInjury", "CarYearsOfExposure")), ]) + geom_line(aes( x = YearQtr, y = value, color = variable )) 




#  ----
names(autoclaims)
# function for lower part of ggpairs data
lowerFn <- function(data, mapping, method , ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method = method, ...)
  p
}



ggpairs(autoclaims[, c(4,6:dimension[2])],lower = list(continuous = wrap(lowerFn, method = "loess")), 
        mapping = ggplot2::aes(colour = Type))

melt.auto<- melt(data = autoclaims, id= c("YearQtr","Year", "Qtr", "RiskClass", "Type"))



p.box <- ggplot(data = melt.auto) + geom_boxplot(aes(color = Type, y = value)) + facet_wrap( ~ variable, scales = "free")
p.box

p.box <- ggplot(data = melt.auto) + geom_boxplot(aes(color = RiskClass, y = value)) + facet_wrap( ~ variable, scales = "free")
p.box


p.point <- ggplot(data = melt.auto) + geom_line(aes( x = YearQtr, y = value, color = RiskClass)) + facet_wrap( ~ variable, scales = "free")
p.point

# making plots for personal only, commercial only and both after the other colums have been summmed For
# example for each type there is a corresponding risk class, In order to plot the whole data set withour type column
# we need to sum the risk class for personal and commerical
autoclaims.personal <- autoclaims[autoclaims$Type == "Personal",]
autoclaims.commercial <- autoclaims[autoclaims$Type == "Commercial",]
melt.auto.personal <- melt(data = autoclaims.personal, id= c("YearQtr","Year", "Qtr", "RiskClass", "Type"))
melt.auto.commercial <- melt(data = autoclaims.commercial, id= c("YearQtr","Year", "Qtr", "RiskClass", "Type"))

# make data frame with no type, but maintaining the uniqueness of year, qtr, yearqtr and Risk classs
autoclaims.agg.type <- aggregate(. ~ RiskClass + Year + YearQtr + Qtr, 
                                 data = autoclaims[, !(names(autoclaims) %in% c("Type"))], FUN = sum)

melt.auto.agg.type <- melt(data = autoclaims.agg.type, id= c("YearQtr","Year", "Qtr", "RiskClass"))

# plott

ggplot(data = melt.auto.personal) + geom_line(aes( x = YearQtr, y = value, color = RiskClass)) + facet_wrap( ~ variable, scales = "free")

ggplot(data = melt.auto.commercial) + geom_line(aes( x = YearQtr, y = value, color = RiskClass)) + facet_wrap( ~ variable, scales = "free")

ggplot(data = melt.auto.agg.type) + geom_line(aes( x = YearQtr, y = value, color = RiskClass)) + facet_wrap( ~ variable, scales = "free")


# hello worl


# Only plot one variable
# Remember agg.type is the aggregated personal and commercial
View(autoclaims.agg.type)

ggplot(data = autoclaims.agg.type) + geom_line(aes(x = YearQtr, y = NoBodilyInjury , color = RiskClass))
ggplot(data = autoclaims.agg.type) + geom_line(aes(x = YearQtr, y = CarYearsOfExposure , color = RiskClass))
  


# histogram
ggplot(data = autoclaims) + geom_histogram(aes(NoBodilyInjury))

sum(NoBodilyInjury == 1)

### ---- Some nice Proportion plots


#-----
ggplot(data = autoclaims[autoclaims$Type == "Personal", , drop = F]) + geom_histogram(aes(NoBodilyInjury), bins = 30)


ggplot(data = autoclaims[autoclaims$Type == "Personal" & autoclaims$RiskClass == "SML", , drop = F]) + geom_histogram(aes(NoBodilyInjury), bins = 30)






