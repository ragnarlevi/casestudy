# load packages
require(tidyverse)
require(readxl)
require(GGally)
require(reshape2)
# set wd
setwd("ISEG/Actuarial Science Courses @ ISEG/case study/")

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

names(autoclaims) <- tmp
head(autoclaims)


# add year + qtr
autoclaims$YearQtr <- autoclaims$Year + 2.5*autoclaims$Qtr/10

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





