library(readxl)
autocar <- read_excel("2019-student-research-case-study-data.xlsx", range = "B10:P2170")
attach(autocar)
colnames(autocar) <- c("Year", "Qtr", "RiskClass", "Type", "Exposure", "NC1", "NC2", "NC3", "NC4", 
                       "NC5", "AC1", "AC2", "AC3", "AC4", "AC5")

#Rate of Number of Claims (normalized by exposure)
RNC1 = NC1/Exposure
RNC2 = NC2/Exposure
RNC3 = NC3/Exposure
RNC4 = NC4/Exposure
RNC5 = NC5/Exposure

#Rate of Amount of Claims (normalized by exposure)
RAC1 = AC1/Exposure
RAC2 = AC2/Exposure
RAC3 = AC3/Exposure
RAC4 = AC4/Exposure
RAC5 = AC5/Exposure

#Total Number of Claims
TNC = (NC1+NC2+NC3+NC4+NC5)
#Total Amount of Claims
TAC = (AC1+AC2+AC3+AC4+AC5)
#Add total number and amount to the data set
autocar <- cbind(autocar, TNC, TAC)

# Assumptions and clarifations: 
# There are 54 risk classes: Vehicle size (3), Driver age (3), Driver risk (3), Type (2).
# We want to find the average claim size for every risk class using the 40 observations.
# We assume that the average claim size per category per risk class is stable over time.
# This means that we can aggregate the total data for all risk classes.

# The proportion of exposure per risk class is assumed to have a steady-state distribution.

sum(Exposure)

aggrautocar <- aggregate(autocar, by = list(unique.values = RiskClass), FUN = mean)

mean(RAC2[Type=="Personal"])

autocar[,c(16:25)] <- c(RNC1, RNC2,RNC3,RNC4,RNC5,RAC1,RAC2,RAC3,RAC4,RAC5)
colnames(autocar) <- c("Year", "Qtr", "RiskClass", "Type", "Exposure", "NC1", "NC2", "NC3", "NC4", 
                       "NC5", "AC1", "AC2", "AC3", "AC4", "AC5","RNC1", "RNC2","RNC3","RNC4","RNC5","RAC1","RAC2","RAC3","RAC4","RAC5")

plot(RAC1[RiskClass=="LSL"])
sum(Exposure[RiskClass=="SML"])


# Steady-state proportions
autocar$RiskClass <- as.factor(autocar$RiskClass)

TotalExp = c()
for (i in factor(RiskClass)){
  TotalExp[i] = sum(Exposure[RiskClass==i])
  next
}
factors(RiskClass)

PropExposure = TotalExp/sum(Exposure)
PropExposure


# Finding the average amount of claims per unit of exposure, per category for all risk classes
Rate1 = c()
Rate2 = c()
Rate3 = c()
Rate4 = c()
Rate5 = c()
TotalRate = c()

for (i in factor(RiskClass)){
  Rate1[i] = sum(AC1[RiskClass==i])/TotalExp[i]
  Rate2[i] = sum(AC2[RiskClass==i])/TotalExp[i]
  Rate3[i] = sum(AC3[RiskClass==i])/TotalExp[i]
  Rate4[i] = sum(AC4[RiskClass==i])/TotalExp[i]
  Rate5[i] = sum(AC5[RiskClass==i])/TotalExp[i]
  TotalRate[i] = sum(TAC[RiskClass==i])/TotalExp[i]
  next
}

hist(TAC[RiskClass=="SMA"])

Rate1
Rate2
Rate3
Rate4
Rate5
TotalRate
autocar <- cbind(autocar, Rate1,Rate2,Rate3,Rate4,Rate5,TotalRate)

TotalExp <- matrix(rep(0, 54), 27)
autocar$Type <- as.factor(autocar$Type)

autocar$RiskClass = as.factor(RiskClass)
autocar$Type = as.factor(Type)


for (i in factor(RiskClass)){
  for (j in factor(Type)){
    TotalExp[i,j] = sum(Exposure[RiskClass==i & Type==j])
  }
}

glmtry <- glm(TNC ~ Type+RiskClass, data=autocar, family=poisson())
summary(glmtry)

if (startsWith(RiskClass, "S") == T){
  return(1)
}

aggrautocar <- aggregate(autocar[,c(5)], by = list(autocar$Type, autocar$RiskClass), FUN = sum)
aggrautocar2 <- aggregate(autocar[,c(6:15)], by = list(autocar$Type, autocar$RiskClass), FUN = sum)
aggrautocar3 <- cbind(aggrautocar, aggrautocar2[,c(3:12)])
colnames(autocaragg)[1:3] <- c("Type", "RiskClass", "Exposure")
autocaragg <- aggrautocar3
attach(autocaragg)
TNC = NC1 + NC2 + NC3 + NC4 + NC5
TAC = AC1 + AC2 + AC3 + AC4 + AC5
autocaragg = cbind(autocaragg, TNC, TAC)

vehiclesize = rep(NA, 54)
driverage = rep(NA, 54)
driverrisk = rep(NA, 54)
autocaragg = cbind(autocaragg, vehiclesize, driverage, driverrisk)


#Vehicle size: small = 0, medium = 1, large = 2.
firstletterS = grepl("^S", x = autocaragg$RiskClass)
firstletterM = grepl("^M", x = autocaragg$RiskClass)
firstletterL = grepl("^L", x = autocaragg$RiskClass)

for (i in 1:54){
  if (firstletterS[i]==TRUE){
    autocaragg$vehiclesize[i] = 0
  } else if (firstletterM[i]==TRUE){
    autocaragg$vehiclesize[i] = 1
  } else if (firstletterL[i]==TRUE){
    autocaragg$vehiclesize[i] = 2
  }
  next
}

#Driver age: young = 0, medium = 1, large = 2.
secondletterY = grepl("^.Y", x = autocaragg$RiskClass)
secondletterM = grepl("^.M", x = autocaragg$RiskClass)
secondletterS = grepl("^.S", x = autocaragg$RiskClass)

for (i in 1:54){
  if (secondletterY[i]==TRUE){
    autocaragg$driverage[i] = 0
  } else if (secondletterM[i]==TRUE){
    autocaragg$driverage[i] = 1
  } else if (secondletterS[i]==TRUE){
    autocaragg$driverage[i] = 2
  }
  next
}

#Driver risk: low = 0, average = 1, high = 2.
thirdletterL = grepl("^..L", x = autocaragg$RiskClass)
thirdletterA = grepl("^..A", x = autocaragg$RiskClass)
thirdletterH = grepl("^..H", x = autocaragg$RiskClass)

for (i in 1:54){
  if (thirdletterL[i]==TRUE){
    autocaragg$driverrisk[i] = 0
  } else if (thirdletterA[i]==TRUE){
    autocaragg$driverrisk[i] = 1
  } else if (thirdletterH[i]==TRUE){
    autocaragg$driverrisk[i] = 2
  }
  next
}

#First, we add some variables adjusted by exposure to the dataset:
RNC1 = NC1/Exposure
RNC2 = NC2/Exposure
RNC3 = NC3/Exposure
RNC4 = NC4/Exposure
RNC5 = NC5/Exposure
RTNC = TNC/Exposure
RAC1 = AC1/Exposure
RAC2 = AC2/Exposure
RAC3 = AC3/Exposure
RAC4 = AC4/Exposure
RAC5 = AC5/Exposure
RTAC = TAC/Exposure

autocaragg = cbind(autocaragg, RNC1, RNC2, RNC3, RNC4, RNC5, RAC1, RAC2, RAC3, RAC4, RAC5)

#Performing a GLM on Type, Vehicle size, Driver age, Driver Risk:

glmtry <- glm(data = autocaragg, formula = RTAC~Type+driverage+driverrisk+vehiclesize)
summary(glmtry)


