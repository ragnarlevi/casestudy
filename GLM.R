

# Read and change names ----
autocar <- read_excel("2019-student-research-case-study-data.xlsx", range = "B10:P2170")

# names(autocar)
names(autocar) <- c("Year", "Qtr", "RiskClass", "Type", "Exposure", "NC_BI", "NC_PD", "NC_COM", "NC_COL", 
                       "NC_PI", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI")


# Add Variables ----
#creating factors for vehicle size variable
autocar$VehicleSize <- ""
autocar$VehicleSize[grepl("^S", x = autocar$RiskClass)] <- "S" 
autocar$VehicleSize[grepl("^M", x = autocar$RiskClass)] <- "M" 
autocar$VehicleSize[grepl("^L", x = autocar$RiskClass)] <- "L" 

#creating factors for driver age variable
autocar$DriverAge <- ""
autocar$DriverAge[grepl("^.Y", x = autocar$RiskClass)] <- "Y"
autocar$DriverAge[grepl("^.M", x = autocar$RiskClass)] <- "M"
autocar$DriverAge[grepl("^.S", x = autocar$RiskClass)] <- "S"

#creating factors for driver risk variable
autocar$DriverRisk <- ""
autocar$DriverRisk[grepl("^..L", x = autocar$RiskClass)] <- "L"
autocar$DriverRisk[grepl("^..A", x = autocar$RiskClass)] <- "A"
autocar$DriverRisk[grepl("^..H", x = autocar$RiskClass)] <- "H"

# autocar$Qtr <- as.factor(autocar$Qtr)
# autocar$Type <- as.factor(autocar$Type)
# autocar$VehicleSize <- as.factor(autocar$VehicleSize)
# autocar$DriverAge <- as.factor(autocar$DriverAge)
# autocar$DriverRisk <- as.factor(autocar$DriverRisk)
autocar$time <- autocar$Year + 2.5*as.numeric(autocar$Qtr)/10

# GLM model ----
#We make a list of glm's (outlier included) divided into raw data (outlier) and refined data
glm <- list()

#Model number of claims in every claim category

glm$ol$Number$Model$BI <- glm(data = autocar, formula = NC_BI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$PD <- glm(data = autocar, formula = NC_PD ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$COM <- glm(data = autocar, formula = NC_COM ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$COL <- glm(data = autocar, formula = NC_COL ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$PI <- glm(data = autocar, formula = NC_PI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))


#First, we create a new variable called average claim amount = Total Claim Amount / Number of Claims
autocar$AAC_BI <- autocar$AC_BI/autocar$NC_BI
autocar$AAC_PD <- autocar$AC_PD/autocar$NC_PD
autocar$AAC_COM <- autocar$AC_COM/autocar$NC_COM
autocar$AAC_COL <- autocar$AC_COL/autocar$NC_COL
autocar$AAC_PI <- autocar$AC_PI/autocar$NC_PI


#Model average amounts using gamma glm
glm$ol$Amount$Model$BI <- glm(data = autocar, formula = AAC_BI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_BI)
glm$ol$Amount$Model$PD <- glm(data = autocar, formula = AAC_PD ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_BI)
glm$ol$Amount$Model$COM <- glm(data = autocar, formula = AAC_COM ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_BI)
glm$ol$Amount$Model$COL <- glm(data = autocar, formula = AAC_COL ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_BI)
glm$ol$Amount$Model$PI <- glm(data = autocar, formula = AAC_PI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"),weights = NC_BI)



# Filter the data -----

 melt.autocar <- melt(autocar[, names(autocar) %in% c("time", "AC_COM", "RiskClass", "Type", "NC_COM")] , id = c("Type", "RiskClass", "time"))
# ggplot(melt.autocar[melt.autocar$Type == "Commercial",]) + geom_line(aes(x = time, y = value, color = RiskClass))  + facet_wrap( ~ variable)

# linear regression for amount comprehensive and number comprehnsive
lm.no.outlier <- list()
autocar.tmp <- autocar[autocar$time != 2010.75,]
#View(autocar.tmp)

# need to do so for each risk class and each type, Put it in a list and make it with a for loop

for(j in unique(as.character(autocar.tmp$Type))){
  # we need to use unique because we only want to do the linear regression one
  for(i in unique(as.character(autocar.tmp$RiskClass))){
    lm.no.outlier$AC_COM[[j]][[i]] <- lm(AC_COM ~ Year+ factor(Qtr), data = autocar.tmp[autocar.tmp$Type == j & autocar.tmp$RiskClass == i, ])
    lm.no.outlier$NC_COM[[j]][[i]] <- lm(NC_COM ~ Year+ factor(Qtr), data = autocar.tmp[autocar.tmp$Type == j & autocar.tmp$RiskClass == i, ])
  }
  
}

# Then We predict and put into new data frame called ..
autocar.refined <- autocar
# defining our predict var
predict.val <- data.frame(Year = 2010, Qtr = 3)

for(j in unique(as.character(autocar.tmp$Type))){
  # we need to use unique because we only want to do the linear regression one
  for(i in unique(as.character(autocar.tmp$RiskClass))){
    # PREDICT FOR AC_COM
    autocar.refined$AC_COM[autocar.refined$Type == j & autocar$RiskClass == i & autocar.refined$Year == 2010 & autocar.refined$Qtr ==3] <- 
      predict.lm(lm.no.outlier$AC_COM[[j]][[i]], predict.val)
    # Predict for NC_com and we round up to get integer
    autocar.refined$NC_COM[autocar.refined$Type == j & autocar$RiskClass == i & autocar.refined$Year == 2010 & autocar.refined$Qtr ==3] <-
      ceiling(predict.lm(lm.no.outlier$NC_COM[[j]][[i]], predict.val))
  }
  
}

# Compare
melt.autocar.refined <- melt(autocar.refined[, names(autocar.refined) %in% c("time", "AC_COM", "RiskClass", "Type", "NC_COM")] , id = c("Type", "RiskClass", "time"))

# Figure For Personal types
p1 <- ggplot(melt.autocar[melt.autocar$Type == "Personal",]) + geom_line(aes(x = time, y = value, color = RiskClass)) +
  geom_vline(xintercept = 2010.75) + facet_wrap( ~ variable, scales = "free") + ggtitle("Personal raw data")
p2 <- ggplot(melt.autocar.refined[melt.autocar.refined$Type == "Personal",]) + geom_line(aes(x = time, y = value, color = RiskClass)) +
  geom_vline(xintercept = 2010.75) + facet_wrap( ~ variable, scales = "free") + ggtitle("Personal refined data")

# Figure for Commercial types
p3 <- ggplot(melt.autocar[melt.autocar$Type == "Commercial",]) + geom_line(aes(x = time, y = value, color = RiskClass)) +
  geom_vline(xintercept = 2010.75) + facet_wrap( ~ variable, scales = "free") + ggtitle("Commercial raw data")
p4 <- ggplot(melt.autocar.refined[melt.autocar.refined$Type == "Commercial",]) + geom_line(aes(x = time, y = value, color = RiskClass)) +
  geom_vline(xintercept = 2010.75) + facet_wrap( ~ variable, scales = "free") + ggtitle("Commercial refined data")


#multiplot(p1,p2, p3, p4, plotlist = NULL, cols = 1)

# multiplot(p3,p4, plotlist = NULL, cols = 1)



# Perform GLM's for refined data (after adjusting the outlier) ----


# NC_BI model ----
tmp <- glm(data = autocar.refined, formula = NC_BI ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time, no frequency inflation
tmp <- glm(data = autocar.refined, formula = NC_BI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove Qtr as well
tmp_2 <- glm(data = autocar.refined, formula = NC_BI ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
anova(tmp_2, tmp, test = "F") 
# yes reomve tmp
glm$rd$Number$Model$BI <- tmp_2 ##############################################--
remove(tmp, tmp_2)

# NC_PD model ----
tmp <- glm(data = autocar.refined, formula = NC_PD ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp <- glm(data = autocar.refined, formula = NC_PD ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove Qtr as well
tmp_2 <- glm(data = autocar.refined, formula = NC_PD ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
anova(tmp_2, tmp, test = "F") 
# yes reomve tmp
glm$rd$Number$Model$PD <- tmp_2 ##############################################--
remove(tmp, tmp_2)

# NC_COM model ----
tmp <- glm(data = autocar.refined, formula = NC_COM ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp <- glm(data = autocar.refined, formula = NC_COM ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove Qtr as well
tmp_2 <- glm(data = autocar.refined, formula = NC_COM ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
anova(tmp_2, tmp, test = "F") 
# We include qtr
glm$rd$Number$Model$COM <- tmp ##############################################--
remove(tmp, tmp_2)

# NC_COL model----
tmp <- glm(data = autocar.refined, formula = NC_COL ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp <- glm(data = autocar.refined, formula = NC_COL ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove Qtr as well
tmp_2 <- glm(data = autocar.refined, formula = NC_COL ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
anova(tmp_2, tmp, test = "F") 
# We include qtr
glm$rd$Number$Model$COL <- tmp ##############################################--
remove(tmp, tmp_2)

# NC_PI model ----
tmp <- glm(data = autocar.refined, formula = NC_PI ~  time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp <- glm(data = autocar.refined, formula = NC_PI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove Qtr as well
tmp_2 <- glm(data = autocar.refined, formula = NC_PI ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
anova(tmp_2, tmp, test = "F") 
# We remove qtr
glm$rd$Number$Model$PI <- tmp_2 ##############################################--
remove(tmp, tmp_2)


# We only changed COM but we just make this var again for all
autocar.refined$AAC_BI <- autocar.refined$AC_BI/autocar.refined$NC_BI
autocar.refined$AAC_PD <- autocar.refined$AC_PD/autocar.refined$NC_PD
autocar.refined$AAC_COM <- autocar.refined$AC_COM/autocar.refined$NC_COM
autocar.refined$AAC_COL <- autocar.refined$AC_COL/autocar.refined$NC_COL
autocar.refined$AAC_PI <- autocar.refined$AC_PI/autocar.refined$NC_PI


# AAC_BI model ----
tmp <- glm(data = autocar.refined, formula = AAC_BI ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_BI)
summary(tmp)
tmp_2 <- glm(data = autocar.refined, formula = AAC_BI ~ time  + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_BI)
summary(tmp_2)
anova(tmp, tmp_2, test = "F") 
# we include Qtr
glm$rd$Amount$Model$BI <- tmp
remove(tmp, tmp_2)

# AAC_PD model ----
tmp <- glm(data = autocar.refined, formula = AAC_PD ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_PD)
summary(tmp)
tmp_2 <- glm(data = autocar.refined, formula = AAC_PD ~ time  + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_PD)
summary(tmp_2)
anova(tmp, tmp_2, test = "F") 
# we include Qtr
glm$rd$Amount$Model$PD <- tmp
remove(tmp, tmp_2)

## AAC_COM ----
tmp <- glm(data = autocar.refined, formula = AAC_COM ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_COM)
summary(tmp)
tmp_2 <- glm(data = autocar.refined, formula = AAC_COM ~ time  + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_COM)
summary(tmp_2)
anova(tmp, tmp_2, test = "F") 
# we include Qtr
glm$rd$Amount$Model$COM <- tmp
remove(tmp, tmp_2)

## AAC_COL ----
tmp <- glm(data = autocar.refined, formula = AAC_COL ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_COL)
summary(tmp)
tmp_2 <- glm(data = autocar.refined, formula = AAC_COL ~ time  + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_COL)
summary(tmp_2)
anova(tmp, tmp_2, test = "F") 
# we include Qtr
glm$rd$Amount$Model$COL <- tmp
remove(tmp, tmp_2)

## AAC_PI ----
tmp <- glm(data = autocar.refined, formula = AAC_PI ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_PI)
summary(tmp)
tmp_2 <- glm(data = autocar.refined, formula = AAC_PI ~ time  + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_PI)
summary(tmp_2)
anova(tmp, tmp_2, test = "F") 
# we include Qtr
glm$rd$Amount$Model$PI <- tmp
remove(tmp, tmp_2)


# Adjust COM ----
# Because we basically lowerd the amount and number of the COM coverage we need to adjust the intercept because in the next years we can have another natural disaster

ol <- sum(autocar$AAC_COM)
rd <- sum(autocar.refined$AAC_COM)
ol-rd
(ol-rd)/ol
ol/rd
glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"] <- glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"]  + log(ol/rd)
remove(ol, rd)

ol <- sum(autocar$NC_COM)
rd <- sum(autocar.refined$NC_COM)
ol-rd
(ol-rd)/ol
ol/rd
glm$rd$Number$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"] <- glm$rd$Number$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"]  + log(ol/rd)

remove(ol, rd)

# Finally get the estimates for each Risk Class, type, qtr etc..
# get estimates
# for(i in 1:length(glm)){
#   glm[[i]]$Number$df <- GetEstimates(Data.List = glm[[i]], m = "Number")
#   glm[[i]]$Amount$df <- GetEstimates(Data.List = glm[[i]], m = "Amount")
#     
# }


