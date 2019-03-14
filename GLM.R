

# Read data  and change names ----
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

# Include a time column, Note this time begins in 2009.25 and ends in 2019
autocar$time <- autocar$Year + 2.5*as.numeric(autocar$Qtr)/10

# GLM model ----
#We make a list of glm models, the lis is divided into ol = ordinary data and rd = refined data
# These seections are then divded into Number and Amount
glm <- list()


# First we fit a glm model for the unrefined data frame, just as a initial step
#Model number of claims in every claim category

# Note we have the Exposure as an offset
glm$ol$Number$Model$BI <- glm(data = autocar, formula = NC_BI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$PD <- glm(data = autocar, formula = NC_PD ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$COM <- glm(data = autocar, formula = NC_COM ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$COL <- glm(data = autocar, formula = NC_COL ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$PI <- glm(data = autocar, formula = NC_PI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))


#We create a new variable called average claim amount = Total Claim Amount / Number of Claims
# Because we want to estimate the average amount per claim
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



# Next, the data is filtered/refined because of the outlier

# linear regression for amount comprehensive and number comprehnsive
lm.no.outlier <- list()
autocar.tmp <- autocar[autocar$time != 2010.75,] # remove the point

# We need to make a model for each risk class and each type, Put it in a list.

for(j in unique(as.character(autocar.tmp$Type))){
  # we need to use unique because we only want to do the linear regression one
  for(i in unique(as.character(autocar.tmp$RiskClass))){
    lm.no.outlier$AC_COM[[j]][[i]] <- lm(AC_COM ~ Year+ factor(Qtr), data = autocar.tmp[autocar.tmp$Type == j & autocar.tmp$RiskClass == i, ])
    lm.no.outlier$NC_COM[[j]][[i]] <- lm(NC_COM ~ Year+ factor(Qtr), data = autocar.tmp[autocar.tmp$Type == j & autocar.tmp$RiskClass == i, ])
  }
  
}

# Then We predict and put into new data frame called ..
autocar.refined <- autocar
# defining our predict var, is should be a data frame with the same column name 
# as the independent variables of the ols
predict.val <- data.frame(Year = 2010, Qtr = 3)

# This for loop estimates the new point for each Type and RiskClass
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
tmp.autocar.refined <- autocar.refined[, names(autocar.refined) %in% c("time", "AC_COM", "NC_COM")]
tmp.autocar.refined <- aggregate( . ~ time, data = tmp.autocar.refined, FUN = sum)
melt.autocar.refined <- melt(tmp.autocar.refined , id = c( "time"))

tmp.autocar <- autocar[, names(autocar) %in% c("time", "AC_COM", "NC_COM")]
tmp.autocar <- aggregate( . ~ time, data = tmp.autocar, FUN = sum)
melt.autocar <- melt(tmp.autocar , id = c( "time"))
# Change the Name for the plot
melt.autocar$variable <- as.character(melt.autocar$variable)
melt.autocar$variable[melt.autocar$variable == "NC_COM"] <- "Number of Claims"
melt.autocar$variable[melt.autocar$variable == "AC_COM"] <- "Total amount"

melt.autocar.refined$variable <- as.character(melt.autocar.refined$variable)
melt.autocar.refined$variable[melt.autocar.refined$variable == "NC_COM"] <- "Number of Claims"
melt.autocar.refined$variable[melt.autocar.refined$variable == "AC_COM"] <- "Total amount"

# Figure For Personal types
p1 <- ggplot(melt.autocar) + geom_line(aes(x = time, y = value), color = "#4E87A0", size = 1) +
  geom_vline(xintercept = 2010.75, alpha = 0.8) +
  facet_wrap( ~ variable, scales = "free" ) +
  ggtitle("Raw data") + 
  theme_bw(base_size = 12) + 
  scale_x_continuous(name = "Year", breaks = c(2010.75), labels = c("2010.75"), expand = c(0,0))+
  scale_y_continuous(name = "", expand = c(0,0), labels = "", breaks = c())+
  theme(legend.position="none")

p2 <- ggplot(melt.autocar.refined) + geom_line(aes(x = time, y = value), color = "#4E87A0", size = 1) +
  geom_vline(xintercept = 2010.75, alpha = 0.8) + facet_wrap( ~ variable, scales = "free") + ggtitle("Refined data")+
  theme_bw(base_size = 12) + 
  scale_x_continuous(name = "Year", breaks = c(2010.75), labels = c("2010.75"), expand = c(0,0))+
  scale_y_continuous(name = "", expand = c(0,0), labels = "", breaks = c())+
  theme(legend.position="none")

# plot 
# multiplot(p1,p2, plotlist = NULL, cols = 1)



# Perform GLM's for refined data (after adjusting the outlier) ----

# We check the Significance of paramters and compare two models with an anova test


# NC_BI model ----
tmp <- glm(data = autocar.refined, formula = NC_BI ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time, no frequency inflation
tmp_2 <- glm(data = autocar.refined, formula = NC_BI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
# remove Qtr as well
tmp_3 <- glm(data = autocar.refined, formula = NC_BI ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_3)
anova(tmp_2, tmp_3, test = "F") 
# yes reomve tmp
glm$rd$Number$Model$BI <- tmp_3 ################MODEL##############################--
remove(tmp, tmp_2, tmp_3)

# NC_PD model ----
tmp <- glm(data = autocar.refined, formula = NC_PD ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp_2 <- glm(data = autocar.refined, formula = NC_PD ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
# remove Qtr as well
tmp_3 <- glm(data = autocar.refined, formula = NC_PD ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_3)
anova(tmp_2, tmp_3, test = "F") 
# yes reomve QTR
glm$rd$Number$Model$PD <- tmp_3 ###################MODEL###########################--
remove(tmp, tmp_2)

# NC_COM model ----
tmp <- glm(data = autocar.refined, formula = NC_COM ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp_2 <- glm(data = autocar.refined, formula = NC_COM ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
# remove Qtr as well
tmp_3 <- glm(data = autocar.refined, formula = NC_COM ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_3)
anova(tmp_2, tmp_3, test = "F") 
# We include qtr
glm$rd$Number$Model$COM <- tmp_2 ##############################################--
remove(tmp, tmp_2, tmp_3)

# NC_COL model----
tmp <- glm(data = autocar.refined, formula = NC_COL ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp_2 <- glm(data = autocar.refined, formula = NC_COL ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
# remove Qtr as well
tmp_3 <- glm(data = autocar.refined, formula = NC_COL ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_3)
anova(tmp_2, tmp_3, test = "F") 
# We include qtr
glm$rd$Number$Model$COL <- tmp_2 ##############################################--
remove(tmp, tmp_2)

# NC_PI model ----
tmp <- glm(data = autocar.refined, formula = NC_PI ~  time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp)
# remove time
tmp_2 <- glm(data = autocar.refined, formula = NC_PI ~ factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_2)
# remove Qtr as well
tmp_3 <- glm(data = autocar.refined, formula = NC_PI ~ factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk) + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
summary(tmp_3)
anova(tmp_2, tmp_3, test = "F") 
# We remove qtr
glm$rd$Number$Model$PI <- tmp_3 ##############################################--
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
# remove Type
tmp_3 <- glm(data = autocar.refined, formula = AAC_BI ~ time + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_BI)
summary(tmp_3)
anova(tmp_3, tmp_2, test = "F") 
# we include remove Qtr and type
glm$rd$Amount$Model$BI <- tmp_3
remove(tmp, tmp_2, tmp_3)

# AAC_PD model ----
tmp <- glm(data = autocar.refined, formula = AAC_PD ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_PD)
summary(tmp)
tmp_2 <- glm(data = autocar.refined, formula = AAC_PD ~ time  + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_PD)
summary(tmp_2)
tmp_3 <- glm(data = autocar.refined, formula = AAC_PD ~ time + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_PD)
summary(tmp_3)
anova(tmp_3, tmp_2, test = "F") 
# we include remove qtr and type
glm$rd$Amount$Model$PD <- tmp_3
remove(tmp, tmp_2, tmp_3)

## AAC_COM ----
tmp <- glm(data = autocar.refined, formula = AAC_COM ~ time + factor(Qtr) + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_COM)
summary(tmp)
tmp_2 <- glm(data = autocar.refined, formula = AAC_COM ~ time  + factor(Type) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_COM)
summary(tmp_2)
# only remove type
tmp_3 <- glm(data = autocar.refined, formula = AAC_COM ~ time  + factor(Qtr) + factor(VehicleSize) + factor(DriverAge) + factor(DriverRisk), family = quasi(variance="mu", link = "log"), weights = NC_COM)
summary(tmp_3)
anova(tmp_3, tmp, test = "F") 
# we include Qtr remove Type
glm$rd$Amount$Model$COM <- tmp_3
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
# we remove qtr
glm$rd$Amount$Model$PI <- tmp_2
remove(tmp, tmp_2)








# Adjust COM ----
# Because we basically lowerd the amount and number of the COM coverage we need to adjust the intercept
# because in the next years we can have another natural disaster

ol <- sum(autocar$AAC_COM)
rd <- sum(autocar.refined$AAC_COM)
glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"] <- glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"]  + log(ol/rd)
remove(ol, rd)

ol <- sum(autocar$NC_COM)
rd <- sum(autocar.refined$NC_COM)

glm$rd$Number$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"] <- glm$rd$Number$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"]  + log(ol/rd)

remove(ol, rd)


# change time slope (interest to the average)
bi_i <- glm$rd$Amount$Model$BI$coefficients[names(glm$rd$Amount$Model$BI$coefficients) == "time"]
pd_i <- glm$rd$Amount$Model$PD$coefficients[names(glm$rd$Amount$Model$PD$coefficients) == "time"]
com_i <- glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "time"]
col_i <- glm$rd$Amount$Model$COL$coefficients[names(glm$rd$Amount$Model$COL$coefficients) == "time"]
pi_i <- glm$rd$Amount$Model$PI$coefficients[names(glm$rd$Amount$Model$PI$coefficients) == "time"]

mean_i <- mean(c(bi_i,pd_i,com_i,col_i,pi_i))

glm$rd$Amount$Model$BI$coefficients[names(glm$rd$Amount$Model$BI$coefficients) == "time"] <- mean_i
glm$rd$Amount$Model$PD$coefficients[names(glm$rd$Amount$Model$PD$coefficients) == "time"] <- mean_i
glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "time"] <- mean_i
glm$rd$Amount$Model$COL$coefficients[names(glm$rd$Amount$Model$COL$coefficients) == "time"] <- mean_i
glm$rd$Amount$Model$PI$coefficients[names(glm$rd$Amount$Model$PI$coefficients) == "time"] <- mean_i

# we also need to fix the intercept
glm$rd$Amount$Model$BI$coefficients[names(glm$rd$Amount$Model$BI$coefficients) == "(Intercept)"] <- glm$rd$Amount$Model$BI$coefficients[names(glm$rd$Amount$Model$BI$coefficients) == "(Intercept)"] + (bi_i-mean_i)*2009
glm$rd$Amount$Model$PD$coefficients[names(glm$rd$Amount$Model$PD$coefficients) == "(Intercept)"] <- glm$rd$Amount$Model$PD$coefficients[names(glm$rd$Amount$Model$PD$coefficients) == "(Intercept)"] + (pd_i-mean_i)*2009
glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"] <- glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "(Intercept)"] + (com_i-mean_i)*2020
glm$rd$Amount$Model$COL$coefficients[names(glm$rd$Amount$Model$COL$coefficients) == "(Intercept)"] <- glm$rd$Amount$Model$COL$coefficients[names(glm$rd$Amount$Model$COL$coefficients) == "(Intercept)"] + (col_i-mean_i)*2009
glm$rd$Amount$Model$PI$coefficients[names(glm$rd$Amount$Model$PI$coefficients) == "(Intercept)"] <-  glm$rd$Amount$Model$PI$coefficients[names(glm$rd$Amount$Model$PI$coefficients) == "(Intercept)"] + (pi_i-mean_i)*2009


