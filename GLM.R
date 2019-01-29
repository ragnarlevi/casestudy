library(readxl)
autocar <- read_excel("2019-student-research-case-study-data.xlsx", range = "B10:P2170")
attach(autocar)

names(autocar)
names(autocar) <- c("Year", "Qtr", "RiskClass", "Type", "Exposure", "NC_BI", "NC_PD", "NC_COM", "NC_COL", 
                       "NC_PI", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI")

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

autocar$Qtr <- as.factor(autocar$Qtr)
autocar$Type <- as.factor(autocar$Type)
autocar$VehicleSize <- as.factor(autocar$VehicleSize)
autocar$DriverAge <- as.factor(autocar$DriverAge)
autocar$DriverRisk <- as.factor(autocar$DriverRisk)

#We make a list of glm's (outlier included) divided into raw data (outlier) and refined data
glm <- list()

#Model number of claims in every claim category
glm$ol$Number$Model$BI <- glm(data = autocar, formula = NC_BI~Qtr+Type+VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = poisson(link = "log"))
glm$ol$Number$Model$PD <- glm(data = autocar, formula = NC_PD~Qtr+Type+VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = poisson(link = "log"))
glm$ol$Number$Model$COM <- glm(data = autocar, formula = NC_COM~Qtr+Type+VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = poisson(link = "log"))
glm$ol$Number$Model$COL <- glm(data = autocar, formula = NC_COL~Qtr+Type+VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = poisson(link = "log"))
glm$ol$Number$Model$PI <- glm(data = autocar, formula = NC_PI~Qtr+Type+VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = poisson(link = "log"))

#show results
glm$ol
summary(glm$ol$Number$Model$BI)


#First, we create a new variable called average claim amount = Total Claim Amount / Number of Claims
autocar$AAC_BI <- autocar$AC_BI/autocar$NC_BI
autocar$AAC_PD <- autocar$AC_PD/autocar$NC_PD
autocar$AAC_COM <- autocar$AC_COM/autocar$NC_COM
autocar$AAC_COL <- autocar$AC_COL/autocar$NC_COL
autocar$AAC_PI <- autocar$AC_PI/autocar$NC_PI


#Model average amounts using gamma glm
glm$ol$Amount$Model$BI <- glm(data = autocar, formula = AAC_BI~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = Gamma(link = "log"), weights = NC_BI)
glm$ol$Amount$Model$PD <- glm(data = autocar, formula = AAC_PD~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = Gamma(link = "log"), weights = NC_PD)
glm$ol$Amount$Model$COM <- glm(data = autocar, formula = AAC_COM~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = Gamma(link = "log"), weights = NC_COM)
glm$ol$Amount$Model$COL <- glm(data = autocar, formula = AAC_COL~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = Gamma(link = "log"), weights = NC_COL)
glm$ol$Amount$Model$PI <- glm(data = autocar, formula = AAC_PI~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = Gamma(link = "log"), weights = NC_PI)

summary(glm$ol$Amount$Model$PI)

#
