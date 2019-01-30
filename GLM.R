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
glm$ol$Number$Model$BI <- glm(data = autocar, formula = NC_BI ~ Qtr + Type + VehicleSize + DriverAge + DriverRisk + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$PD <- glm(data = autocar, formula = NC_PD ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$COM <- glm(data = autocar, formula = NC_COM ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$COL <- glm(data = autocar, formula = NC_COL ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$ol$Number$Model$PI <- glm(data = autocar, formula = NC_PI ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))


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
glm$ol$Amount$Model$BI <- glm(data = autocar, formula = AAC_BI~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_BI)
glm$ol$Amount$Model$PD <- glm(data = autocar, formula = AAC_PD~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_PD)
glm$ol$Amount$Model$COM <- glm(data = autocar, formula = AAC_COM~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_COM)
glm$ol$Amount$Model$COL <- glm(data = autocar, formula = AAC_COL~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_COL)
glm$ol$Amount$Model$PI <- glm(data = autocar, formula = AAC_PI~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_PI)

summary(glm$ol$Amount$Model$PI)


# linear Regression
autocar$time <- autocar$Year + 2.5*as.numeric(autocar$Qtr)/10

autocar$time

melt.autocar <- melt(autocar[, names(autocar) %in% c("time", "AC_COM", "RiskClass", "Type", "NC_COM")] , id = c("Type", "RiskClass", "time"))
ggplot(melt.autocar[melt.autocar$Type == "Commercial",]) + geom_line(aes(x = time, y = value, color = RiskClass))  + facet_wrap( ~ variable)

# linear regression for amount comprehensive and number comprehnsive
lm.no.outlier <- list()
autocar.tmp <- autocar[autocar$time != 2010.75,]
#View(autocar.tmp)

library(reshape2)
library(ggplot2)
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

# borrowed this function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2, plotlist = NULL, cols = 1)

multiplot(p3,p4, plotlist = NULL, cols = 1)



#Perform GLM's for refined data (after adjusting the outlier)

#Model number of claims in every claim category
glm$rd$Number$Model$BI <- glm(data = autocar.refined, formula = NC_BI ~ Qtr + Type + VehicleSize + DriverAge + DriverRisk + offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$rd$Number$Model$PD <- glm(data = autocar.refined, formula = NC_PD ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$rd$Number$Model$COM <- glm(data = autocar.refined, formula = NC_COM ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$rd$Number$Model$COL <- glm(data = autocar.refined, formula = NC_COL ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))
glm$rd$Number$Model$PI <- glm(data = autocar.refined, formula = NC_PI ~ Qtr + Type + VehicleSize+DriverAge+DriverRisk+ offset(log(Exposure)), family = quasi(variance="mu", link = "log"))

#show results
glm$rd
summary(glm$ol$Number$Model$BI)

#Model average amounts using gamma glm (for refined data)
glm$rd$Amount$Model$BI <- glm(data = autocar.refined, formula = AAC_BI~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_BI)
glm$rd$Amount$Model$PD <- glm(data = autocar.refined, formula = AAC_PD~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_PD)
glm$rd$Amount$Model$COM <- glm(data = autocar.refined, formula = AAC_COM~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_COM)
glm$rd$Amount$Model$COL <- glm(data = autocar.refined, formula = AAC_COL~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_COL)
glm$rd$Amount$Model$PI <- glm(data = autocar.refined, formula = AAC_PI~Qtr+Type+VehicleSize+DriverAge+DriverRisk, family = quasi(variance="mu^2", link = "log"), weights = NC_PI)

summary(glm$rd$Amount$Model$PI)

