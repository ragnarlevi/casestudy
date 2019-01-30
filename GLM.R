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

# multiplot(p1,p2, plotlist = NULL, cols = 1)

# multiplot(p3,p4, plotlist = NULL, cols = 1)



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


# Next we need to get our estimates
library(dplyr) # library that contains data frames to join data frames


# Declare help functions
# CHeck if a numeric is empty or not
isEmpty <- function(x) {
  return(identical(x, numeric(0)))
}

calcEstimates <- function(df, model){
  # df data frame with our information
  # model, list of number or amount models
  
  
  # appends corresponding estimates to the df
  df$estimate <- 0
  
  for(i in 1:dim(df)[1]){
    # Just to get shorter name
   
    tmp <- as.data.frame(model[[df$coverage[i]]]$coefficient)
    names(tmp) <- c("coef")
    # intercept is always the same
    beta_intercept <-  tmp$coef[rownames(tmp) == "(Intercept)"]
    
    
    
    # Qtr
    tmp.qtr <- as.numeric(df$Qtr[i])
    tmp.qtr <- paste("Qtr",tmp.qtr, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.qtr])){
      # is empty
      beta_qtr <- 0
    }else{
      # not empy
      beta_qtr <- tmp$coef[rownames(tmp) == tmp.qtr]
    }
    
    # Type
    tmp.type <- as.character(df$Type[i])
    tmp.type <- paste("Type",tmp.type, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.type])){
      # is empty
      beta_type <- 0
    }else{
      # not empy
      beta_type <- tmp$coef[rownames(tmp) == tmp.type]
    }
    
    # VehicleSize
    # First letter
    tmp.size <- toupper(substr(df$RiskClass[i],1,1))
    tmp.size <- paste("VehicleSize",tmp.size, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.size])){
      # is empty
      beta_size <- 0
    }else{
      # not empy
      beta_size <- tmp$coef[rownames(tmp) == tmp.size]
    }
    
    # VehicleAge
    # second letter
    tmp.age <- toupper(substr(df$RiskClass[i],2,2))
    tmp.age <- paste("VehicleRisk",tmp.age, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.age])){
      # is empty
      beta_age <- 0
    }else{
      # not empy
      beta_age <- tmp$coef[rownames(tmp) == tmp.age]
    }
    
    
    
    # VehicleRisk
    # third letter
    tmp.risk <- toupper(substr(df$RiskClass[i],3,3))
    tmp.risk <- paste("VehicleRisk",tmp.risk, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.risk])){
      # is empty
      beta_risk <- 0
    }else{
      # not empy
      beta_risk <- tmp$coef[rownames(tmp) == tmp.risk]
    }
    
    
    # Finnaly we insert our esitmate
    df$estimate[i] <- exp(beta_intercept + beta_qtr + beta_type + beta_size + beta_age + beta_risk)
    
  }
  
  return(df)
}


GetEstimates <- function(Data.List, m){
  # Data.list for example gld$rd
  # m, "Number" or "Amount"


  
  # get namse of  coverage (policies)
  df.1 <- data.frame(coverage = names(Data.List[[m]]$Model))
  # Type
  df.2 <- data.frame(Type = unique(autocar$Type))
  # Risk Classes
  df.3 <- data.frame(RiskClass = unique(autocar$RiskClass))
  # quarters
  df.4 <- data.frame(Qtr = c(1,2,3,4))
  
  
  df <- merge(df.1, df.2)
  df <- merge(df, df.3)
  df <- merge(df, df.4)
  # should be 1080 rows....1080 estimates wooppyy
  
  df <- calcEstimates(df = df, model = Data.List[[m]]$Model)
  # change the Names
  if(m == "Number"){
    names(df)[names(df) == "estimate"] <- "NPerExposure"
  }else{
    names(df)[names(df) == "estimate"] <- "AAC"
  }
  
  return(df)

  
}






# Create functions that Can Will ltake different growth rates and list to speculate about the future

EstimateGrowth <- function(Data.List, growth, time.frame = 1, df){
  # Data.List is our list with one scenario. Exaple Data.List = glm$rd if we want to see how our refined data behaves
  # growth is a list if FUNCTIONS that explains the growth for each Risk Class
  # time.frame is vector/numeric measured in years for each year there are 4 quarters. Default value is prediction for one year
  # df is the dataframe we want to append our new predictions (probably will always be autocar raw data)
  
  
  
  # get estimates
  Data.List$Number$df <- GetEstimates(Data.List = Data.List, m = "Number")
  Data.List$Amount$df <- GetEstimates(Data.List = Data.List, m = "Amount")

  
  df.number <- Data.List$Number$df
  df.amount <- Data.List$Amount$df
  start.year <- max(as.numeric(df$Year))
  # append to Our data.Frame 
  for( i in time.frame){
    # This for loop counts time
    
    for(j in 1:dim(df.number)[1]){
      # Create tmp data.frame. This tmp df will be added to the prediction data frame once every missing value has been filled 
      tmp <- df[1,]
      tmp$Year <- start.year + time.frame[i]
      tmp$Qtr <- df.number$Qtr[j]
      tmp$RiskClass <- df.number$RiskClass[j]
      tmp$Type <- df.number$Type[j]
      # Here is where the fun part comes/ this is something we really don't know about
      bool <- df$Year == start.year & df$Qtr == tmp$Qtr & df$RiskClass == tmp$RiskClass & df$Type == tmp$Type
      tmp$Exposure <- growth(exposure = df$Exposure[bool])
      # add numbers
      # We need to get right row
      bool <- df.number$Type == tmp$Type & df.number$RiskClass == tmp$RiskClass & df.number$Qtr == tmp$Qtr
      tmp$NC_BI <- tmp$Exposure*df.number$NPerExposure[df.number$coverage == "BI" & bool]
      tmp$NC_PD <- tmp$Exposure*df.number$NPerExposure[df.number$coverage == "PD" & bool]
      tmp$NC_COM <- tmp$Exposure*df.number$NPerExposure[df.number$coverage == "COM" & bool]
      tmp$NC_COL <- tmp$Exposure*df.number$NPerExposure[df.number$coverage == "COL" & bool]
      tmp$NC_PI <- tmp$Exposure*df.number$NPerExposure[df.number$coverage == "PI" & bool]
      
      # and for the amount
      bool <- df.amount$Type == tmp$Type & df.amount$RiskClass == tmp$RiskClass & df.amount$Qtr == tmp$Qtr
      tmp$AC_BI <- df.amount$AAC[df.amount$coverage == "BI" & bool]*tmp$NC_BI
      tmp$AC_PD <- df.amount$AAC[df.amount$coverage == "PD" & bool]*tmp$NC_PD
      tmp$AC_COM <- df.amount$AAC[df.amount$coverage == "COM" & bool]*tmp$NC_COM
      tmp$AC_COL <- df.amount$AAC[df.amount$coverage == "COL" & bool]*tmp$NC_COL
      tmp$AC_PI <- df.amount$AAC[df.amount$coverage == "PI" & bool]*tmp$NC_PI
      
      
      # Finnaly append it to the data frame
      
      df <- rbind(df, tmp)
      
    }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  # We Return a list with the information needed
  ret <- list()
  ret$prediction <- df
  ret$Data.List <- Data.List
  return(ret)
  
}

growth <- function(t = 1, exposure){
  # growth rate is dependent on time
  
  
  return(exposure)
}
time.frame <- 1:10

test <- EstimateGrowth(Data.List = glm$rd, growth = growth, time.frame = time.frame, df = autocar)


# plot to test

tmp <- test$prediction
tmp$time <- tmp$Year + 2.5*as.numeric(tmp$Qtr)/10
# Compare
melt.tmp <- melt(tmp[, names(tmp) %in% c("time", "AC_COM", "RiskClass", "Type", "NC_COM")] , id = c("Type", "RiskClass", "time"))

# Figure For Personal types
p1 <- ggplot(melt.tmp[melt.tmp$Type == "Personal",]) + geom_line(aes(x = time, y = value, color = RiskClass)) +
  facet_wrap( ~ variable, scales = "free") + ggtitle("Personal raw data")
p1
