# Declare help functions


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


# CHeck if a numeric is empty or not
isEmpty <- function(x) {
  return(identical(x, numeric(0)))
}

calcEstimates <- function(df.main, model){
  # df.main data frame with our information
  # model, list of number or amount models

  
  df.main$estimate <- 0
  
  
  for(i in 1:dim(df.main)[1]){
    # Just to get shorter name
    
    tmp.coverage <- as.character(df.main$coverage[i])
    # tmp.coverage <- "COL"
    tmp <- as.data.frame(model[[tmp.coverage]]$coefficient)
    names(tmp) <- c("coef")
    # intercept is always the same
    beta_intercept <-  tmp$coef[rownames(tmp) == "(Intercept)"]
    
    
    
    # Qtr
    tmp.qtr <- as.numeric(df.main$Qtr[i])
    tmp.qtr <- paste("Qtr",tmp.qtr, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.qtr])){
      # is empty
      beta_qtr <- 0
    }else{
      # not empy
      beta_qtr <- tmp$coef[rownames(tmp) == tmp.qtr]
    }
    
    # Type
    tmp.type <- as.character(df.main$Type[i])
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
    tmp.size <- toupper(substr(df.main$RiskClass[i],1,1))
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
    tmp.age <- toupper(substr(df.main$RiskClass[i],2,2))
    tmp.age <- paste("DriverAge",tmp.age, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.age])){
      # is empty
      beta_age <- 0
    }else{
      # not empy
      beta_age <- tmp$coef[rownames(tmp) == tmp.age]
    }
    
    
    
    # VehicleRisk
    # third letter
    tmp.risk <- toupper(substr(df.main$RiskClass[i],3,3))
    tmp.risk <- paste("DriverRisk",tmp.risk, sep ="")
    if(isEmpty(tmp$coef[rownames(tmp) == tmp.risk])){
      # is empty
      beta_risk <- 0
    }else{
      # not empy
      beta_risk <- tmp$coef[rownames(tmp) == tmp.risk]
    }
    
    
    # Finnaly we insert our esitmate
    df.main$estimate[i] <- exp(beta_intercept + beta_qtr + beta_type + beta_size + beta_age + beta_risk)
    
  }
  
  return(df.main)
}


GetEstimates <- function(Data.List, m){
  # Data.list for example gld$rd
  # m, "Number" or "Amount"
  
  # coverage
  df.1 <- data.frame(coverage = names(Data.List[[m]]$Model))
  # Type
  df.2 <- data.frame(Type = unique(autocar$Type))
  # Risk Classes
  df.3 <- data.frame(RiskClass = unique(autocar$RiskClass))
  # quarters
  df.4 <- data.frame(Qtr = c("1","2","3","4"))
  
  df.main <- merge(df.1, df.2)
  df.main <- merge(df.main, df.3)
  df.main <- merge(df.main, df.4)
  df.main <- as.data.frame(df.main)
  # df.main$VehicleSize <- substr(df.main$RiskClass,1,1)
  # df.main$DriverAge <- substr(df.main$RiskClass,2,2)
  # df.main$DriverRisk <- substr(df.main$RiskClass,3,3)
  # df.main$Exposure <- 1
  # predict( object = model$BI , newdata =df.main)
  
  
  df.main <- calcEstimates(df.main = df.main, model = Data.List[[m]]$Model)
  # change the Names
  if(m == "Number"){
    names(df.main)[names(df.main) == "estimate"] <- "NPerExposure"
  }else{
    names(df.main)[names(df.main) == "estimate"] <- "AAC"
  }
  
  return(df.main)
  
  
}
loop.fun <- function(j, i, df.main, loop.df, growth, start.year, df.number, df.amount, exposure){
  tmp <- df.main[1,]
  tmp$Year <- start.year + time.frame[i]
  tmp$Qtr <- loop.df$Qtr[j]
  tmp$RiskClass <- as.character(loop.df$RiskClass[j])
  tmp$Type <- as.character(loop.df$Type[j])
  tmp$Autonomy <-  loop.df$Autonomy[j]
  
  
  myvar <<- tmp
  # Here is where the fun part comes/ this is something we really don't know about
   tmp$Exposure <- growth(Type = as.character(tmp$Type),
                         Qtr = as.numeric(tmp$Qtr),
                         RiskClass = as.character(tmp$RiskClass),
                         Year = tmp$Year,
                         exposure = exposure,
                         Autonomy = as.character(tmp$Autonomy)
                         )
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
  return(tmp)
}


EstimateGrowth <- function(Data.List, growth, time.frame = 0, df.main, lapply.Fun, exposure = exposure){
  # Data.List is our list with one scenario. Exaple Data.List = glm$rd if we want to see how our refined data behaves
  # growth is a list if FUNCTIONS that explains the growth for each Risk Class
  # time.frame is vector/numeric measured in years for each year there are 4 quarters. Default value is prediction for one year
  # df is the dataframe we want to append our new predictions (probably will always be autocar raw data)
  
  
  
  # get estimates
  Data.List$Number$df <- GetEstimates(Data.List = Data.List, m = "Number")
  Data.List$Amount$df <- GetEstimates(Data.List = Data.List, m = "Amount")
  
  
  
  
  df.number <- Data.List$Number$df
  df.amount <- Data.List$Amount$df
  start.year <- max(as.numeric(df.main$Year))
  
  # this data frame includes what we want to loo
  df.1 <- data.frame(Type = unique(df.main$Type))
  df.2 <- data.frame(RiskClass = unique(df.main$RiskClass))
  df.3 <- data.frame(Qtr = c(1,2,3,4))
  df.4 <- data.frame(Autonomy = c("A0", "A1", "A2", "A3", "A4", "A5"))
  
  loop.df <- merge(df.1,df.2)
  loop.df <- merge(loop.df, df.3)
  loop.df <- merge(loop.df, df.4)
  loop.df <- as.data.frame(loop.df)
  
  # append to Our data.Frame 
  for( i in time.frame){
    # This for loop counts time
    # lapply much faster then a for loop
    r <- lapply(X = 1:dim(loop.df)[1], 
                FUN = loop.fun, i = i, 
                df.main = df.main, 
                loop.df = loop.df,
                growth = growth, 
                start.year = start.year,
                df.amount = df.amount,
                df.number = df.number,
                exposure = exposure)
    
    names(df.main)
    names(do.call(rbind,r))
    df.main <-  rbind(df.main, do.call(rbind,r))
    
    
  }
  
  
  # View(df.main[df.main$RiskClass == "SML" & df.main$Type == "Personal", names(df.main) %in% c("Exposure", "Year")])
  
  # We Return a list with the information needed
  ret <- list()
  ret$prediction <- df.main
  ret$Data.List <- Data.List
  return(ret)
  
}


findProportions <- function(df.main, years = NULL){
  
  df.main$prop <- 0
  df.main$Qtr <- as.numeric(df.main$Qtr)
  
  
  if(is.null(years)){
    years <- unique(df.main$Year) 
  }
  for(i in years){
    for(j in 1:4){
      te.p <- sum(df.main$Exposure[df.main$Qtr == j & df.main$Year == i & df.main$Type == "Personal"])
      te.c <- sum(df.main$Exposure[df.main$Qtr == j & df.main$Year == i & df.main$Type == "Commercial"])
      df.main$prop[df.main$Qtr == j & df.main$Type == "Personal" & df.main$Year == i] <-  df.main$Exposure[df.main$Qtr == j & df.main$Type == "Personal" & df.main$Year == i]/te.p
      df.main$prop[df.main$Qtr == j & df.main$Type == "Commercial" & df.main$Year == i] <-  df.main$Exposure[df.main$Qtr == j & df.main$Type == "Commercial" & df.main$Year == i]/te.c
      
    
      }
    
  }
  return(df.main)

  
}


plot.func <- function(predict.df){
  # predict.df - the main data frame
  
  plots <- list()
  
  # Autonomy evolution
  tmp <- predict.df[, names(predict.df) %in% c("time", "Exposure", "Autonomy")]
  tmp <- aggregate(formula = . ~ time + Autonomy, data = tmp, FUN = sum)
  plots$Autonomy.evolution <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure, color = Autonomy)) + ggtitle("Total Autonomy evolution") + theme(plot.title = element_text(size = 10))
  
  # plots$Autonomy.evolution
  
  # Personal evolution
  tmp <- predict.df[predict.df$Type == "Personal", names(predict.df) %in% c("time", "Exposure", "Autonomy")]
  tmp <- aggregate(formula = . ~ time + Autonomy, data = tmp, FUN = sum)
  plots$Autonomy.evolution.personal <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure, color = Autonomy)) + ggtitle("Total Autonomy evolution for personal") + theme(plot.title = element_text(size = 10))
  
  # plots$Autonomy.evolution.personal
  
  # Commercial evolution
  tmp <- predict.df[predict.df$Type == "Commercial", names(predict.df) %in% c("time", "Exposure", "Autonomy")]
  tmp <- aggregate(formula = . ~ time + Autonomy, data = tmp, FUN = sum)
  plots$Autonomy.evolution.commercial <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure, color = Autonomy)) + ggtitle("Total Autonomy commercial") + theme(plot.title = element_text(size = 10))
  #plots$Autonomy.evolution.commercial
  
  # PLot the Autonomy evolutions
  
  # multiplot(plots$Autonomy.evolution.personal, plots$Autonomy.evolution, plots$Autonomy.evolution.commercial, cols = 2)
  
  
  # Plot Claims evolution personal
  tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
  tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = sum)
  tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))
  
  plots$frequency.Personal <- ggplot(data = tmp.melt[tmp.melt$Type == "Personal", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Personal frequency evolution")
  
  # plot amount evolution personal
  tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "AAC_BI", "AAC_PD", "AAC_COM", "AAC_COL", "AAC_PI")]
  tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = sum)
  tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))
  
  plots$amount.Personal <- ggplot(data = tmp.melt[tmp.melt$Type == "Personal", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Personal mount evolution")

  # Plot Claims evolution Commercial
  tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
  tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = sum)
  tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))
  
  plots$frequency.Commercial <- ggplot(data = tmp.melt[tmp.melt$Type == "Commercial", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Commercial frequency evolution")
  
  # plot amount evolution Commercial
  tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "AAC_BI", "AAC_PD", "AAC_COM", "AAC_COL", "AAC_PI")]
  tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = sum)
  tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))
  
  plots$amount.Commercial <- ggplot(data = tmp.melt[tmp.melt$Type == "Commercial", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Commercial amount evolution")
  

  
  return(plots)
  
}








model <- function(time.frame, func.personal, func.commercial, param.personal, param.commercial,exp.growth.personal, exp.growth.commercial,
                  safeline.prop.aut, freq.pct, loss.pct){
  # time.frame i the time frame we are considering
  # func.personal - differential function for personal
  # func.commerfcial - differential function for commercial
  # param.personal and param.commercial - paramteter for the differential function is a list of functions
  # exp.growth.xxxx - for differential equation How much is carbia autonomous market growing (Exposre for whole of carbia)
  # safeline.prop.aut - proportions that safelife wants
  # freq.pct - frequency loss/gains for the autonomous levels for each coverage
  # loss.pct - amount pct loss/gains for aut lvels for each coverage
  
  
  
  # General functions call
  source("functions.R")
  # GLM - parameters
  
  # Call the GLM.R script, estimates number and AAC for current automoblies. Also
  source("GLM.R")
  
  
  # Premiums for each autonomu class
  source("autonomyClaimAmounts.R")
  # estimates for A0 from the glm model
  estimates.A0 <- A0initialParam(glm.list = glm, refine = "rd") 
  claims.estimates <- autonomyPremium(estimates.A0 = estimates.A0, type = "same", time.frame = time.frame, start.year = 2019, lvls = lvls)
  
  # Proportions of Risk Classes within autonomy classes
  source("RiskClassProp.R")
  df.main <- autocar
  prop <- riskClassProp(df.main, time.frame = time.frame, start.year = 2019, lvls = lvls)
  
  # Autonomy Proportions
  source("autonomyProportions.R")
  # initial value
  
  mark.share <- 0.34 # Given
  
  # Estimation for whole of Carbia
  personal.exposure <- sum(df.main$Exposure[df.main$Year == 2018 & df.main$Qtr == 4 & df.main$Type == "Personal"])/mark.share
  commercial.exposure <- sum(df.main$Exposure[df.main$Year == 2018 & df.main$Qtr == 4 & df.main$Type == "Commercial"])/mark.share
  # create initial exposure for personal
  
  init.personal <- rep(x = 0, length = length(lvls))
  names(init.personal) <- lvls
  init.personal[1] <- personal.exposure
  
  autonomyChange <- list()
  autonomyChange$Personal <- autonomyRateContinous(time.frame = time.frame, func = func.personal, param = param.personal, init = init.personal, exp.growth = exp.growth.personal)
  #autonomyChange$Personal$plot
  
  # Commercial exposure
  # initial vector has to have the right names
  init.commercial <- rep(x = 0, length = length(lvls))
  names(init.commercial) <- lvls
  init.commercial[1] <- commercial.exposure
  
  autonomyChange$Commercial <-  autonomyRateContinous(time.frame = time.frame, func = func.commercial, param = param.commercial, init = init.commercial, exp.growth = exp.growth.commercial)
  
  
  # Now we basically have everything, we can put it into one data frame
  
  # we need to make sure the data types are the same within every data frame when we join them
  claims.estimates$Qtr <- as.numeric(claims.estimates$Qtr)
  claims.estimates$RiskClass <- as.character(claims.estimates$RiskClass)
  prop$Qtr <- as.numeric(prop$Qtr)
  
  predict.df <- full_join(claims.estimates, prop, by = c("Qtr","RiskClass", "Type", "Autonomy", "Year"))
  # create the time column
  predict.df$time <- predict.df$Year + 2.5*as.numeric(predict.df$Qtr)/10
  
  # Then add the autonomy exposure. Like the data frame is set up It has to be done with loop/lapply, unfortunately
  # Start by initalizing columns
  # t is what we will loop over, by = 0.25 because the 0.25 is quarter, we actually need to start at time 0.25 which is year 2019 Qtr 1
  
  # INITIALIZE THE COLUMN
  predict.df$Exposure <- 0

  for(type in unique(predict.df$Type)){
    tmp <- autonomyChange[[type]]$out$time
    # start at 2 because 1 is the inital exposure start point which we already know
    for(i in 2:length(tmp)){
      # adjust time to year and quarter
      time <- tmp[i]

      year <- 2018 + ceiling(time)
      qtr <- (time * 4) %% 4
      if(qtr == 0){ qtr <-  4}

      for(l in lvls){
        # multiply exposure with marketshare
        tmp.exposure <- autonomyChange[[type]]$out[i, l]*safeline.prop.aut[[type]][safeline.prop.aut[[type]]$time == time, l]

        predict.df$Exposure[predict.df$Qtr == qtr & predict.df$Year == year & predict.df$Type == type & predict.df$Autonomy == l] <- tmp.exposure


      }
    }
  }
  
  
  # Ajust frequency of claims and amount of claims
  for(i in lvls){
    predict.df$NC_BI[predict.df$Autonomy == i] <- predict.df$NC_BI[predict.df$Autonomy == i]*freq.pct[[i]]$BI
    predict.df$NC_PD[predict.df$Autonomy == i] <- predict.df$NC_PD[predict.df$Autonomy == i]*freq.pct[[i]]$PD
    predict.df$NC_COM[predict.df$Autonomy == i] <- predict.df$NC_COM[predict.df$Autonomy == i]*freq.pct[[i]]$COM
    predict.df$NC_COL[predict.df$Autonomy == i] <- predict.df$NC_COL[predict.df$Autonomy == i]*freq.pct[[i]]$COL
    predict.df$NC_PI[predict.df$Autonomy == i] <- predict.df$NC_PI[predict.df$Autonomy == i]*freq.pct[[i]]$PI
    
    predict.df$AAC_BI[predict.df$Autonomy == i] <- predict.df$AAC_BI[predict.df$Autonomy == i]*freq.pct[[i]]$BI
    predict.df$AAC_PD[predict.df$Autonomy == i] <- predict.df$AAC_PD[predict.df$Autonomy == i]*freq.pct[[i]]$PD
    predict.df$AAC_COM[predict.df$Autonomy == i] <- predict.df$AAC_COM[predict.df$Autonomy == i]*freq.pct[[i]]$COM
    predict.df$AAC_COL[predict.df$Autonomy == i] <- predict.df$AAC_COL[predict.df$Autonomy == i]*freq.pct[[i]]$COL
    predict.df$AAC_PI[predict.df$Autonomy == i] <- predict.df$AAC_PI[predict.df$Autonomy == i]*freq.pct[[i]]$PI
  }
  
  
  # We need to adjust the exposure by the prop column
  predict.df$Exposure <- predict.df$Exposure*predict.df$prop
  # Next we append our current data so we can plot some graphs representing the forecast
  # names(predict.df)
  
  df.main <- autocar[, names(autocar) %in% c("Year", "Qtr", "RiskClass", "Type", "Exposure", "NC_BI",
                                             "NC_PD", "NC_COM", "NC_COL", "NC_PI", "AAC_BI", "AAC_PD",
                                             "AAC_COM", "AAC_COL", "AAC_PI")]
  # Find the proportion
  df.main <- findProportions(df.main = df.main)
  
  # We have to make sure the NC column is per exposure like the predict colum
  df.main$NC_BI <- df.main$NC_BI/df.main$Exposure
  df.main$NC_PD <- df.main$NC_PD/df.main$Exposure
  df.main$NC_COM <- df.main$NC_COM/df.main$Exposure
  df.main$NC_COL <- df.main$NC_COL/df.main$Exposure
  df.main$NC_PI <- df.main$NC_PI/df.main$Exposure
  
  df.main$Autonomy <- "A0"
  
  # rearrange so they have the same data types
  df.main$time <- df.main$Year + 2.5*as.numeric(df.main$Qtr)/10
  df.main <- df.main[,names(predict.df)]
  df.main$Qtr <- as.numeric(df.main$Qtr)
  
  
  # Finally combine the data.frames
  
  predict.df <- rbind(df.main, predict.df)
  
  
  
  # return list
  ret <- list()
  ret$df <- predict.df
  ret$autonomyChange <- autonomyChange
  
  return(ret)
}
