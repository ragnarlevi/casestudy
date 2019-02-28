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
                  safelife.prop.aut, freq.pct, loss.pct){
  # time.frame i the time frame we are considering
  # func.personal - differential function for personal
  # func.commerfcial - differential function for commercial
  # param.personal and param.commercial - paramteter for the differential function is a list of functions
  # exp.growth.xxxx - for differential equation How much is carbia autonomous market growing (Exposre for whole of carbia)
  # safelife.prop.aut - proportions that safelife wants
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
        tmp.exposure <- autonomyChange[[type]]$out[i, l]*safelife.prop.aut[[type]][safelife.prop.aut[[type]]$time == time, l]

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



freq.loss.tidy <- function(freq.pct, loss.pct){
  
  for(i in names(freq.pct)){
    freq.pct[[i]]$Autonomy <- i
    loss.pct[[i]]$Autonomy <- i
  }
  
  
  tmp <- full_join(do.call(what = rbind, args = freq.pct), do.call(what = rbind, args = loss.pct), by = c("time", "Autonomy"))
  
  tmp$Year <- 2018 + ceiling(tmp$time)
  tmp$Qtr <- (tmp$time * 4) %% 4
  tmp$Qtr[tmp$Qtr == 0] <- 4
  
 return(tmp[, !(names(tmp) %in% c("time")), drop = F])
  
}

# freq.loss.tidy(freq.pct = freq.pct, loss.pct = loss.pct)



model.2 <- function(time.frame, autocar, glm.list, safelife.market.share, carbia.exposure, carb.commercial.pct, carb.personal.pct, freq.pct, loss.pct){
  # time frame
  # autocar - original data frae
  # glm.list <- the glm list
  # safelife.market.share
  # carb.personal.pct 
  # carb.commercial.pct 
  # carbia.exposure
  
  lvls <- c("A0", "A1", "A2")
  # Let'ts make a data frame with the future year, Qtr, type, Risk Class, Autonomy columns
  tmp.df <- data.frame(Year = 2019 + time.frame)
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(Autonomy = lvls) , stringsAsFactors=FALSE))
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(Qtr = c(1, 2, 3, 4))))
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(RiskClass = unique(autocar$RiskClass)), stringsAsFactors=FALSE))
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(Type = unique(autocar$Type)), stringsAsFactors=FALSE))
  
  instances <- tmp.df
  rm(tmp.df)
  

  # estimates for A0 from the glm model
  # estimates.A0 <- A0initialParam(glm.list = glm.list)
  # claims.estimates <- autonomyPremium(estimates.A0 = estimates.A0, type = "same", time.frame = time.frame, start.year = 2019, lvls = lvls)
  # claims.estimates$RiskClass <- as.character(claims.estimates$RiskClass)
  
  # The Risk Classes are same for A0 and A1
  # For A2 we only consider vehicle size 
  
  # Create tmp data frame with autonomy
  
  # tmp <- claims.estimates[claims.estimates$Autonomy == "A2", ]
  # # remove then add back
  # claims.estimates <- claims.estimates[claims.estimates$Autonomy != "A2", ]
  # # Change  Risk Class
  # tmp$RiskClass[substr(tmp$RiskClass,1,1) == "S"] <- "Small" 
  # tmp$RiskClass[substr(tmp$RiskClass,1,1) == "M"] <- "Medium" 
  # tmp$RiskClass[substr(tmp$RiskClass,1,1) == "L"] <- "Large"
  # 
  # tmp <- aggregate(formula = . ~ Type + RiskClass + Qtr + Year + Autonomy, data = tmp, FUN = mean)  
  # 
  # 
  # claims.estimates <- rbind(claims.estimates, tmp)
  
  # calculate proportions
  df.main <-  autocar
  
  prop <- riskClassProp(df.main = autocar, time.frame = time.frame, start.year = 2019, lvls = lvls)
  # prop$RiskClass <- as.character(prop$RiskClass)
  # # we need to change according to the A2 new risk classes
  # 
  # tmp <- prop[prop$Autonomy == "A2", ]
  # # remove then add back
  # prop <- prop[prop$Autonomy != "A2", ]
  # # Change  Risk Class
  # tmp$RiskClass[substr(tmp$RiskClass,1,1) == "S"] <- "Small" 
  # tmp$RiskClass[substr(tmp$RiskClass,1,1) == "M"] <- "Medium" 
  # tmp$RiskClass[substr(tmp$RiskClass,1,1) == "L"] <- "Large"
  # 
  # tmp <- aggregate(formula = . ~ Type + RiskClass + Qtr + Year + Autonomy, data = tmp, FUN = sum)  
  # # then we need to divide we are assuming same every year
  # tmp.p.total <- sum(tmp$prop[tmp$Type == "Personal"  & tmp$Qtr == "1" & tmp$Year == 2019 & tmp$Autonomy == "A2"])
  # tmp.c.total <- sum(tmp$prop[tmp$Type == "Commercial"  & tmp$Qtr == "1" & tmp$Year == 2019 & tmp$Autonomy == "A2"])
  # 
  # tmp$prop[tmp$Type == "Personal"] <- tmp$prop[tmp$Type == "Personal"]/tmp.p.total
  # tmp$prop[tmp$Type == "Commercial"] <- tmp$prop[tmp$Type == "Commercial"]/tmp.c.total
  # 
  # prop <- rbind(prop, tmp)
  
  # next we set the exposure we transfrom the date frames so we can multiply them more easily
  exposure <- exposure.tidy(safelife.market.share = safelife.market.share, 
                            carbia.exposure = carbia.exposure, 
                            carb.commercial.pct = carb.commercial.pct, 
                            carb.personal.pct = carb.commercial.pct)
  
  # Finally we add stuff
  predict.df <- as.data.frame(full_join(instances, prop, by = c("Qtr","RiskClass", "Type", "Autonomy", "Year")))

  predict.df <- left_join(x = predict.df, y = exposure,  by = c("Qtr", "Type", "Autonomy", "Year"))
  # add time variable
  
  predict.df$time <- predict.df$Year + 2.5*predict.df$Qtr/10
  
  
  # Then we multiply prop with exposure to adjust the Risk Class exposure
  
  predict.df$Exposure <- predict.df$Exposure*predict.df$prop
  
  # Now we use the model to get the exposure, dive Risk Class into 3 columns and remove the original
  predict.df$VehicleSize <- ""
  predict.df$VehicleSize[grepl("^S", x = predict.df$RiskClass)] <- "S" 
  predict.df$VehicleSize[grepl("^M", x = predict.df$RiskClass)] <- "M" 
  predict.df$VehicleSize[grepl("^L", x = predict.df$RiskClass)] <- "L" 
  
  #creating factors for driver age variable
  predict.df$DriverAge <- ""
  predict.df$DriverAge[grepl("^.Y", x = predict.df$RiskClass)] <- "Y"
  predict.df$DriverAge[grepl("^.M", x = predict.df$RiskClass)] <- "M"
  predict.df$DriverAge[grepl("^.S", x = predict.df$RiskClass)] <- "S"
  
  #creating factors for driver risk variable
  predict.df$DriverRisk <- ""
  predict.df$DriverRisk[grepl("^..L", x = predict.df$RiskClass)] <- "L"
  predict.df$DriverRisk[grepl("^..A", x = predict.df$RiskClass)] <- "A"
  predict.df$DriverRisk[grepl("^..H", x = predict.df$RiskClass)] <- "H"
  
  predict.df <- predict.df[, !(names(predict.df) %in% "RiskClass")]
  

  
  for(i in lvls){
    # Then we predict
    # NC
    predict.df$NC_BI <- exp(predict(object = glm.list$Number$Model$BI, newdata = predict.df))
    predict.df$NC_PD <- exp(predict(object = glm.list$Number$Model$PD, newdata = predict.df))
    predict.df$NC_COM <- exp(predict(object = glm.list$Number$Model$COM, newdata = predict.df))
    predict.df$NC_COL <- exp(predict(object = glm.list$Number$Model$COL, newdata = predict.df))
    predict.df$NC_PI <- exp(predict(object = glm.list$Number$Model$PI, newdata = predict.df))
  
    # AAC
    predict.df$AAC_BI <- exp(predict(object = glm.list$Amount$Model$BI, newdata = predict.df))
    predict.df$AAC_PD <- exp(predict(object = glm.list$Amount$Model$PD, newdata = predict.df))
    predict.df$AAC_COM <- exp(predict(object = glm.list$Amount$Model$COM, newdata = predict.df))
    predict.df$AAC_COL <- exp(predict(object = glm.list$Amount$Model$COL, newdata = predict.df))
    predict.df$AAC_PI <- exp(predict(object = glm.list$Amount$Model$PI, newdata = predict.df))
    
    # Then multiply to find the amount
    
    predict.df$AC_BI <- predict.df$NC_BI*predict.df$AAC_BI
    predict.df$AC_PD <- predict.df$NC_PD*predict.df$AAC_PD
    predict.df$AC_COM <- predict.df$NC_COM*predict.df$AAC_COM
    predict.df$AC_COL <- predict.df$NC_COL*predict.df$AAC_COL
    predict.df$AC_PI <- predict.df$NC_PI*predict.df$AAC_PI
    
  }
  
  
  # loss and frequency multpliers
  multipliers <- freq.loss.tidy(freq.pct = freq.pct, loss.pct = loss.pct)
  
  predict.df <- left_join(x = predict.df, y = multipliers, by = c("Autonomy", "Year", "Qtr"))
  
  # Ajust frequency of claims and amount of claims
  predict.df$NC_BI <- predict.df$NC_BI*predict.df$NC_BI.pct
  predict.df$NC_PD <- predict.df$NC_PD*predict.df$NC_PD.pct
  predict.df$NC_COM <- predict.df$NC_COM*predict.df$NC_COM.pct
  predict.df$NC_COL <- predict.df$NC_COL*predict.df$NC_COL.pct
  predict.df$NC_PI <- predict.df$NC_PI*predict.df$NC_PI.pct
  
  predict.df$AAC_BI <- predict.df$AAC_BI*predict.df$AAC_BI.pct
  predict.df$AAC_PD <- predict.df$AAC_PD*predict.df$AAC_PD.pct
  predict.df$AAC_COM <- predict.df$AAC_COM*predict.df$AAC_COM.pct
  predict.df$AAC_COL <- predict.df$AAC_COL*predict.df$AAC_COL.pct
  predict.df$AAC_PI <- predict.df$AAC_PI*predict.df$AAC_PI.pct
  
  # A2 only has 3 Classes So we 
  
  predict.df$RiskClass <- paste(predict.df$VehicleSize, predict.df$DriverAge, predict.df$DriverRisk, sep = "")
  predict.df <- predict.df[, !(names(predict.df) %in% c("VehicleSize", "DriverAge", "DriverRisk"))]
  
  # Next we need to aggregate A2
  
  tmp <- predict.df[predict.df$Autonomy == "A2", ]
  # remove then add back
  predict.df <- predict.df[predict.df$Autonomy != "A2", ]
  # Change  Risk Class
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "S"] <- "Small" 
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "M"] <- "Medium" 
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "L"] <- "Large"
  
  # take mean
  tmp <- aggregate(formula = . ~ Type + RiskClass + Qtr + Year + Autonomy, data = tmp, FUN = mean)  
  # but we need to fix the proportion
  # then we need to divide we are assuming same every year
  tmp.p.total <- sum(tmp$prop[tmp$Type == "Personal"  & tmp$Qtr == "1" & tmp$Year == 2019 & tmp$Autonomy == "A2"])
  tmp.c.total <- sum(tmp$prop[tmp$Type == "Commercial"  & tmp$Qtr == "1" & tmp$Year == 2019 & tmp$Autonomy == "A2"])

  tmp$prop[tmp$Type == "Personal"] <- tmp$prop[tmp$Type == "Personal"]/tmp.p.total
  tmp$prop[tmp$Type == "Commercial"] <- tmp$prop[tmp$Type == "Commercial"]/tmp.c.total

  sum(tmp$prop[tmp$Type == "Personal" & tmp$Qtr == 1 & tmp$Year == 2019])
  predict.df <- rbind(predict.df, tmp)
  
  # Rbind the historical data but first we need to make sure the columns are the same

  tmp <- autocar
  tmp$Autonomy <- "A0"
  tmp <- findProportions(df.main = tmp)
  
  # We have to make sure the NC column is per exposure like  in the predict data frame
 
  # add multipliers, they are just one
  tmp$NC_BI.pct <- 1
  tmp$NC_PD.pct <- 1
  tmp$NC_COM.pct <- 1
  tmp$NC_COL.pct <- 1
  tmp$NC_PI.pct <- 1
  
  tmp$AAC_BI.pct <- 1
  tmp$AAC_PD.pct <- 1
  tmp$AAC_COM.pct <- 1
  tmp$AAC_COL.pct <- 1
  tmp$AAC_PI.pct <- 1
  
  
  
  
  # We need to have the followign column to use rbind but these information is missing
  tmp$carb.pct <- -99
  tmp$carbia_exposure <- -99
  tmp$safelife.pct <- -99
  
  tmp$time <- tmp$Year + 2.5*as.numeric(tmp$Qtr)/10
  tmp$Qtr <- as.numeric(tmp$Qtr)
  
  
  # make sure the columns match
  tmp <- tmp[, names(predict.df), drop = F]
  # bind them
  predict.df <- rbind(predict.df, tmp)
  
  return(predict.df)
  
}

exposure.tidy <- function( safelife.market.share, carbia.exposure, carb.commercial.pct, carb.personal.pct){
  
  safelife.market.share <- melt(safelife.market.share, id.vars = "time")
  names(safelife.market.share)[names(safelife.market.share) %in% c("variable")] <- "Autonomy"
  names(safelife.market.share)[names(safelife.market.share) %in% c("value")] <- "safelife.pct"
  
  carbia.exposure <-  melt(carbia.exposure, id.vars = "time")
  names(carbia.exposure)[names(carbia.exposure) %in% c("variable")] <- "Type"
  names(carbia.exposure)[names(carbia.exposure) %in% c("value")] <- "carbia_exposure"
  
  carb.commercial.pct <-  melt(carb.commercial.pct, id.vars = "time")
  names(carb.commercial.pct)[names(carb.commercial.pct) %in% c("variable")] <- "Autonomy"
  names(carb.commercial.pct)[names(carb.commercial.pct) %in% c("value")] <- "carb.pct"
  # add type
  carb.commercial.pct$Type <- "Commercial"
  carb.personal.pct <-  melt(carb.personal.pct, id.vars = "time")
  names(carb.personal.pct)[names(carb.personal.pct) %in% c("variable")] <- "Autonomy"
  names(carb.personal.pct)[names(carb.personal.pct) %in% c("value")] <- "carb.pct"
  carb.personal.pct$Type <- "Personal"
  
  # now we make one large 
  
  exposure <- as.data.frame(rbind(carb.commercial.pct, carb.personal.pct))
  exposure <- as.data.frame(full_join(x = exposure, y = carbia.exposure, by = c("Type", "time")))
  exposure <- as.data.frame(full_join(x = exposure, y = safelife.market.share, by = c("Autonomy", "time")))
  
  # then we multply the carb.pct with carbia eposure to get the "correct" carbia eposire
  
  exposure$carbia_exposure <- exposure$carbia_exposure*exposure$carb.pct
  # Then we fin the safelife exposure
  exposure$Exposure <- exposure$safelife.pct*exposure$carbia_exposure
  
  # Finally add Year and Quarter
  exposure$Year <- 2018 + ceiling(exposure$time)
  exposure$Qtr <- (exposure$time * 4) %% 4
  exposure$Qtr[exposure$Qtr == 0] <- 4
  
  return(exposure[, c("Autonomy", "carb.pct", "Type", "carbia_exposure", "safelife.pct", "Exposure", "Year", "Qtr"), drop = F])
}


history.tidy <- function(autocar){
  

  
  
  
}

