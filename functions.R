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

# Function that calculations proportion of risk classes 
findProportions <- function(df.main, years = NULL){
  
  # initialize column
  df.main$prop <- 0
  df.main$Qtr <- as.numeric(df.main$Qtr)
  
  # if null we find proprotion for all the years
  if(is.null(years)){
    years <- unique(df.main$Year) 
  }
  
  # for loop that calculates the proportions for each type
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

# This function takes in the frequecny and amount multipliers lists 
# and makes a data frame out of it for easy table join
freq.loss.tidy <- function(freq.pct, loss.pct){
  # freq.pct - frequency multipliers list
  # loss.pct - loss multipliers list
  
  # add a column to the data frames in the list called autonomy as that is one key of the main data frame
  # used in the function model.2
  for(i in names(freq.pct)){
    freq.pct[[i]]$Autonomy <- i
    loss.pct[[i]]$Autonomy <- i
  }
  
  
  tmp <- inner_join(do.call(what = rbind, args = freq.pct), do.call(what = rbind, args = loss.pct), by = c("time", "Autonomy"))
  
  # add Year and Qtr as they are keys in the main table
  tmp$Year <- 2018 + ceiling(tmp$time)
  tmp$Qtr <- (tmp$time * 4) %% 4
  tmp$Qtr[tmp$Qtr == 0] <- 4
  
 return(tmp[, !(names(tmp) %in% c("time")), drop = F])
  
}


# main function
model.2 <- function(time.frame, autocar, glm.list, safelife.market.share, carbia.exposure, carb.commercial.pct, carb.personal.pct, freq.pct, loss.pct, MR.fac.a2 , IS.fac.a2 , CR.fac.a2, interest, MR.fac.a1 , IS.fac.a1 , CR.fac.a1, sl.enter.year, sl.enter.qtr ){
  # time frame
  # autocar - original data frae
  # glm.list <- the glm list used for prediction
  # safelife.market.share
  # carb.personal.pct 
  # carb.commercial.pct 
  # carbia.exposure
  # freq.pct - list of frequency multipliers 
  # loss.pct - list of loss multipliers
  # MR.fac.a2 - malfunction risk A2 multiplieres
  # IS.fac.a2 - infrastructure risk a2 multipliers
  # CR.fac.a2 - cyber risk a2 multipliers
  # interest - inflation used
  # MR.fac.a1 - malfunction risk A1 multiplieres
  # IS.fac.a1 - infrastructure risk a1 multipliers
  # CR.fac.a1 - cyber risk a1 multipliers
  # sl.enter.year - safelife entrance year
  # sl.enter.qtr  - safelife entrance year
  
  # define levels
  lvls <- c("A0", "A1", "A2")
  # Let'ts make a data frame with the future year, Qtr, type, Risk Class, Autonomy columns
  tmp.df <- data.frame(Year = 2019 + time.frame)
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(Autonomy = lvls) , stringsAsFactors=FALSE))
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(Qtr = c(1, 2, 3, 4))))
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(RiskClass = unique(autocar$RiskClass)), stringsAsFactors=FALSE))
  tmp.df <- as.data.frame(merge(tmp.df, data.frame(Type = unique(autocar$Type)), stringsAsFactors=FALSE))
  
  instances <- tmp.df
  rm(tmp.df)
  
  
  # calculate proportions
  # this code takes a verage of the history proprotions
  ##########
  # prop <- riskClassProp(df.main = autocar, time.frame = time.frame, start.year = 2019, lvls = lvls)
  # predict.df <- inner_join(instances, prop, by = c("Qtr","RiskClass", "Type", "Autonomy", "Year"))
  ###########
  # This tokes takes the most recent observed proportions
  ##########################
  prop <- findProportions(df.main = autocar)
  # select only what we want
  prop <- prop[prop$Year == 2018 & prop$Qtr == 4, names(prop) %in% c("RiskClass", "Type", "prop")]
  # Then join
  predict.df <- inner_join(prop, instances, by = c("RiskClass", "Type"))
  ###########################
  
  
  # We start bultind the main data frame which includes everythin
  # It is important to note that Qtr, RiskClass, Type, Autonomy and Year 
  
 
  
  # get exposure from carbia and safelife number
  exposure <- exposure.tidy(safelife.market.share = safelife.market.share, 
                            carbia.exposure = carbia.exposure, 
                            carb.commercial.pct = carb.commercial.pct, 
                            carb.personal.pct = carb.personal.pct)
  
  # We need to remove set exposure to zero if safelife has not entered the market
  exposure$Exposure[exposure$Year < sl.enter.year & exposure$Autonomy != "A0"] <- 0
  # and then remove Quarter
  exposure$Exposure[exposure$Year == sl.enter.year & exposure$Qtr < sl.enter.qtr & exposure$Autonomy != "A0"] <- 0
  
  
  
  predict.df <- left_join(x = predict.df, y = exposure,  by = c("Qtr", "Type", "Autonomy", "Year"))
  # add time variable
  predict.df$time <- predict.df$Year + 2.5*predict.df$Qtr/10
  
  
  # Then we multiply prop with exposure to adjust the Risk Class exposure
  # this is basically exposure breakdown to riskclasses and type
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
  
  # remove RiskClass for 
  predict.df <- predict.df[, !(names(predict.df) %in% "RiskClass")]
  

  # Do the prediction
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
  
  # Remove columns we don't need
  predict.df <- predict.df[, !(names(predict.df) %in% c("carb.pct", "carbia_exposure", "safelife.pct"))]
  
  # loss and frequency multpliers
  multipliers <- freq.loss.tidy(freq.pct = freq.pct, loss.pct = loss.pct)
  
  # add the multiplers to the 
  predict.df <- left_join(x = predict.df, y = multipliers, by = c("Autonomy", "Year", "Qtr"))
  
  # Ajust frequency of claims and amount of claims for the
  # autonomy levels, the multipliers are 1 for A0
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
  
  # Then multiply to find the total amount
  predict.df$AC_BI <- predict.df$NC_BI*predict.df$AAC_BI
  predict.df$AC_PD <- predict.df$NC_PD*predict.df$AAC_PD
  predict.df$AC_COM <- predict.df$NC_COM*predict.df$AAC_COM
  predict.df$AC_COL <- predict.df$NC_COL*predict.df$AAC_COL
  predict.df$AC_PI <- predict.df$NC_PI*predict.df$AAC_PI
  
  # add the Risk Class back
  predict.df$RiskClass <- paste(predict.df$VehicleSize, predict.df$DriverAge, predict.df$DriverRisk, sep = "")
  predict.df <- predict.df[, !(names(predict.df) %in% c("VehicleSize", "DriverAge", "DriverRisk"))]
  
  
  # A2 only has 3 Classes So we need to aggregate with mean and sum

  # tmp will hold the changes
  tmp <- predict.df[predict.df$Autonomy == "A2", ]
  # remove A2 temporarily from the main data frame and then add it back once it is finished
  predict.df <- predict.df[predict.df$Autonomy != "A2", ]
  # Change  Risk Class
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "S"] <- "Small"
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "M"] <- "Medium"
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "L"] <- "Large"

  # we need to change the NC to frequency per exposure
  
  tmp$NC_BI <- tmp$NC_BI/tmp$Exposure
  tmp$NC_PD <- tmp$NC_PD/tmp$Exposure
  tmp$NC_COM <- tmp$NC_COM/tmp$Exposure
  tmp$NC_COL <- tmp$NC_COL/tmp$Exposure
  tmp$NC_PI <- tmp$NC_PI/tmp$Exposure
  
  
  # tmp.1 includes proportions and Exposure along wit the keys, they sum when aggregated
  tmp.1 <- tmp[, c("Year", "Autonomy", "Qtr", "Type", "time", "RiskClass", "Exposure", "prop")]
  tmp.1 <- aggregate(. ~ Year + Autonomy + Qtr + Type + time + RiskClass, data = tmp.1, FUN = sum)
  # tmp.2 includes the coverages which should be averaged
  tmp.2 <- tmp[, !(names(tmp) %in% c("Exposure", "prop"))]
  tmp.2 <- aggregate(. ~ Year + Autonomy + Qtr + Type + time + RiskClass, data = tmp.2, FUN = mean)
  

  # join them
  tmp <- inner_join(x = tmp.1, y = tmp.2, by = c("Year", "Autonomy", "Qtr", "Type", "time", "RiskClass"))
  
  # get number of claims again
  tmp$NC_BI <- tmp$NC_BI*tmp$Exposure
  tmp$NC_PD <- tmp$NC_PD*tmp$Exposure
  tmp$NC_COM <- tmp$NC_COM*tmp$Exposure
  tmp$NC_COL <- tmp$NC_COL*tmp$Exposure
  tmp$NC_PI <- tmp$NC_PI*tmp$Exposure
  
  # Get Total amount 
  
  tmp$AC_BI <- tmp$NC_BI*tmp$AAC_BI
  tmp$AC_PD <- tmp$NC_PD*tmp$AAC_PD
  tmp$AC_COM <- tmp$NC_COM*tmp$AAC_COM
  tmp$AC_COL <- tmp$NC_COL*tmp$AAC_COL
  tmp$AC_PI <- tmp$NC_PI*tmp$AAC_PI

  # Add back to the main data frame
  predict.df <- rbind(predict.df, tmp)

   
  # Add new coverages
  tmp <- autocar
  tmp$Autonomy <- "A0"
  # loss per exposure
  total.loss.A0 <- cbind(tmp[, c("time", "RiskClass", "Type", "Autonomy")] ,rowSums(cbind(tmp$AC_BI/tmp$Exposure, 
                                                                                                 tmp$AC_PD/tmp$Exposure, 
                                                                                                 tmp$AC_COL/tmp$Exposure, 
                                                                                                 tmp$AC_COM/tmp$Exposure, 
                                                                                                 tmp$AC_PI/tmp$Exposure)))
  names(total.loss.A0) <- c("time", "RiskClass", "Type", "Autonomy", "total.loss")
  # we only want A0, and only time = 2019 and then remove the time column
  total.loss.A0 <-  total.loss.A0[total.loss.A0$Autonomy == "A0" & total.loss.A0$time == 2019, ]
  total.loss.A0 <- total.loss.A0[, !(names(total.loss.A0) %in% "time")]
  # A1 has the same values as A0
  total.loss.A1 <- total.loss.A0
  # we need to change the Autonomy level
  total.loss.A1$Autonomy <- "A1"
  # A2 is a little more trickier because not the same Risk Classes but we just aggregate
  total.loss.A2 <- total.loss.A0
  total.loss.A2$Autonomy <- "A2"
  total.loss.A2$RiskClass[substr(total.loss.A2$RiskClass,1,1) == "S"] <- "Small"
  total.loss.A2$RiskClass[substr(total.loss.A2$RiskClass,1,1) == "M"] <- "Medium"
  total.loss.A2$RiskClass[substr(total.loss.A2$RiskClass,1,1) == "L"] <- "Large"
  
  total.loss.A2 <- aggregate(. ~RiskClass + Type + Autonomy, data = total.loss.A2, FUN = mean)
  
  # Then we bind
  total.loss <- rbind(total.loss.A0, total.loss.A1, total.loss.A2)
  # A0 should be zero because A0 does not have the new coverage
  total.loss$total.loss[total.loss$Autonomy == "A0"] <- 0
  
  # Now we can join the tables on the keys
  
  predict.df <- inner_join(x = predict.df, y = total.loss, by = c("RiskClass", "Type", "Autonomy"))
  # View(predict.df[predict.df$Type == "Personal" & predict.df$Autonomy == "A2", c("time", "total.loss")])
  
  # create new coverage and adjust ALSO ADD exposure!
  predict.df$AC_MR <- predict.df$total.loss
  predict.df$AC_MR[predict.df$Autonomy == "A1"] <- predict.df$AC_MR[predict.df$Autonomy == "A1"]*MR.fac.a1
  predict.df$AC_MR[predict.df$Autonomy == "A2"] <- predict.df$AC_MR[predict.df$Autonomy == "A2"]*MR.fac.a2
  predict.df$AC_MR <- predict.df$AC_MR*predict.df$Exposure
  
  
  # INFRASTRUCTURE rIS
  # create new coverage and adjust
  predict.df$AC_IS <- predict.df$total.loss
  predict.df$AC_IS[predict.df$Autonomy == "A1"] <- predict.df$AC_IS[predict.df$Autonomy == "A1"]*IS.fac.a1
  predict.df$AC_IS[predict.df$Autonomy == "A2"] <- predict.df$AC_IS[predict.df$Autonomy == "A2"]*IS.fac.a2
  predict.df$AC_IS <- predict.df$AC_IS*predict.df$Exposure
  
  # Cyber Risk
  # create new coverage and adjust
  predict.df$AC_CR <- predict.df$total.loss
  predict.df$AC_CR[predict.df$Autonomy == "A1"] <- predict.df$AC_CR[predict.df$Autonomy == "A1"]*CR.fac.a1
  predict.df$AC_CR[predict.df$Autonomy == "A2"] <- predict.df$AC_CR[predict.df$Autonomy == "A2"]*CR.fac.a2
  predict.df$AC_CR <- predict.df$AC_CR*predict.df$Exposure
  
  # Finnaly remove the total loss column
  predict.df <- predict.df[, !(names(predict.df) %in% "total.loss")]
  
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
  
  tmp$AC_MR <- 0
  #tmp$AAC_MR <- 0
  #tmp$NC_MR <- 0

  tmp$AC_IS <- 0
  #tmp$AAC_IS <- 0
  #tmp$NC_IS <- 0
  
  #tmp$NC_CR <- 0
  #tmp$AAC_CR <- 0
  tmp$AC_CR <- 0
  
  
  tmp$time <- tmp$Year + 2.5*as.numeric(tmp$Qtr)/10
  tmp$Qtr <- as.numeric(tmp$Qtr)
  
  
  # make sure the columns match
  tmp <- tmp[, names(predict.df), drop = F]
  # bind them
  predict.df <- rbind(predict.df, tmp)
  
  # Finally, we make present value factor and PV AAC and PV AC
  today <- 2019 # today is 2019
  
  predict.df$pv_factor <- exp((predict.df$time-2019)*interest)
  
  
  # Remark NOTE!!!!!!!!!! New coverages have not been accumulated
  # so we accumulate with interest and divide again for the sake of 
  # consistancy
  
  # accumulate new coverages
  predict.df$AC_MR <- predict.df$AC_MR*predict.df$pv_factor
  predict.df$AC_IS <- predict.df$AC_IS*predict.df$pv_factor
  predict.df$AC_CR <- predict.df$AC_CR*predict.df$pv_factor
  
  # take present value
  predict.df$AC_BI_PV <- predict.df$AC_BI/predict.df$pv_factor
  predict.df$AC_PD_PV <- predict.df$AC_PD/predict.df$pv_factor
  predict.df$AC_COM_PV <- predict.df$AC_COM/predict.df$pv_factor
  predict.df$AC_COL_PV <- predict.df$AC_COL/predict.df$pv_factor
  predict.df$AC_PI_PV <- predict.df$AC_PI/predict.df$pv_factor
  predict.df$AC_MR_PV <- predict.df$AC_MR/predict.df$pv_factor
  predict.df$AC_CR_PV <- predict.df$AC_CR/predict.df$pv_factor
  predict.df$AC_IS_PV <- predict.df$AC_IS/predict.df$pv_factor
  
  predict.df$AAC_BI_PV <- predict.df$AAC_BI/predict.df$pv_factor
  predict.df$AAC_PD_PV <- predict.df$AAC_PD/predict.df$pv_factor
  predict.df$AAC_COM_PV <- predict.df$AAC_COM/predict.df$pv_factor
  predict.df$AAC_COL_PV <- predict.df$AAC_COL/predict.df$pv_factor
  predict.df$AAC_PI_PV <- predict.df$AAC_PI/predict.df$pv_factor
  
  
  # Calculate Premiums
  predict.df$BI_pv_prem <- predict.df$AC_BI_PV/predict.df$Exposure
  predict.df$PD_pv_prem <- predict.df$AC_PD_PV/predict.df$Exposure
  predict.df$COM_pv_prem <- predict.df$AC_COM_PV/predict.df$Exposure
  predict.df$COL_pv_prem <- predict.df$AC_COL_PV/predict.df$Exposure
  predict.df$PI_pv_prem <- predict.df$AC_PI_PV/predict.df$Exposure
  predict.df$MR_pv_prem <- predict.df$AC_MR_PV/predict.df$Exposure
  predict.df$CR_pv_prem <- predict.df$AC_CR_PV/predict.df$Exposure
  predict.df$IS_pv_prem <- predict.df$AC_IS_PV/predict.df$Exposure
  
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
  exposure <- as.data.frame(inner_join(x = exposure, y = carbia.exposure, by = c("Type", "time")))
  exposure <- as.data.frame(inner_join(x = exposure, y = safelife.market.share, by = c("Autonomy", "time")))
  
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


riskClassProp <- function(df.main, time.frame, start.year = 2019, lvls){
  # df.main - The data.frame
  # Exposure year what exposure year to use 
  
  
  # calculate the proportion for each year and qtr then fin the averages
  # 27 Risk clasess 40 dates 
  
  tmp.prop <- list()
  count <- 1
  
  # we want to make the rows od the data frames correspond to the same row, so we order the Risk Class
  
  for (i in unique(df.main$Year)){
    for(j in unique(df.main$Qtr)){
      
      tmp <- df.main[df.main$Year == i & df.main$Qtr == j & df.main$Type == "Personal", ]
      total.exposure <- sum(tmp$Exposure)
      tmp.df <- data.frame(prop = tmp$Exposure/total.exposure, RiskClass = tmp$RiskClass, stringsAsFactors = F)
      
      tmp.prop[[count]] <- tmp.df[order(tmp.df$RiskClass), "prop" ,drop = F]
      
      count <- count + 1
    }
  }
  
  # take average
  tmp.prop <- do.call(what = cbind, args = tmp.prop)
  prop.personal <- rowSums(x = tmp.prop)/dim(tmp.prop)[2]
  
  
  tmp.prop <- list()
  count <- 1
  
  # we want to make the rows od the data frames correspond to the same row, so we order the Risk Class
  
  for (i in unique(df.main$Year)){
    for(j in unique(df.main$Qtr)){
      
      tmp <- df.main[df.main$Year == i & df.main$Qtr == j & df.main$Type == "Commercial", ]
      total.exposure <- sum(tmp$Exposure)
      tmp.df <- data.frame(prop = tmp$Exposure/total.exposure, RiskClass = tmp$RiskClass, stringsAsFactors = F)
      
      tmp.prop[[count]] <- tmp.df[order(tmp.df$RiskClass), "prop" ,drop = F]
      
      count <- count + 1
    }
  }
  
  # take average
  tmp.prop <- do.call(what = cbind, args = tmp.prop)
  prop.commercial <- rowSums(x = tmp.prop)/dim(tmp.prop)[2]
  
  prop <- data.frame(RiskClass = sort(unique(df.main$RiskClass)), Personal = prop.personal, Commercial = prop.commercial)
  
  # instead of two columns we want to have a Type Column, an prop colum
  prop <- melt(prop, id = "RiskClass")
  names(prop)[names(prop) == "variable"] <- "Type"
  names(prop)[names(prop) == "value"] <- "prop"
  
  
  # Then we need to merge year and quarter
  # new years
  tmp.df <- data.frame(Year = time.frame + start.year)
  prop <- as.data.frame(merge(prop, tmp.df))
  
  tmp.df <- data.frame(Qtr = sort(unique(df.main$Qtr)))
  prop <- as.data.frame(merge(prop, tmp.df))
  
  tmp.df <- data.frame(Autonomy = lvls)
  prop <- as.data.frame(merge(prop, tmp.df))
  
  
  return(prop)
  
}



plot.main <- function(predict.df){
  
  # Descriptve plots
  # Exposure
  tmp <- predict.df[predict.df$time<= 2019, c("time", "Exposure", "Type")]
  tmp <- aggregate(. ~ time + Type, data = tmp, FUN = sum)
  
  exposure.descriptive <- ggplot() + 
    geom_line(data = tmp, mapping = aes(x = time, y = Exposure , color = Type), size = 2) + 
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 5.5, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)))+
    scale_y_continuous(expand = c(0,0), 
                       name = "Car years of exposure in thousands", 
                       breaks = c(0, 100000, 200000, 300000, 400000, 500000),
                       labels = c("0", "100", "200", "300", "400", "500"),
                       limits = c(0, 500000)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year",
                       breaks = c(2009:2019),
                       labels = c(2009:2019),
                       limit = c(2009,2019.25)) +
    scale_color_manual(values = c("Commercial" = "#004F71", "Personal" =  "#8DC8E8")) +
    ggtitle("Safelife's historical exposure")
  
  ggsave("graphs/exposure_descriptive.png", device = "png",plot = exposure.descriptive, width = 60, height = 20, units = "cm")
  
  
  
  # AC per coverage
  
  tmp <- predict.df[predict.df$time<= 2019, c("time", "Type", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV")]
  
  tmp <- aggregate(. ~ time + Type, data = tmp, FUN = sum)
  tmp.melt <- melt(tmp, id.vars = c("time", "Type"))
  
  
  ac.descriptive <- ggplot() + 
    geom_bar(data = tmp.melt, 
             mapping = aes(x = time, y = value, fill = variable),
             stat = "identity", 
             position = "stack",
             width = 0.19) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5))) + 
    scale_y_continuous(expand = c(0,0), 
                       name = "Total loss in millions", 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)*10^(9),
                       labels = c("0", "200", "400", "600", "800", "1000"),
                       limits = c(0, 1000000000)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year",
                       breaks = c(2010, 2012, 2014, 2016, 2018),
                       labels = c("2010", "2012", "2014", "2016", "2018"),
                       limit = c(2009,2019))+
    ggtitle("Total claim amount (present value)") +
    facet_wrap(. ~ Type) + 
    labs(fill = "Coverage\n")+
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI"))
  
  ggsave("graphs/claimamount_descriptive.png", device = "png",plot = ac.descriptive, width = 60, height = 20, units = "cm")
  
  # NC per coverage
  
  tmp <- predict.df[predict.df$time<= 2019, c("time", "Type", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
  tmp <- aggregate(. ~ time + Type, data = tmp, FUN = sum)
  tmp.melt <- melt(tmp, id.vars = c("time", "Type"))
  nc.descriptive <- ggplot() + 
    geom_bar(data = tmp.melt, 
             mapping = aes(x = time, y = value, fill = variable),
             stat = "identity", 
             position = "stack",
             width = 0.19) +
    theme_bw(base_size = 17) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5))) + 
    scale_y_continuous(expand = c(0,0), 
                       name = "Number of claims in thousands", 
                       breaks = c(0, 100000, 200000, 300000),
                       labels = c("0", "100", "200", "300"),
                       limits = c(0, 300000)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year",
                       breaks = c(2009:2019),
                       labels = c(2009:2019),
                       limit = c(2009,2019.25)) +
    ggtitle("Claim frequency") +
    facet_wrap(. ~ Type) + 
    labs(fill = "Coverage\n")+
    scale_fill_manual(values = c("NC_BI" = "#D9E1E2", "NC_PD" = "#BBDDE6"  ,"NC_COM" = "#71B2C9", "NC_COL" = "#4E87A0","NC_PI" = "#072B31"),
                      labels = c("NC_BI" = "BI", "NC_PD" = "PD","NC_COM" =  "COM","NC_COL" =  "COL","NC_PI" =  "PI"))
  
  ggsave("graphs/frequency_descriptive.png", device = "png",plot = nc.descriptive, width = 60, height = 20, units = "cm")
  
  
  # Exposure groth, Autonomy levels included
  tmp <- predict.df[, c("time", "Exposure", "Type", "Autonomy")]
  tmp <- aggregate(. ~ time + Type + Autonomy, data = tmp, FUN = sum)
  tmp$Autonomy <- factor(tmp$Autonomy, levels = c("A0", "A1", "A2"), ordered = T)
  tmp <- tmp[order(tmp$Autonomy, decreasing = T),]
  tmp.1 <- tmp[tmp$time <= 2019,]
  tmp.2 <- tmp[tmp$time > 2019,]
  
  exposure.growth <- ggplot() + 
    geom_bar(data = tmp.1, mapping = aes(x = time, y = Exposure, fill = Autonomy, group = Type) , 
             stat = "identity", 
             position = "stack",
             width = 0.15,
             alpha = 1) +
    geom_bar(data = tmp.2, mapping = aes(x = time, y = Exposure, fill = Autonomy, group = Type) , 
             stat = "identity", 
             position = "stack", 
             width = 0.15,
             alpha = 1) +
    facet_wrap(. ~ Type) + 
    scale_fill_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8")) + 
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1, "cm")) + 
    scale_y_continuous(expand = c(0,0), 
                       name = "Exposure in thousands", 
                       breaks = c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000),
                       labels = c("0", "100", "200", "300", "400", "500", "600", "700"),
                       limits = c(0, 700000)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year") +
    ggtitle("Safelife`s exposure") +
    labs(fill = "Levels")
  
  ggsave("graphs/exposure_growth.png", device = "png",plot = exposure.growth, width = 60, height = 20, units = "cm")
  
  
  
  # Total Claim amount and only A1 and A2
  tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  tmp.A1andA2 <- tmp[tmp$Autonomy != "A0",]
  
  # Add as Column
  tmp$Case <- "Total"
  tmp.A1andA2$Case <- "A1 and A2"
  
  # combine
  tmp <- rbind(tmp, tmp.A1andA2)
  # Remove Autonomy
  tmp <- tmp[, !(names(tmp) %in% "Autonomy")]
  # aggregate
  tmp <- aggregate(. ~ time + Case, data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time", "Case"))
  
  #Plot: predicted total claim amount (present value) *new plot*
  totalclaim.comparison <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1,"cm")) + 
    labs(fill = "Coverage")+
    facet_wrap(. ~ Case) + 
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IR"))
  
  
  ggsave("graphs/total_claim_comparison.png", device = "png",plot = totalclaim.comparison, width = 60, height = 20, units = "cm")
  
  
  # total claim 
  tmp <- predict.df[, names(predict.df) %in% c("time", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  tmp <- aggregate(. ~ time , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time"))

  
  totalclaim <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5))) + 
    labs(fill = "Coverage")+
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IR"))
  
  ggsave("graphs/total_claim.png", device = "png",plot = totalclaim, width = 60, height = 20, units = "cm")
  
  
  # NO-AV
  tmp <- predict.df[predict.df$Autonomy == "A0", names(predict.df) %in% c("time", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV")]
  tmp <- aggregate(. ~ time , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time"))
  
  
  totalclaim.NOAV <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5))) + 
    labs(fill = "Coverage")+
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI"))
  
  ggsave("graphs/total_claim_NO_AV.png", device = "png",plot = totalclaim.NOAV, width = 60, height = 20, units = "cm")
  
  
  
  # Premiums
  premiums.t.pv <- predict.df[, c("time", "RiskClass", "Type", "Autonomy", 
                            "BI_pv_prem","PD_pv_prem", "COM_pv_prem", "COL_pv_prem",
                            "PI_pv_prem", "MR_pv_prem", "CR_pv_prem", "IS_pv_prem")]
  
  premiums.t.pv <- aggregate(. ~ time + RiskClass + Type + Autonomy, data = premiums.t.pv, FUN = sum )
  
  tmp <- premiums.t.pv[, !(names(premiums.t.pv) %in% c( "RiskClass", "Type"))]
  tmp <- aggregate(. ~ time + Autonomy, data = tmp, FUN = mean)
  tmp.melt <- melt(tmp, id.vars = c("time", "Autonomy"))
  

  premiums <- ggplot() + 
    geom_line(data = tmp.melt, mapping = aes(x = time, y = value, color = variable), size = 1.1) +
    facet_wrap(. ~ Autonomy, scales = "fixed")+
    theme_bw(base_size = 16) +
    scale_color_manual(values = c("BI_pv_prem" = "#D9E1E2", "PD_pv_prem" = "#BBDDE6"  ,"COM_pv_prem" = "#71B2C9", "COL_pv_prem" = "#4E87A0","PI_pv_prem" = "#072B31", "MR_pv_prem" = "#D45D00", "CR_pv_prem" = "#FDBE87", "IS_pv_prem" = "#F68D2E"),
                       labels = c("BI_pv_prem" = "BI", "PD_pv_prem" = "PD","COM_pv_prem" =  "COM", "COL_pv_prem" =  "COL","PI_pv_prem" =  "PI","MR_pv_prem" =  "MR","CR_pv_prem" =  "CR","IS_pv_prem" =  "IR"))+
    theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)), 
          #  legend.position = c(0.85, 0.2) ,
          panel.spacing = unit(2, "lines"),
          plot.margin = unit(c(0.3,0.8,0.3,1), "cm")) + 
    scale_y_continuous(expand = c(0,0),
                       name = TeX("Premiums in  $\\hat{C}$"),
                       breaks = seq(0, 8, by = 1)*100,
                       labels = seq(0, 8, by = 1)*100,
                       limits = c(0, 800)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year",
                       breaks = seq(2010, 2030, by = 5),
                       labels = seq(2010, 2030, by = 5),
                       limit = c(2009,2030.25)) +
    ggtitle("Average Discounted Premiums")+
    labs(color = "Coverage")
  
  
  ggsave("graphs/premiums_pv_mean.png", device = "png",plot = premiums, width = 60, height = 20, units = "cm")
  
  
  # Proportions plot
  tmp <- predict.df[, c("time", "RiskClass", "prop", "Type", "Autonomy")]
  
  prop.plot <- ggplot(data = tmp)+ 
    geom_line(mapping = aes(x = time, y = prop, color = RiskClass), size = 1.02) + 
    facet_wrap(. ~ Type + Autonomy, scales = "free") +
    theme_bw(base_size = 20) +
    scale_y_continuous(name = "Percentage") +
    scale_x_continuous(name = "Year") 
  
  ggsave("graphs/prop_plot.png", device = "png",plot = prop.plot, width = 60, height = 40, units = "cm")
  
  
  
  # plot amount evolution personal
  tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "AAC_BI", "AAC_PD", "AAC_COM", "AAC_COL", "AAC_PI", "AAC_MR", "AAC_IS", "AAC_CR")]
  tmp <- aggregate(formula = . ~ time + Autonomy , data = tmp, FUN = mean)
  tmp.melt <- melt(data = tmp, id = c("time", "Autonomy"))
  
  amount.Personal <- ggplot(data = tmp.melt) + 
    geom_line(mapping = aes(x = time, y = value, color = Autonomy), size = 1.1) +
    facet_wrap(facets = . ~ variable, scales = "free") +
    ggtitle("Total oss per claim for the standard coverages ") +
    scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8")) + 
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1, "cm")) + 
    scale_y_continuous(expand = c(0.05,0), 
                       name = TeX("Amount in   $\\hat{C}$")) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year") +
    ggtitle("The mean amount for each standard coverage") +
    labs(fill = "Levels") + geom_vline(xintercept = 2019, alpha = 0.8)
  
  ggsave("graphs/amount_example.png", device = "png",plot = amount.Personal, width = 60, height = 30, units = "cm")
  
  
  # Plot Claims evolution Commercial
  tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
  tmp <- aggregate(formula = . ~ time + Autonomy , data = tmp, FUN = sum)
  tmp.melt <- melt(data = tmp, id = c("time", "Autonomy"))
  
  frequency.personal <- ggplot(data = tmp.melt) + 
    geom_line(mapping = aes(x = time, y = value, color = Autonomy), size = 1.1) +
    facet_wrap(facets = . ~ variable, scales = "free") + 
    ggtitle("Frequency of claims for the standard coverages ") +
    scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8")) + 
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1, "cm")) + 
    scale_y_continuous(expand = c(0.05,0), 
                       name = TeX("Amount in   $\\hat{C}$")) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year") +
    ggtitle("The mean number of claims for each standard coverage") +
    labs(fill = "Levels") + geom_vline(xintercept = 2019, alpha = 0.8)
  
 # frequency.personal
  
  ggsave("graphs/frequency_example.png", device = "png",plot = frequency.personal, width = 60, height = 30, units = "cm")
    
  
}


plot.scenarios <- function(all, history, safelife.ms, carb.c.exp.pct, carb.p.exp.pct, A2.hit.time){
  
  # All Scenarios COMBINED
  # Next we safe plots, a folder called graph is needed in the working directory
  
  
  tmp.history.pv <- history[, c("time", "Case", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]
  tmp.all.pv <- all[, c("time", "Case", "Scenario", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]
  
  
  tmp.history.pv <- cbind(tmp.history.pv[, c("time", "Case", "Scenario")], rowSums(tmp.history.pv[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))
  tmp.all.pv <- cbind(tmp.all.pv[, c("time", "Case", "Scenario")], rowSums(tmp.all.pv[, c("AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_MR_PV", "AC_CR_PV", "AC_IS_PV")]))
  
  names(tmp.history.pv) <- c("time", "Case", "Scenario", "PurePremium")
  names(tmp.all.pv) <- c("time", "Case", "Scenario", "PurePremium")
  
  
  tmp.all.pv <- aggregate(. ~ time + Scenario+ Case, data = tmp.all.pv, FUN = sum)
  tmp.history.pv <- aggregate(. ~ time + Scenario + Case, data = tmp.history.pv, FUN = sum)
  # 
  # tmp.pv <- rbind(tmp.all.pv, tmp.history.pv)
  # 
  # tmp.all.pv$Scenario <- factor(tmp.all.pv$Scenario,
  #                               levels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "Combined"),
  #                               ordered = T)
  # tmp.all.pv <- tmp.all.pv[order(tmp.all.pv$Scenario),]
  # 
  # tmp.history.pv$Scenario <- factor(tmp.history.pv$Scenario,
  #                                   levels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "Combined"),
  #                                   ordered = T)
  # tmp.history.pv <- tmp.history.pv[order(tmp.history.pv$Scenario),]
  # 
  # tmp.all.pv$Scenario2 <- factor(tmp.all.pv$Scenario, 
  #                                labels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "bold(Combined)"))
  # 
  # tmp.history.pv$Scenario2 <- factor(tmp.history.pv$Scenario, 
  #                                    labels = c("Autonomous % of Carbia", "Safelife Marketshare", "Multipliers", "Coverage", "bold(Combined)"))
  # 
  
  # We dont want to display Combined
  tmp.all.pv <- tmp.all.pv[tmp.all.pv$Scenario != "Combined",]
  tmp.history.pv <- tmp.history.pv[tmp.history.pv$Scenario != "Combined",]
  
  p.pv <-  ggplot() + 
    facet_wrap(. ~ Scenario, scales = "free_x")+
    geom_ribbon(data = dcast(tmp.all.pv, time + Scenario ~  Case, value.var = "PurePremium"), mapping = aes(x = time, ymin = Downward,  ymax = Upward), alpha = 0.8, fill = "#6191B4")+
    geom_line(data = tmp.all.pv, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1) +
    geom_line(data = tmp.history.pv, mapping = aes(x = time, y = PurePremium, color = Case), size = 1.1 ) +
    theme_bw(base_size = 16) +
    scale_color_manual(values = c("Base" = "#004680", "Upward" = "#4B82A8", "Downward" = "#4B82A8"))+
    theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)), 
          panel.spacing = unit(2, "lines"),
          plot.margin = unit(c(0.3,0.8,0.3,1), "cm")) + 
    scale_y_continuous(expand = c(0,0), 
                       name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(0, 2.2, by = 0.2)*10^(9),
                       labels = seq(0, 2.2, by = 0.2),
                       limits = c(0, 2200000000)) +
    scale_x_continuous(expand = c(0,0), 
                       name = "Year",
                       breaks = seq(2010, 2030, by = 5),
                       labels = seq(2010, 2030, by = 5),
                       limit = c(2009,2030)) +
    ggtitle("Expected total loss for different scenarios")

  ggsave("graphs/totalloss_scenarios.png", device = "png",plot = p.pv, width = 60, height = 20, units = "cm")
  
  
# Plot marketshare Scenarios

  # We dont want to plot  A2 when is not in the market
  tmp <- safelife.ms
  tmp$A2[tmp$time< A2.hit.time ] <- NA
  
  tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
  tmp <- tmp[order(tmp$Case),]
  
  m.safe <- melt(data = tmp, id = c("time", "Case"))
  
  safelife.ms.scenarios <- ggplot() + geom_line( data = m.safe,mapping = aes(x = time, y = value, color = variable), size = 1.1) + 
    scale_x_continuous(name ="Year", breaks = seq(from=0,to=20,by=2), label = as.character(2019 + seq(from=0,to=20,by=2)), expand = c(0,0)) +
    scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1.02), expand = c(0,0)) + # expand -> y axis begins at 0 strict
    theme_bw(base_size = 20) +
    scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8"))+
    theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
    labs(color = "Level") + ggtitle("Safelife's market share per autonomy level")   +
    facet_wrap(. ~ Case, scales = "free")
  
  ggsave("graphs/safelife_ms_shocks.png", device = "png",plot = safelife.ms.scenarios, width = 40, height = 20, units = "cm")
  
  # Do the same but only base
  tmp <- safelife.ms[safelife.ms$Case == "Base", c("time", "A0", "A1", "A2")]
  tmp$A2[tmp$time< A2.hit.time ] <- NA
  
  m.safe <- melt(data = tmp, id = c("time"))
  
  safelife.ms.base <- ggplot() + geom_line( data = m.safe ,mapping = aes(x = time, y = value, color = variable), size = 1.1) + 
    scale_x_continuous(name ="Year", breaks = seq(from=0,to=20,by=2), label = as.character(2019 + seq(from=0,to=20,by=2)), expand = c(0,0)) +
    scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1.02), expand = c(0,0)) + # expand -> y axis begins at 0 strict
    theme_bw(base_size = 20) +
    scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8"))+
    theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
    labs(color = "Level") + ggtitle("Safelife's market share per autonomy level")
  
  
  ggsave("graphs/safelife_ms_base.png", device = "png",plot = safelife.ms.base, width = 40, height = 20, units = "cm")
  
  # Plot for Commercial and Personal exposure percentage
  
  tmp <- carb.c.exp.pct
  
  tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
  tmp <- tmp[order(tmp$Case),]
  
  m.carb <- melt(data = tmp, id.vars = c("time","Case"))
  m.carb$value[m.carb$variable == "A2" & m.carb$value == 0] <- NA
  
  c.exp.carb.graph <- ggplot() + geom_line( data = m.carb,mapping = aes(x = time, y = value, color = variable), size = 1.1) + 
    scale_x_continuous(name ="Year", breaks = seq(from=0,to=20,by=2), label = as.character(2019 + seq(from=0,to=20,by=2)), expand = c(0,0)) +
    scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1), expand = c(0,0)) + # expand -> y axis begins at 0 strict
    theme_bw(base_size = 20) +
    scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8"))+
    theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
    labs(color = "Level") + ggtitle("Proportion of exposure per autonomy level for personal vehicles")  +
    facet_wrap(. ~ Case, scales = "free")
  
  ggsave("graphs/commercial_exposure_pct.png", device = "png",plot = c.exp.carb.graph, width = 60, height = 20, units = "cm")
  
  tmp <- carb.p.exp.pct
  tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
  tmp <- tmp[order(tmp$Case),]
  
  m.carb <- melt(data = tmp, id.vars = c("time","Case"))
  m.carb$value[m.carb$variable == "A2" & m.carb$value == 0] <- NA
  
  p.exp.carb.graph <- ggplot() + geom_line( data = m.carb,mapping = aes(x = time, y = value, color = variable), size = 1.1) + 
    scale_x_continuous(name ="Year", breaks = seq(from=0,to=20,by=2), label = as.character(2019 + seq(from=0,to=20,by=2)), expand = c(0,0)) +
    scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1), expand = c(0,0)) + # expand -> y axis begins at 0 strict
    theme_bw(base_size = 20) +
    scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8"))+
    theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
    labs(color = "Level") + ggtitle("Proportion of exposure per autonomy level for commercial vehicles")   +
    facet_wrap(. ~ Case, scales = "free")
  
  ggsave("graphs/personal_exposure_pct.png", device = "png",plot = p.exp.carb.graph, width = 60, height = 20, units = "cm")
  
  # Cobine into one
  
   carb.p.exp.pct$Type <- "Personal"
   carb.c.exp.pct$Type <- "Commercial"
   
   tmp <- rbind(carb.p.exp.pct, carb.c.exp.pct)
   tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
   tmp <- tmp[order(tmp$Case),]
   
   m.carb <- melt(data = tmp, id.vars = c("time","Case", "Type"))
   m.carb$value[m.carb$variable == "A2" & m.carb$value == 0] <- NA
   
   exp.carb.graph <- ggplot() + geom_line( data = m.carb,mapping = aes(x = time, y = value, color = variable), size = 1.1) + 
     scale_x_continuous(name ="Year", breaks = seq(from=0,to=20,by=2), label = as.character(2019 + seq(from=0,to=20,by=2)), expand = c(0,0)) +
     scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1), expand = c(0,0)) + # expand -> y axis begins at 0 strict
     theme_bw(base_size = 17) +
     scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8"))+
     theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
           axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
           axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)),
           strip.text = element_text(size=12)) +
     labs(color = "Level") + ggtitle("Proportion of exposure per autonomy level")    +
     facet_wrap(. ~ Type + Case, scales = "free") 
   
   ggsave("graphs/exposure_pct_pandc.png", device = "png",plot = exp.carb.graph, width = 60, height = 20, units = "cm")
   
   
   # Do the same bust only Base
   tmp <- rbind(carb.p.exp.pct, carb.c.exp.pct)
   tmp <- tmp[tmp$Case =="Base", c("time", "A0", "A1", "A2", "Type")] 
   m.tmp <- melt(data = tmp, id.vars = c("time", "Type"))
   m.tmp$value[m.tmp$variable == "A2" & m.tmp$value == 0] <- NA
   
   exp.carb.graph.base <- ggplot() + geom_line( data = m.tmp,mapping = aes(x = time, y = value, color = variable), size = 1.1) + 
     scale_x_continuous(name ="Year", breaks = seq(from=0,to=20,by=2), label = as.character(2019 + seq(from=0,to=20,by=2)), expand = c(0,0)) +
     scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1), expand = c(0,0)) + # expand -> y axis begins at 0 strict
     theme_bw(base_size = 17) +
     scale_color_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8"))+
     theme(plot.title = element_text(margin = margin(t = 0, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
           axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 0, l = 0)),
           axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)),
           strip.text = element_text(size=12)) +
     labs(color = "Level") + ggtitle("Proportion of exposure per autonomy level")    +
     facet_wrap(. ~ Type, scales = "free") 
   
   ggsave("graphs/exposure_pct_pandc_base.png", device = "png",plot = exp.carb.graph.base, width = 60, height = 20, units = "cm")
   
   
   
  
  # Combine all and history into one data frame
  
  # Both include the year 2019! But History does not have Case Down and Up
  # Add history to the cases Down and up Also
  history.up <- history
  history.up$Case <- "Upward"
  
  history.down <- history
  history.down$Case <- "Downward"
  all.comb <- rbind(history, all[all$time != 2019, ], history.down, history.up)
  
  # Autonomous vehicle percentage
  
  tmp <- all.comb[all.comb$Scenario == "Autonomous % of Carbia", names(all.comb) %in% c("time", "Case", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
  tmp <- tmp[order(tmp$Case),]
  tmp <- aggregate(. ~ time + Case , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time", "Case"))
  
  
  totalclaim.pct <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss shocking autonomous % of Carbia") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1,"cm")) + 
    labs(fill = "Coverage") +
    facet_wrap(. ~ Case) +
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IS"))
  
  ggsave("graphs/totalclaim_pct_shocks.png", device = "png",plot = totalclaim.pct, width = 60, height = 20, units = "cm")
  
  # Coverage shock
  tmp <- all.comb[all.comb$Scenario == "Coverage", names(all.comb) %in% c("time", "Case", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
  tmp <- tmp[order(tmp$Case),]
  tmp <- aggregate(. ~ time + Case , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time", "Case"))
  
  
  totalclaim.coverage <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss shocking new coverage") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1,"cm")) + 
    labs(fill = "Coverage") +
    facet_wrap(. ~ Case) +
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IS"))
  
  ggsave("graphs/totalclaim_coverage_shocks.png", device = "png",plot = totalclaim.coverage, width = 60, height = 20, units = "cm")
  
  
  # Multipliers shock
  tmp <- all.comb[all.comb$Scenario == "Frequency and amount", names(all.comb) %in% c("time", "Case", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
  tmp <- tmp[order(tmp$Case),]
  tmp <- aggregate(. ~ time + Case , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time", "Case"))
  
  
  totalclaim.mult <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss shocking frequency and amount") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1,"cm")) + 
    labs(fill = "Coverage") +
    facet_wrap(. ~ Case) +
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IS"))
  
  ggsave("graphs/totalclaim_multipliers_shocks.png", device = "png",plot = totalclaim.mult, width = 60, height = 20, units = "cm")
  
  
  # Safelife marketshare shock
  tmp <- all.comb[all.comb$Scenario == "Safelife Marketshare", names(all.comb) %in% c("time", "Case", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  tmp$Case <- factor(tmp$Case, levels = c("Downward", "Base", "Upward"), ordered = T)
  tmp <- tmp[order(tmp$Case),]
  tmp <- aggregate(. ~ time + Case , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time", "Case"))
  
  
  totalclaim.ms <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss shocking safelife's market share") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1,"cm")) + 
    labs(fill = "Coverage") +
    facet_wrap(. ~ Case) +
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IS"))
  
  ggsave("graphs/totalclaim_ms_shocks.png", device = "png",plot = totalclaim.ms, width = 60, height = 20, units = "cm")
  
  # Combined shock
  tmp <- all.comb[all.comb$Scenario == "Combined", names(all.comb) %in% c("time", "Case", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV", "AC_IS_PV", "AC_CR_PV", "AC_MR_PV")]
  
  tmp <- aggregate(. ~ time + Case , data = tmp, FUN = sum)
  
  tmp.melt <- melt(data = tmp, id = c("time", "Case"))
  
  
  totalclaim.comb <- ggplot(data = tmp.melt) + 
    geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity", width = 0.2) +
    ggtitle("Expected total loss shocks combined") + 
    scale_y_continuous(name = TeX("Loss in billions  $\\hat{C}$") , 
                       breaks = seq(from=0,to=1.800,by=0.100) * 10^9, 
                       labels = seq(from=0,to=1.800,by=0.100), 
                       expand = c(0,0),
                       limits = c(0, 1.8)*10^9) + 
    scale_x_continuous(name = "Year", expand = c(0,0)) +
    theme_bw(base_size = 20) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 20, l = 5.5), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          axis.title.y = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)),
          panel.spacing = unit(1,"cm")) + 
    labs(fill = "Coverage") +
    facet_wrap(. ~ Case) +
    scale_fill_manual(values = c("AC_BI_PV" = "#D9E1E2", "AC_PD_PV" = "#BBDDE6"  ,"AC_COM_PV" = "#71B2C9", "AC_COL_PV" = "#4E87A0","AC_PI_PV" = "#072B31", "AC_MR_PV" = "#D45D00", "AC_CR_PV" = "#FDBE87", "AC_IS_PV" = "#F68D2E"),
                      labels = c("AC_BI_PV" = "BI", "AC_PD_PV" = "PD","AC_COM_PV" =  "COM","AC_COL_PV" =  "COL","AC_PI_PV" =  "PI","AC_MR_PV" =  "MR","AC_CR_PV" =  "CR","AC_IS_PV" =  "IS"))
  
  ggsave("graphs/totalclaim_combined_shocks.png", device = "png",plot = totalclaim.comb, width = 60, height = 20, units = "cm")
  
  
  
}


