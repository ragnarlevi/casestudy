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


freq.loss.tidy <- function(freq.pct, loss.pct){
  
  for(i in names(freq.pct)){
    freq.pct[[i]]$Autonomy <- i
    loss.pct[[i]]$Autonomy <- i
  }
  
  
  tmp <- inner_join(do.call(what = rbind, args = freq.pct), do.call(what = rbind, args = loss.pct), by = c("time", "Autonomy"))
  
  tmp$Year <- 2018 + ceiling(tmp$time)
  tmp$Qtr <- (tmp$time * 4) %% 4
  tmp$Qtr[tmp$Qtr == 0] <- 4
  
 return(tmp[, !(names(tmp) %in% c("time")), drop = F])
  
}

# freq.loss.tidy(freq.pct = freq.pct, loss.pct = loss.pct)



model.2 <- function(time.frame, autocar, glm.list, safelife.market.share, carbia.exposure, carb.commercial.pct, carb.personal.pct, freq.pct, loss.pct, MR.fac.a2 , IS.fac.a2 , CR.fac.a2, interest, MR.fac.a1 , IS.fac.a1 , CR.fac.a1 ){
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
  
  
  # calculate proportions
  # df.main <-  autocar
  
  prop <- riskClassProp(df.main = autocar, time.frame = time.frame, start.year = 2019, lvls = lvls)

  # get exposure from carbia and safelife number
  exposure <- exposure.tidy(safelife.market.share = safelife.market.share, 
                            carbia.exposure = carbia.exposure, 
                            carb.commercial.pct = carb.commercial.pct, 
                            carb.personal.pct = carb.personal.pct)
  

  
  # Finally we add stuff
  predict.df <- inner_join(instances, prop, by = c("Qtr","RiskClass", "Type", "Autonomy", "Year"))

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
  

  
 # for(i in lvls){
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
    
    
  #}
  
  # Remove columns we don't need
  predict.df <- predict.df[, !(names(predict.df) %in% c("carb.pct", "carbia_exposure", "safelife.pct"))]
  
  
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
  
  # Then multiply to find the amount
  
  predict.df$AC_BI <- predict.df$NC_BI*predict.df$AAC_BI
  predict.df$AC_PD <- predict.df$NC_PD*predict.df$AAC_PD
  predict.df$AC_COM <- predict.df$NC_COM*predict.df$AAC_COM
  predict.df$AC_COL <- predict.df$NC_COL*predict.df$AAC_COL
  predict.df$AC_PI <- predict.df$NC_PI*predict.df$AAC_PI
  
  
  predict.df$RiskClass <- paste(predict.df$VehicleSize, predict.df$DriverAg, predict.df$DriverRisk, sep = "")
  predict.df <- predict.df[, !(names(predict.df) %in% c("VehicleSize", "DriverAge", "DriverRisk"))]
  
  
  # A2 only has 3 Classes So we 
  # Next we need to aggregate A2

  tmp <- predict.df[predict.df$Autonomy == "A2", ]
  # remove then add back
  predict.df <- predict.df[predict.df$Autonomy != "A2", ]
  # Change  Risk Class
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "S"] <- "Small"
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "M"] <- "Medium"
  tmp$RiskClass[substr(tmp$RiskClass,1,1) == "L"] <- "Large"

  # tmp.1 includes proportions and Exposure along wit the keys, they sum when aggregated
  tmp.1 <- tmp[, c("Year", "Autonomy", "Qtr", "Type", "time", "RiskClass", "Exposure", "prop", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
  tmp.1 <- aggregate(. ~ Year + Autonomy + Qtr + Type + time + RiskClass, data = tmp.1, FUN = sum)
  # tmp.2 includes the coverages which should be averaged
  tmp.2 <- tmp[, !(names(tmp) %in% c("Exposure", "prop", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI"))]
  tmp.2 <- aggregate(. ~ Year + Autonomy + Qtr + Type + time + RiskClass, data = tmp.2, FUN = mean)

  # join them
  tmp <- full_join(x = tmp.1, y = tmp.2, by = c("Year", "Autonomy", "Qtr", "Type", "time", "RiskClass"))
  tmp$AC_BI <- tmp$NC_BI*tmp$AAC_BI
  tmp$AC_PD <- tmp$NC_PD*tmp$AAC_PD
  tmp$AC_COM <- tmp$NC_COM*tmp$AAC_COM
  tmp$AC_COL <- tmp$NC_COL*tmp$AAC_COL
  tmp$AC_PI <- tmp$NC_PI*tmp$AAC_PI

  predict.df <- rbind(predict.df, tmp)

   
  # Add new coverages

  # sum nc
  total.coverage.sum.nc <- mean(rowSums(autocar[, c("NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")])/autocar$Exposure)
  #
 
  # nO SEASONALITY LETS REGRESS
  lm.BI <- lm(formula = AAC_BI ~ time, data = predict.df)
  lm.PD <- lm(formula = AAC_PD ~ time, data = predict.df)
  lm.COM <- lm(formula = AAC_COM ~ time, data = predict.df)
  lm.COL <- lm(formula = AAC_COL ~ time, data = predict.df)
  lm.PI <- lm(formula = AAC_PI ~ time, data = predict.df)
  # linear non seasonal AAC
  x.BI <- predict(object = lm.BI, predict.df)
  x.PD <- predict(object = lm.PD, predict.df)
  x.COM <- predict(object = lm.COM, predict.df)
  x.COL <- predict(object = lm.COL, predict.df)
  x.PI <- predict(object = lm.PI, predict.df)
  
  x <- rowSums( cbind(x.BI, x.PD, x.COM, x.COL, x.PI))
  
  # Malfunction Risk, make sure A0 is just 0 or NA
  predict.df$NC_MR <- 0
  predict.df$NC_MR[predict.df$Autonomy != "A0"] <- total.coverage.sum.nc*rowMeans(predict.df[predict.df$Autonomy != "A0", c("NC_BI.pct", "NC_PD.pct", "NC_COM.pct", "NC_COL.pct", "NC_PI.pct")])
  predict.df$AAC_MR <- x
  predict.df$AC_MR <- 0
  predict.df$AC_MR[predict.df$Autonomy == "A1"] <- predict.df$AAC_MR[predict.df$Autonomy == "A1"]*predict.df$NC_MR[predict.df$Autonomy == "A1"]*predict.df$Exposure[predict.df$Autonomy == "A1"]*MR.fac.a1
  predict.df$AC_MR[predict.df$Autonomy == "A2"] <- predict.df$AAC_MR[predict.df$Autonomy == "A2"]*predict.df$NC_MR[predict.df$Autonomy == "A2"]*predict.df$Exposure[predict.df$Autonomy == "A2"]*MR.fac.a2
  
  # INFRASTRUCTURE rIS
  predict.df$NC_IS <- 0
  predict.df$NC_IS[predict.df$Autonomy != "A0"] <- total.coverage.sum.nc*rowMeans(predict.df[predict.df$Autonomy != "A0", c("NC_BI.pct", "NC_PD.pct", "NC_COM.pct", "NC_COL.pct", "NC_PI.pct")])
  predict.df$AAC_IS <- x
  predict.df$AC_IS <- 0
  predict.df$AC_IS[predict.df$Autonomy == "A1"] <- predict.df$AAC_IS[predict.df$Autonomy == "A1"]*predict.df$NC_IS[predict.df$Autonomy == "A1"]*predict.df$Exposure[predict.df$Autonomy == "A1"]*IS.fac.a1
  predict.df$AC_IS[predict.df$Autonomy == "A2"] <- predict.df$AAC_IS[predict.df$Autonomy == "A2"]*predict.df$NC_IS[predict.df$Autonomy == "A2"]*predict.df$Exposure[predict.df$Autonomy == "A2"]*IS.fac.a2

  # Cyber Risk
  predict.df$NC_CR <- 0
  predict.df$NC_CR[predict.df$Autonomy != "A0"] <- total.coverage.sum.nc*rowMeans(predict.df[predict.df$Autonomy != "A0", c("NC_BI.pct", "NC_PD.pct", "NC_COM.pct", "NC_COL.pct", "NC_PI.pct")])
  predict.df$AAC_CR <- x
  predict.df$AC_CR <- 0
  predict.df$AC_CR[predict.df$Autonomy == "A1"] <- predict.df$AAC_CR[predict.df$Autonomy == "A1"]*predict.df$NC_CR[predict.df$Autonomy == "A1"]*predict.df$Exposure[predict.df$Autonomy == "A1"]*CR.fac.a1
  predict.df$AC_CR[predict.df$Autonomy == "A2"] <- predict.df$AAC_CR[predict.df$Autonomy == "A2"]*predict.df$NC_CR[predict.df$Autonomy == "A2"]*predict.df$Exposure[predict.df$Autonomy == "A2"]*CR.fac.a2
  
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
  tmp$AAC_MR <- 0
  tmp$NC_MR <- 0

  tmp$AC_IS <- 0
  tmp$AAC_IS <- 0
  tmp$NC_IS <- 0
  
  tmp$NC_CR <- 0
  tmp$AAC_CR <- 0
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
  predict.df$AAC_MR_PV <- predict.df$AAC_MR/predict.df$pv_factor
  predict.df$AAC_CR_PV <- predict.df$AAC_CR/predict.df$pv_factor
  predict.df$AAC_IS_PV <- predict.df$AAC_IS/predict.df$pv_factor
  
  
  
  
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



