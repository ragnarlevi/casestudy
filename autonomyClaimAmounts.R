# Autonomy Claims ----


# Setting the glm.list as a nice data frame
A0initialParam <- function(glm.list, refine){
  # glm.list - glm list created by GLM
  # refine - what instance of data cleaning
  
  df.number <- glm[[refine]]$Number$df
  df.amount <- glm[[refine]]$Amount$df
  # We need to have the coverage as columns but not as a row factor/strings
  
  tmp.n.bi <- df.number[df.number$coverage == "BI", names(df.number) %in% c("Type", "RiskClass", "Qtr", "NPerExposure")]
  names(tmp.n.bi) <- c("Type", "RiskClass", "Qtr", "NC_BI")
  
  tmp.n.pd <- df.number[df.number$coverage == "PD", names(df.number) %in% c("Type", "RiskClass", "Qtr", "NPerExposure")]
  names(tmp.n.pd) <- c("Type", "RiskClass", "Qtr", "NC_PD")
  
  tmp.n.com <- df.number[df.number$coverage == "COM", names(df.number) %in% c("Type", "RiskClass", "Qtr", "NPerExposure")]
  names(tmp.n.com) <- c("Type", "RiskClass", "Qtr", "NC_COM")
  
  tmp.n.col <- df.number[df.number$coverage == "COL", names(df.number) %in% c("Type", "RiskClass", "Qtr", "NPerExposure")]
  names(tmp.n.col) <- c("Type", "RiskClass", "Qtr", "NC_COL")
  
  tmp.n.pi <- df.number[df.number$coverage == "PI", names(df.number) %in% c("Type", "RiskClass", "Qtr", "NPerExposure")]
  names(tmp.n.pi) <- c("Type", "RiskClass", "Qtr", "NC_PI")
  
  number <- full_join(tmp.n.bi, tmp.n.pd, by = c("Type", "RiskClass","Qtr"))
  number <- full_join(number, tmp.n.com, by = c("Type", "RiskClass","Qtr"))
  number <- full_join(number, tmp.n.col, by = c("Type", "RiskClass","Qtr"))
  number <- full_join(number, tmp.n.pi, by = c("Type", "RiskClass","Qtr"))
  
  tmp.n.bi <- df.amount[df.amount$coverage == "BI", names(df.amount) %in% c("Type", "RiskClass", "Qtr", "AAC")]
  names(tmp.n.bi) <- c("Type", "RiskClass", "Qtr", "AAC_BI")
  
  tmp.n.pd <- df.amount[df.amount$coverage == "PD", names(df.amount) %in% c("Type", "RiskClass", "Qtr", "AAC")]
  names(tmp.n.pd) <- c("Type", "RiskClass", "Qtr", "AAC_PD")
  
  tmp.n.com <- df.amount[df.amount$coverage == "COM", names(df.amount) %in% c("Type", "RiskClass", "Qtr", "AAC")]
  names(tmp.n.com) <- c("Type", "RiskClass", "Qtr", "AAC_COM")
  
  tmp.n.col <- df.amount[df.amount$coverage == "COL", names(df.amount) %in% c("Type", "RiskClass", "Qtr", "AAC")]
  names(tmp.n.col) <- c("Type", "RiskClass", "Qtr", "AAC_COL")
  
  tmp.n.pi <- df.amount[df.amount$coverage == "PI", names(df.amount) %in% c("Type", "RiskClass", "Qtr", "AAC")]
  names(tmp.n.pi) <- c("Type", "RiskClass", "Qtr", "AAC_PI")
  
  amount <- full_join(tmp.n.bi, tmp.n.pd, by = c("Type", "RiskClass","Qtr"))
  amount <- full_join(amount, tmp.n.com, by = c("Type", "RiskClass","Qtr"))
  amount <- full_join(amount, tmp.n.col, by = c("Type", "RiskClass","Qtr"))
  amount <- full_join(amount, tmp.n.pi, by = c("Type", "RiskClass","Qtr"))
  
  
  
  df.combined <- as.data.frame(full_join(number, amount, by = c("Type", "RiskClass", "Qtr")))
  
  return(df.combined)
  
}


autonomyPremium <- function(estimates.A0, type = "same", time.frame = 0:20, start.year = 2019, lvls){
  # estimates.A0 - estimates from the GLM.R
  # time.frame  - How long we do this
  
  
  if(type == "same"){
    # If always the same we just repeat the data.frame
    tmp <- estimates.A0
    # we need to have columns not rows
    r <- lapply(X = time.frame, FUN = function(x, tmp){
      tmp$Year <- x + start.year
      return(tmp)
    }, tmp = tmp)
    
    tmp <- do.call(what = rbind, args = r)
    # Add Autonomy
    r <- lapply(X = lvls, FUN = function(x, tmp){
      tmp$Autonomy <- x
      return(tmp)
    }, tmp = tmp)
    
    tmp <- do.call(what = rbind, args = r)
    return(tmp)
  }
}


aut.prem <- function(glm.list, type = "glm.original", predict.df){
  
  if(type == "glm.original"){

    # We also need to Add vehicle Size and stuff
    #creating factors for vehicle size variable
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
    
    # set facctor variables
    predict.df$Qtr <- factor(predict.df$Qtr)
    # initialize
    predict.df$AC_BI[predict.df$Autonomy == i] <- 0
    predict.df$AC_PD[predict.df$Autonomy == i] <- 0
    predict.df$AC_COM[predict.df$Autonomy == i] <- 0
    predict.df$AC_COL[predict.df$Autonomy == i] <- 0
    predict.df$AC_PI[predict.df$Autonomy == i] <- 0
    
    
    
    for(i in c("A0", "A1", "A2", "A3", "A4", "A5")){
      # Then we predict
      # NC
      predict.df$NC_BI[predict.df$Autonomy == i] <- predict(object = glm.list$Number$Model$BI, newdata = predict.df[predict.df$Autonomy == i,])
      predict.df$NC_PD[predict.df$Autonomy == i] <- predict(object = glm.list$Number$Model$PD, newdata = predict.df[predict.df$Autonomy == i, ])
      predict.df$NC_COM[predict.df$Autonomy == i] <- predict(object = glm.list$Number$Model$COM, newdata = predict.df[predict.df$Autonomy == i, ])
      predict.df$NC_COL[predict.df$Autonomy == i] <- predict(object = glm.list$Number$Model$COL, newdata = predict.df[predict.df$Autonomy == i,])
      predict.df$NC_PI[predict.df$Autonomy == i] <- predict(object = glm.list$Number$Model$PI, newdata = predict.df[predict.df$Autonomy == i,])
      # AAC
      predict.df$AAC_BI[predict.df$Autonomy == i] <- predict(object = glm.list$Amount$Model$BI, newdata = predict.df[predict.df$Autonomy == i,])
      predict.df$AAC_PD[predict.df$Autonomy == i] <- predict(object = glm.list$Amount$Model$PD, newdata = predict.df[predict.df$Autonomy == i,])
      predict.df$AAC_COM[predict.df$Autonomy == i] <- predict(object = glm.list$Amount$Model$COM, newdata = predict.df[predict.df$Autonomy == i,])
      predict.df$AAC_COL[predict.df$Autonomy == i] <- predict(object = glm.list$Amount$Model$COL, newdata = predict.df[predict.df$Autonomy == i,])
      predict.df$AAC_PI <- predict(object = glm.list$Amount$Model$PI, newdata = predict.df)
      
      # Then multiply to find the amount
      
      predict.df$AC_BI[predict.df$Autonomy == i] <- predict.df$Exposure[predict.df$Autonomy == i]*predict.df$NC_BI[predict.df$Autonomy == i]*predict.df$AAC_BI[predict.df$Autonomy == i]
      predict.df$AC_PD[predict.df$Autonomy == i] <- predict.df$Exposure[predict.df$Autonomy == i]*predict.df$NC_PD[predict.df$Autonomy == i]*predict.df$AAC_PD[predict.df$Autonomy == i]
      predict.df$AC_COM[predict.df$Autonomy == i] <- predict.df$Exposure[predict.df$Autonomy == i]*predict.df$NC_COM[predict.df$Autonomy == i]*predict.df$AAC_COM[predict.df$Autonomy == i]
      predict.df$AC_COL[predict.df$Autonomy == i] <- predict.df$Exposure[predict.df$Autonomy == i]*predict.df$NC_COL[predict.df$Autonomy == i]*predict.df$AAC_COL[predict.df$Autonomy == i]
      predict.df$AC_PI[predict.df$Autonomy == i] <- predict.df$Exposure[predict.df$Autonomy == i]*predict.df$NC_PI[predict.df$Autonomy == i]*predict.df$AAC_PI[predict.df$Autonomy == i]

      
      
    }
    
    # unfactor again
    predict.df$Qtr <- as.character(predict.df$Qtr)
    return(predict.df)
    
    
    
  }else if(type == "multiply"){
    # We assume we have the Right And thus We just multiply
    predict.df$AC_BI<- predict.df$Exposure*predict.df$NC_BI*predict.df$AAC_BI
    predict.df$AC_PD <- predict.df$Exposure*predict.df$NC_PD*predict.df$AAC_PD
    predict.df$AC_COM <- predict.df$Exposure*predict.df$NC_COM*predict.df$AAC_COM
    predict.df$AC_COL <- predict.df$Exposure*predict.df$NC_COL*predict.df$AAC_COL
    predict.df$AC_PI <- predict.df$Exposure*predict.df$NC_PI*predict.df$AAC_PI
    return(predict.df)
    
  }
  
}






# Number of claims ----


# considers the development in number of claims altough it is difficult to encorporate this function
change.in.no.claims <- function(type = "sigmoid", time.frame, pct.decrease = 0.9, t.point = 0, last.four.points, dampening.constant = 10, slice.point = 0 ){
  # type, what function to use
  # time.frame ...time frame
  # pct.decrease reached in the end (infinity)
  # t.point is tipping point 
  # dampening constant if that is needed
  # slice.point - where to connect two functions has to be a number divisible by 0.25
  
  # + 0.25 because time = 0 is something we have aldready observed
  t <- seq(from = min(time.frame)+0.25, to = max(time.frame), by = 0.25)
  df <- data.frame(time = t)

  
  if( type == "sigmoid"){
      # t is time, 
      # t.point is tipping point (where second derivative is zero)
      # param[1] is reduction in percentage
      
      df$decrease <- 1- pct.decrease/(1+exp(-(t-t.point)))
      
  }else if(type == "exp"){
    # we need to find the correct parameters with solver
    
    tmp <- function(param,t){
      a <- param[1]*exp(-param[2]*0) - 1
      b <- param[1]*exp(-param[2]*max(t)) - (1-pct.decrease)
      return(c(a,b))
    }
    tmp_sq <- function(param, t){crossprod(tmp(param, t),tmp(param, t))}
    
    out <- nlm(f = tmp_sq, p = c(1, 0.1), t = t)
   # out
    df$decrease <- out$estimate[1]*exp(-out$estimate[2]*t)
  }else if(type == "sigmoid.exp"){
    
    # we need to to (1-expression)*0.5 to compress is between 1 and 0
     sig <- 1- pct.decrease/(1+exp(-(t-t.point)))
     # fin y -coordinate of slice point
     y.slice <- sig[t == slice.point]
     # define t for sigmoid
     t.sig <- t[t<slice.point]
     # define t where exp function is ued
     t.exp <- t[t>= slice.point]
     
     tmp <- function(param,t){
       a <- param[1]*exp(-param[2]*min(t)) - 1
       b <- param[1]*exp(-param[2]*max(t)) - (1-pct.decrease)
       return(c(a,b))
     }
     tmp_sq <- function(param, t){crossprod(tmp(param, t),tmp(param, t))}
     
     out <- nlm(f = tmp_sq, p = c(1, 0.2), t = t.exp)
     exp.val <- out$estimate[1]*exp(-out$estimate[2]*t)
     
     df$decrease <- c(sig[t<slice.point], exp.val[t>=slice.point])
     
     
     
    }

  df$number <- last.four.points
  df$forecast <- df$decrease*df$number

  
  #plot(df$time,df$forecast)
  return(df)
  
}


# Amount changes ----


amount.changes <- function(type = "linear", time.frame = 0:20, inflation = 0.025, last.four.points = c(1,1,1,1), data, extracost = 1, start.year = 2019){
  # type - type of amount change to be used
  # time.frame ....time frame
  # last.four.points - previous year quarters
  
  # define time, we start at 0.25 because 0 is the last observed value
  t <- seq(from = min(time.frame)+0.25, to = max(time.frame), by = 0.25)
  df <- data.frame(time = t)
 # df$inflation <- (1+inflation)^(t)
  
  df$time = df$time + start.year
  
  if(type == "linear"){
    
    df.1 <- data.frame(Type = unique(data$Type))
    df.2 <- data.frame(RiskClass = unique(data$RiskClass))
    # we will estimate for each type and each Risk Class
    df.types <- as.data.frame(merge(df.1, df.2))
    index <- 1:dim(df.types)[1]
    
    predictions <- lapply(X = index, function(x, df, df.types, data){
      
      rc <- df.types$RiskClass[x]
      type <- df.types$Type[x]
      
      tmp <- data[data$RiskClass == rc & data$Type == type, ]
      tmp <- tmp[, names(tmp) %in% c("time", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI")]
      
      #bi
      lm <- lm(formula = AC_BI ~  time , data = tmp)
      df$AC_BI <- predict.lm(object = lm, newdata = df)
      
      # PD
      lm <- lm(formula = AC_PD ~  time , data = tmp)
      df$AC_PD <- predict.lm(object = lm, newdata = df)
      
      # COM
      lm <- lm(formula = AC_COM ~  time , data = tmp)
      df$AC_COM <- predict.lm(object = lm, newdata = df)
      
      # COL
      lm <- lm(formula = AC_COL ~  time , data = tmp)
      df$AC_COL <- predict.lm(object = lm, newdata = df)
      
      # PI
      lm <- lm(formula = AC_PI ~  time , data = tmp)
      df$AC_PI <- predict.lm(object = lm, newdata = df)
      
      df$RiskClass <- rc
      df$Type <- type
      
      return(df)
    }, df = df, df.types = df.types ,data = data)
    
    predictions <- do.call(what = rbind, args = predictions)

    # combine
    tmp <- rbind(predictions[, names(predictions) %in% c("time", "Type", "RiskClass", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI")], data[, names(data) %in% c("time", "Type", "RiskClass", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI")])
    
    
  }


return(tmp)
  
}
