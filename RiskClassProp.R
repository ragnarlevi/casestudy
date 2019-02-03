

riskClassProp <- function(type = "same", df.main, exposure.year = 2018, time.frame, start.year = 2019){
  # type - what proportion model to use
  # df.main - The data.frame
  # Exposure year what exposure year to use 
  
  
  if(type == "same"){
    
    # Divide into type and personal
    te <- list()
    for(i in 1:4){
      te$Personal[[paste("Qtr", i, sep ="")]] <- sum(df.main$Exposure[df.main$Qtr == i & df.main$Year == exposure.year & df.main$Type == "Personal"])
      te$Commercial[[paste("Qtr", i, sep ="")]] <- sum(df.main$Exposure[df.main$Qtr == i & df.main$Year == exposure.year & df.main$Type == "Commercial"])
    }
    # create a data frame with the proportion 
    prop <- df.main[df.main$Year == exposure.year, names(df.main) %in%  c("Qtr", "Exposure", "RiskClass", "Type", "Autonomy")]
    # calculate the proportions
    prop$prop <- 0

    # Personal
    prop$prop[prop$Qtr == 1 & prop$Type == "Personal"] <- prop$Exposure[prop$Qtr == 1 & prop$Type == "Personal"]/te$Personal[names(te$Personal) == "Qtr1"]
    prop$prop[prop$Qtr == 2 & prop$Type == "Personal"] <- prop$Exposure[prop$Qtr == 2 & prop$Type == "Personal"]/te$Personal[names(te$Personal) == "Qtr2"]
    prop$prop[prop$Qtr == 3 & prop$Type == "Personal"] <- prop$Exposure[prop$Qtr == 3 & prop$Type == "Personal"]/te$Personal[names(te$Personal) == "Qtr3"]
    prop$prop[prop$Qtr == 4 & prop$Type == "Personal"] <- prop$Exposure[prop$Qtr == 4 & prop$Type == "Personal"]/te$Personal[names(te$Personal) == "Qtr4"]
    # Commercial
    prop$prop[prop$Qtr == 1 & prop$Type == "Commercial"] <- prop$Exposure[prop$Qtr == 1 & prop$Type == "Commercial"]/te$Commercial[names(te$Commercial) == "Qtr1"]
    prop$prop[prop$Qtr == 2 & prop$Type == "Commercial"] <- prop$Exposure[prop$Qtr == 2 & prop$Type == "Commercial"]/te$Commercial[names(te$Commercial) == "Qtr2"]
    prop$prop[prop$Qtr == 3 & prop$Type == "Commercial"] <- prop$Exposure[prop$Qtr == 3 & prop$Type == "Commercial"]/te$Commercial[names(te$Commercial) == "Qtr3"]
    prop$prop[prop$Qtr == 4 & prop$Type == "Commercial"] <- prop$Exposure[prop$Qtr == 4 & prop$Type == "Commercial"]/te$Commercial[names(te$Commercial) == "Qtr4"]
   
    
    # assume the proportions are the same for every class and add to
    r <- lapply(X = c("A0", "A1", "A2", "A3", "A4", "A5"), FUN = function(x, prop){
      prop$Autonomy <- x
      return(prop)
      
    }, prop = prop)
    
    prop <- do.call(what = rbind, args = r)
    
    # Finally add Years
    r <- lapply(X = time.frame, FUN = function(x, prop){
      prop$Year <- x + start.year
      return(prop)
      
    }, prop = prop)
    prop <- do.call(what = rbind, args = r)
    
    return(prop[, names(prop) %in% c("Qtr", "RiskClass", "Type", "Autonomy", "prop", "Year"), drop = F])
  }
  
}





