EstimateGrowth <- function(Data.List, growth, time.frame = 1, df.main){
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
  
  loop.df <- merge(df.1,df.2)
  loop.df <- merge(loop.df, df.3)
  loop.df <- as.data.frame(loop.df)
  
  # append to Our data.Frame 
  for( i in time.frame){
    # This for loop counts time
    
    # There was one error the df.dataframe loops over each coverage but that is unnecessay and wrong
    # because we deal with the coverage inside the for loop. We only want to do this for each type, RiskClass and Qtr
    for(j in 1:dim(loop.df)[1]){
      # Create tmp data.frame. This tmp df will be added to the prediction data frame once every missing value has been filled 
      tmp <- df.main[1,]
      tmp$Year <- start.year + time.frame[i]
      tmp$Qtr <- loop.df$Qtr[j]
      tmp$RiskClass <- as.character(loop.df$RiskClass[j])
      tmp$Type <- as.character(loop.df$Type[j])
      # Here is where the fun part comes/ this is something we really don't know about
      bool <- df.main$Year == start.year & df.main$Qtr == tmp$Qtr & df.main$RiskClass == tmp$RiskClass & df.main$Type == tmp$Type
      tmp$Exposure <- growth(exposure = df.main$Exposure[bool])
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
      
      df.main <- rbind(df.main, tmp)
      
    }
    
  }
  
  
  # View(df.main[df.main$RiskClass == "SML" & df.main$Type == "Personal", names(df.main) %in% c("Exposure", "Year")])
  
  # We Return a list with the information needed
  ret <- list()
  ret$prediction <- df.main
  ret$Data.List <- Data.List
  return(ret)
  
}
