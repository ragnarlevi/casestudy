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
  
  
  # appends corresponding estimates to the df.main
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
  
  
  
  # get namse of  coverage (policies)
  df.1 <- data.frame(coverage = names(Data.List[[m]]$Model))
  # Type
  df.2 <- data.frame(Type = unique(autocar$Type))
  # Risk Classes
  df.3 <- data.frame(RiskClass = unique(autocar$RiskClass))
  # quarters
  df.4 <- data.frame(Qtr = c(1,2,3,4))
  
  
  df.main <- merge(df.1, df.2)
  df.main <- merge(df.main, df.3)
  df.main <- merge(df.main, df.4)
  df.main <- as.data.frame(df.main)
  # should be 1080 rows....1080 estimates wooppyy
  
  df.main <- calcEstimates(df.main = df.main, model = Data.List[[m]]$Model)
  # change the Names
  if(m == "Number"){
    names(df.main)[names(df.main) == "estimate"] <- "NPerExposure"
  }else{
    names(df.main)[names(df.main) == "estimate"] <- "AAC"
  }
  
  return(df.main)
  
  
}

loop.fun <- function(j, i, df.main, loop.df, growth, start.year, df.number, df.amount){
  tmp <- df.main[1,]
  tmp$Year <- start.year + time.frame[i]
  tmp$Qtr <- loop.df$Qtr[j]
  tmp$RiskClass <- as.character(loop.df$RiskClass[j])
  tmp$Type <- as.character(loop.df$Type[j])
  # Here is where the fun part comes/ this is something we really don't know about
  bool <- df.main$Year == start.year & df.main$Qtr == tmp$Qtr & df.main$RiskClass == tmp$RiskClass & df.main$Type == tmp$Type
  tmp$Exposure <- growth(Type = tmp$Type,
                         Qtr = tmp$Qtr,
                         RiskClass = tmp$RiskClass,
                         df.main = df.main,
                         t = 2018)
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


EstimateGrowth <- function(Data.List, growth, time.frame = 1, df.main, lapply.Fun, ...){
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
    # lapply much faster then a for loop
    r <- lapply(X = 1:dim(loop.df)[1], 
                FUN = loop.fun, i = i, 
                df.main = df.main, 
                loop.df = loop.df,
                growth = growth, 
                start.year = start.year,
                df.amount = df.amount,
                df.number = df.number)
    
   df.main <-  rbind(df.main, do.call(rbind,r))
    
    
  }
  
  
  # View(df.main[df.main$RiskClass == "SML" & df.main$Type == "Personal", names(df.main) %in% c("Exposure", "Year")])
  
  # We Return a list with the information needed
  ret <- list()
  ret$prediction <- df.main
  ret$Data.List <- Data.List
  return(ret)
  
}
