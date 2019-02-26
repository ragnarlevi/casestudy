

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





