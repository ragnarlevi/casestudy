#---- Most recent exposure the same

growth.naive <- function(t = NULL, propAutonomy = NULL, propRC = NULL, Type = NULL, RiskClass = NULL, Coverage = NULL, acRate =NULL, nRate = NULL, df.main = NULL, Qtr = NULL){
  
  
  return(df.main$Exposure[df.main$Qtr == Qtr & df.main$RiskClass == RiskClass & df.main$Type == Type & df.main$Year == t])
}
  
