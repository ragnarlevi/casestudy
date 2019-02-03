# Autonomy Claims ----

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


autonomyPremium <- function(estimates.A0, type = "same", time.frame = 0:20, start.year = 2019){
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
    r <- lapply(X = c("A0", "A1", "A2", "A3", "A4", "A5"), FUN = function(x, tmp){
      tmp$Autonomy <- x
      return(tmp)
    }, tmp = tmp)
    
    tmp <- do.call(what = rbind, args = r)
    return(tmp)
  }
}







