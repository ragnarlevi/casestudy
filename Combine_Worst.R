
A0 <- 0.34-0.005*t
A1 <- exp(-t*0.2)*0.16+0.34
A2 <- exp(-t*0.19)*0.16+0.34
safelife.market.share <- data.frame(time = t,
                                    A0 = A0,
                                    A1 = A1,
                                    A2 = A2)



## Carbia personal percentages
A1 <- function(t){
  if(t<5){
    (exp(0.01*t)-1)*exp((t-5)*0.05)
  }
  else{
    exp(0.01*t)-1
  }
}
A2 <- function(t){
  if(t<3){
    0
  }else{
    0.0005*(t-3)^2#exp(0.015*(t-3))-1
  }
}
A1 <- Vectorize(FUN = A1, vectorize.args = "t")
A2 <- Vectorize(FUN = A2, vectorize.args = "t")
A0 <- 1-A1(t)-A2(t)



carb.personal.pct <- data.frame(time = t,
                                A0 = A0,
                                A1 = A1(t),
                                A2 = A2(t))



A1 <- function(t){
  if(t<5){
    (exp(0.012*t)-1)*exp((t-5)*0.05)
  }
  else{
    exp(0.012*t)-1
  }
}
A2 <- function(t){
  if(t<3){
    0
  }else{
    0.001*(t-3)^2#exp(0.015*(t-3))-1
  }
}
A1 <- Vectorize(FUN = A1, vectorize.args = "t")
A2 <- Vectorize(FUN = A2, vectorize.args = "t")
A0 <- 1-A1(t)-A2(t)

carb.commercial.pct <- data.frame(time = t,
                                  A0 = A0,
                                  A1 = A1(t),
                                  A2 = A2(t))



### Exposure of Personal and Commermcial
mark.share.p <- 0.34
mark.share.c <- 0.34

personal.exposure <- sum(autocar$Exposure[autocar$Year == 2018 & autocar$Qtr == 4 & autocar$Type == "Personal"])/mark.share.p
commercial.exposure <- sum(autocar$Exposure[autocar$Year == 2018 & autocar$Qtr == 4 & autocar$Type == "Commercial"])/mark.share.c 

carbia.exposure <- data.frame(time = t,
                              Personal = personal.exposure*(1+t*0.005),
                              Commercial = commercial.exposure*(1+0.025)^t)

# Define as lists, same names 
freq.pct <- list()

freq.pct$A0 <- data.frame(time = t, 
                          NC_BI.pct = rep(1, length(t)), 
                          NC_PD.pct = rep(1, length(t)), 
                          NC_COM.pct = rep(1, length(t)), 
                          NC_COL.pct = rep(1, length(t)), 
                          NC_PI.pct = rep(1, length(t)))

freq.pct$A1 <- data.frame(time = t, 
                          NC_BI.pct = (0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0)/2, 
                          NC_PD.pct = (0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0)/2,
                          NC_COM.pct = rep(1, length(t)), 
                          NC_COL.pct = (0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0)/2,
                          NC_PI.pct = (0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0)/2)

freq.pct$A2 <- data.frame(time = t, 
                          NC_BI.pct = (0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0)/2, 
                          NC_PD.pct = (0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0)/2, 
                          NC_COM.pct = rep(1, length(t)), 
                          NC_COL.pct = (0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0)/2, 
                          NC_PI.pct = (0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0)/2)

loss.pct <- list()
loss.pct$A0 <- data.frame(time = t, 
                          AAC_BI.pct = rep(1, length(t)), 
                          AAC_PD.pct = rep(1, length(t)), 
                          AAC_COM.pct = rep(1, length(t)), 
                          AAC_COL.pct = rep(1, length(t)), 
                          AAC_PI.pct = rep(1, length(t)))

loss.pct$A1 <- data.frame(time = t, 
                          AAC_BI.pct = rep(1, length(t)), 
                          AAC_PD.pct = rep(1, length(t)), 
                          AAC_COM.pct = rep((1.3-1)/2+1, length(t)), 
                          AAC_COL.pct = rep((1.3-1)/2+1, length(t)), 
                          AAC_PI.pct = rep(1, length(t)))

loss.pct$A2 <- data.frame(time = t, 
                          AAC_BI.pct = rep(1, length(t)), 
                          AAC_PD.pct = rep(1, length(t)), 
                          AAC_COM.pct = rep((1.5-1)/2+1, length(t)), 
                          AAC_COL.pct = rep((1.5-1)/2+1, length(t)), 
                          AAC_PI.pct = rep(1, length(t)))




# interst
bi_i <- glm$rd$Amount$Model$BI$coefficients[names(glm$rd$Amount$Model$BI$coefficients) == "time"]
pd_i <- glm$rd$Amount$Model$PD$coefficients[names(glm$rd$Amount$Model$PD$coefficients) == "time"]
com_i <- glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "time"]
col_i <- glm$rd$Amount$Model$COL$coefficients[names(glm$rd$Amount$Model$COL$coefficients) == "time"]
pi_i <- glm$rd$Amount$Model$PI$coefficients[names(glm$rd$Amount$Model$PI$coefficients) == "time"]

mean_i <- mean(c(bi_i,pd_i,com_i,col_i,pi_i))

combine_Worst <- model.2(time.frame = time.frame[1:(length(time.frame)-1)], autocar = autocar, glm.list = glm$rd, 
                         safelife.market.share = safelife.market.share, carbia.exposure = carbia.exposure, carb.commercial.pct = carb.commercial.pct, 
                         carb.personal.pct = carb.personal.pct, freq.pct = freq.pct, loss.pct = loss.pct, 
                         MR.fac.a1 = 0.0255/3, IS.fac.a1 = 0.0127/3, CR.fac.a1 = 0.0464/3, interest = mean_i,
                         MR.fac.a2 = 0.0255/3, IS.fac.a2 = 0.0127/3, CR.fac.a2 = 0.0764/3,
                         sl.enter.year = sl.enter.year, 
                         sl.enter.qtr = sl.enter.qtr) 
