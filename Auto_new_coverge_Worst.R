
A0 <- 0.34 + 0.005*t
A1 <- exp(-t*0.2)*0.6 +0.4
A2<- exp(-t*0.19)*0.6 +0.4

safelife.market.share <- data.frame(time = t,
                                    A0 = A0,
                                    A1 = A1,
                                    A2 = A2)

m.safe <- melt(data = safelife.market.share, id = "time")

ggplot(data = m.safe) + geom_line(mapping = aes(x = time, y = value, color = variable)) + 
  scale_x_continuous(name ="Year", breaks = c(time.frame), label = as.character(2019 + time.frame), expand = c(0,0)) +
   scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1), expand = c(0,0)) + # expand -> y axis begins at 0 strict
    theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill =  "#D3D3D3"),
    panel.background = element_rect(fill =  "#D3D3D3"),
    legend.background = element_rect(fill =  "#D3D3D3")) +
  labs(color = "Autonomy") + ggtitle("Safelife marketshare for each autonomy level") 



A1 <- exp(0.01*t)-1
A2 <- function(t){
  if(t<3){
    0
  }else{
    0.0009*(t-3)^2#exp(0.015*(t-3))-1
  }
}
A2 <- Vectorize(FUN = A2, vectorize.args = "t")
A0 <- 1-A1-A2(t)
  


carb.personal.pct <- data.frame(time = t,
                                A0 = A0,
                                A1 = A1,
                                A2 = A2(t))

m.carb <- melt(data = carb.personal.pct, id.vars = "time")
m.carb$value[m.carb$variable == "A2" & m.carb$value == 0] <- NA


ggplot(data = m.carb) + geom_line(mapping = aes(x = time, y = value, color = variable)) + 
    scale_x_continuous(name ="Year", breaks = c(time.frame), label = as.character(2019 + time.frame)) +
   scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1)) +
    theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  labs(color = "Autonomy") + ggtitle("Personal autonomy level proportions in Carbia")

# fix axisx position


A1 <- exp(0.012*t)-1
A2 <- function(t){
  if(t<3){
    0
  }else{
    0.0019*(t-3)^2#exp(0.015*(t-3))-1
  }
}
A2 <- Vectorize(FUN = A2, vectorize.args = "t")
A0 <- 1-A1-A2(t)

carb.commercial.pct <- data.frame(time = t,
                                A0 = A0,
                                A1 = A1,
                                A2 = A2(t))

m.carb <- melt(data = carb.commercial.pct, id.vars = "time")
m.carb$value[m.carb$variable == "A2" & m.carb$value == 0] <- NA
#m.carb$variable <- factor(m.carb$variable, levels = c("A2", "A1", "A0"), ordered = T)

#ggplot(data = m.carb) + geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity") + 
 # scale_fill_manual(values=c("#027984", "#0d0284", "#0755c1"))
ggplot(data = m.carb) + geom_line(mapping = aes(x = time, y = value, color = variable)) + 
      scale_x_continuous(name ="Year", breaks = c(time.frame), label = as.character(2019 + time.frame)) +
   scale_y_continuous(name ="Percentage", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0%", "20%","40%", "60%", "80%", "100%"), limits = c(0,1)) +
    theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  labs(color = "Autonomy") + ggtitle("Commercial autonomy level proportions in Carbia")

### Exposure of Personal and Commermcial
mark.share.p <- 0.34
mark.share.c <- 0.34

personal.exposure <- sum(autocar$Exposure[autocar$Year == 2018 & autocar$Qtr == 4 & autocar$Type == "Personal"])/mark.share.p
commercial.exposure <- sum(autocar$Exposure[autocar$Year == 2018 & autocar$Qtr == 4 & autocar$Type == "Commercial"])/mark.share.c 

carbia.exposure <- data.frame(time = t,
                              Personal = personal.exposure*(1+t*0.005),
                              Commercial = commercial.exposure*(1+0.025)^t)


m.carbia.exposure <- melt(data = carbia.exposure, id.vars = c("time"))

ggplot(data = m.carbia.exposure) + geom_line(mapping = aes(x = time, y = value, color = variable)) +
        scale_x_continuous(name ="Year", breaks = c(time.frame), label = as.character(2019 + time.frame)) +
   scale_y_continuous(name ="Exposure") +
    theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  labs(color = "Type") + ggtitle("Exposure growth in Carbia")
  

# Define as lists, same names 
freq.pct <- list()

freq.pct$A0 <- data.frame(time = t, 
                          NC_BI.pct = rep(1, length(t)), 
                          NC_PD.pct = rep(1, length(t)), 
                          NC_COM.pct = rep(1, length(t)), 
                          NC_COL.pct = rep(1, length(t)), 
                          NC_PI.pct = rep(1, length(t)))

freq.pct$A1 <- data.frame(time = t, 
                          NC_BI.pct = 0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0, 
                          NC_PD.pct = 0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0,
                          NC_COM.pct = rep(1, length(t)), 
                          NC_COL.pct = 0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0,
                          NC_PI.pct = 0.05*(1-carb.personal.pct$A0) + 0.2*carb.personal.pct$A0)

freq.pct$A2 <- data.frame(time = t, 
                          NC_BI.pct = 0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0, 
                          NC_PD.pct = 0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0, 
                          NC_COM.pct = rep(1, length(t)), 
                          NC_COL.pct = 0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0, 
                          NC_PI.pct = 0.025*(1-carb.personal.pct$A0) + 0.1*carb.personal.pct$A0)

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
                          AAC_COM.pct = rep(1.3, length(t)), 
                          AAC_COL.pct = rep(1.3, length(t)), 
                          AAC_PI.pct = rep(1, length(t)))

loss.pct$A2 <- data.frame(time = t, 
                          AAC_BI.pct = rep(1, length(t)), 
                          AAC_PD.pct = rep(1, length(t)), 
                          AAC_COM.pct = rep(1.5, length(t)), 
                          AAC_COL.pct = rep(1.5, length(t)), 
                          AAC_PI.pct = rep(1, length(t)))


# interst
bi_i <- glm$rd$Amount$Model$BI$coefficients[names(glm$rd$Amount$Model$BI$coefficients) == "time"]
pd_i <- glm$rd$Amount$Model$PD$coefficients[names(glm$rd$Amount$Model$PD$coefficients) == "time"]
com_i <- glm$rd$Amount$Model$COM$coefficients[names(glm$rd$Amount$Model$COM$coefficients) == "time"]
col_i <- glm$rd$Amount$Model$COL$coefficients[names(glm$rd$Amount$Model$COL$coefficients) == "time"]
pi_i <- glm$rd$Amount$Model$PI$coefficients[names(glm$rd$Amount$Model$PI$coefficients) == "time"]

mean_i <- mean(c(bi_i,pd_i,com_i,col_i,pi_i))

Auto_Coverage_Worst <- model.2(time.frame = time.frame[1:(length(time.frame)-1)], autocar = autocar, glm.list = glm$rd, 
                               safelife.market.share = safelife.market.share, carbia.exposure = carbia.exposure, carb.commercial.pct = carb.commercial.pct, 
                               carb.personal.pct = carb.personal.pct, freq.pct = freq.pct, loss.pct = loss.pct, 
                               MR.fac.a1 = 0.0255/3, IS.fac.a1 = 0.0127/3, CR.fac.a1 = 0.0464/3, interest = mean_i,
                               MR.fac.a2 = 0.0255/3, IS.fac.a2 = 0.0127/3, CR.fac.a2 = 0.0764/3) 
