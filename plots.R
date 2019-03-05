

plots <- list()

# Change the default style of ggplot (plot titles are now centered by default)
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.title = element_text(margin = margin(t = 15, r = 5.5, b = 5.5, l = 5.5)))
theme_update(axis.title.x = element_text(margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)))
theme_update(axis.title.y = element_text(margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 20)))


# remove exposure = 0, but also keep 

predict.df.zero <- predict.df[predict.df$Exposure == 0, ]
predict.df <- predict.df[predict.df$Exposure != 0, ]

# Total Exposure Growth
tmp <- predict.df[, names(predict.df) %in% c("time", "Exposure")]
tmp <- aggregate(formula = . ~ time , data = tmp, FUN = sum)

plots$total.exposure <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure)) + ggtitle("Total years of exposure") + scale_y_continuous(name = "Years of exposure in 100,000s", breaks = c(0:8) * 10^5, labels = c(0:8), limits = c(0, 800000) ) + scale_x_continuous(name = "Year")

plots$total.exposure

# Autonomy evolution
tmp <- predict.df[, names(predict.df) %in% c("time", "Exposure", "Autonomy")]
tmp <- aggregate(formula = . ~ time + Autonomy, data = tmp, FUN = sum)
tmp$Exposure[tmp$Exposure == 0 & tmp$Autonomy == "A2"] <- NA

plots$Autonomy.evolution <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure, color = Autonomy)) + ggtitle("Total years of exposure for each autonomy level") + theme(plot.title = element_text(size = 10)) + scale_y_continuous(name = "Years of exposure in 100,000s", breaks = c(0:8) * 10^5, labels = c(0:8) ) + scale_x_continuous(name = "Year")

plots$Autonomy.evolution

# Personal evolution
tmp <- predict.df[predict.df$Type == "Personal", names(predict.df) %in% c("time", "Exposure", "Autonomy")]
tmp <- aggregate(formula = . ~ time + Autonomy, data = tmp, FUN = sum)
tmp$Exposure[tmp$Exposure == 0 & tmp$Autonomy == "A2"] <- NA

plots$Autonomy.evolution.personal <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure, color = Autonomy)) + ggtitle("Total years of exposure for each autonomy level (personal type)") + theme(plot.title = element_text(size = 10)) + scale_y_continuous(name = "Years of exposure in 100,000s", breaks = c(0:8) * 10^5, labels = c(0:8) ) + scale_x_continuous(name = "Year")

plots$Autonomy.evolution.personal

# Commercial evolution
tmp <- predict.df[predict.df$Type == "Commercial", names(predict.df) %in% c("time", "Exposure", "Autonomy")]
tmp <- aggregate(formula = . ~ time + Autonomy, data = tmp, FUN = sum)

plots$Autonomy.evolution.commercial <- ggplot(data = tmp) + geom_line(aes(x = time, y = Exposure, color = Autonomy)) + ggtitle("Total years of exposure for each autonomy level (commercial type)") + theme(plot.title = element_text(size = 10)) + scale_y_continuous(name = "Years of exposure in 100,000s", breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4) * 10^5, labels = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4) ) + scale_x_continuous(name = "Year")

plots$Autonomy.evolution.commercial

# PLot the Autonomy evolutions

# multiplot(plots$Autonomy.evolution.personal, plots$Autonomy.evolution, plots$Autonomy.evolution.commercial, cols = 2)


# Plot Claims evolution personal
tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI", "Exposure")]


tmp <- tmp[, !(names(tmp) %in% "Exposure")]



tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = mean)
tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))

plots$frequency.Personal <- ggplot(data = tmp.melt[tmp.melt$Type == "Personal", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Personal frequency per exposure evolution aggregated With mean")

plots$frequency.Personal

# plot amount evolution personal
tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "AAC_BI", "AAC_PD", "AAC_COM", "AAC_COL", "AAC_PI")]
tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = mean)
tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))

plots$amount.Personal <- ggplot(data = tmp.melt[tmp.melt$Type == "Personal", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Personal mount evolution aggregated With mean")

plots$amount.Personal

# Plot Claims evolution Commercial
tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = mean)
tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))

plots$frequency.Commercial <- ggplot(data = tmp.melt[tmp.melt$Type == "Commercial", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Commercial frequency evolution aggregated With mean")

plots$frequency.Commercial
# plot amount evolution Commercial
tmp <- predict.df[, names(predict.df) %in% c("time", "Autonomy", "Type", "AAC_BI", "AAC_PD", "AAC_COM", "AAC_COL", "AAC_PI")]
tmp <- aggregate(formula = . ~ time + Autonomy + Type, data = tmp, FUN = mean)
tmp.melt <- melt(data = tmp, id = c("time", "Autonomy", "Type"))

plots$amount.Commercial <- ggplot(data = tmp.melt[tmp.melt$Type == "Commercial", ]) + geom_line(mapping = aes(x = time, y = value, color = Autonomy)) + facet_wrap(facets = . ~ variable, scales = "free") + ggtitle("Commercial amount evolution aggregated With mean")
plots$amount.Commercial




# Plot AC
tmp <- predict.df[, names(predict.df) %in% c("time", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "AC_IS", "AC_CR", "AC_MR")]
tmp <- aggregate(formula = . ~ time , data = tmp, FUN = sum)
tmp.melt <- melt(data = tmp, id = c("time"))

ggplot(data = tmp.melt) + geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity") + ggtitle("Total claim amount")

# Plot AC Personal
tmp <- predict.df[ predict.df$Type == "Personal", names(predict.df) %in% c("time", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "AC_CR", "AC_MR", "AC_IS")]
tmp <- aggregate(formula = . ~ time , data = tmp, FUN = sum)
tmp.melt <- melt(data = tmp, id = c("time"))

ggplot(data = tmp.melt) + geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity") + ggtitle("Total personal claim amount") 

# Plot AC Commercial
tmp <- predict.df[ predict.df$Type == "Commercial", names(predict.df) %in% c("time", "AC_BI", "AC_PD", "AC_COM", "AC_COL", "AC_PI", "AC_CR", "AC_MR", "AC_IS")]
tmp <- aggregate(formula = . ~ time , data = tmp, FUN = sum)
tmp.melt <- melt(data = tmp, id = c("time"))

ggplot(data = tmp.melt) + geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity") + ggtitle("Total commercial claim amount") 


# Plot NC
tmp <- predict.df[, names(predict.df) %in% c("time", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
tmp <- aggregate(formula = . ~ time , data = tmp, FUN = sum)
tmp.melt <- melt(data = tmp, id = c("time"))

ggplot(data = tmp.melt) + geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity") + ggtitle("total claims")

# Plot NC Personal
tmp <- predict.df[ predict.df$Type == "Personal", names(predict.df) %in% c("time", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
tmp <- aggregate(formula = . ~ time , data = tmp, FUN = sum)
tmp.melt <- melt(data = tmp, id = c("time"))

ggplot(data = tmp.melt) + geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity") + ggtitle("Total personal claims") 

# Plot NC Commercial
tmp <- predict.df[ predict.df$Type == "Commercial", names(predict.df) %in% c("time", "NC_BI", "NC_PD", "NC_COM", "NC_COL", "NC_PI")]
tmp <- aggregate(formula = . ~ time , data = tmp, FUN = sum)
tmp.melt <- melt(data = tmp, id = c("time"))

ggplot(data = tmp.melt) + geom_bar(mapping = aes(x = time, y = value, fill = variable) , stat = "identity") + ggtitle("Total commercial claims") 

tmp <- predict.df[, c("time", "RiskClass", "prop", "Type", "Autonomy")]

ggplot(data = tmp)+ geom_line(mapping = aes(x = time, y = prop, color = RiskClass)) + facet_wrap(. ~ Type + Autonomy, scales = "free")




# Exposure PLot --------------------
tmp <- predict.df[, c("time", "Exposure", "Type", "Autonomy")]

tmp <- aggregate(. ~ time + Type + Autonomy, data = tmp, FUN = sum)
tmp$Autonomy <- factor(tmp$Autonomy, levels = c("A0", "A1", "A2"), ordered = T)
tmp <- tmp[order(tmp$Autonomy, decreasing = T),]

tmp.1 <- tmp[tmp$time <= 2019,]
tmp.2 <- tmp[tmp$time > 2019,]

ggplot() + geom_bar(data = tmp.1, mapping = aes(x = time, y = Exposure, fill = Autonomy, group = Type) , 
                    stat = "identity", 
                    position = "stack", 
                    width = 0.2,
                    alpha = 1) +
  geom_bar(data = tmp.2, mapping = aes(x = time, y = Exposure, fill = Autonomy, group = Type) , 
           stat = "identity", 
           position = "stack", 
           width = 0.2,
           alpha = 1) +
  facet_wrap(. ~ Type) + 
  scale_fill_manual(values = c("A2" =  "#004F71", "A1" = "#298FC2", "A0" = "#8DC8E8")) + 
  theme_bw(base_size = 20) + 
  scale_y_continuous(expand = c(0,0), 
                     name = "Exposure in thousands", 
                     breaks = c(0, 100000, 200000, 300000, 400000, 500000),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits = c(0, 500000)) +
  scale_x_continuous(expand = c(0,0), 
                     name = "Time")


# Descriptive statistics -----

# Exposure
tmp <- predict.df[predict.df$time<= 2019, c("time", "Exposure", "Type")]
tmp <- aggregate(. ~ time + Type, data = tmp, FUN = sum)

ggplot() + geom_line(data = tmp, mapping = aes(x = time, y = Exposure , color = Type), size = 2) + 
  theme_bw(base_size = 19) +
  scale_y_continuous(expand = c(0,0), 
                     name = "Exposure in thousands", 
                     breaks = c(0, 100000, 200000, 300000, 400000, 500000),
                     labels = c("0", "100", "200", "300", "400", "500"),
                     limits = c(0, 500000)) +
  scale_x_continuous(expand = c(0,0), 
                     name = "Time",
                     breaks = c(2010, 2012, 2014, 2016, 2018),
                     labels = c("2010", "2012", "2014", "2016", "2018"),
                     limit = c(2009,2019)) +
  scale_color_manual(values = c("Commercial" = "#004F71", "Personal" =  "#298FC2")) +
  ggtitle("Exposure growth for Safelife")

  
# AC per coverage

tmp <- predict.df[predict.df$time<= 2019, c("time", "Type", "AC_BI_PV", "AC_PD_PV", "AC_COM_PV", "AC_COL_PV", "AC_PI_PV")]

tmp <- aggregate(. ~ time + Type, data = tmp, FUN = sum)
tmp.melt <- melt(tmp, id.vars = c("time", "Type"))

ggplot() + geom_bar(data = tmp.melt, 
                    mapping = aes(x = time, y = value, fill = variable),
                    stat = "identity", 
                    position = "stack",
                    width = 0.19) +
  theme_bw(base_size = 17) +
  scale_y_continuous(expand = c(0,0), 
                     name = "Severity in billions", 
                     breaks = c(0, 0.3, 0.6, 0.9, 1.2)*10^(9),
                     labels = c("0", "30", "60", "90", "120"),
                     limits = c(0, 1200000000)) +
  scale_x_continuous(expand = c(0,0), 
                     name = "Time",
                     breaks = c(2010, 2012, 2014, 2016, 2018),
                     labels = c("2010", "2012", "2014", "2016", "2018"),
                     limit = c(2009,2019))+
  ggtitle("Severity with present value factor of 2019") +
  facet_wrap(. ~ Type)
  


