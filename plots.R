

## plots
```{r}

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

```