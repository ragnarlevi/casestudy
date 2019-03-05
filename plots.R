

tmp <- predict.df[, c("time", "Exposure", "Type", "Autonomy")]

tmp <- aggregate(. ~ time + Type + Autonomy, data = tmp, FUN = sum)
tmp$Autonomy <- factor(tmp$Autonomy, levels = c("A0", "A1", "A"), ordered = T)
tmp <- tmp[order(tmp$Autonomy, decreasing = T),]

ggplot(tmp) + geom_bar(mapping = aes(x = time, y = Exposure, fill = Autonomy, group = Type) , 
                       stat = "identity", 
                       position = "stack", 
                       width = 0.2) +
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


