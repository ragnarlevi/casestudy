library(readxl)
autocar <- read_excel("2019-student-research-case-study-data.xlsx", range = "B10:P2170")
attach(autocar)
colnames(autocar) <- c("Year", "Qtr", "RiskClass", "Type", "Exposure", "NCBodilyInjury", "NCPropertyDamage", "NCComprehensive", "NCCollision", 
                       "NCPersonalInjury", "ACBodilyInjury", "ACPropertyDamage",
                       "ACComprehensive", "ACCollision", "ACPersonalInjury")
