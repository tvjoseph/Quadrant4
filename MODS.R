# JMJPFU
# 1-Oct-2016

library(dplyr)
library(ggplot2)

MOS <- Mind_of_ds %>% select(Conductance,Bat)

Mos_agg <- MOS %>% group_by(Bat) %>% summarise(sd(Conductance))

colnames(Mos_agg) <- c("Bat","Conductance")

q2 <- ggplot(data=Mos_agg,aes(Bat,Conductance,color=Bat)) + geom_point()
q2 + geom_smooth() + theme(axis.text.x = element_text(angle=70,hjust = 1))

q3 <- ggplot(data=Mind_of_ds,aes(ind,Conductance,group=1,color=Bat)) + geom_point()
q3 + geom_smooth() + theme(axis.text.x = element_text(angle=70,hjust = 1)) + facet_grid(.~Bat)