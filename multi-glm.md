setwd('/Users/liyixian1990/desktop/test2')
aquatic<-read.csv('24adp.csv',row.names=1)
aquatic.scale<-data.frame(scal(aquatic))#给归一化一下
alm<-glm(He~BOD+Fecal+TDS+TP+TNFux+WT+TN_nitrite1,data=aquatic.scale)
summary(alm)

lm<-lm(He~BOD+Fecal+TDS+TP+TNFux+WT+TN_nitrite,data=aquatic.scale)
summary(lm)
llm<-summay(alm)$coefficients
write.csv(llm,'llmresults.csv')

library(ggthemes)
library(ggplot2)
pass<-read.csv('llmresuls.csv')
st<-ggplot(pas, aes(Estimates, Factors))+
  geom_point(size=8,color = "#E18727FF")+
  geom_errorbarh(aes(xmax = Estimate +`S`, xmin = Estimate -`SE`),size= 1,height = 0.2, colour = "#00A07FF") +
  scale_x_continuous(limits= c(-0.5, 1.0))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", linewidth = 1) +
  xlab('Effct size ')+ 
  ylab(' ')+
  theme_few()+
  theme(axis.text.x = element_text(size = 10, color = "black"))+
  theme(axis.text.y = element_text(size = 10, color = "black"))+
  theme(title=element_text(size=14))+
  annotate("text", label = "**",
           x = 0.5, y = 6, size =6, colour = "black")+
  annotate("text", label = "***",
           x = 0.5, y = 7, size =6, colour = "black")+
  annotate("text", label = "***",
           x = 0.5, y = 8, size =6, colour = "black")

st
dev.off()