library(dplyr)
#library(stats)
library(psych)
library(ggplot2)

library(reshape2)

setwd('D:/AI in parkinsonism/rplot')
df<-read.table('upperAi.csv', sep = ',', header = TRUE, colClasses = 'character')

colnames(df)<-c('RefNo','Authors','Aim','Extremity',
                'Instrumentation','Subjects','Algorithm',
                'Specificity','Sensitivity','Accuracy',
                'Notes','No_of_Subjects')

df$Specificity<-as.numeric(df$Specificity)
df$Sensitivity<-as.numeric(df$Sensitivity)
df$Accuracy<-as.numeric(df$Accuracy)
df$No_of_Subjects<-as.numeric(df$No_of_Subjects)
print(max(df$No_of_Subjects,na.rm=TRUE))
df$No_of_Subjects<-cut(df$No_of_Subjects,breaks=c(1,24,50,100,200))


df$Goal<-as.factor(df$Aim)
df$Algorithm<-as.factor(df$Algorithm)

df<-melt(df,measure.vars=c('Accuracy','Sensitivity','Specificity'), variable.name ='measure')
df$value<-as.numeric(df$value)

dfDiag<-df[which(df$Aim=='diagnosis'),]
dfAssess<-df[which(df$Aim=='assessment'),]

# gDiag<-ggplot(data=dfDiag)+geom_bar(aes(Authors,value, fill=measure), stat = 'identity', position ='dodge')+
#   facet_grid(.~Algorithm,scales = "free", space = "free")+
#   theme(axis.text.x=element_text(angle=90, hjust=1))+
#   scale_fill_manual(values=c('#fdb863','#b2abd2','#5e3c99'))+
#   ggtitle('LOWER EXTREMITIES in diagnostics: Performance measures by algorithm\n')+
#   theme(legend.title=element_blank())+
#   ylab('[%]')+
#   xlab('')

df<-df[which(df$Algorithm!='DA'),]



gUp<-ggplot(data=df)+
  geom_point(aes(measure,value,shape=Instrumentation,color=measure,size=No_of_Subjects,stroke=2),alpha=0.7)+
  
  facet_grid(Aim~Algorithm,scales = "free", space = "free")+
  scale_fill_manual(values=c('#fdb863','#b2abd2','#5e3c99'),name=element_blank())+
  scale_color_manual(values=c('#e66101','#fdb863','#5e3c99'),name=element_blank())+
  #geom_boxplot(aes(measure,value,fill=measure))+
  ggtitle('UPPER EXTREMITIES: Performance measures by algorithm\n')+
  theme(axis.ticks.x=element_blank(),
        axis.text = element_text(size=15),
        strip.text = element_text(size=16),
        plot.title=element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  guides(colour = guide_legend(override.aes = list(size=7)),
         shape = guide_legend(override.aes = list(size=5)))+
  scale_x_discrete(labels=c("Ac","Se","Sp"))+
  scale_size_manual(labels=c('<25','[25,50]','(50,100]','>100'),values=c(2,4,6,13), name='Patients #')+
  scale_shape_manual(values=c(25,18,8,7,15,1))+
  ylab('[%]')+
  xlab('')


