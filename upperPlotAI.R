library(dplyr)
#library(stats)
library(psych)
library(ggplot2)

library(reshape2)

setwd('D:/AI in parkinsonism/rplot/fwd')
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
  scale_fill_manual(values=c('#5e3c99','#fdb863','#b2abd2'),name=element_blank())+
  scale_color_manual(values=c('#5e3c99','#e66101','#fdb863'),name=element_blank())+
  #geom_boxplot(aes(measure,value,fill=measure))+
  theme_light()+
  ggtitle('Performance measures by algorithm based on kinematics of upper extremities\n')+
  theme(axis.ticks.x=element_blank(),
        axis.text = element_text(size=17,family = 'serif'),
        strip.text = element_text(size=18,family = 'serif'),
        plot.title=element_text(size=18,family = 'serif'),
        legend.text = element_text(size=17, family = 'serif'),
        legend.title = element_text(size=17,family = 'serif'))+


  scale_x_discrete(labels=c("Ac","Se","Sp"))+
  scale_size_manual(labels=c('<25','[25,50]','(50,100]','>100'),values=c(2,4,6,10), name='Number of patients')+
  scale_shape_manual(values=c(25,18,8,7,15,1))+
  guides(colour = guide_legend(override.aes = list(size=7), order = 1),
         shape = guide_legend(override.aes = list(size=5), order = 2),
         size = guide_legend(order = 3))+
  ylab('[%]')+
  xlab('')


ggsave('gUpper500dpiLight.tiff', width = 15, height = 9, dpi = 500)


