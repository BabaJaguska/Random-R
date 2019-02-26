library(dplyr)
#library(stats)
library(psych)
library(ggplot2)

library(reshape2)

setwd('D:/AI in parkinsonism/rplot/fwd')
df<-read.table('AICarambaTabelaLower.csv', sep = ',', header = TRUE, colClasses = 'character')

colnames(df)<-c('RefNo','Authors','Aim','Extremity',
                'Instrumentation','Subjects','Algorithm',
                'Specificity','Sensitivity','Accuracy',
                'Notes','No_of_Subjects')

df$Specificity<-as.numeric(df$Specificity)
df$Sensitivity<-as.numeric(df$Sensitivity)
df$Accuracy<-as.numeric(df$Accuracy)
df$No_of_Subjects<-as.numeric(df$No_of_Subjects)
#h<-hist(df$No_of_Subjects)
print(min(df$No_of_Subjects))
df$No_of_Subjects<-cut(df$No_of_Subjects,breaks=c(1,24,50,100,600))

df$Goal<-as.factor(df$Aim)
df$Algorithm<-as.factor(df$Algorithm)

df<-melt(df,measure.vars=c('Accuracy','Sensitivity','Specificity'), variable.name ='measure')
df$value<-as.numeric(df$value)

dfDiag<-df[which(df$Aim=='diagnosis'),]
dfFOGdetect<-df[which(df$Aim=='FoG detection'),]
dfFOGpredict<-df[which(df$Aim=='FoG prediction'),]
dfAssess<-df[which(df$Aim=='assessment'),]
dfDiagFog<-df[which(df$Aim=='diagnosis' | df$Aim=='FoG detection'),]

# gDiag<-ggplot(data=dfDiag)+geom_bar(aes(Authors,value, fill=measure), stat = 'identity', position ='dodge')+
#   facet_grid(.~Algorithm,scales = "free", space = "free")+
#   theme(axis.text.x=element_text(size=15,angle=90, hjust=1))+
#   scale_fill_manual(values=c('#fdb863','#b2abd2','#5e3c99'))+
#   ggtitle('LOWER EXTREMITIES in diagnostics: Performance measures by algorithm\n')+
#   theme(legend.title=element_blank())+
#   ylab('[%]')+
#  xlab('')

g<-ggplot(data=dfDiagFog)+
  #geom_boxplot(aes(measure,value,fill=measure))+
  geom_point(aes(measure,value,shape=Instrumentation,color=measure,size=No_of_Subjects,stroke=2 ), alpha = 0.8)+

  facet_grid(Aim~Algorithm,scales = "free", space = "free")+
  scale_fill_manual(values=c('#5e3c99','#fdb863','#b2abd2'))+
  scale_color_manual(values=c('#5e3c99','#e66101','#fdb863'),name=element_blank())+
  #geom_boxplot(aes(measure,value,fill=measure))+
  ggtitle('Performance measures by algorithm based on kinematics of lower extremities\n')+
  theme_light()+
  theme(axis.ticks.x=element_blank(),
        axis.text = element_text(size=17,family = 'serif'),
        strip.text = element_text(size=18,family = 'serif'),
        plot.title=element_text(size=18,family = 'serif'),
        legend.text = element_text(size=17,family = 'serif'),
        legend.title = element_text(size=17,family = 'serif'))+
  guides(colour = guide_legend(override.aes = list(size=7), order = 1),
         shape = guide_legend(override.aes = list(size=5),order = 2),
         size = guide_legend(order = 3))+
        
  scale_x_discrete(labels=c("Ac","Se","Sp"))+
  scale_size_manual(labels=c('<25','[25,50]','(50,100]','>100'),values=c(3,5,7,10),name='Number of patients')+
  scale_shape_manual(values=c(1,17,18,7))+
  ylab('[%]')+
  xlab('')

ggsave('gLower500dpiLight.tiff', width = 13, height = 9, dpi = 500)


# gFOGd<-ggplot(data=dfFOGdetect)+geom_bar(aes(Authors,value, fill=measure), stat = 'identity', position ='dodge')+
#   facet_grid(.~Algorithm,scales = "free", space = "free")+ theme(axis.text.x=element_text(angle=90, hjust=1))+
#   scale_fill_manual(values=c('#fdb863','#b2abd2','#5e3c99'))+ggtitle('FOG detection: Performance measures by algorithm\n')+
#   theme(legend.title=element_blank())+
#   ylab('[%]')+
#   xlab('')
# 
# gFOGp<-ggplot(data=dfFOGpredict)+geom_bar(aes(Authors,value, fill=measure), stat = 'identity', position ='dodge')+
#   facet_grid(.~Algorithm,scales = "free", space = "free")+ theme(axis.text.x=element_text(angle=90, hjust=1))+
#   scale_fill_manual(values=c('#fdb863','#b2abd2','#5e3c99'))+ggtitle('FOG prediction: Performance measures by algorithm\n')+
#   theme(legend.title=element_blank())+
#   ylab('[%]')+
#   xlab('')
# 
# gAssess<-ggplot(data=dfAssess)+geom_bar(aes(Authors,value, fill=measure), stat = 'identity', position ='dodge')+
#   facet_grid(.~Algorithm,scales = "free", space = "free")+ theme(axis.text.x=element_text(angle=90, hjust=1))+
#   scale_fill_manual(values=c('#fdb863','#b2abd2','#5e3c99'))+ggtitle('LOWER EXTREMITIES in assessment: Performance measures by algorithm\n')+
#   theme(legend.title=element_blank())+
#   ylab('[%]')+
#   xlab('')