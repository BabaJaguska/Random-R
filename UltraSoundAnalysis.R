# Osteoporoza UZV podaci BONESTIM

###Data comes in a .csv file containing the following:
###Name: Patient Name
###Date: When the ultrasound measurement was done;DayMonthYear
###Stage: In what stage of the study was the US performed
###LeftRight: Either L-left side or R-right side of the back
###Position: PVM-paravertebral muscles;ThL-Thoracolumbar;L4/5
###Rest: Thickness of the muscle in rest [mm]
###Activation: Thickness of the muscle in activation [mm]

library(ggplot2)
library(grid)

# Set working directory
setwd('C:/Users/TECNALIA2014BELI/Desktop/Minjino/!BONESTIM/osteoporoza')

# Read the data
df<-read.table("UltraSoundDataforR.csv",sep=";",header=TRUE)

# Add a column for relative activation = Activation-rest/Rest
# df$RelativeActivation<-(df$Activation-df$Rest)/df$Rest;
df$RelativeActivation<-df$Activation/df$Rest

# subset so that mid term measurements are not taken into account
dfNoMid<-df[which(df$Stage !="2AfterFirstSeries "),] ## pazi bre imas space na kraju stageva -.-

# Plot
RelativePlot<-ggplot(df)+
  geom_bar(aes(Position,RelativeActivation,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~LeftRight)+
  ggtitle("Relative activation of muscles as measured by ultrasound")+
  ylab("Relative Activation = (Activ-Rest)/Rest")+
  scale_fill_manual(values = c("#DCADD1", "#feb24c","6D6135"))
  

AbsolutePlotRest<-ggplot(df)+
  geom_bar(aes(Position,Rest,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~LeftRight)+
  ggtitle("Muscle thickness in rest as measured by ultrasound")+
  ylab("Muscle thickness in rest [mm]")+
  scale_fill_manual(values = c("#ffeda0", "#feb24c","#f03b20"))+
  coord_cartesian(ylim=c(0, 32))

AbsolutePlotActivation<-ggplot(df)+
  geom_bar(aes(Position,Activation,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~LeftRight)+
  ggtitle("Muscle thickness in activation as measured by ultrasound")+
  scale_fill_manual(values = c("#ffeda0", "#feb24c","#f03b20"))+
  ylab("Muscle thickness in activation [mm]")+
  coord_cartesian(ylim=c(0, 32))


AbsolutePlotRestBeforeAfter<-ggplot(dfNoMid)+
  geom_bar(aes(Position,Rest,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~LeftRight)+
  ggtitle("Muscle thickness in rest as measured by ultrasound")+
  ylab("Muscle thickness in rest [mm]")+
  scale_fill_manual(values = c("#feb24c","#f03b20"))+
  coord_cartesian(ylim=c(0, 32))

AbsolutePlotActivationBeforeAfter<-ggplot(dfNoMid)+
  geom_bar(aes(Position,Activation,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~LeftRight)+
  ggtitle("Muscle thickness in activation as measured by ultrasound")+
  scale_fill_manual(values = c("#feb24c","#f03b20"))+
  ylab("Muscle thickness in activation [mm]")+
  coord_cartesian(ylim=c(0, 32))

RelativePlotBeforeAfter<-ggplot(dfNoMid)+
  geom_bar(aes(Position,RelativeActivation,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~LeftRight)+
  ggtitle("Relative activation of muscles as measured by ultrasound")+
  ylab("Relative Activation = Activation-Rest [mm]")+
  scale_fill_manual(values = c("#feb24c","#f03b20"))

# Average left and right
# split into left and right
samoLevo<-dfNoMid[which(dfNoMid$LeftRight=="L"),]
samoDesno<-dfNoMid[which(dfNoMid$LeftRight=="R"),]
colnames(samoLevo)[4]<-"Left"
colnames(samoDesno)[4]<-"Right"
# add right column 

levoDesno<-cbind(samoLevo,DesnoRest=samoDesno$Rest,DesnoActivation=samoDesno$Activation,DesnoRel=samoDesno$RelativeActivation)
levoDesno$AverageRest<-(levoDesno$Rest+levoDesno$DesnoRest)/2
levoDesno$AverageActivation<-(levoDesno$Activation+levoDesno$DesnoActivation)/2
levoDesno$AverageRelative<-100*(levoDesno$AverageActivation-levoDesno$AverageRest)/levoDesno$AverageRest
levels(levoDesno$Name)<-c("Subject 1","Subject 2")
levels(levoDesno$Stage)<-c("Before treatment","After 30 sessions","After 60 sessions")
levels(levoDesno$Position)<-c("Th5","ThL","L4/5")

g1<-ggplot(levoDesno)+
  geom_bar(aes(Position,AverageRest,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~.)+
  ggtitle("Muscle thickness in rest as measured by ultrasound AVERAGE")+
  ylab("Muscle thickness in rest [mm]")+
  scale_fill_manual(values = c("#feb24c","#f03b20"))
#geom_text(aes(Position,3,label=AverageRest),position = "stack")

  
g2<-ggplot(levoDesno)+
  geom_bar(aes(Position,AverageActivation,fill=Stage),stat="identity",position="dodge")+
  facet_grid(Name~.)+
  ggtitle("Muscle thickness in activation as measured by ultrasound AVERAGE")+
  ylab("Muscle thickness in activation [mm]")+
  scale_fill_manual(values = c("#feb24c","#f03b20"))
  #geom_text(aes(Position,3,label=AverageActivation),position = "stack")
  

g3<-ggplot(levoDesno)+
  geom_bar(aes(Position,AverageRelative,fill=Stage,width=0.7),stat="identity",position="dodge")+
  facet_grid(Name~.)+
  ggtitle("Change in relative muscle thickness at three spinal levels\n")+
  ylab("(Activation - Rest)/Rest [%]\n")+
  xlab("")+
  scale_fill_manual(values = c("#7E7A67","#DA6992"))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24),
        title=element_text(size=28),
        strip.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_blank(),
        legend.key.size=unit(2.5,"line"))
  #geom_text(aes(Position,3,label=AverageRelative),position = "stack")