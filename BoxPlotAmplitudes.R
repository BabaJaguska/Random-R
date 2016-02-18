
###### amplitude za senzorni prag, prijatnu stimulaciju i prag bola
###### podaci u vidu:
# Ime
# frekvencija
# Nivo stimulacije: 0 - prag osecaja, 1 - prijatno, 2- prag bola
# polje 1
# polje 2
# ...
# polje 8

library(ggplot2) # plotting
library(extrafont) # fonts >>unused<<
library(grid) # ggplot2 needs it 
library(reshape) # restructuring data 
library(plyr) # data manipulation
library(data.table) # data frame subsetting and such
library(magrittr) # Ceci n'est pas un pipe; allows for pipe-like programming

# Set working directory
setwd("C:/Users/TECNALIA2014BELI/Desktop/Minjino/!BONESTIM/Amplitude")

# Read the data on current amplitudes and such
data<-read.table("!AmplitudeBre.txt",colClasses=c("character",rep("numeric",10)))
# Give them pretty names
names(data)<-c("Name","Frequency","Level",as.character(1:8))


# Read the data on BMI and AGE
dataAGE<-read.table("VisinaTezinaGodine.txt",colClasses=c("character",rep("numeric",2)))
# Give them pretty names
names(dataAGE)<-c("Name","BMI","Age")

# merge the data on age with data on amplitudes
databackup<-data
data<-merge(data,dataAGE)

# restructure data
# Sada su kolone: Name, Frequency, Level, Pad (factor), value
data1 <- melt(data,  id.vars = c("Name","BMI","Age","Frequency","Level"), variable_name = "Pad")
data1$Frequency<-as.factor(data1$Frequency)
data1$Level<-as.factor(data1$Level)

# change levels of Level factor variable, using library plyr
data1$Level<-mapvalues(data1$Level, from = c("0", "1","2"), to = c("Sensory Threshold", "Pleasant Sensation","Pain Threshold"))

# sad radimo samo sa 80Hz
data1<-data1[which(data1$Frequency==80),]
data1$Pad<-as.character(data1$Pad)

padnames<-c()
for (i in 1:8){
  padnames<-c(padnames,paste("Pad",as.character(i),sep=" "))
}
data1$Pad<-mapvalues(data1$Pad,from=c("1","2","3","4","5","6","7","8"),to=padnames) # give facets prettier names


# Now plot us the boxies

theme_set(theme_gray()) 
g<-ggplot(data1)+
  # geom_smooth(method="loess",aes(group=Level,colour = Level))+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
  # geom_point(aes(size=4,color=Level))+
  geom_boxplot(aes(Level,value,fill = Level))+
  scale_fill_manual(values = c("#fde0dd", "#fa9fb5","#c51b8a"))+
  ylab("Current Amplitude [mA]")+
  facet_grid(.~Pad)+
  scale_x_discrete(labels="")+
  xlab("")+
  ggtitle("Current intensity [mA] on each electrode pad for all healthy subjects")+
  coord_cartesian(ylim=c(0, 25))+
  theme(legend.title=element_blank(), # Remove title for all legends
        axis.ticks.x=element_blank(),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.text=element_text(size=12),
        strip.text.x = element_text(size = 13,face="bold"),
        plot.margin = unit(c(2,1,1,1),"cm"),
        title = element_text(vjust=2,size=14))
