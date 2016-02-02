# Reads a file with data on multiple people's stimulaiton thresholds
# Finds the mean, std and such
# Does some plotting


#  Amplitude za svakog ispitanika posebno


###### amplitude za senzorni prag, prijatnu stimulaciju i prag bola
###### BoneSTIM projekat
###### podaci u vidu:
# Ime
# frekvencija
# Nivo stimulacije: 0 - prag osecaja, 1 - prijatno, 2- prag bola
# polje 1
# polje 2
# ...
# polje 8

library(ggplot2) # plotting
library(reshape) # restructuring data 
library(plyr) # data manipulation
library(data.table) # data frame subsetting and such
library(magrittr) # Ceci n'est pas un pipe; allows for pipe-like programming

# Set working directory
setwd("C:/Users/TECNALIA2014BELI/Desktop/Minjino/!BONESTIM/Amplitude")

# Read the data on current amplitudes and such
data<-read.table("Amplitude.txt",colClasses=c("character",rep("numeric",10)))
# Give them pretty names
names(data)<-c("Name","Frequency","Level",as.character(1:8))


# Read the data on BMI and AGE
dataAGE<-read.table("VisinaTezinaGodine.txt",colClasses=c("character",rep("numeric",2)))
# Give them pretty names
names(dataAGE)<-c("Name","BMI","Age")

# merge the data on age with data on amplitudes
data<-merge(data,dataAGE)
databackup<-data


# restructure data
# Sada su kolone: Name, Frequency, Level, Pad (factor), value
data1 <- melt(data,  id.vars = c("Name","BMI","Age","Frequency","Level"), variable_name = "Pad")
data1$Frequency<-as.factor(data1$Frequency)
data1$Level<-as.factor(data1$Level)

# change levels of Level factor variable, using library plyr
data1$Level<-mapvalues(data1$Level, from = c("0", "1","2"), to = c("Sensory Threshold", "Pleasant Sensation","Pain Threshold"))


# divide by sex!
data1$Sex<-NA
ZenskaImenaIndex<-which(data1$Name %in% c("milica",
                                          "nadica",
                                          "minja",
                                          "nadica2",
                                          "itsaso",
                                          "aleksandra",
                                          "jovana",
                                          "aja",
                                          "vukica",
                                          "mina",
                                          "ana",
                                          "dragica"))
data1[ZenskaImenaIndex,ncol(data1)]<-"Female"
data1[-ZenskaImenaIndex,ncol(data1)]<-"Male"
data1$Sex<-as.factor(data1$Sex)
female<-data1[which(data1$Sex=="Female"),]
male<-data1[which(data1$Sex=="Male"),]



# plot
g<-ggplot(male, aes(Pad,value))+
  geom_line(aes(group=Name,colour = Name))+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
  geom_point(aes(size=4,color=Name))+
  facet_grid(Level~Frequency)+
  ylab("Current Amplitude [mA]")+
  theme(legend.title=element_blank())+ # Remove title for all legends
  guides(size=FALSE) # Remove size legend
g

# da vidimo BMI
# data1$BMI<-cut(data1$BMI,5) # podeli u x intervala

gx<-ggplot(data1[which(data1$Pad==4),], aes(Age,value))+
  geom_smooth(method="loess",aes(fill=Level,color=Level))+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
  geom_point(aes(size=1,color=Level))+
  facet_grid(Level~Frequency)+
  ylab("Current Amplitude [mA]")+
  # theme(legend.title=element_blank())+ # Remove title for all legends
  guides(size=FALSE) # Remove size legend
  guides(color=FALSE) # Remove color legend
  guides(fill=FALSE) # Remove color legend
gx

# Sve amplitude zajedno (local regression smooth)

# plot
g1<-ggplot(female, aes(Pad,value))+
  geom_smooth(aes(group=Level,colour = Level),method="loess")+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
  geom_point(aes(size=2,color=Level))+
  facet_grid(.~Frequency)+
  ylab("Current Amplitude [mA]")+
  ggtitle("Current Amplitudes for each pad [mA] FEMALE")+
  theme(legend.title=element_blank())+ # Remove title for all legends
  guides(size=FALSE) # Remove size legend

g2<-ggplot(male, aes(Pad,value))+
  geom_smooth(aes(group=Level,colour = Level),method="loess")+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
  geom_point(aes(size=2,color=Level))+
  facet_grid(.~Frequency)+
  ylab("Current Amplitude [mA]")+
  ggtitle("Current Amplitudes for each pad [mA] MALE")+
  theme(legend.title=element_blank())+ # Remove title for all legends
  guides(size=FALSE) # Remove size legend

g1

g2

# Srednje vrednosti i STD

### Means and SDs by Frequency, Level and Pad

#################################### FEMALE #############################################

D<-female %>% 
  data.table %>%    
  .[, `:=`(Mean=mean(value,trim=0.25),  # add columns mean (trim 25%?) (or put median!) and Sd-,Sd+, grouping by Freq,Lev,Pad
           SD=sd(value),
           LowSD=mean(value)-sd(value),
           HighSD=mean(value)+sd(value),
          Q25=quantile(value,probs = 0.1)),  
    by=list(Frequency, Level,Pad)] %>%
  .[,Name:=NULL]%>%  #ukidas imena
  .[,BMI:=NULL]%>% # ukidas BMI
  .[,Age:=NULL]%>% # ukidas AGE
  # Data Table nije bas zgodan. D[1] nije prva kolona vec prvi red,
  # a D[,1] je ...nista.
  .[,value:=NULL] %>%
  .[which(!duplicated(.)),] %>%
  # Sad ima 120 redova (5 frekv x 3 nivoa x 8 padova)
  data.frame


f1<-ggplot(D[which(D$Frequency==4 | D$Frequency==80),], aes(Pad,Mean))+
  # geom_line(aes(group=Level,colour = Level))+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
  geom_point(aes(size=4,color=Level))+
  geom_line(aes(group=Level,colour = Level))+
  geom_line(aes(Pad, Q25,group=Level,colour = Level,alpha=0.5))+
  # geom_line(aes(Pad, HighSD,group=Level,colour = Level,alpha=0.5))+
  facet_grid(.~Frequency)+
  ylab("MEAN Current Amplitude [mA]")+
  ggtitle("Mean Current Amplitude [mA] and St Dev FEMALE")+
  theme(legend.title=element_blank())+ # Remove title for all legends
  guides(size=FALSE) # Remove size legend

# Dodaj sredinu izmedju pleasant i pain threshold

# Divide by Level
ST<-D[which(D$Level=="Sensory Threshold"),]
Pleasant<-D[which(D$Level=="Pleasant Sensation"),]
PT<-D[which(D$Level=="Pain Threshold"),]

F<-D$Frequency %>% 
  levels %>% # Moguce vrednosti frekvencija
  as.numeric

# Transpose, so you have 3 frames with 5 frequencies x 8 pads
STMEANS<-c()
PleasantMEANS<-c()
PTMEANS<-c()
PTQuantile25<-c()

for (i in 1:length(F)){
  STMEANS<-ST[which(ST$Frequency==F[i]),]$Mean %>%
    rbind(STMEANS,.)
  PleasantMEANS<-Pleasant[which(Pleasant$Frequency==F[i]),]$Mean %>%
    rbind(PleasantMEANS,.)
  PTMEANS<-PT[which(PT$Frequency==F[i]),]$Mean %>%
    rbind(PTMEANS,.)
  PTQuantile25<-PT[which(PT$Frequency==F[i]),]$Q25 %>%
    rbind(PTQuantile25,.)
}

rownames(STMEANS)<-as.character(F)
rownames(PleasantMEANS)<-as.character(F)
rownames(PTMEANS)<-as.character(F)
rownames(PTQuantile25)<-as.character(F)
colnames(STMEANS)<-paste("Pad",as.numeric(1:8),sep="")
colnames(PleasantMEANS)<-colnames(STMEANS)
colnames(PTMEANS)<-colnames(STMEANS)
colnames(PTQuantile25)<-colnames(STMEANS)
####
# Find the middle between the means for Pleasant and Pain

MiddlePain<-c()
FiftyPercentPlus<-c()

for (i in 1:nrow(PTMEANS)){
  MiddlePain<-rbind(MiddlePain,round((PTMEANS[i,]+PleasantMEANS[i,])/2))
  FiftyPercentPlus<-rbind(FiftyPercentPlus,round(PleasantMEANS[i,]*1.5))
  
}
rownames(MiddlePain)<-as.character(F)
rownames(FiftyPercentPlus)<-as.character(F)

############################################### Male #######################################

D1<-male %>% 
  data.table %>%    
  .[, `:=`(Mean=mean(value,trim=0.25),  # add columns mean (trim 25%?) (or put median!) and Sd-,Sd+, grouping by Freq,Lev,Pad
           SD=sd(value),
           LowSD=mean(value)-sd(value),
           HighSD=mean(value)+sd(value),
           Q25=quantile(value,probs = 0.1)),  
    by=list(Frequency, Level,Pad)] %>%
  .[,Name:=NULL]%>%  #ukidas imena
  .[,BMI:=NULL]%>% # ukidas BMI
  .[,Age:=NULL]%>% # ukidas AGE
  # Data Table nije bas zgodan. D[1] nije prva kolona vec prvi red,
  # a D[,1] je ...nista.
  .[,value:=NULL] %>%
  .[which(!duplicated(.)),] %>%
  # Sad ima 120 redova (5 frekv x 3 nivoa x 8 padova)
  data.frame


f2<-ggplot(D1[which(D1$Frequency==4 | D1$Frequency==80),], aes(Pad,Mean))+
  # geom_line(aes(group=Level,colour = Level))+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
  geom_point(aes(size=4,color=Level))+
  geom_line(aes(group=Level,colour = Level))+
  geom_line(aes(Pad, Q25,group=Level,colour = Level,alpha=0.5))+
  # geom_line(aes(Pad, HighSD,group=Level,colour = Level,alpha=0.5))+
  facet_grid(.~Frequency)+
  ylab("MEAN Current Amplitude [mA]")+
  ggtitle("Mean Current Amplitude [mA] and St Dev FEMALE")+
  theme(legend.title=element_blank())+ # Remove title for all legends
  guides(size=FALSE) # Remove size legend

# Dodaj sredinu izmedju pleasant i pain threshold

# Divide by Level
ST1<-D1[which(D1$Level=="Sensory Threshold"),]
Pleasant1<-D1[which(D1$Level=="Pleasant Sensation"),]
PT1<-D1[which(D1$Level=="Pain Threshold"),]

# Transpose, so you have 3 frames with 5 frequencies x 8 pads
STMEANS1<-c()
PleasantMEANS1<-c()
PTMEANS1<-c()
PTQuantile251<-c()

for (i in 1:length(F)){
  STMEANS1<-ST1[which(ST1$Frequency==F[i]),]$Mean %>%
    rbind(STMEANS1,.)
  PleasantMEANS1<-Pleasant1[which(Pleasant1$Frequency==F[i]),]$Mean %>%
    rbind(PleasantMEANS1,.)
  PTMEANS1<-PT1[which(PT1$Frequency==F[i]),]$Mean %>%
    rbind(PTMEANS1,.)
  PTQuantile251<-PT1[which(PT1$Frequency==F[i]),]$Q25 %>%
    rbind(PTQuantile251,.)
}

rownames(STMEANS1)<-as.character(F)
rownames(PleasantMEANS1)<-as.character(F)
rownames(PTMEANS1)<-as.character(F)
rownames(PTQuantile251)<-as.character(F)
colnames(STMEANS1)<-paste("Pad",as.numeric(1:8),sep="")
colnames(PleasantMEANS1)<-colnames(STMEANS1)
colnames(PTMEANS1)<-colnames(STMEANS1)
colnames(PTQuantile251)<-colnames(STMEANS1)
####
# Find the middle between the means for Pleasant and Pain

MiddlePain1<-c()
FiftyPercentPlus1<-c()

for (i in 1:nrow(PTMEANS1)){
  MiddlePain1<-rbind(MiddlePain1,round((PTMEANS1[i,]+PleasantMEANS1[i,])/2))
  FiftyPercentPlus1<-rbind(FiftyPercentPlus1,round(PleasantMEANS1[i,]*1.5))
  
}
rownames(MiddlePain1)<-as.character(F)
rownames(FiftyPercentPlus1)<-as.character(F)
f1

f2


# Prijatno na 80 (1. ZENE i 2. MUSKARCI)


# print(round(PleasantMEANS[5,]))
# print(round(PleasantMEANS1[5,]))

# Sredina izmedju bola i prijatnog: ZENE 

# MiddlePain

# Sredina izmedju bola i prijatnog: MUSKARCI

# MiddlePain1

# 150% Prijatnog ZENE


# FiftyPercentPlus

# 150% Prijatnog MUSKARCI


# FiftyPercentPlus1


# Pisi amplitude za AL TENS i TENS u fajlove

TensZene<-round(PleasantMEANS[5,])
TensMuskarci<-round(PleasantMEANS1[5,])
ALTensZene<-MiddlePain[1,]
ALTensMuskarci<-MiddlePain1[1,]


write.table(TensZene,file="TENS_80Hz_Female.csv",row.names = FALSE, col.names = FALSE)
write.table(TensMuskarci,file="TENS_80Hz_Male.csv",row.names = FALSE, col.names = FALSE)
write.table(ALTensZene,file="AL_TENS_4Hz_Female.csv",row.names = FALSE,col.names = FALSE)
write.table(ALTensMuskarci,file="AL_TENS_4Hz_Male.csv",row.names = FALSE,col.names = FALSE)

# ### Odstupanje
# 
# TF<-female[which(female$Frequency==80 & female$Level=="Pleasant Sensation"),] 
# for (i in 1:8){
#     TF[which(TF$Pad==i),]$value <- TF[which(TF$Pad==i),]$value-TensZene[i]
#   }
# 
# TM<-male[which(male$Frequency==80 & male$Level=="Pleasant Sensation"),] 
# for (i in 1:8){
#   TM[which(TM$Pad==i),]$value <- TM[which(TM$Pad==i),]$value-TensMuskarci[i]
# }                    
# 
# AF<-female[which(female$Frequency==4 & female$Level=="Pleasant Sensation"),] 
# for (i in 1:8){
#   AF[which(AF$Pad==i),]$value <- AF[which(AF$Pad==i),]$value-ALTensZene[i]
# }
# 
# AM<-male[which(male$Frequency==4 & male$Level=="Pleasant Sensation"),] 
# for (i in 1:8){
#   AM[which(AM$Pad==i),]$value <- AM[which(AM$Pad==i),]$value-ALTensMuskarci[i]
# }
# 
# odstupanje<-rbind(TF,TM,AF,AM)
# 
# gg<-ggplot(odstupanje, aes(Pad,value))+
#   geom_line(aes(group=Name,colour = Name))+ # OBRATI PAZNJU NA GROUP! Inace ne veze tacke
#   geom_point(aes(size=4,color=Name))+
#   facet_grid(Frequency~Sex)+
#   ylab("Difference in Current Amplitude [mA]")+
#   theme(legend.title=element_blank())+ # Remove title for all legends
#   guides(size=FALSE)+ # Remove size legend
#   ggtitle("Actual value minus value chosen by Minja")
