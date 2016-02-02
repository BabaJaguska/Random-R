## Takes files with correct and guessed answers, calculates the accuracy, plots some shizz

library(caret) # for confusion matrices
library(magrittr) # for pipelines
library(data.table) # for...well,data tables

# Set working directory. Though not necessary because of file.choose
setwd("C:/Users/TECNALIA2014BELI/Desktop/Minjino/MAXSENS/")

#Read file(s)
files<-choose.files(caption="Select file(s) to read",multi=TRUE)
data<-lapply(files,read.table)

# Strip filename from path
fileNames<-sapply(files,FUN=function(x){
                          temp<-strsplit(basename(x),"\\.")
                          temp<-temp[[1]][1]
                          return(temp)})

# Calculate percentage accuracy
# The 2nd column holds the task
# The 4th column holds the guess
AccuracyPercentage<-function(x){
  tacno=sum(x$V2==x$V4)
  Percent=(tacno/(nrow(x)))*100
  return(Percent)
  }

Percentages<-sapply(data,AccuracyPercentage)


# Plot the accuracies
n=length(Percentages)
b<-barplot(Percentages,
        col=cm.colors(n),
        border="pink",
        main="Percentage of accurately recognized messages",
        xlab="Session",
        ylim = c(0,100),
        names.arg = fileNames,
        cex.axis = 0.7,
        cex.names = 0.7)
text(b,40,as.character(round(Percentages,digits=2)))


# Confusion Matrix if only one subject
Confusions<-lapply(data,FUN=function(x){
                            cm<-confusionMatrix(x$V2,x$V4)
                            return(cm)})

#### ALL TOGETHER Confusion Matrix ###
All<-data.frame()
for (i in 1:length(data)){
                All<-rbind(All,data[[i]])}
TotalConfusion<-confusionMatrix(All$V2,All$V4)
TotalTable<-as.data.frame(TotalConfusion$table)
g<-ggplot(TotalTable)+
    geom_tile(aes(x=Prediction, y=Reference, fill=Freq))+
    scale_fill_gradient(breaks=seq(from=0, to=500, by=50),low="darkred",high="white")+
    ggtitle("Confusion Matrix - All Patients, all sessions Total")


######### Day by Day ##########

# napravi data frame od procenata i dodaj imena fajlova
dfPercentages<-as.data.frame(Percentages)
dfPercentages<-cbind(dfPercentages,fileNames)
rownames(dfPercentages)<-as.character(1:nrow(dfPercentages))


# Iz imena fajla izvuci podatak o Danu
dfPercentages$Day<-NA
for (i in 1:5){
  izraz<-paste(as.character(i),"day",sep="")
  dfPercentages$Day[grep(izraz,dfPercentages$fileNames)]<-i
}

# A valjalo bi da imas i imena
dfPercentages$Name<-NA
for (i in 1:nrow(dfPercentages)){
    temp<-dfPercentages$fileNames[i]
    if (substr(temp,8,10)=="One") {dfPercentages$Name[i]<-"SubjectOne" }
      else if (substr(temp,8,10)=="Two") {dfPercentages$Name[i]<-"SubjectTwo"}
        else if (substr(temp,8,10)=="Thr") {dfPercentages$Name[i]<-"SubjectThree"}
          else if (substr(temp,8,10)=="Fou") {dfPercentages$Name[i]<-"SubjectFour"}
            else if (substr(temp,8,10)=="Fiv") {dfPercentages$Name[i]<-"SubjectFive"}
              else if (substr(temp,8,10)=="Six") {dfPercentages$Name[i]<-"SubjectSix"}
                else if (substr(temp,8,10)=="Sev") {dfPercentages$Name[i]<-"SubjectSeven"}
                  else if (substr(temp,8,10)=="Eig") {dfPercentages$Name[i]<-"SubjectEight"}
}

# a zatim i to da li je pre ili posle "treninga"
dfPercentages$PrePosle<-NA
for (i in 1:nrow(dfPercentages)){
  temp<-as.character(dfPercentages$fileNames[i])
  n<-nchar(temp)
  if (substr(temp,n,n)=="2") {dfPercentages$PrePosle[i]<-"After"}
  else dfPercentages$PrePosle[i]<-"Before"
}

dfPercentages$PrePosle<-factor(dfPercentages$PrePosle,levels=c("Before","After"))
dfPercentages$Percentages<-round(dfPercentages$Percentages,digits=1)

# crtaj
gg<-ggplot(dfPercentages)+
  geom_bar(aes(PrePosle,Percentages,fill=PrePosle,width=0.6),stat="identity")+
  facet_grid(Name~Day)+
  ggtitle("Psychometric test accuracy for each patient before and after training")+
  xlab("")+
  ylab("Accurately recognized stimulation sites [%]")+
  theme(legend.title=element_blank())+
  geom_text(aes(PrePosle,Percentages,label=Percentages))

## aj sad srednje vrednosti nadji PO DANIMA za sve subjekte

dfPercentages<-dfPercentages %>%
              data.table %>%
              .[,`:=`(Mean=mean(Percentages)),
                    by=Day]%>%
                data.frame

ff<-ggplot(dfPercentages,aes(Day,Mean))+
            geom_point(aes(size=6))+
            geom_line()+
            ggtitle('Stimulation Site Recognition Accuracy [%]')+
            xlab('Day')+
            ylab('Mean accuracy for each test day')+
            ylim(85,100)+
            geom_point(aes(Day,Percentages))+
            theme(legend.title=element_blank())+ # Remove title for all legends
            guides(size=FALSE) # Remove size legend
  
# Svi sem prvog dana
NoDay1<-dfPercentages[which(dfPercentages$Day>1),];
ffNoDay1<-ggplot(NoDay1,aes(Day,Mean))+
  geom_boxplot(aes(Day,Percentages,group=Day),fill="#fde0dd",color="#636363")+
  geom_point(aes(Day,Mean),size=5,color="#c51b8a")+
  geom_line(aes(Day,Mean),color="#c51b8a")+
  ggtitle('Stimulation Site Recognition Accuracy')+
  xlab('')+
  ylab('Accuracy for all patients on each test day [%]')+
  coord_cartesian(ylim=c(85,102))+
  geom_point(aes(Day,Percentages),color="#fa9fb5",size=3)+
  theme(legend.title=element_blank())+ # Remove title for all legends
  # guides(size=FALSE)+ # Remove size legend
  scale_x_discrete(breaks = c(2,3,4,5),
                  labels = c("Day 2", "Day 3", "Day 4", "Day 5"))
  
ffNoDay1
              
