library(ggplot2)
#library(nlme)

setwd("C:/Users/TECNALIA2014BELI/Desktop/Minjino/kojesta/Data Science")
df<-read.table("ajmo.csv",sep=",",header=TRUE)
df$Tezinska.grupa<-as.factor(df$Tezinska.grupa)
nrow=dim(df)[1]
df$SportGrupa<-rep("BORILACKI",nrow)
df$SportGrupa[which(df$Sport %in% c("NOG","RUK","KOS"))]<-"LOPTA"
df$SportGrupa[which(df$Sport =="NESP")]<-"NESPORT"
df$SportGrupa<-as.factor(df$SportGrupa)

levels(df$Mjerenje.I.T.F.)=levels(df$Mjerenje.I.T.F.)[c(2,3,1)]


############ MTR #####################

# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$MTR,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
                                  FUN=function(x)mean(x,na.rm=TRUE))
df$MTR[which(is.na(df$MTR))]<-temp[which(is.na(df$MTR))]

# plot
MTR<-ggplot(df)+
  geom_boxplot(aes(df$Mjerenje.I.T.F,df$MTR,fill=df$Mjerenje.I.T.F.))+
  geom_point(aes(df$Mjerenje.I.T.F,df$MTR,alpha=0.7))+
  facet_grid(SportGrupa~Tezinska.grupa)+
  ggtitle("MTR")+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))+
  xlab("Mjerenje")+
  ylab("MTR")

######################## MDS #############

# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$MDS,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
          FUN=function(x)mean(x,na.rm=TRUE))
df$MDS[which(is.na(df$MDS))]<-temp[which(is.na(df$MDS))]

# plot
MDS<-ggplot(df)+
  geom_boxplot(aes(df$Mjerenje.I.T.F,df$MDS,fill=df$Mjerenje.I.T.F.))+
  geom_point(aes(df$Mjerenje.I.T.F,df$MDS,alpha=0.7))+
  facet_grid(SportGrupa~Tezinska.grupa)+
  ggtitle("MDS")+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))+
  xlab("Mjerenje")+
  ylab("MDS")



############# MPN #################

# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$MPN,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
          FUN=function(x)mean(x,na.rm=TRUE))
df$MPN[which(is.na(df$MPN))]<-temp[which(is.na(df$MPN))]

# plot
MPN<-ggplot(df)+ #mpn ima neke outliere koji kvare plot
  geom_boxplot(aes(df$Mjerenje.I.T.F,df$MPN,fill=df$Mjerenje.I.T.F.))+
  geom_point(aes(df$Mjerenje.I.T.F,df$MPN,alpha=0.7))+
  facet_grid(SportGrupa~Tezinska.grupa)+
  ylim(0,40)+
  xlab("Mjerenje")+
  ylab("MPN")+
  ggtitle("MPN")+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))

####################### MPT #########################
# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$MPT,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
          FUN=function(x)mean(x,na.rm=TRUE))
df$MPT[which(is.na(df$MPT))]<-temp[which(is.na(df$MPT))]

# plot
MPT<-ggplot(df)+ 
  geom_boxplot(aes(df$Mjerenje.I.T.F,df$MPT,fill=df$Mjerenje.I.T.F.))+
  geom_point(aes(df$Mjerenje.I.T.F,df$MPT,alpha=0.7))+
  facet_grid(SportGrupa~Tezinska.grupa)+
  ggtitle("MPT")+
  xlab("Mjerenje")+
  ylab("MPT")+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))

  
############### MPR #####################################
# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$MPR,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
          FUN=function(x)mean(x,na.rm=TRUE))
df$MPR[which(is.na(df$MPR))]<-temp[which(is.na(df$MPR))]

# plot
MPR<-ggplot(df)+ 
  geom_boxplot(aes(df$Mjerenje.I.T.F,df$MPR,fill=df$Mjerenje.I.T.F.))+
  geom_point(aes(df$Mjerenje.I.T.F,df$MPR,alpha=0.7))+
  facet_grid(SportGrupa~Tezinska.grupa)+
  ggtitle("MPR")+
  xlab("Mjerenje")+
  ylab("MPR")+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))


######################## MIV ###################
# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$MIV,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
          FUN=function(x)mean(x,na.rm=TRUE))
df$MIV[which(is.na(df$MIV))]<-temp[which(is.na(df$MIV))]

# plot
MIV<-ggplot(df)+ 
  geom_boxplot(aes(df$Mjerenje.I.T.F,df$MIV,fill=df$Mjerenje.I.T.F.))+
  geom_point(aes(df$Mjerenje.I.T.F,df$MIV,alpha=0.7))+
  facet_grid(SportGrupa~Tezinska.grupa)+
  ggtitle("MIV")+
  xlab("Mjerenje")+
  ylab("MIV")+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))

############ F6 #############
# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$F6,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
          FUN=function(x)mean(x,na.rm=TRUE))
df$F6[which(is.na(df$F6))]<-temp[which(is.na(df$F6))]

# plot
F6<-ggplot(df)+ #mpn ima neke outliere koji kvare plot
  geom_boxplot(aes(df$Mjerenje.I.T.F,df$F6,fill=df$Mjerenje.I.T.F.))+
  geom_point(aes(df$Mjerenje.I.T.F,df$F6,alpha=0.7))+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))+
  facet_grid(SportGrupa~Tezinska.grupa)+
  ggtitle("F6")+
  xlab("Merenje")+
  ylab("F6")

#################### ITM ###########

# Impute missing values by mean of the value
# grouped by tezinska.grupa, sport.grupa and mjerenje

temp<-ave(df$ITM,df$SportGrupa,df$Tezinska.grupa,df$Mjerenje.I.T.F.,
          FUN=function(x)mean(x,na.rm=TRUE))
df$ITM[which(is.na(df$ITM))]<-temp[which(is.na(df$ITM))]

# plot
ITM<-ggplot(df)+ #mpn ima neke outliere koji kvare plot
  geom_boxplot(aes(df$SportGrupa,df$ITM,fill=df$SportGrupa))+
  ggtitle("ITM ovisno od sportske grupe")+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))+
  xlab("Mjerenje")+
  ylab("ITM")

############### THREE WAY MIXED ANOVA ###############
modelMTR <- aov(MTR ~ (SportGrupa*Tezinska.grupa*Mjerenje.I.T.F.) +
               Error(R.br./(Mjerenje.I.T.F.)),data=df)
modelMDS <- aov(MDS ~ (SportGrupa*Tezinska.grupa*Mjerenje.I.T.F.) +
               Error(R.br./(Mjerenje.I.T.F.)),data=df)
modelMPN <- aov(MPN ~ (SportGrupa*Tezinska.grupa*Mjerenje.I.T.F.) +
               Error(R.br./(Mjerenje.I.T.F.)),data=df)
modelMPT <- aov(MPT ~ (SportGrupa*Tezinska.grupa*Mjerenje.I.T.F.) +
               Error(R.br./(Mjerenje.I.T.F.)),data=df)
modelMPR <- aov(MPR ~ (SportGrupa*Tezinska.grupa*Mjerenje.I.T.F.) +
               Error(R.br./(Mjerenje.I.T.F.)),data=df)
modelMIV <- aov(MIV ~ (SportGrupa*Tezinska.grupa*Mjerenje.I.T.F.) +
               Error(R.br./(Mjerenje.I.T.F.)),data=df)
modelF6 <- aov(F6 ~ (SportGrupa*Tezinska.grupa*Mjerenje.I.T.F.) +
               Error(R.br./(Mjerenje.I.T.F.)),data=df)

# model summaries
summary(modelMTR)
summary(modelMDS)
summary(modelMPN)
summary(modelMPT)
summary(modelMPR)
summary(modelMIV)
summary(modelF6)

# Zavisnost ITM od F6
ITMsaF6<-cor(df$ITM,df$F6,method="pearson")
g<-ggplot(df)+geom_point(aes(ITM,F6))+
  geom_smooth(aes(ITM,F6),method="loess")+
  ggtitle("Ovisnost F6 od ITM kod svih ispitanika")

# ITM od sportske grupe 
ITMsport<-aov(ITM~SportGrupa,data=df)
summary(ITMsport)
ITMsportT<-pairwise.t.test(df$ITM,df$SportGrupa,p.adjust.method = "bonferroni")
