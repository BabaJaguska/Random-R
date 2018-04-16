### HEALTHY PATIENTS ULTRASOUND MEASUREMENTS ###

#Data contained in C:/Users/TECNALIA2014BELI/Desktop/Minjino/!BONESTIM/UZV dubina
#In a .csv file named "ZdraviUzvCsv.csv"

###Contains the following columns:
##Name: Name of the subject
##BackRegion: where was the measurement performed {L4,Th10,Th5}
##TransducerPosition: Was it measured closer to the spine or not {InnerSide,OuterSide}
##Frequency[Hz]: {20,40}
##Amplitude[mA]: current amplitude used to elicit contraction
##Mode: {Voluntary,StimulationAdjacent,StimulationSpacing1}
##ThicknessBefore[mm]:thickness of the target muscle in rest
##ThicknessAfter[mm]: thickness of the target muscle in activation
##ImageNo: integer, to link with physical ultrasound images
##comment: well, comment

library(ggplot2)
library(tidyr) # hm ima pipe operator ovde
library(dplyr)


setwd('C:/Users/TECNALIA2014BELI/Desktop/Minjino/!BONESTIM/UZV dubina')

filename<-"ZdraviUzvCsv.csv"
classes<-c("character","factor","factor","factor","integer","factor","double","double","integer","character")
df<-read.table(filename,header=TRUE,sep = ";",colClasses=classes)

df$RelChange<-100*(df$ThicknessAfter.mm.-df$ThicknessBefore.mm.)/df$ThicknessBefore.mm.

df$Mode<-as.factor(df$Mode)
levels(df$Mode)<-c("Voluntary","Stim Config 1","Stim Config 2")

## Plot those relatives

# work with 40Hz fornow
df40<-df[which(df$Frequency.Hz. %in% c("40","")),]

g<-ggplot(df)+
  geom_point(aes(Mode,RelChange,color=Mode,shape=Frequency.Hz.,size=3))+
  facet_grid(Name~BackRegion)
  

g1<-ggplot(df40)+
  geom_boxplot(aes(Mode,RelChange,fill=Mode))+
  geom_point(aes(Mode,RelChange,color=Name))+
  facet_grid(.~BackRegion)+
  geom_line(aes(Mode,RelChange,group=Name,color=Name))+
  scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))+
  ggtitle("Relative muscle activation in reference to thickness in rest")+
  ylab("Relative thickness [%]")+
  xlab("Measurement condition")+
  scale_x_discrete(labels=c("Voluntary","Stimulation","Stimulation2"))

# Residuals for normality assessment
# work with 40Hz only
  df40$Residual<-ave(df40$RelChange,df40$BackRegion,df40$Mode,
                     FUN=function(x){
                                    x-mean(x)
                     }) 
  
  normality<-shapiro.test(df40$Residual)

# Doesn't pass the normality test because of Milos 
# and his outlier at Th5
# So let me just work without it, so I can use ANOVA like a man
  
  df40NoM<-df40[which(!(df40$Name=="Milos" & df40$BackRegion=="Th5")),] 
# check the histograms
  
  histMilos<-hist(df40$Residual)
  histNoMilos<-hist(df40NoM$Residual)

# and check Shapiro again
  normalityNoM<-shapiro.test(df40NoM$Residual)
# now it cannot reject the null hypothesis
# and I can procede to use parametric tests

# onda ggplot na ovaj novi dataset
  g2<-ggplot(df40NoM)+
    geom_boxplot(aes(Mode,RelChange,fill=Mode))+
    geom_point(aes(Mode,RelChange))+
    facet_grid(.~BackRegion)+
    scale_fill_manual(values = c("#ffeda0","#feb24c","#f03b20"))+
    ggtitle("Relative muscle activation under three experimental conditions at three spinal levels")+
    ylab("Relative thickness [%]")+
    xlab("")+
    theme_bw(base_size=14)+
    theme(legend.text=element_text(size=14),
          legend.key.size=unit(2,"line"),
          strip.text.x = element_text(size = 14),
          axis.text.x = element_blank())
   
    # scale_x_discrete(labels=c("Voluntary","Stimulation","Stimulation2"))
  
  # pojedinacno na backregion
  dfL4<-df40NoM[which(df40NoM$BackRegion=="L4"),]
  dfTh5<-df40NoM[which(df40NoM$BackRegion=="Th5"),]
  dfTh10<-df40NoM[which(df40NoM$BackRegion=="Th10"),]
  
  # anova
  L<-anova(lm(RelChange~Mode,data=dfL4))
  T<-anova(lm(RelChange~Mode,data=dfTh10))
  C<-anova(lm(RelChange~Mode,data=dfTh5))
  
  # e a mozda ovo bolje
  Lrep<-aov(RelChange~factor(Mode)+Error(factor(Name)),data=dfL4)
  Trep<-aov(RelChange~factor(Mode)+Error(factor(Name)),data=dfTh10)
  Crep<-aov(RelChange~factor(Mode)+Error(factor(Name)),data=dfTh5)
  #da, ovo, repeated measures ANOVA
  
  summary(Lrep)
  summary(Trep)
  summary(Crep)
  
  # t tests
  t1<-pairwise.t.test(dfL4$RelChange,dfL4$Mode,paired=TRUE,p.adjust.method ="bonferroni")
  t2<-pairwise.t.test(dfTh10$RelChange,dfTh10$Mode,paired=TRUE,p.adjust.method = "bonferroni")
  t3<-pairwise.t.test(dfTh5$RelChange,dfTh5$Mode,paired=TRUE,p.adjust.method = "bonferroni")
  
  #### ajmo sad i sa 20Hz
  dfSve<-df
  dfSve$Mode<-as.character(dfSve$Mode)
  dfSve$Mode[dfSve$Frequency.Hz.==20]<-"4.Spaced20Hz"
  dfSve<-dfSve[which(dfSve$Name!="Minja"),] 
  
  dfSVEL4<-dfSve[which(dfSve$BackRegion=="L4"),]
  dfSVETh5<-dfSve[which(dfSve$BackRegion=="Th5"),]
  dfSVETh10<-dfSve[which(dfSve$BackRegion=="Th10"),]
  
  Lsve<-anova(lm(RelChange~Mode,data=dfSVEL4))
  Tsve<-anova(lm(RelChange~Mode,data=dfSVETh10))
  Csve<-anova(lm(RelChange~Mode,data=dfSVETh5))
  
  g3<-ggplot(dfSve)+
    geom_boxplot(aes(Mode,RelChange,fill=Mode))+
    geom_point(aes(Mode,RelChange,color=Name))+
    facet_grid(.~BackRegion)+
    ggtitle("Relative muscle activation in reference to thickness in rest")+
    ylab("Relative thickness [%]")+
    xlab("Measurement condition")
    #scale_x_discrete(labels=c("Voluntary","Stimulation","Stimulation2"))
  
  t1<-pairwise.t.test(dfSVEL4$RelChange,dfSVEL4$Mode,paired=TRUE,p.adjust.method ="bonferroni")
  t2<-pairwise.t.test(dfSVETh10$RelChange,dfSVETh10$Mode,paired=TRUE,p.adjust.method = "bonferroni")
  t3<-pairwise.t.test(dfSVETh5$RelChange,dfSVETh5$Mode,paired=TRUE,p.adjust.method = "bonferroni")
  
  ## nevezano za back region
  out<-anova(lm(RelChange~Mode,data=dfSve))
  out2<-pairwise.t.test(dfSve$RelChange,dfSve$Mode,paired=TRUE,p.adjust.method="bonferroni")
  
  # a ako gledas samo stim spaced i stim adjacent regardless of back region
  tout<-pairwise.t.test(df40$RelChange,df40$Mode,paired=TRUE,p.adjust.method = "bonferroni")

  # nezavisno od back na 40
  AOV <- aov(RelChange~factor(Mode)+Error(factor(Name)), data = df40)
  summary(AOV)
  
  # samo t test na adj i spaced
  imaLi<-t.test(df40$RelChange[df40$Mode=="Stim Config 1"],
         df40$RelChange[df40$Mode=="Stim Config 2"],
         paired=TRUE)
  
  noVol<-df40NoM[which(df40NoM$Mode!="Voluntary"),]
  gWhole<-ggplot(noVol)+
    geom_boxplot(aes(Mode,RelChange,fill=Mode))+
    geom_point(aes(Mode,RelChange))+
    scale_fill_manual(values = c("#feb24c","#f03b20"))+
    ggtitle("Activation induced by two electrode configurations irrespective of position")+
    ylab("Relative thickness [%]")+
    xlab("")+
    theme_bw(base_size=14)+
    theme(legend.text=element_text(size=14),
          legend.key.size=unit(2,"line"),
          strip.text.x = element_text(size = 14),
          axis.text.x = element_blank())
  
  
  ######### means ###
  grpL4<-dfL4%>%
    group_by(Mode)%>%
    summarize(meanChange=mean(RelChange), std=sd(RelChange))
  
  grpTh5<-dfTh5%>%
    group_by(Mode)%>%
    summarize(meanChange=mean(RelChange),std=sd(RelChange))
  
  grpTh10<-dfTh10%>%
    group_by(Mode)%>%
    summarize(meanChange=mean(RelChange),std=sd(RelChange))
  
  
  
  