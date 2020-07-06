library(plyr)
library(dplyr)
#library(stats)
library(psych)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gmodels)

setwd('C:\\data')
# df1<-read.table('Hrvanje rez.ankete - 11-13 god.csv', sep = ',', header = TRUE, colClasses = 'character', fileEncoding = "UTF-8")
# df2<-read.table('Hrvanje rez.ankete - 14-15 god.csv', sep = ',', header = TRUE, colClasses = 'character', fileEncoding = "UTF-8")
# df3<-read.table('Hrvanje rez.ankete - 16-17 god.csv', sep = ',', header = TRUE, colClasses = 'character', fileEncoding = "UTF-8")
# 
# names(df3)[4]<-names(df2)[4]
# 
# df<-rbind(df1,df2,df3)

df <- read.table('HrvanjeTabelaSVI.csv', sep = ',', header = TRUE, colClasses = 'character')


fixComma<-function(str){
    
  return(gsub(',','.',str))
  
}


quantify<-function(str){
  
  posDigit<-regexpr('[0-9]',str)
  s<-paste(substr(str,posDigit,posDigit),'-',substr(str,1,posDigit-2))
  return(s)
  
}

quantify2<-function(str){
  
  posDigit<-regexpr('[0-9]',str)
  s<-as.numeric(substr(str,posDigit,posDigit))
  return(s)
  
}

for (i  in 11:82){
  df[,i]<-sapply(df[,i],quantify)
}



for (i in c(1,5)){
  df[,i]<-sapply(df[,i],fixComma)
}


df<-plyr::rename(df,c('Koli.K.AFko.si.star..u.godinama.i.mjesecima..' ='Starost.god.mes'))
df$Starost.god.mes<-as.numeric(df$Starost.god.mes)

# ========================================================================================================

# grupisi starost
df$Starost.grupa <-c()
df$Starost.grupa[df$Starost.god.mes<14]<-1
df$Starost.grupa[which(df$Starost.god.mes<16 & df$Starost.god.mes>=14)]<-2 #14,15
df$Starost.grupa[df$Starost.god.mes>=16]<-3  #16,17
df$Starost.grupa<-as.factor(df$Starost.grupa)


df$Spol<-sapply(df$Spol, function(x) toupper(x))
df$Spol<-as.factor(df$Spol)

dfAll<-df
CrossTable(dfAll$Starost.grupa,df$Spol)

#==========================================================================================================

#df$Koliko.dugo.treniras.hrvanje..u.godinama.i.mjesecima..<-as.numeric(df$Koliko.dugo.treniras.hrvanje..u.godinama.i.mjesecima..)
df$Koliko.minuta.traje.tvoj.trening<-as.factor(df$Koliko.minuta.traje.tvoj.trening)
df$Koliko.puta.tjedno.treniras.<-  as.factor(as.numeric(df$Koliko.puta.tjedno.treniras.))
df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Europskom.prvenstvu<-as.factor(df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Europskom.prvenstvu)
df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.prvenstvu.Hrvatske <-as.numeric(df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.prvenstvu.Hrvatske)
df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Svjetskom.prvenstvu <-as.factor(df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Svjetskom.prvenstvu)


#===========================================================================================================
##########odbaci djevojcice
df<- df[which(df$Spol=='M'),]

youngstersIdx<-which(df$Starost.grupa == '1')
set.seed(1234)
randomOdabrani<-sample(youngstersIdx,55,replace = FALSE)
dfMali <- df[randomOdabrani,]
dfOstali<-df[which(df$Starost.grupa!="1"),]
df<-rbind(dfMali,dfOstali)
CrossTable(df$Starost.grupa)

dfNum<-df
for (i  in 11:82){
  dfNum[,i]<-sapply(df[,i],quantify2)
}

#===================================================================================================================
################################################################
##### POSVECENOST SPORTU #######
#########################################


#### Zabava u sportu
posIdx <-11:16
chAlpha<- psych::alpha(dfNum[,posIdx])
print('Chronback Alpha for Posvecenost sportu group of questions')
print(chAlpha$total)
round(kurtosi(dfNum[,posIdx]),2)
round(skew(dfNum[,posIdx]),2)
zabavaMean <- sapply(dfNum[,posIdx],mean)
zabavaSD <- sapply(dfNum[,posIdx],sd)
zabavaKS<-sapply(dfNum[,posIdx], ks.test)

cross<-function(x){
  lol <- CrossTable(x)
  return(rbind(lol$t,round(100*lol$prop.row,2)))
}

a<-sapply(dfNum[,11:16],FUN=cross)
a



########################################################################

##### Koliko se ponosis?


# svi ukupno koliko se ponose
ttempPonoseSvi <- as.data.frame(table(df$Koliko.se.ponosis.kada.drugima.govoris.da.treniras.ovaj.sport.))
piepercent<-as.character(round(ttempPonoseSvi$Freq*100/sum(ttempPonoseSvi$Freq),2))
piepercent<-paste(piepercent,'%')
pie(ttempPonoseSvi$Freq, labels = piepercent,col=brewer.pal(5,"BrBG"))
legend("topleft",c('malo','osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"BrBG"))
title('Koliko se ponosis ovim sportom?')

#===========================================================================================================
####### KOLIKO SI PREDAN TRENIRANJU?



ttempPred<-as.data.frame(table(df$Koliko.si.predan.u.treniranju.ovog.sporta.))
piepercent<-as.character(round(ttempPred$Freq*100/sum(ttempPred$Freq),2))
piepercent<-paste(piepercent,'%')
pie(ttempPred$Freq, labels = piepercent,col=brewer.pal(5,"PRGn"))
legend("topleft",c('malo','osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"PRGn"))
title('Koliko ste predani treniranju ovog sporta?')
#==========================================================================================================

##### Nastaviti trenirati



# gZeliNastaviti <-ggplot(data=dfNum)+geom_point(aes(Starost.grupa ,Koliko.zelis.nastaviti.trenirati.ovaj.sport., size = ))+
#   
#   xlab('')+
#   ggtitle('Koliko ispitanici zele nastavitii da se bave sportom prema spolu i starosnoj dobi')+
#   theme(#axis.ticks.x=element_blank(),
#     axis.text = element_text(size=15,angle = 60),
#     strip.text = element_text(size=16),
#     plot.title=element_text(size=16),
#     legend.text = element_text(size=15),
#     legend.title = element_text(size=15))+
#   guides(colour = guide_legend(override.aes = list(size=7)),
#          shape = guide_legend(override.aes = list(size=5)))
# gZeliNastaviti

################ Odlucan da nastavi
tOdlucanDaNastavi <- as.data.frame(table(Spol=dfNum$Spol,dfNum$Koliko.si.odlu.an.da.nastavis.trenirati.ovaj.sport.,df$Starost.grupa))
ttempOdlucanDaNastavi <- as.data.frame(table(Spol=dfNum$Spol,Starosna.skupina=dfNum$Starost.grupa))
tOdlucanDaNastavi<-plyr::rename(tOdlucanDaNastavi,c('Var2'='NivoOdlucanDaNastavia','Var3'='Starosna.skupina'))
tOdlucanDaNastavi$RelativnaFrekvencija <-NA
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==1)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==1)]*100/ttempOdlucanDaNastavi[1,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==2)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==2)]*100/ttempOdlucanDaNastavi[3,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==3)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==3)]*100/ttempOdlucanDaNastavi[5,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==1)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==1)]*100/ttempOdlucanDaNastavi[2,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==2)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==2)]*100/ttempOdlucanDaNastavi[4,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==3)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==3)]*100/ttempOdlucanDaNastavi[6,3]



levels(tOdlucanDaNastavi$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")

gOdlucanDaNastavi <-ggplot(data=tOdlucanDaNastavi)+geom_bar(aes(NivoOdlucanDaNastavia,RelativnaFrekvencija),fill='maroon', position='dodge',stat='identity')+
  facet_grid(.~Starosna.skupina)+
  xlab('')+
  ggtitle('Koliko su ispitanici odlucni da nastave da se bave sportom prema starosnoj dobi')+
  theme(#axis.ticks.x=element_blank(),
    axis.text = element_text(size=15,angle = 0, color='darkgray'),
    strip.text = element_text(size=16),
    plot.title=element_text(size=16),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15))+
  guides(colour = guide_legend(override.aes = list(size=7)),
         shape = guide_legend(override.aes = list(size=5)))
gOdlucanDaNastavi

#svi odlucan
ttempodl<-as.data.frame(table(df$Koliko.si.odlucan.da.nastavis.trenirati.ovaj.sport.))
piepercent<-as.character(round(ttempodl$Freq*100/sum(ttempodl$Freq),2))
piepercent<-paste(piepercent,'%')
pie(ttempodl$Freq, labels = piepercent,col=brewer.pal(5,"PiYG"))
legend("topleft",c('osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"PiYG"))
title('Koliko si odlucan da nastavis trenirati?')

#===============================================================================================================
##### Koliko bi bilo tesko prestati da treniras
tBiloBiTesko <- as.data.frame(table(dfNum$Koliko.bi.ti.tesko.bilo.prestati.trenirati.ovaj.sport.,df$Starost.grupa))
ttempBiloBiTesko <- as.data.frame(table(Spol=dfNum$Spol,Starosna.skupina=dfNum$Starost.grupa))
tBiloBiTesko<-plyr::rename(tBiloBiTesko,c('Var1'='NivoBiloBiTeskoa','Var2'='Starosna.skupina'))

tBiloBiTesko$RelativnaUcestanost[which(tBiloBiTesko$Starosna.skupina==1)] <- 100*tBiloBiTesko$Freq[which(tBiloBiTesko$Starosna.skupina==1)]/55
tBiloBiTesko$RelativnaUcestanost[which(tBiloBiTesko$Starosna.skupina==2)]<- 100*tBiloBiTesko$Freq[which(tBiloBiTesko$Starosna.skupina==2)]/50
tBiloBiTesko$RelativnaUcestanost[which(tBiloBiTesko$Starosna.skupina==3)]<- 100*tBiloBiTesko$Freq[which(tBiloBiTesko$Starosna.skupina==3)]/43


levels(tBiloBiTesko$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")

gBiloBiTesko <-ggplot(data=tBiloBiTesko)+geom_bar(aes(NivoBiloBiTeskoa,RelativnaUcestanost), fill='maroon',position='dodge',stat='identity')+
  facet_grid(.~Starosna.skupina)+
  ggtitle('Koliko ispitanici smatraju da bi im bilo tesko da napuste sport prema starosnoj dobi')+
  xlab('')+
  ylab('Relativna ucestanost odgovora [%]')+
  theme_gray()+
  theme(#axis.ticks.x=element_blank(),
        axis.text = element_text(size=15,angle = 0,color='darkgray'),
        strip.text = element_text(size=16),
        plot.title=element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  guides(colour = guide_legend(override.aes = list(size=7)),
         shape = guide_legend(override.aes = list(size=5)))
  
gBiloBiTesko

# svi ukupno koliko bi tesko bilo
ttempBiloBiTeskoSvi<-as.data.frame(table(df$Koliko.bi.ti.tesko.bilo.prestati.trenirati.ovaj.sport.))
piepercent<-as.character(round(ttempBiloBiTeskoSvi$Freq*100/sum(ttempBiloBiTeskoSvi$Freq),2))
piepercent<-paste(piepercent,'%')
pie(ttempBiloBiTeskoSvi$Freq, labels = piepercent,col=brewer.pal(5,"PiYG"))
legend("topleft",c('nimalo','malo','osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"PiYG"))
title('Koliko bi vam bilo tesko da napustite ovaj sport?')

spoltab<-prop.table(table(df$Spol))
print('Udeo uzorka prema spolu')
print(spoltab)

spolVSstarost<-prop.table(table(df$Spol,df$Starost.grupa))
print('Udeo uzorka prema spolu i starosnoj skupini')
print(spolVSstarost)

table(df$Starost.grupa)
CrossTable(df$Spol,df$Starost.grupa,format = 'SPSS')

#=================================================================================
#################### Zabava
zabIdx = 17:21
chAlphaZabava<- psych::alpha(dfNum[,zabIdx])
print('Chronback Alpha for Zabava u sportu group of questions')
print(chAlphaZabava$total)

round(kurtosi(dfNum[,zabIdx]),2)
round(skew(dfNum[,zabIdx]),2)
sapply(dfNum[,zabIdx],mean)
sapply(dfNum[,zabIdx],sd)

ks.test(dfNum[,zabIdx[1]],'pnorm')

az<-sapply(dfNum[,zabIdx],FUN=cross)
az

#==================================================================================================================
##### intrinzicna motivacija
motIdx<-22:39
chAlphaMot<- psych::alpha(dfNum[,motIdx],check.keys = TRUE)
print('Chronback Alpha for Intrinzicna motivaja group of questions')
print(chAlphaMot$total)
print("!!!")
round(kurtosi(dfNum[,motIdx]),2)
round(skew(dfNum[,motIdx]),2)
aa <-sapply(dfNum[,motIdx],mean,na.rm=TRUE)
sapply(dfNum[,motIdx],sd,na.rm=TRUE)

am<-sapply(dfNum[,motIdx],FUN=cross)
sapply(dfNum[,motIdx],FUN=ks.test, y='pnorm')

## vidi ukupno odgovore na Osjecam pritisak...

pritisak<-data.frame(table(dfNum$Osjecam.pritisak.dok.nastupam.na.natjecanjima))
pritisakPlot<-ggplot(data=pritisak)+geom_bar(aes(Var1,Freq),stat='identity',fill = 'purple')+
  ggtitle('Osjecam pritisak dok nastupam na natjecanjima')+
  xlab('')+
  ylab('Broj odgovora')+
  theme_light()+
  theme(#axis.ticks.x=element_blank(),
    axis.text = element_text(size=16,angle = 0,color='darkgray'),
    strip.text = element_text(size=16),
    plot.title=element_text(size=16),
    axis.title=element_text(size=14))
pritisakPlot

# a vidi po starosti...

pritisakStarost <-data.frame(table(dfNum$Osjecam.pritisak.dok.nastupam.na.natjecanjima,df$Starost.grupa))
pritisakStarost <-plyr::rename(pritisakStarost, c('Var1' = 'NivoPritiska','Var2' = 'StarosnaGrupa'))

pritisakStarost$RelativnaUcestanost[which(pritisakStarost$StarosnaGrupa==1)] <- 100*pritisakStarost$Freq[which(pritisakStarost$StarosnaGrupa==1)]/55
pritisakStarost$RelativnaUcestanost[which(pritisakStarost$StarosnaGrupa==2)]<- 100*pritisakStarost$Freq[which(pritisakStarost$StarosnaGrupa==2)]/50
pritisakStarost$RelativnaUcestanost[which(pritisakStarost$StarosnaGrupa==3)]<- 100*pritisakStarost$Freq[which(pritisakStarost$StarosnaGrupa==3)]/43

levels(pritisakStarost$StarosnaGrupa)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")

gPritisakStarost <-ggplot(data=pritisakStarost)+geom_bar(aes(NivoPritiska,RelativnaUcestanost), fill='maroon',position='dodge',stat='identity')+
  facet_grid(.~StarosnaGrupa)+
  ggtitle('Da li ispitanici osjecaju pritisak u natjecanju prema starosnoj dobi')+
  xlab('')+
  ylab('Relativna ucestanost odgovora [%]')+
  theme_gray()+
  theme(#axis.ticks.x=element_blank(),
    axis.text = element_text(size=15,angle = 0,color='darkgray'),
    strip.text = element_text(size=16),
    plot.title=element_text(size=16),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15))+
  guides(colour = guide_legend(override.aes = list(size=7)),
         shape = guide_legend(override.aes = list(size=5)))
gPritisakStarost

#==============================================================================================================================
####### skala socijalne orijentacije
socIdx <-40:57
chAlphaSoc<- psych::alpha(dfNum[,socIdx])
print('Chronback Alpha for Socijalna orijentacija group of questions')
print(chAlphaSoc$total)
round(kurtosi(dfNum[,socIdx]),2)
round(skew(dfNum[,socIdx]),2)
sapply(dfNum[,socIdx],mean, na.rm = TRUE)
sapply(dfNum[,socIdx],sd, na.rm = TRUE)
sapply(dfNum[,socIdx],ks.test,y = 'pnorm')

asoc<-sapply(dfNum[,socIdx],FUN=cross)
asoc


#crtaj drustvo za kino
tempkino<-as.data.frame(table(dfNum$Kad.trebam.drustvo.za.kino.ili.partnera.za.video.igrice..bez.problema.nadem.nekog.iz.kluba))
piepercent<-as.character(round(tempkino$Freq*100/sum(tempkino$Freq),2))
piepercent<-paste(piepercent,'%')
pie(tempkino$Freq, labels = piepercent,col=brewer.pal(5,"PiYG"))
legend("topleft",c('nimalo','malo','osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"PiYG"))
title('Kad trebam drustvo za kino ili partnera za video igrice \n bez problema nadjem nekog iz kluba')

# rodjendani
tempkino<-as.data.frame(table(dfNum$Rodendane.slavim.s.prijateljima.iz.kluba))
piepercent<-as.character(round(tempkino$Freq*100/sum(tempkino$Freq),2))
piepercent<-paste(piepercent,'%')
pie(tempkino$Freq, labels = piepercent,col=brewer.pal(5,"PiYG"))
legend("topleft",c('nimalo','malo','osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"PiYG"))
title('Rodjendane slavim s prijateljima iz kluba')


# ohrabruju suborci
tempkino<-as.data.frame(table(dfNum$Moji.prijatelji.suborci.me.ohrabruju.kad.pogrijesim))
piepercent<-as.character(round(tempkino$Freq*100/sum(tempkino$Freq),2))
piepercent<-paste(piepercent,'%')
pie(tempkino$Freq, labels = piepercent,col=brewer.pal(5,"BrBG"))
legend("topleft",c('nimalo','malo','osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"BrBG"))
title('Moji prijatelji suborci me ohrabruju kad pogrijesim')

# ne bi ostavili na cjedilu
tempkino<-as.data.frame(table(dfNum$Moji.decki.iz.kluba.me.nikad.ne.bi.ostavili.na.cjedilu))
piepercent<-as.character(round(tempkino$Freq*100/sum(tempkino$Freq),2))
piepercent<-paste(piepercent,'%')
pie(tempkino$Freq, labels = piepercent,col=brewer.pal(5,"BrBG"))
legend("topleft",c('nimalo','malo','osrednje','jako','izrazito jako'),cex=1,fill=brewer.pal(5,"BrBG"))
title('Moji decki iz kluba me nikad ne bi ostavili na cjedilu')
#=============================================================================================================================
### roditelji
rodIdx<-58:82
chAlphaPotpora<- psych::alpha(dfNum[,rodIdx],na.rm = TRUE)
print("cronbach alpha for roditeljska potpora deci")
print(chAlphaPotpora$total)
round(kurtosi(dfNum[,rodIdx]),2)
round(skew(dfNum[,rodIdx]),2)
m <- sapply(dfNum[,rodIdx],mean,na.rm = TRUE)
round(unname(m),2)

m <- sapply(dfNum[,rodIdx],sd,na.rm = TRUE)
round(unname(m),2)

ks <- sapply(dfNum[,rodIdx],ks.test, y = 'pnorm')


amr<-sapply(dfNum[,rodIdx],FUN=cross)
amr
#=============================================================================================================
######### factor analysis



zabPCA <- prcomp(dfNum[,zabIdx])
summary(zabPCA)
zabPCA$rotation[,c(1,2)]

posPCA<-prcomp(dfNum[,posIdx])
summary(posPCA)
posPCA$rotation[,c(1,2)]

motFa<-irt.fa(dfNum[,motIdx], nfactors = 2)
motFa$fa


socPCA<-prcomp(dfNum[,socIdx])
summary(socPCA)
socPCA$rotation[,1:6]

rodFa <-irt.fa(dfNum[,rodIdx]) #  pca ima greske u SVD zbog nula u crosstabovima
rodFa$fa

#===

zajednickaIskustva <- dfNum[,socIdx[c(10,15,16,17,18)]]
  trenerskaPohvala <- dfNum[,socIdx[c(1,2,3,4,5,6)]]
  povjerenjeUSUborce<-dfNum[,socIdx[c(7,8,9,11,12)]]
  roditeljskiPonos <- dfNum[,rodIdx[c(5,8,13,16)]]
  zajednickoBavljenjeSportom <-dfNum[,rodIdx[c(4,12,20,24)]]
  
  xx <-psych::alpha(zajednickaIskustva)
  xx$total
  
  
 yy <-psych::alpha(trenerskaPohvala)
 yy$total
 
 yy <-psych::alpha(povjerenjeUSUborce)
 yy$total
 
 yy <-psych::alpha(roditeljskiPonos)
 yy$total
  
 yy <-psych::alpha(zajednickoBavljenjeSportom)
 yy$total

 
 napredovanjeTrud <- dfNum[,motIdx[c(2,3,4,6,9,13,14,15,16)]]
 yy <-psych::alpha(napredovanjeTrud)
 yy$total
 
 
 pritisak <- dfNum[,motIdx[c(5,10,11)]]
 yy <-psych::alpha(pritisak)
 yy$total
 
 zadovolj <- dfNum[,motIdx[c(1,7,8,13)]]
 yy <-psych::alpha(zadovolj)
 yy$total
 
 uzivanje <- dfNum[,zabIdx[c(1,2,3,5)]]
 yy <-psych::alpha(uzivanje)
 yy$total
 
 
 simpatije <- dfNum[zabIdx[c(4,2)]]
 yy <-psych::alpha(simpatije)
 yy$total
#===============================================================================================================

library(GGally)
library(CCA)
library(MASS)
library(CCP)