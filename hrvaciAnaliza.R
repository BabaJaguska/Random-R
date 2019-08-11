library(plyr)
library(dplyr)
#library(stats)
library(psych)
library(ggplot2)
library(reshape2)

setwd('D:/hrvaci/')
df<-read.table('HrvanjeTabelaSVI.csv', sep = ',', header = TRUE, colClasses = 'character')



fixComma<-function(str){
    
  return(gsub(',','.',str))
  
}


quantify<-function(str){
  
  posDigit<-regexpr('[0-9]',str)
  return(substr(str,posDigit,posDigit))
  
}

for (i  in 11:82){
  df[,i]<-as.numeric(sapply(df[,i],quantify))
}
for (i in c(1,5)){
  df[,i]<-sapply(df[,i],fixComma)
}


df<-plyr::rename(df,c('Koli.K.AFko.si.star..u.godinama.i.mjesecima..' ='Starost.god.mes'))
df$Starost.god.mes<-as.numeric(df$Starost.god.mes)


# grupisi starost
df$Starost.grupa <-c()
df$Starost.grupa[df$Starost.god.mes<14]<-1
df$Starost.grupa[which(df$Starost.god.mes<16 & df$Starost.god.mes>=14)]<-2 #14,15
df$Starost.grupa[df$Starost.god.mes>=16]<-3  #16,17
df$Starost.grupa<-as.factor(df$Starost.grupa)


df$Spol<-sapply(df$Spol, function(x) toupper(x))
df$Spol<-as.factor(df$Spol)
#df$Koliko.dugo.treniras.hrvanje..u.godinama.i.mjesecima..<-as.numeric(df$Koliko.dugo.treniras.hrvanje..u.godinama.i.mjesecima..)
df$Koliko.minuta.traje.tvoj.trening<-as.factor(df$Koliko.minuta.traje.tvoj.trening)
df$Koliko.puta.tjedno.treniras.<-as.factor(df$Koliko.puta.tjedno.treniras.)
df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Europskom.prvenstvu<-as.factor(df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Europskom.prvenstvu)
df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.prvenstvu.Hrvatske <-as.numeric(df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.prvenstvu.Hrvatske)
df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Svjetskom.prvenstvu <-as.factor(df$Koji.je.tvoj.najbolji.ostvareni.rezultat.na.Svjetskom.prvenstvu)

################################################################
##### POSVECENOST SPORTU #######
#########################################


#### Zabava u sportu


t <- as.data.frame(table(Spol=df$Spol,df$Koliko.Vam.je.zabavno.bavljenje.vasim.sportom.,df$Starost.grupa))
ttemp <- as.data.frame(table(Spol=df$Spol,df$Starost.grupa))
t<-plyr::rename(t,c('Var2'='NivoZabave','Var3'='Starosna.skupina'))
t$RelativnaFrekvencija <-NA
t$RelativnaFrekvencija[which(t$Spol=='M' & t$Starosna.skupina ==1)] <-t$Freq[which(t$Spol=='M' & t$Starosna.skupina ==1)]*100/ttemp[1,3]
t$RelativnaFrekvencija[which(t$Spol=='M' & t$Starosna.skupina ==2)] <-t$Freq[which(t$Spol=='M' & t$Starosna.skupina ==2)]*100/ttemp[3,3]
t$RelativnaFrekvencija[which(t$Spol=='M' & t$Starosna.skupina ==3)] <-t$Freq[which(t$Spol=='M' & t$Starosna.skupina ==3)]*100/ttemp[5,3]
t$RelativnaFrekvencija[which(t$Spol!='M' & t$Starosna.skupina ==1)] <-t$Freq[which(t$Spol!='M' & t$Starosna.skupina ==1)]*100/ttemp[2,3]
t$RelativnaFrekvencija[which(t$Spol!='M' & t$Starosna.skupina ==2)] <-t$Freq[which(t$Spol!='M' & t$Starosna.skupina ==2)]*100/ttemp[4,3]
t$RelativnaFrekvencija[which(t$Spol!='M' & t$Starosna.skupina ==3)] <-t$Freq[which(t$Spol!='M' & t$Starosna.skupina ==3)]*100/ttemp[6,3]

  
b<-c(0.5,1.5,2.5,3.5,4.5,5.5)
levels(t$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")
a<-hist(df$Koliko.Vam.je.zabavno.bavljenje.vasim.sportom.,breaks = b)
gZabavno <-ggplot(data=t)+geom_bar(aes(NivoZabave,RelativnaFrekvencija,fill=Spol), position='dodge',stat='identity')+
                  facet_grid(.~Starosna.skupina)+
                  ggtitle('Koliko ispitnici pronalaze zabavu u svom sportu prema spolu i starosnoj dobi')
gZabavno


########################################################################

##### Koliko se ponosis?
tPonos <- as.data.frame(table(Spol=df$Spol,df$Koliko.se.ponosis.kada.drugima.govoris.da.treniras.ovaj.sport.,df$Starost.grupa))
ttempPonos <- as.data.frame(table(Spol=df$Spol,Starosna.skupina=df$Starost.grupa))
tPonos<-plyr::rename(tPonos,c('Var2'='NivoPonosa','Var3'='Starosna.skupina'))
tPonos$RelativnaFrekvencija <-NA
tPonos$RelativnaFrekvencija[which(tPonos$Spol=='M' & tPonos$Starosna.skupina ==1)] <-tPonos$Freq[which(tPonos$Spol=='M' & tPonos$Starosna.skupina ==1)]*100/ttempPonos[1,3]
tPonos$RelativnaFrekvencija[which(tPonos$Spol=='M' & tPonos$Starosna.skupina ==2)] <-tPonos$Freq[which(tPonos$Spol=='M' & tPonos$Starosna.skupina ==2)]*100/ttempPonos[3,3]
tPonos$RelativnaFrekvencija[which(tPonos$Spol=='M' & tPonos$Starosna.skupina ==3)] <-tPonos$Freq[which(tPonos$Spol=='M' & tPonos$Starosna.skupina ==3)]*100/ttempPonos[5,3]
tPonos$RelativnaFrekvencija[which(tPonos$Spol!='M' & tPonos$Starosna.skupina ==1)] <-tPonos$Freq[which(tPonos$Spol!='M' & tPonos$Starosna.skupina ==1)]*100/ttempPonos[2,3]
tPonos$RelativnaFrekvencija[which(tPonos$Spol!='M' & tPonos$Starosna.skupina ==2)] <-tPonos$Freq[which(tPonos$Spol!='M' & tPonos$Starosna.skupina ==2)]*100/ttempPonos[4,3]
tPonos$RelativnaFrekvencija[which(tPonos$Spol!='M' & tPonos$Starosna.skupina ==3)] <-tPonos$Freq[which(tPonos$Spol!='M' & tPonos$Starosna.skupina ==3)]*100/ttempPonos[6,3]


b<-c(0.5,1.5,2.5,3.5,4.5,5.5)
levels(tPonos$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")
a<-hist(df$Koliko.se.ponosis.kada.drugima.govoris.da.treniras.ovaj.sport.,breaks = b)
gPonos <-ggplot(data=tPonos)+geom_bar(aes(NivoPonosa,RelativnaFrekvencija,fill=Spol), position='dodge',stat='identity')+
  facet_grid(.~Starosna.skupina)+
  ggtitle('Koliko se ispitnici ponose svojim sportom prema spolu i starosnoj dobi')
gPonos

####### KOLIKO SI PREDAN TRENIRANJU?

tPredan <- as.data.frame(table(Spol=df$Spol,df$Koliko.si.predan.u.treniranju.ovog.sporta.,df$Starost.grupa))
ttempPredan <- as.data.frame(table(Spol=df$Spol,Starosna.skupina=df$Starost.grupa))
tPredan<-plyr::rename(tPredan,c('Var2'='NivoPredana','Var3'='Starosna.skupina'))
tPredan$RelativnaFrekvencija <-NA
tPredan$RelativnaFrekvencija[which(tPredan$Spol=='M' & tPredan$Starosna.skupina ==1)] <-tPredan$Freq[which(tPredan$Spol=='M' & tPredan$Starosna.skupina ==1)]*100/ttempPredan[1,3]
tPredan$RelativnaFrekvencija[which(tPredan$Spol=='M' & tPredan$Starosna.skupina ==2)] <-tPredan$Freq[which(tPredan$Spol=='M' & tPredan$Starosna.skupina ==2)]*100/ttempPredan[3,3]
tPredan$RelativnaFrekvencija[which(tPredan$Spol=='M' & tPredan$Starosna.skupina ==3)] <-tPredan$Freq[which(tPredan$Spol=='M' & tPredan$Starosna.skupina ==3)]*100/ttempPredan[5,3]
tPredan$RelativnaFrekvencija[which(tPredan$Spol!='M' & tPredan$Starosna.skupina ==1)] <-tPredan$Freq[which(tPredan$Spol!='M' & tPredan$Starosna.skupina ==1)]*100/ttempPredan[2,3]
tPredan$RelativnaFrekvencija[which(tPredan$Spol!='M' & tPredan$Starosna.skupina ==2)] <-tPredan$Freq[which(tPredan$Spol!='M' & tPredan$Starosna.skupina ==2)]*100/ttempPredan[4,3]
tPredan$RelativnaFrekvencija[which(tPredan$Spol!='M' & tPredan$Starosna.skupina ==3)] <-tPredan$Freq[which(tPredan$Spol!='M' & tPredan$Starosna.skupina ==3)]*100/ttempPredan[6,3]



levels(tPredan$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")

gPredan <-ggplot(data=tPredan)+geom_bar(aes(NivoPredana,RelativnaFrekvencija,fill=Spol), position='dodge',stat='identity')+
  facet_grid(.~Starosna.skupina)+
  ggtitle('Koliko su ispitanici predani svom sportu prema spolu i starosnoj dobi')
gPredan

##### Nastaviti trenirati

tZeliNastaviti <- as.data.frame(table(Spol=df$Spol,df$Koliko.zelis.nastaviti.trenirati.ovaj.sport.,df$Starost.grupa))
ttempZeliNastaviti <- as.data.frame(table(Spol=df$Spol,Starosna.skupina=df$Starost.grupa))
tZeliNastaviti<-plyr::rename(tZeliNastaviti,c('Var2'='NivoZeliNastavitia','Var3'='Starosna.skupina'))
tZeliNastaviti$RelativnaFrekvencija <-NA
tZeliNastaviti$RelativnaFrekvencija[which(tZeliNastaviti$Spol=='M' & tZeliNastaviti$Starosna.skupina ==1)] <-tZeliNastaviti$Freq[which(tZeliNastaviti$Spol=='M' & tZeliNastaviti$Starosna.skupina ==1)]*100/ttempZeliNastaviti[1,3]
tZeliNastaviti$RelativnaFrekvencija[which(tZeliNastaviti$Spol=='M' & tZeliNastaviti$Starosna.skupina ==2)] <-tZeliNastaviti$Freq[which(tZeliNastaviti$Spol=='M' & tZeliNastaviti$Starosna.skupina ==2)]*100/ttempZeliNastaviti[3,3]
tZeliNastaviti$RelativnaFrekvencija[which(tZeliNastaviti$Spol=='M' & tZeliNastaviti$Starosna.skupina ==3)] <-tZeliNastaviti$Freq[which(tZeliNastaviti$Spol=='M' & tZeliNastaviti$Starosna.skupina ==3)]*100/ttempZeliNastaviti[5,3]
tZeliNastaviti$RelativnaFrekvencija[which(tZeliNastaviti$Spol!='M' & tZeliNastaviti$Starosna.skupina ==1)] <-tZeliNastaviti$Freq[which(tZeliNastaviti$Spol!='M' & tZeliNastaviti$Starosna.skupina ==1)]*100/ttempZeliNastaviti[2,3]
tZeliNastaviti$RelativnaFrekvencija[which(tZeliNastaviti$Spol!='M' & tZeliNastaviti$Starosna.skupina ==2)] <-tZeliNastaviti$Freq[which(tZeliNastaviti$Spol!='M' & tZeliNastaviti$Starosna.skupina ==2)]*100/ttempZeliNastaviti[4,3]
tZeliNastaviti$RelativnaFrekvencija[which(tZeliNastaviti$Spol!='M' & tZeliNastaviti$Starosna.skupina ==3)] <-tZeliNastaviti$Freq[which(tZeliNastaviti$Spol!='M' & tZeliNastaviti$Starosna.skupina ==3)]*100/ttempZeliNastaviti[6,3]



levels(tZeliNastaviti$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")

gZeliNastaviti <-ggplot(data=tZeliNastaviti)+geom_bar(aes(NivoZeliNastavitia,RelativnaFrekvencija,fill=Spol), position='dodge',stat='identity')+
  facet_grid(.~Starosna.skupina)+
  ggtitle('Koliko ispitanici zele nastavitii da se bave sportom prema spolu i starosnoj dobi')
gZeliNastaviti

################ Odlucan da nastavi
tOdlucanDaNastavi <- as.data.frame(table(Spol=df$Spol,df$Koliko.si.odlu.an.da.nastavis.trenirati.ovaj.sport.,df$Starost.grupa))
ttempOdlucanDaNastavi <- as.data.frame(table(Spol=df$Spol,Starosna.skupina=df$Starost.grupa))
tOdlucanDaNastavi<-plyr::rename(tOdlucanDaNastavi,c('Var2'='NivoOdlucanDaNastavia','Var3'='Starosna.skupina'))
tOdlucanDaNastavi$RelativnaFrekvencija <-NA
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==1)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==1)]*100/ttempOdlucanDaNastavi[1,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==2)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==2)]*100/ttempOdlucanDaNastavi[3,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==3)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol=='M' & tOdlucanDaNastavi$Starosna.skupina ==3)]*100/ttempOdlucanDaNastavi[5,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==1)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==1)]*100/ttempOdlucanDaNastavi[2,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==2)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==2)]*100/ttempOdlucanDaNastavi[4,3]
tOdlucanDaNastavi$RelativnaFrekvencija[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==3)] <-tOdlucanDaNastavi$Freq[which(tOdlucanDaNastavi$Spol!='M' & tOdlucanDaNastavi$Starosna.skupina ==3)]*100/ttempOdlucanDaNastavi[6,3]



levels(tOdlucanDaNastavi$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")

gOdlucanDaNastavi <-ggplot(data=tOdlucanDaNastavi)+geom_bar(aes(NivoOdlucanDaNastavia,RelativnaFrekvencija,fill=Spol), position='dodge',stat='identity')+
  facet_grid(.~Starosna.skupina)+
  ggtitle('Koliko su ispitanici OdlucanDaNastavii svom sportu prema spolu i starosnoj dobi')
gOdlucanDaNastavi

##### Koliko bi bilo tesko prestati da treniras
tBiloBiTesko <- as.data.frame(table(Spol=df$Spol,df$Koliko.bi.ti.tesko.bilo.prestati.trenirati.ovaj.sport.,df$Starost.grupa))
ttempBiloBiTesko <- as.data.frame(table(Spol=df$Spol,Starosna.skupina=df$Starost.grupa))
tBiloBiTesko<-plyr::rename(tBiloBiTesko,c('Var2'='NivoBiloBiTeskoa','Var3'='Starosna.skupina'))
tBiloBiTesko$RelativnaFrekvencija <-NA
tBiloBiTesko$RelativnaFrekvencija[which(tBiloBiTesko$Spol=='M' & tBiloBiTesko$Starosna.skupina ==1)] <-tBiloBiTesko$Freq[which(tBiloBiTesko$Spol=='M' & tBiloBiTesko$Starosna.skupina ==1)]*100/ttempBiloBiTesko[1,3]
tBiloBiTesko$RelativnaFrekvencija[which(tBiloBiTesko$Spol=='M' & tBiloBiTesko$Starosna.skupina ==2)] <-tBiloBiTesko$Freq[which(tBiloBiTesko$Spol=='M' & tBiloBiTesko$Starosna.skupina ==2)]*100/ttempBiloBiTesko[3,3]
tBiloBiTesko$RelativnaFrekvencija[which(tBiloBiTesko$Spol=='M' & tBiloBiTesko$Starosna.skupina ==3)] <-tBiloBiTesko$Freq[which(tBiloBiTesko$Spol=='M' & tBiloBiTesko$Starosna.skupina ==3)]*100/ttempBiloBiTesko[5,3]
tBiloBiTesko$RelativnaFrekvencija[which(tBiloBiTesko$Spol!='M' & tBiloBiTesko$Starosna.skupina ==1)] <-tBiloBiTesko$Freq[which(tBiloBiTesko$Spol!='M' & tBiloBiTesko$Starosna.skupina ==1)]*100/ttempBiloBiTesko[2,3]
tBiloBiTesko$RelativnaFrekvencija[which(tBiloBiTesko$Spol!='M' & tBiloBiTesko$Starosna.skupina ==2)] <-tBiloBiTesko$Freq[which(tBiloBiTesko$Spol!='M' & tBiloBiTesko$Starosna.skupina ==2)]*100/ttempBiloBiTesko[4,3]
tBiloBiTesko$RelativnaFrekvencija[which(tBiloBiTesko$Spol!='M' & tBiloBiTesko$Starosna.skupina ==3)] <-tBiloBiTesko$Freq[which(tBiloBiTesko$Spol!='M' & tBiloBiTesko$Starosna.skupina ==3)]*100/ttempBiloBiTesko[6,3]



levels(tBiloBiTesko$Starosna.skupina)<-c("11 do 13 godina","14 i 15 godina", "16 i 17 godina")

gBiloBiTesko <-ggplot(data=tBiloBiTesko)+geom_bar(aes(NivoBiloBiTeskoa,RelativnaFrekvencija,fill=Spol), position='dodge',stat='identity')+
  facet_grid(.~Starosna.skupina)+
  ggtitle('Koliko su ispitanici BiloBiTeskoi svom sportu prema spolu i starosnoj dobi')
gBiloBiTesko
