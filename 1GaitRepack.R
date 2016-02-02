# Reading, writing, repacking, removing characters

library("dplyr")
setwd("C:/Users/TECNALIA2014BELI/Desktop/Minjino/kojesta/gaitrite")



####################################################################
#Definisi funkciju za citanje
####################################################################

Prepakuj<-function(DataGore,DataDole){
## Get parameter names
Parametri<-DataGore[,1]

#Combine gore i dole
Data<-data.frame(DataGore[,-1],DataDole[,-1])
rownames(Data)<-as.character(1:nrow(Data)) # Set row names, so that they don't duplicate


### Remove all useless dashes
## Define the remove function
BezCrte<-function(x){
  n<-nchar(x)
if(!is.na(x)&&substr(x,n,n)=="-"){
  return(substr(x,1,n-1))
}
else {return(x)}
}
## apply the function onto the dataframe
Data <- as.data.frame(lapply(Data,FUN = function(x) {sapply(x,FUN=BezCrte)}))


### replace dots with zeros
## Define the replace dots function
Tacke<-function(x){
  x<-as.character(x)
  if (x==".") return(0)
  else return (x)
}
## Apply the function to our dataframe
Data<-as.data.frame(lapply(Data,FUN=function(x){sapply(x,FUN=Tacke)}))


## Recreate dataframe with rows as patients and columns as parameters
## Initiate empty dataframes
DataBaseL<-data.frame()
DataMotorL<-data.frame()
DataMentalL<-data.frame()
DataBaseR<-data.frame()
DataMotorR<-data.frame()
DataMentalR<-data.frame()

## Pack! LEVA i DESNA
Nclmn<-ncol(Data)
for (i in 1:Nclmn){
  if (i%%8==1) {DataBaseL<-rbind(DataBaseL,as.numeric(as.character(Data[,i])))
  } else if (i%%8==2) {DataBaseR<-rbind(DataBaseR,as.numeric(as.character(Data[,i])))
    }  else if (i%%8==3){DataMotorL<-rbind(DataMotorL,as.numeric(as.character(Data[,i])))
      } else if (i%%8==4){DataMotorR<-rbind(DataMotorR,as.numeric(as.character(Data[,i])))
       }   else if (i%%8==5){DataMentalL<-rbind(DataMentalL,as.numeric(as.character(Data[,i])))
         }   else if (i%%8==6){DataMentalR<-rbind(DataMentalR,as.numeric(as.character(Data[,i])))
  }  
}




# Izbaci glupe parametre, poput duzine i sirine stopala
izbaci<-c(1,2,3,10,11,14,26,45,46)
DataBaseL<-DataBaseL[,-izbaci]
DataMotorL<-DataMotorL[,-izbaci]
DataMentalL<-DataMentalL[,-izbaci]
DataBaseR<-DataBaseR[,-izbaci]
DataMotorR<-DataMotorR[,-izbaci]
DataMentalR<-DataMentalR[,-izbaci]


# Usrednji levu i desnu nogu
N<-ncol(DataBaseR)
M<-nrow(DataBaseR)

DataBase<-data.frame()
DataMotor<-data.frame()
DataMental<-data.frame()

for (i in 1:N){
  for (j in 1:M) {
    if (is.na(DataBaseR[j,i])){ DataBase[j,i]<-DataBaseL[j,i]
                                DataMotor[j,i]<-DataMotorL[j,i]
                                DataMental[j,i]<-DataMentalL[j,i]
    }    else {
    DataBase[j,i]<-(DataBaseL[j,i]+DataBaseR[j,i])/2
    DataMotor[j,i]<-(DataMotorL[j,i]+DataMotorR[j,i])/2
    DataMental[j,i]<-(DataMentalL[j,i]+DataMentalR[j,i])/2
    }
  }
}

#Set column names
colnames(DataBase)<-paste(Parametri[-izbaci],"Base")
colnames(DataMotor)<-paste(Parametri[-izbaci],"Motor")
colnames(DataMental)<-paste(Parametri[-izbaci],"Mental")


# BEZ COMBINED
Data<-data.frame(DataBase,DataMotor,DataMental)
}

################################
# PAKUJ
#######################################

#PREPAKUJ KONTROLE
DataGore<-read.csv("Prvih40GORE.csv",stringsAsFactors=F,colClasses="character")
DataDole<-read.csv("Prvih40DOLE.csv",stringsAsFactors=F,colClasses="character")
DataCTRL<-Prepakuj(DataGore,DataDole)


#PREPAKUJ PD
DataGore<-read.csv("Drugih40GORE.csv",stringsAsFactors=F,colClasses="character")
DataDole<-read.csv("Drugih40DOLE.csv",stringsAsFactors=F,colClasses="character")
DataPD<-Prepakuj(DataGore,DataDole)

##### Pakuj sve zajedno

# Dodaj dijagnozu za CTRL
nr<-nrow(DataCTRL)
dijagnoza<-rep("CTRL",nr)
DataCTRL<-data.frame(DataCTRL, dijagnoza)


# Dodaj dijagnozu za PD
nrp<-nrow(DataPD)
dijagnoza<-rep("PD",nrp)
DataPD<-data.frame(DataPD,dijagnoza)

# Smuljaj zajedno CTRL i PD po redovima
Data<-rbind(DataCTRL,DataPD)

# dodaj i imena na kraj
Imena<-read.csv("Imena.csv",header=TRUE)
Data<-data.frame(Data,Imena)

# upisi u .csv
write.csv(Data,"!PrepakovaniSVIfinal.csv")

















