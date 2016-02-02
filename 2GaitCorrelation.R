# Correlation matrix and removing highly correlated parameters as dimension reduction before classification

##### Definisi funkciju za racunanje korelacije, crtanje matrice
##### i izbacivanje kako bi se smanjio pair-wise correlation
##### Kao ulaz uneti data frame sa sve dijagnozom i imenima ali BEZ rednih brojeva
##### Kao i zeljeni cutoff(cut) (npr. 0.9 za izbacivanje svih korelacija preko 0.9)
##### Izlaz ce biti data frame sa imenima i dijagnozom i odabranim parametrima

IzbaciCorr<-function(x,cut){
  
  # Korelaciona matrica svih parametara osim rednih brojeva, imena i dijagnoza
   CorrMatrix<-cor(x[,-c(ncol(x),ncol(x)-1)])
  
  # crtaj matricu
  par(omi=c(0.5,0,0,0.5))  
  corrplot(CorrMatrix, method="square",type="full", order="hclust",tl.col="black", tl.cex=0.4,tl.srt=60)
  
  # Nadji koje parametre treba izbaciti
  # na osnovu kriterijuma uzajamne korelacije sa drugom promenljivom >0.9
  out <- findCorrelation(CorrMatrix, cutoff = cut)
  x<-x[,-out]
  return(x)
}

## then you take your training set and apply the function onto it
## like: training<-IzbaciCorr(training[,-1],0.85)
