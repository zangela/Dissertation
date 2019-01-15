####################################################################################

#CALCOLO RETI NEURALI

####################################################################################


library(nnet)
set.seed(123)

#x=matrice covariate
#y=y binaria ins.verifica

reti.class=nnet(x,y,data=ins.stima,decay=0.016,size=5,maxit=10000)
#ci mette un po a girare
pred.reti=predict(reti.class,newdata=ins.ver,type="class")
tabella.sommario(pred.reti,ins.ver$genere)
pred.reti2=predict(reti.class,newdata=ins.ver,type="raw")

#y.numeric.v= y binaria nell'ins.ver

curva5=lift.roc(pred.reti2,y.numeric.v,type='crude')

prev <- NA
for(i in 1:length(pred.reti2)){ prev[i] <- pred.reti2[[i]] }
auc.nnet <- auc(y.numeric.v, pred.reti)[1]


####################################################################################

#MODELLO 1:PASSATE vs ALL

####################################################################################

y_binary<-matrix(-1,nrow=nrow(ins.stima),1)
for (k in 1: nrow(ins.stima))       # classificazione ONE vs ONE
{
  if (as.numeric(ins.stima[k,6])==0) #passate contro all
  {
    y_binary[k,1]=1
  }
}

#PROBLEMA: non ho una matrice già definita per intero di covariate, xk con la funzione train la calcolato a pezzetti

