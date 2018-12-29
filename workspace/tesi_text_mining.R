rm(list=ls())

# Chiamiamo i pacchetti necessari
library(MASS)
library(tidyverse)
library(tm)
library(lsa) #pacchetto per le stopwords in più lingue
library(caret)
library(wordcloud)
library(devtools) #installarla se necessario
library (TextWiller)
library(ggplot2) 
require(tau)
library(e1071) #svm tradizionale
library(ggplot2) 
library(SnowballC)
library(kernlab) #svm online

##################################################################
#
#
#
#
#                   CARICAMENTO e SISTEMAZIONE DATI
#
#
#
#
# ##################################################################

dati=read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = ".", encoding = "UTF-8")
column=ncol(dati)+7 
datidef = matrix(0,nrow=nrow(dati), ncol=column) #ora creiamo la matrice con cui lavoreremo in seguito
colnames(datidef)=c("Mese", "Giorno","Ora","Fascia","Y","Y_cod","NameSender","DomainSender","NameReceiver","DomainReceiver","Internal","Obj","Motivation", "Sbloccata")
for (i in 1:nrow(dati))
{
  print(i)
  index =1
  for (j in 1:ncol(dati))
  {
    if(j==1) #data e ora
    {
      object = dati[i,j]
      tmp=strsplit(toString(object), "-")
      
      foo =strsplit(toString(tmp[[1]][1]), ":")
      foo1 =strsplit(toString(tmp[[1]][2]), ":")
      datidef[i,index]=as.numeric(foo[[1]][2])
      index = index+1
      datidef[i,index]=as.numeric(foo[[1]][3])
      index = index+1
      datidef[i,index]=as.numeric(foo1[[1]][1])
      
      if ((as.numeric(datidef[i,index])<18)&(as.numeric(datidef[i,index]) >7))
      {
        index = index+1
        datidef[i,index]=1 #fascia lavorativa
        index = index+1
      }
      else
      {
        index = index+1
        datidef[i,index]=0 #fascia notturna
        index = index+1
      }
    }
    else if (j==2)
    {
      object = toString(dati[i,j])
      if(object == "email passed")
      {
        datidef[i,index]=toString(dati[i,j])
        index=index+1
        datidef[i,index]=0
        index=index+1
      }
      else if(object == "email rejected")
      {
        datidef[i,index]=toString(dati[i,j])
        index=index+1
        datidef[i,index]=1
        index=index+1
      }
      else if(object == "email quarantined")
      {
        datidef[i,index]=toString(dati[i,j])
        index=index+1
        datidef[i,index]=2
        index=index+1
      }
    }
    else if((j==3)|(j==4)) #sender
    {
      object = dati[i,j]
      tmp=strsplit(toString(object), "@")
      datidef[i,index]=tmp[[1]][1]
      index=index+1
      datidef[i,index]=tmp[[1]][2]
      index=index+1
    }
    if((j!=1)&(j!=2)&(j!=3)&(j!=4)) #per tutte le colonne che non devo modificare
    {
      datidef[i,index+1]=toString(dati[i,j])
      index=index+1
    }
  }
}
rn = rep(0,nrow(datidef))
for (i in 1:nrow(datidef)) #codifica mittenti in base al dominio
{
  rn[i]=i
  if ( (as.character(datidef[i,8])=="steelco-usa.com")|(as.character(datidef[i,8])=="steelco.veniceplaza.net")|(as.character(datidef[i,8])=="steelcogroup.com")|(as.character(datidef[i,8])=="steelcoservice.com")|(as.character(datidef[i,8])=="steelcospa.com"))
    datidef[i,11] = 1
}
rownames(datidef)=rn

##################################################################
#
#                 SUDDIVISIONE IN STIMA E VERIFICA
#
##################################################################

set.seed(123)
campione= sample(1:nrow(datidef),800)# regola del 70-30 (per dati completi sarebbe 365000)
campione
ins.stima= datidef[campione,]			
ins.ver = datidef[-campione,]

##################################################################
#
#
#
#
#                           ANALISI DESCRITTIVE
#
#
#
#
##################################################################


##################################################################
#1) ANALISI SULLA DISTRIBUZIONE DI Y CODIFICATA COME NUMERICA
##################################################################

freqass_y=table(ins.stima[,6])                               #calcolo freq assolute
freqrel=as.numeric(freqass_y/sum(freqass_y))               #calcolo freq relative

barplot(freqass_y/sum(freqass_y), ylab="Frequenze relative", main="Distribuzione della tipologia di email",
        ylim=(0:1), col=2:4, xlab="Codifica email")

##################################################################
#2)ANALISI SULLA DISTRIBUZIONE DEI MITTENTI (INTERNI O ESTERNI) 
##################################################################

freqass_in=table(ins.stima[,11]) #calcolo freq assolute
barplot(freqass_in/sum(freqass_in), ylab="Frequenze relative", main="Distribuzione per tipologia di mittente",
        ylim=(0:1), col=3:5)

#0->esterni
#1->interni

stee_p=0
stee_r=0
stee_q=0

no_stee_p=0
no_stee_r=0
no_stee_q=0

for (i in 1:nrow(ins.stima))
{
  if (as.numeric(ins.stima[i,11])==1)  #Domini interni:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      stee_p=stee_p+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      stee_r=stee_r+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      stee_q=stee_q+1
  }
  else #Domini esterni:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      no_stee_p=no_stee_p+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      no_stee_r=no_stee_r+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      no_stee_q=no_stee_q+1
  }
}


#Interni:
tot_int=(stee_p+stee_r+stee_q)
stee_p_rel=stee_p/tot_int
stee_q_rel=stee_q/tot_int
stee_r_rel=stee_r/tot_int

interni=cbind(stee_p_rel,stee_r_rel,stee_q_rel)
colnames(interni)=c('pass','rige','quar')


#Esterni:
tot_est=(no_stee_p+no_stee_r+no_stee_q)
no_stee_p_rel=no_stee_p/tot_est
no_stee_q_rel=no_stee_q/tot_est
no_stee_r_rel=no_stee_r/tot_est

esterne=cbind(no_stee_p_rel,no_stee_r_rel,no_stee_q_rel)
colnames(esterne)=c('pass','rige','quar')

par(mfrow=c(1,2))
barplot(interni, ylab="Frequenze relative", main="Distr per mittenti interni",ylim=(0:1), col=2:4)
#correttamente tutte le email mandate dal dominio interno passano per il Firewall senza essere bloccate->risultato scontao
barplot(esterne, ylab="Frequenze relative", main="Distr per mittenti esterni",ylim=(0:1), col=2:4)


##################################################################
#3)ANALISI SULLA DISTRIBUZIONE DELLE EMAIIL SBLOCCATE
##################################################################

#devo prendere solo le y_cod=2 e verificare la proporzione di email sbloccate
#calcoliamo quanto email in quarantena ci sono e lo salviamo in num_quarantene 
conteggio=table(ins.stima[,5])
num_quarantene=conteggio[[2]]

freqass_sb=0
for (i in 1:nrow(ins.stima))
{
  if ((as.numeric(ins.stima[i,6])==2) &&(as.numeric(ins.stima[i,14])==1))
  {
    freqass_sb = freqass_sb+1
  }
}
freqass_sb=(freqass_sb/num_quarantene)
freqass_sb      #->sarebbe l'errore commesso da parte del Firewall
par(mfrow=c(1,1))

##################################################################
#4)ANALISI SULLA DISTRIBUZIONE PER FASCIA ORARIA
##################################################################

#capire quante email vengono mandate nelle diverse fasce orarie; 
freqass_fascia=table(ins.stima[,4]) #calcolo freq assolute
freqrel_fascia=as.numeric(freqass_fascia/sum(freqass_fascia)) #calcolo freq relative

barplot(freqass_fascia/sum(freqass_fascia), ylab="Frequenze relative", main="Distribuzione delle email per fascia oraria",
        ylim=(0:1), col=2:3)

#=0 fascia notturna
#=1 fascia lavorativa

#capire nella fascia lavorativa (e non) quante email dei tre tipi ci sono-> capiamo la distribuzione delle email (delivedere, quarantened e rejected) nelle due fascie orarie
#in "conteggio" abbiamo gi? il tot di email dei tre tipi: ci prendiamo quello che ci interessa

num_passed=conteggio[[1]]
num_rejected=conteggio[[3]]

ps0=0
rj0=0
qr0=0

ps1=0
rj1=0
qr1=0

for (i in 1:nrow(ins.stima))
{
  if (as.numeric(ins.stima[i,4])==0)  #Fascia notturna:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      ps0=ps0+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      rj0=rj0+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      qr0=qr0+1
  }
  else #Fascia lavorativa:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      ps1=ps1+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      rj1=rj1+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      qr1=qr1+1
  }
}

#Fascia notturna:
tot_not=(ps0+rj0+qr0)
ps0_rel=ps0/tot_not
qr0_rel=qr0/tot_not
rj0_rel=rj0/tot_not

fascia_not=cbind(ps0_rel,rj0_rel,qr0_rel)
colnames(fascia_not)=c('pass','rige','quar')

#Fascia lavorativa:
tot_lav=(ps1+rj1+qr1)
ps1_rel=ps1/tot_lav
qr1_rel=qr1/tot_lav
rj1_rel=rj1/tot_lav

fascia_lav=cbind(ps1_rel,rj1_rel,qr1_rel)
colnames(fascia_lav)=c('pass','rige','quar')

par(mfrow=c(1,2))
barplot(fascia_not, ylab="Frequenze relative", main="Distr fascia notturna",ylim=(0:1), col=2:4)
barplot(fascia_lav, ylab="Frequenze relative", main="Distr fascia lavorativa",ylim=(0:1), col=2:4)
par(mfrow=c(1,1))

##################################################################
#5)ANALISI SULLA DISTRIBUZIONE PER MESE
##################################################################

#capire quante email vengono mandate nei diversi mesi ->mese dev'essere un fattore!
freq_mese=table(ins.stima[,1])

k = sort(as.numeric(names(freq_mese)))
f = matrix(0, nrow=1, ncol=length(freq_mese))
names =as.numeric(names(freq_mese))
idx =1
for (idx in 1:ncol(f))
  
{
  for (i in 1:length(freq_mese))
  {
    if (k[idx]==names[i])
    {
      
      f[1,idx]= as.numeric(freq_mese[i])
      i = length(freq_mese)+1
    }
  }
}
colnames(f)=k

barplot(f/sum(freq_mese), ylab="Frequenze relative", main="Distribuzione email per Mese", col=2:5, ylim=c(0:1))


#capire nei vari mesi quante email dei tre tipi ci sono (capire se ci sono stati mesi pi? intensi di altri)

num_ago=f[[1]]
num_sett=f[[2]]
num_ott=f[[3]]
num_nov=f[[4]]

psa=0
rja=0
qra=0

pss=0
rjs=0
qrs=0

pso=0
rjo=0
qro=0

psn=0
rjn=0
qrn=0


for (i in 1:nrow(ins.stima))
{
  if (as.numeric(ins.stima[i,1])==8)  #Agosto:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      psa=psa+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      rja=rja+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      qra=qra+1
  }
  else if (as.numeric(ins.stima[i,1])==9)  #Settembre:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      pss=pss+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      rjs=rjs+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      qrs=qrs+1
  }
  else if (as.numeric(ins.stima[i,1])==10)  #Ottobre:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      pso=pso+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      rjo=rjo+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      qro=qro+1
  }
  else #Novembre:
  {
    if(as.numeric(ins.stima[i,6])==0) #passate 
      psn=psn+1
    if(as.numeric(ins.stima[i,6])==1) #rigettate
      rjn=rjn+1
    if (as.numeric(ins.stima[i,6])==2) #quarantene
      qrn=qrn+1
  }
  
}


#Agosto:
psa_rel=psa/num_ago
qra_rel=qra/num_ago
rja_rel=rja/num_ago

agosto=cbind(psa_rel,rja_rel,qra_rel)
colnames(agosto)=c('pass','rige','quar')

#Settembre:
pss_rel=pss/num_sett
qrs_rel=qrs/num_sett
rjs_rel=rjs/num_sett

settembre=cbind(pss_rel,rjs_rel,qrs_rel)
colnames(settembre)=c('pass','rige','quar')

#Ottobre:
pso_rel=pso/num_ott
qro_rel=qro/num_ott
rjo_rel=rjo/num_ott

ottobre=cbind(pso_rel,rjo_rel,qro_rel)
colnames(ottobre)=c('pass','rige','quar')


#Novembre:
psn_rel=psn/num_nov
qrn_rel=qrn/num_nov
rjn_rel=rjn/num_nov

novembre=cbind(psn_rel,rjn_rel,qrn_rel)
colnames(novembre)=c('pass','rige','quar')

par(mfrow=c(2,2))

barplot(agosto, ylab="Frequenze relative", main="Distr Agosto",ylim=(0:1), col=2:4)
barplot(settembre, ylab="Frequenze relative", main="Distr Settembre",ylim=(0:1), col=2:4)
barplot(ottobre, ylab="Frequenze relative", main="Distr Ottobre",ylim=(0:1), col=2:4)
barplot(novembre, ylab="Frequenze relative", main="Distr Novembre",ylim=(0:1), col=2:4)

par(mfrow=c(1,1))


##################################################################
#6)ANALISI SULLA DISTRIBUZIONE DELLE PASSATE
##################################################################

#vedere la distribuzione delle email passate rispetto ad internal/esternal

int=0
ext=0
for (i in 1:nrow(ins.stima))
{
  if (as.numeric(ins.stima[i,6])==0) #se email passata (tot ne ho 813->corretto)
  {
    if(as.numeric(ins.stima[i,11])==1)  #interne 
      int=int+1
    else 
      ext=ext+1 #esterne 
  }
}

int_rel=int/sum(ins.stima[,6]==0)
ext_rel=ext/sum(ins.stima[,6]==0)

barplot(cbind(int_rel,ext_rel), ylab="Frequenze relative", main="Distr nelle email passate",ylim=(0:1), col=2:3)

##################################################################
#
#
#                           TEXT MINING
#                       nell'INSIEME DI STIMA
#
#
##################################################################


##################################################################
#                         TEXT MINING SULL'OGGETTO
##################################################################


##################################################################
#                         PREPROCESSING
##################################################################


## Numero di caratteri per singolo oggetto
nchars= sapply(as.vector(ins.stima[,12]),nchar) #(conta anche gli spazi)
nchars=as.vector(nchars) #creo un vettore con i numeri di caratteri per oggetto

boxplot(nchars~ins.stima[,5],col=2:4, main="Distribuzione dei caratteri per tipologia di Y", ylab="Freq assolute")

#0=passate
#1=rigettate
#2=quarantenat.test(nchars~tweets$soggettivo)

## Gestione emoticons  
ins.stima[,12]=normalizzaemote(ins.stima[,12])  #trasforma le emoticon in parole EMOTEGOOD EMOTECRY
length(grep("EMOTE",datidef[,12])) #il numero di emoticons trovate in tot
print(grep("EMOTE",datidef[,12]) )
#problema: se trova una parola che termina per(remin)D: la segnala come emoticons!
#capire quanto è grave la cosa. Se produce risultati poco affidabili


# Normalizzazione del testo
ins.stima[,12]=normalizzaTesti(ins.stima[,12],contaStringhe = c("\\?","!","@","#","(\u20AC|euro)","(\\$|dollar)")) 
#Salvo i conteggi delle parole specificate come matrice a parte

conteggi_caratteri=as.data.frame(attributes(ins.stima[,12])$counts)
#problema: NON FUNZIONA!

#Eliminare le stopwords
#nota: molte parole std sono state eliminate da nomalizzaTesti (non trovo, a, il, lo,...)
ins.stima[,12]=removeStopwords(ins.stima[,12], stopwords = c(itastopwords,"re", "rif", stopwords_nl, stopwords_de, stopwords_fr, stopwords_en)) 
#stopwords ->Stopwordlists in German, English, Dutch, French, Polish, and Arab

#ritengo che re,rif siano poco utili ai fini dello studio. Inoltre sono due delle parole più frequenti. Per non sballare le statistiche credo sia opportuno toglierle
ins.stima[,12]=removeNumbers(ins.stima[,12]) #vale quanto detto per re e rif sopra


#Analisi degli n-grammi 
#require(tau)

bigrams = textcnt(ins.stima[,12],method="string",n=2L,split="[[:blank:]]")
sort(bigrams,decreasing=TRUE)[1:20]

# trigrams = textcnt(datidef[,9],method="string",n=3L,split="[[:blank:]]")
# sort(trigrams,decreasing=TRUE)[1:10]

#se voglio creare degli insiemi di parole dati gli n grammi appena trovati
ins.stima[,12] = gsub("assente ufficio", "assente_ufficio", ins.stima[,12])
ins.stima[,12] = gsub("sessione disconnessa", "sessione_disconnessa", ins.stima[,12])
ins.stima[,12] = gsub("purchase order", "purchase_order", ins.stima[,12])
ins.stima[,12] = gsub("ordine acquisto", "ordine_acquisto", ins.stima[,12])#etc

##################################################################
#
#
#
#   inizio CREAZIONE DEL DIZIONARIO NEL CASO ALLDEF.CSV
#   questo ha senso crearlo sul tutto il dataset    
#
#
#
##################################################################
make_dictionary <- function(corpus, tmp) 
{  
  dtm = as.matrix(DocumentTermMatrix(corpus
                                     , control = list( stemming = TRUE, stopwords = itastopwords,
                                                       minWordLength = 2, removeNumbers = TRUE,
                                                       removePunctuation = FALSE, bounds=list(local = c(1,Inf)) ))
  ) 
  coln = colnames(dtm)
  dic = rep("", length(coln)+length(tmp))
  for (i in 1:length(coln))
  {
    dic[i]=coln[i]
  }
  k = length(coln)+1
  if(length(tmp > 0))
  {
    for (i in 1:length(tmp))
    {
      dic[k]=tmp[i]
      k = k+1
    }
  }
  return(dic)
}

n_part = 50
part = as.integer(nrow(datidef)/n_part)
tmp = rep("", 0)
for (i in 1:(n_part+1))
{
  if (i <= 50) 
  {
    if (i == 1)
    {
      corpus = Corpus(VectorSource(datidef[1:part*i,12]))
      tmp = make_dictionary(corpus,tmp)
    }
    else
    {
      corpus = Corpus(VectorSource(datidef[(part*(i-1)+1):(part*i),12]))
      tmp = make_dictionary(corpus,tmp)
    }
  }
  else
  {
    if (part*(i-1) < nrow(datidef))
    {
      corpus = Corpus(VectorSource(datidef[(part*(i-1)+1):nrow(datidef),12]))
      dic = make_dictionary(corpus,tmp)
    }
  }
}
dic= wordStem(dic, language = "english")
dic= wordStem(dic, language = "italian")
dic= wordStem(dic, language = "spanish")
dic= wordStem(dic, language = "danish")
dic= wordStem(dic, language = "french")
dic= wordStem(dic, language = "german")
len = 0
idx = 1
dic=sort(dic)
colnfin = dic
#conto il numero di parole diverse in tutti gli oggetti
for (i in 2:length(dic))
{
  if (colnfin[idx]!=dic[i])
  {
    len = len +1
    idx = idx + 1 
    colnfin[idx]=dic[i]
  }
}
#creo un vettore per memorizzare tutte le parole diverse (serve per colnames) e la matrice objdef finale
dictionary=character(len)
#inserisco tutte le parole diverse in oarine alfabetico in dictionary
for (i in 1:len) 
{
  dictionary[i]= colnfin[i]
}
dictionary
##################################################################
#
#
#
#   fine CREAZIONE DEL DIZIONARIO NEL CASO ALLDEF.CSV
#   presente in ***dictionary***
#
#
##################################################################



##################################################################
#
#
#
#   inizio CREAZIONE DEI DOMINII NEL CASO ALLDEF.CSV
#   ha senso farlo su tutto il dataset
#
#
##################################################################
#ottengo tutti i dominii
len = 0
alldom=datidef[,8]
#dominii in ordine alfabetico
alldom= sort(alldom)
#conto e mi salvo tutti i dominii diversi
tmpdom = character(nrow(datidef))
idx=1
tmpdom[idx]=toString(alldom[idx])
len=1
for (i in 2:nrow(datidef))
{
  if (tmpdom[idx]!=toString(alldom[i]))
  {
    len = len +1
    idx=idx+1
    tmpdom[idx]=toString(alldom[i])
  }
}
#creo un vettore che contiene tutti i dominii Diversi 
domain= character(len)
for (i in 1:len)
{
  domain[i]=tmpdom[i]
}
length(domain)
##################################################################
#
#
#
#   fine CREAZIONE DEI DOMINII NEL CASO ALLDEF.CSV
#   presente in ***domain***
#
#
##################################################################


##################################################################
#
#
#
#   inizio CREAZIONE DELLE MOTIVAZIONI NEL CASO ALLDEF.CSV
#   ha senso farlo su tutto il dataset 
#
#
##################################################################

#ottengo tutti i motivation
len = 0
allmot=datidef[,13]
#motivation in ordine alfabetico
allmot= sort(allmot)
#conto e mi salvo tutte le motivation diverse
tmpmot = character(nrow(datidef))
idx=1
tmpmot[idx]=""
len=1
for (i in 2:nrow(datidef))
{
  if (tmpmot[idx]!=toString(allmot[i]))
  {
    len = len +1
    idx=idx+1
    tmpmot[idx]=toString(allmot[i]) #salvo le motivation diverse in modo unico
  }
}
len = len -1 #ho tolto il primo carattere che è "" utilizzato per saltare tutti quelli vuoti

#creo un vettore che contiene tutti le motivation Diversi 
motivation= character(len)
idx = 2
for (i in 1:len)
{
  motivation[i]=tmpmot[idx]
  idx = idx +1
}
motivation

##################################################################
#
#
#
#   fine CREAZIONE DELLE MOTIVAZIONI NEL CASO ALLDEF.CSV
#   presente in ***motivation***
#
#
##################################################################



# ##################################################################
# #                     ANALISI DELLE FREQUENZE
# #                     non le posso lanciare xk troppo pesanti!!!!!!!!!
# ##################################################################
# 
# 
# #Avremo una matrice molto sparsa
# 
# #per avere la frequenza di ogni singola parola univoca:
# freq_obj = colSums(as.matrix(objdef)) #sarebbe da lanciare su dtm
# 
# #Next, we sort this in descending order to get to know the terms with the highest frequency, as follows:
# ord_obj = sort(freq_obj,decreasing=T)
# top_six=(head(ord_obj)/sum(ord_obj))
# 
# 
# barplot(ord_obj, ylab="Frequenze assolute", main="Parole più frequenti nell'oggetto",ylim=(0:1), col=2)
# 
# 
# 
# farms %>% 
#   ggplot(aes(x = as.data.frame(ord_obj)) +
#            geom_bar())
# 
# 
# 

##################################################################
#                     ASSEGNAZIONE SENTIMENT
##################################################################


#lo useremo come possibile predittore  futuro
sent=sentiment(ins.stima[,12]) #"positivo" (+1), "negativo" (-1),  "neutro" (0)

#unisco già in datidef due dei predittori che mi serviranno poi per svm->manca però conteggio_caratteri che non funziona
ins.stima=cbind(ins.stima,sent,nchars)

#il problema delle emoticons qui fa sbagliare qualche sent a mio avviso

prop.table(table(ins.stima[,15],exclude = NULL)) #ci da la proporzione di sent
barplot(table(ins.stima[,15]),col=2:4, main="Proprorzione dei sent", xlab="Sent", ylab="Freq")

##################################################################
#
#
#
#
#                       ANALISI GRAFICA
#
#
#
#
# ##################################################################
# 
# ##################################################################
# #                     FREQUENZA PAROLE NELL'OGGETTO
# ##################################################################
# 
# 
# wf = data.frame(word=names(ord_obj), freq=ord_obj)
# p = ggplot(subset(wf, freq>50), aes(word, freq)) #♣prendiamo quelle con freq>50
# p = p + geom_bar(stat="identity",color="darkblue", fill="lightblue") 
# p = p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
# 
# 
# #Word Cloud
# set.seed(123)
# 
# wordcloud(names(ord_obj), ord_obj, max.words=50,colors=brewer.pal(6,"Dark2"), random.order=TRUE)
# #piu' scenografico. Occhio al random order 8ogni volta cambia l'ordine
# 
# ##################################################################
# #                     FREQUENZA SENDER
# ##################################################################
# 
# freq_send = colSums(sendomdef)
# 
# wf1 = data.frame(word=names(freq_send), freq=freq_send)
# p1 = ggplot(subset(wf1, freq>5), aes(word, freq)) 
# p1 = p1 + geom_bar(stat="identity",color="darkblue", fill="lightblue") 
# p1 = p1 + theme(axis.text.x=element_text(angle=45, hjust=1)) 
# p1
# 
# #Word Cloud
# set.seed(123)
# wordcloud(names(freq_send), freq_send, max.words=50,colors=brewer.pal(6,"Dark2"), random.order=TRUE)



##################################################################
#
#
#
#   SVM: TODO 
#   start
#
#
##################################################################

#LINK UTILE
#https://data-flair.training/blogs/e1071-in-r/


##################################################################  


#FUNZIONE CREA LE MATRICI DA DARE IN PASTO A SVM


##################################################################  

svm_data <- function(type, vector, min, max)
{
  # la variabile type contiene il fatto che si debba calcolare objdef, domdef o altro
  
  # la variabile vector contiene il dizionario di parole o di dominii in base al type
  
  # min % max è il range di indice di datidef che vengono analizzati 
  # in una singola esecuzione per il calcolo degli input/output di svm
  # SEMPLICE: creo objdef e domdef per le righe di datidef che vanno da min a max ;)
  if (type == 0) # calcolo di objdef
  {
    
    
    mat = matrix(0, nrow=(max-min + 1), ncol=length(dictionary))
    
    index = 0
    for (i in 1:(max-min+1))
    {
      
      obj = strsplit(toString(ins.stima[min +index, 12]), " ") #datidef
      
      for (j in 1:length(obj[[1]]))
      {
        obj[[1]][j] = wordStem(obj[[1]][j], language = "english")
        obj[[1]][j] = wordStem(obj[[1]][j], language = "italian")
        obj[[1]][j] = wordStem(obj[[1]][j], language = "spanish")
        obj[[1]][j] = wordStem(obj[[1]][j], language = "danish")
        obj[[1]][j] = wordStem(obj[[1]][j], language = "french")
        obj[[1]][j] = wordStem(obj[[1]][j], language = "german")
      }
      #print(obj[[1]])
      for (k in 1:length(obj[[1]]))
      {
        if (obj[[1]][k]!= "")
        {
          position=pmatch(obj[[1]][k], dictionary)
          #print(position[[1]])
          #print("ok")
          if (is.na(position[[1]])==FALSE)
          {
            #print(length(dictionary))
            #print(ncol(mat))
            mat[i,position[[1]]]=1
          }
        }
      }
      
      index=index+1
    }
    
  }
  else if (type == 1) # calcolo di domdef
  {
    
    
    mat = matrix(0, nrow=(max-min +1), ncol=length(domain))
    #if(min==1)->corretto che ci sia +1 nel primo for
    #altrimenti 
    
    index = 0
    for (i in 1:(max-min+1))
    {
      obj = as.matrix(as.character(ins.stima[min +index,8]))#datidef
      for (k in 1:nrow(obj))
      {
        
        mittenti=pmatch(obj[k,], domain)
        #print(mittenti[[1]])
        #print("ok")
        if (is.na(mittenti[[1]])==FALSE)
        {
          #print(length(domain)) #domain è lungo 189
          #print(ncol(mat))
          mat[i,mittenti]=1
        }
      }
      index= index + 1
    }
    
  }
  else if (type == 2) # calcolo motivation
  {
    
    mat = matrix(0, nrow=(max-min +1), ncol=length(motivation))
    #if(min==1)->corretto che ci sia +1 nel primo for
    #altrimenti 
    
    index = 0
    for (i in 1:(max-min+1))
    {
      obj = as.matrix(as.character(ins.stima[min +index,13]))#datidef
      for (k in 1:nrow(obj))
      {
        
        motivazione=pmatch(obj[k,], motivation)
        #print(mittenti[[1]])
        #print("ok")
        if (is.na(motivazione[[1]])==FALSE)
        {
          #print(length(domain)) #domain è lungo 189
          #print(ncol(mat))
          mat[i,motivazione]=1
        }
      }
      index= index + 1
    }
    
  }
  return(mat)
}



###########################################################

#SVM ONLINE

#LINK UTILE:
#https://cran.r-project.org/web/packages/kernlab/vignettes/kernlab.pdf    ->LINK PAPAER

#https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/onlearn     ->SPIEGAZIONE COMANDO ONLEARN

#https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/inlearn     ->SPIEGAZIONE COMANDO INLEARN


################################################################


col = length(dictionary)+length(domain)+length(motivation)+5
model <- inlearn(col,kernel="rbfdot",kpar=list(sigma=0.2),type="classification")
div = 11
last = FALSE
max_part = as.integer(nrow(ins.stima)/div)
if (nrow(ins.stima)!=div*max_part)
{
  max_part = max_part+1
}
if (max_part > 1)
{
  for (i in 1:max_part)
  {
    if (i == 1)
    {
      min = 1
      max = div
    }
    else
    {
      min = i*div+1
      if(min > nrow(ins.stima))
        break
      max = (i+1)*div
      if(max > nrow(ins.stima))
        max = nrow(ins.stima)
      
      print(min)
      print(max)
    }
    matrice_svm=cbind(svm_data(0, dictionary, min,max),
                      svm_data(1,domain, min,max),
                      svm_data(2,motivation,min,max),
                      as.numeric(ins.stima[min:max,4]),
                      as.numeric(ins.stima[min:max,11]),
                      as.numeric(ins.stima[min:max,14]),
                      as.numeric(ins.stima[min:max,15]),
                      as.numeric(ins.stima[min:max,16])) #uniamo tutte le matrici e le colonne dei predittori da dare in pasto a svm
    # #fascia          #internal       #sbloccata        #sent           #nchars
    
    model <- onlearn(model,matrice_svm,y,nu=0.03,lambda=0.1) #da modificare nella verifica
    
  }
} else
{
  model <- onlearn(model,matrice_svm,y,nu=0.03,lambda=0.1) 
  sign(predict(model,matrice_svm))#*****************
}



#to do: per calcolo dei valori previsi nell'insieme di verifica copiare il codice qui sopra usando ins.verifica e al posto della riga contrassegnata metterci ******


# calcolo delle statistiche


# Codice from 
https://www.r-bloggers.com/support-vector-machine-simplified-using-r/
  
  
  # Predict Target Label
  valX <-svm.validate[,4:61]
pred <- predict(svm.tune, valX, type=”prob”)[2]

# Model Performance Statistics
pred_val <-prediction(pred[,2], svm.validate$Class)

# Calculating Area under Curve
perf_val <- performance(pred_val,”auc”)
perf_val

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, “tpr”, “fpr”)

# Plot the ROC curve
plot(perf_val, col = “green”, lwd = 1.5)

#Calculating KS statistics
ks <- max(attr(perf_val, “y.values”)[[1]] – (attr(perf_val, “x.values”)[[1]]))
ks


##################################################################
#
#
#
#   SVM: TODO 
#   end
#
#
##################################################################



##################################################################
# #                     FREQUENZA PAROLE NELL'OGGETTO
# ##################################################################
# 
#freq_obj = colSums(as.matrix(objdef)) #sarebbe da lanciare su dtm
# 
# #Next, we sort this in descending order to get to know the terms with the highest frequency, as follows:
# ord_obj = sort(freq_obj,decreasing=T)
# top_six=(head(ord_obj)/sum(ord_obj))
# 
# wf = data.frame(word=names(ord_obj), freq=ord_obj)
# p = ggplot(subset(wf, freq>50), aes(word, freq)) #♣prendiamo quelle con freq>50
# p = p + geom_bar(stat="identity",color="darkblue", fill="lightblue") 
# p = p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
# 
# 
# #Word Cloud
# set.seed(123)
# 
# wordcloud(names(ord_obj), ord_obj, max.words=50,colors=brewer.pal(6,"Dark2"), random.order=TRUE)
# #piu' scenografico. Occhio al random order 8ogni volta cambia l'ordine
# 
# ##################################################################
# #                     FREQUENZA SENDER
# ##################################################################
# 
# freq_send = colSums(sendomdef)
# 
# wf1 = data.frame(word=names(freq_send), freq=freq_send)
# p1 = ggplot(subset(wf1, freq>5), aes(word, freq)) 
# p1 = p1 + geom_bar(stat="identity",color="darkblue", fill="lightblue") 
# p1 = p1 + theme(axis.text.x=element_text(angle=45, hjust=1)) 
# p1
# 
# #Word Cloud
# set.seed(123)
# wordcloud(names(freq_send), freq_send, max.words=50,colors=brewer.pal(6,"Dark2"), random.order=TRUE)

