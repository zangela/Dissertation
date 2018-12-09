
rm(list=ls())

#----------------------------------CARICAMENTO DATI-------------------------------------------
dati<-read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = ".", encoding = "UTF-8")
#summary(dati)

#----------------------------------SISTEMAZIONE DATASET-------------------------------------------
column=ncol(dati)+7 #5
datidef = matrix(0,nrow=nrow(dati), ncol=column) #ora creiamo la matrice con cui lavoreremo in seguito
colnames(datidef)<-c("Mese", "Giorno","Ora","Fascia","Y","Y_cod","NameSender","DomainSender","NameReceiver","DomainReceiver","Internal","Obj","Motivation","Spiegazione", "Sbloccata")

for (i in 1:nrow(dati))
{
  index =1
  for (j in 1:ncol(dati))
  {
    if(j==1) #data e ora
    {
      object = dati[i,j]
      tmp<-strsplit(toString(object), "-")
      
      foo <-strsplit(toString(tmp[[1]][1]), ":")
      foo1 <-strsplit(toString(tmp[[1]][2]), ":")
      datidef[i,index]=as.double(foo[[1]][2])
      index = index+1
      datidef[i,index]=as.double(foo[[1]][3])
      index = index+1
      datidef[i,index]=as.double(foo1[[1]][1])
      
      if ((as.numeric(datidef[i,index]) < 18)&&(as.numeric(datidef[i,index]) >7))
      {
        index = index+1
        datidef[i,index]=1
        index = index+1
      }
      else
      {
        index = index+1
        datidef[i,index]=0
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
      else
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
      tmp<-strsplit(toString(object), "@")
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
for (i in 1:nrow(datidef))
{
  rn[i]=i
}
rownames(datidef)<-rn
#creare un ciclo per raggruppare in fasce orarie le ore 
#una volta reso "ore" numerico



#----------------------------------ANALISI DESCRITTIVE-------------------------------------------------
#ANALISI SULLA DISTRIBUZIONE DI Y CODIFICATA COME NUMERICA

freqass_y<-table(datidef[,6]) #calcolo freq assolute
freqrel<-as.numeric(freqass_y/sum(freqass_y)) #calcolo freq relative

barplot(table(datidef[,6])/sum(freqass_y), ylab="Frequenze relative", main="Distribuzione tipologie di email",
        ylim=(0:1), col=2:4)

#ANALISI SULLA DISTRIBUZIONE DEI MITTENTI (INTERNI O ESTERNI)

freqass_in<-table(datidef[,11]) #calcolo freq assolute
barplot(table(datidef[,11])/sum(freqass_in), ylab="Frequenze relative", main="Distribuzione tipologie di mittenti",
        ylim=(0:1), col=3:4)

#ANALISI SULLA DISTRIBUZIONE DELLE EMAIIL SBLOCCATE
#devo prendere solo le y_cod=2 e verificare la proporzione di email sbloccate

#calcoliamo quanto email in quarantena ci sono e lo salviamo n num_quarantene ????????????
conteggio=table(datidef[,5])
num_quarantene<-conteggio[[2]]

freqass_sb=0
for (i in 1:nrow(datidef))
{
  if ((as.numeric(datidef[i,6])==2) &&(as.numeric(datidef[i,15])==1))
  {
    freqass_sb = freqass_sb+1
  }
}
freqass_sb=freqass_sb/num_quarantene
#ANALISI SULLA DISTRIBUZIONE PER FASCIA ORARIA

#capire quante email vengono mandate nelle diverse fasce orarie; ->fascia dev'essere un fattore!
#capire nella fascia lavorativa (e non) quante email dei tre tipi ci sono

#ANALISI SULLA DISTRIBUZIONE PER MESE

#capire quante email vengono mandate nei diversi mesi ->mese dev'essere un fattore!
#capire nei vari mesi quante email dei tre tipi ci sono (capire se ci sono stati mesi pi� intensi di altri)

#vedere se c'� correlazione tra passate e internal


#----------------------------------TEXT MINING---------------------------------------------------------

#-----------------------------TEXT MINING SULL'OGGETTO-------------------------------------------------

#carico le librerie necessarie
library(tm)
library(lsa)
library(caret)
library(wordcloud)
library(devtools) #installarla se necessario
library (TextWiller)
library(ggplot2) 
require(tau)
#preprocessing

## Numero di caratteri per tweet

nchars= sapply(as.vector(datidef[,12]),nchar) #metto in nchar il numero di caratteri presenti in ciascun oggetto (conta anche gli spazi)
nchars=as.vector(nchars) #creo un vettore con i numeri di caratteri per tweet

boxplot(nchars~datidef[,5],col=2:4)

#0=passate
#1=rigettate
#2=quarantenat.test(nchars~tweets$soggettivo)
#sembrerebbe che le 0 abbiano molta pi? variabilit?
#le 1 abbiano una maggior numero di caratteri, ma piu' ripetitivi


## Gestione emoticons  
datidef[,12]=normalizzaemote(datidef[,12])  #trasforma le emoticon in parole EMOTEGOOD EMOTECRY
length(grep("EMOTE",datidef[,12])) #il numero di emoticons trovate in tot
print(grep("EMOTE",datidef[,12]) )
#problema: se trova una parola che termina per(remin)D: la segnala come emoticons!
#capire quanto è grave la cosa. Se produce risultati poco affidabili


# Normalizzazione del testo
#datidef[,9]=gsub("à","?",datidef[,9])) #dove trovi "…"" ci metti ""

datidef[,12]=normalizzaTesti(datidef[,12],contaStringhe = c("\\?","!","@","#","(\u20AC|euro)","(\\$|dollar)")) 
#Salvo i conteggi delle parole specificate come matrice a parte

conteggi_caratteri=as.data.frame(attributes(datidef[,12])$counts)
#problema: il conteggio risulta pari a zero, strano!

#Alcuni passaggi a mano
#nota: gi? molte parole std sono state eliminate da nomalizzaTesti (non trov ?, a, il, lo,...)
datidef[,12]=removeStopwords(datidef[,12], stopwords = c(itastopwords,"re", "rif", stopwords_nl, stopwords_de, stopwords_fr, stopwords_en)) 
#ritengo che re,rif siano poco utili ai fini dello studio. Inoltre sono due delle parole più frequenti. Per non sballare le statistiche credo sia opportuno toglierle

datidef[,12]=removeNumbers(datidef[,12]) #vale quanto detto per re e rif sopra
#tweets$TEXT <- gsub("( |^)piu( |$)", " più ", tweets$TEXT)

#anali degli n-grammi (vedi codice commentato)
#ricerca n-grammi più frequenti
#require(tau)
 
bigrams <- textcnt(datidef[,12],method="string",n=2L,split="[[:blank:]]")
sort(bigrams,decreasing=TRUE)[1:20]

#TO DO: grafico di sort

# trigrams <- textcnt(datidef[,9],method="string",n=3L,split="[[:blank:]]")
# sort(trigrams,decreasing=TRUE)[1:10]

#se voglio creare degli insiemi di parole dati gli n grammi appena trovati
datidef[,12] <- gsub("assente ufficio", "assente_ufficio", datidef[,12])
datidef[,12] <- gsub("sessione disconnessa", "sessione_disconnessa", datidef[,12])
datidef[,12] <- gsub("purchase order", "purchase_order", datidef[,12])
datidef[,12] <- gsub("ordine acquisto", "ordine_acquisto", datidef[,12])#etc


## Crea Document Term Matrix 
#(= una riga per tweet, una colonna per ogni parola)
corpus <- Corpus(VectorSource(datidef[,12]))
#data(itastopwords)
#Elenco di parole aggiuntive caricate con TextWiller
#senza fare stemming

#se facciamo stemming
dtm <- as.matrix(DocumentTermMatrix(corpus
                                    , control = list( stemming = TRUE, stopwords = itastopwords,
                                                      minWordLength = 2, removeNumbers = TRUE,
                                                      removePunctuation = FALSE, bounds=list(local = c(1,Inf)) ))
) 
library(SnowballC)
coln=colnames(dtm)
coln= wordStem(coln, language = "english")
coln= wordStem(coln, language = "italian")
coln= wordStem(coln, language = "spanish")
coln= wordStem(coln, language = "danish")
coln= wordStem(coln, language = "french")
coln= wordStem(coln, language = "german")
colnames(dtm)<-coln

##################################################################
#
#
#
#
#creo la matrice finale degli oggetti che conta la presenza delle singole parole stemmate
#
#
#
#
##################################################################

len = 0
idx = 1
colnfin = coln
#conto il numero di parole diverse in tutti gli oggetti
for (i in 2:length(coln))
{
  if (coln[idx]!=coln[i])
  {
    len = len +1
    idx = idx + 1 
  }
}
#creo un vettore per memorizzare tutte le parole diverse (serve per colnames) e la matrice objdef finale
word=character(len)
objdef = matrix(0,nrow=nrow(dtm),ncol=len)
#inserisco tutte le parole diverse in oarine alfabetico in word
for (i in 1:len) 
{
  word[i]= colnfin[i]
}
#creo la matrice finale
for (i in 1:length(word))
{
  for (j in 1:ncol(dtm))
  {
    if (word[i]==coln[j])
    {
      objdef[,i]<-objdef[,i]+dtm[,j]
    }
  }
}
#se avevo pi� colonne uguali la presenza � stata sommata, devo portare tutti in termini di 0,1
for (i in 1:nrow(objdef))
{
  for (j in 1:ncol(objdef))
  {
    if (objdef[i,j]>1)
    {
      objdef[i,j]=1
    }
  }
}
colnames(objdef)<-word


#OBJDEF MATRICE CON VALORI DI PRESENZA PER L'OGGETTO


##################################################################
#
#
#
#
#creo la matrice finale dei domini dei sender che conta la presenza dei singoli domini
#
#
#
#
##################################################################
#ottengo tutti i dominii
len = 0
alldom<-datidef[,8]
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
#creo un vettore che contiene tutti i dominii iversi (serve per colnames) e la matrice sendomdef finale
senderdom= character(len)
for (i in 1:len)
{
  senderdom[i]=tmpdom[i]
}
sendomdef = matrix(0, nrow=nrow(datidef), ncol=len)
#creo la matrice finale
for (i in 1:nrow(sendomdef))
{
  for (j in 1:length(senderdom))
  {
    if (datidef[i,8]== senderdom[j])
    {
      sendomdef[i,j]= 1
      j = length(senderdom)+1
    }
  }
}
colnames(sendomdef)<-senderdom


#SENDOMDEF MATRICE DI PRESENZA PER I DOMINII DEI SENDERS


#which(colSums(dtm)==0) non ci sono colonne tutte di 0

#Avremo una matrice molto sparsa
#To get the total frequency of words in the whole corpus, we can sum the values in a row, as follows:

freq_obj <- colSums(as.matrix(dtm))

#For that, we have to first transform the DTM into a matrix, and then sum up the rows to give a single value for each column.
#Next, we sort this in descending order to get to know the terms with the highest frequency, as follows:

ord_obj <- sort(freq_obj,decreasing=T)
head(ord_obj)

# Assegna sentiment ai tw
#lo useremo come predittore in futuro
sent=sentiment(datidef[,12]) #"positivo" (+1), "negativo" (-1),  "neutro" (0)
datidef=cbind(datidef,sent)
#occhio che facendo così crea una prima colonna (col 0 che non c'entra nulla)
#il problema delle emoticons qui fa sbagliare qualche sent a mio avviso

prop.table(table(datidef[,16],exclude = NULL)) #ci da la proporzione di sent
barplot(table(datidef[,16]),col=2:4)

#----------------ANALISI GRAFICA: parole piu' frequenti nell'oggetto
library(ggplot2) 
wf <- data.frame(word=names(freq_obj), freq=freq_obj)
p <- ggplot(subset(wf, freq>50), aes(word, freq)) #♣prendiamo quelle con freq>50
p <- p + geom_bar(stat="identity",color="darkblue", fill="lightblue") 
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
p
#la parola piu' utilizzata e' rif, che appare quando si risponde alle email. Si potrebbe togliere

#Word Cloud
#Word Cloud is another way of representing the frequency of terms in a document. Here, the size of a word indicates its frequency in the document corpus.

#For a Word Cloud for the 50 words that occur most often, use the command given below:
set.seed(123)

wordcloud(names(freq_obj), freq_obj, max.words=50,colors=brewer.pal(6,"Dark2"), random.order=TRUE)
#piu' scenografico. Occhio al random order 8ogni volta cambia l'ordine

