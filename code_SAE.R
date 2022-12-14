##########################################
#    SAE: Prevision et Modelisation de   #
#             donn?es temporelles        #
##########################################

#Instalation des packages
install.packages("forecast")
install.packages("fpp2")
install.packages("readxl")
library("forecast") #à installer lors de la première utilisation
library("fpp2") #à installer lors de la première utilisation
library("readxl")
#Recuperation des donnees
data_mensuelles <- read_excel("DATA/data_mensuelles.xlsx")
data_tot <- read_excel("DATA/data_uni.xlsx")
View(data_mensuelles)
annee<-c(2011,2012,2013,2014,2015,2016,2017,2018,2019)
mois<-c(12,11,10,9,8,7,6,5,4,3,2,1)

#donne de test
" 
annee<-c(2020,2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018)
mois<-c(1,12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
val_y<-c(13.2,18.8, 13.9, 15.5, 16.4, 27.1, 20.6, 19.5, 15.8, 18.2, 14.3, 14.1, 13.4, 18.4, 13.4, 15.6, 17.4, 27, 22, 16.4, 19.9, 17.3, 14.1, 13, 14.5)
"


#Creation de la courbe#
plot(mois,data_mensuelles$an_2011,type="l",col="blue",ylab="Nombre de touristes(millions)",xlab="Mois",main = "Evolution de la frequentation touristique entre 2011 et 2019")
lines(mois,data_mensuelles$an_2012,col="red")
lines(mois,data_mensuelles$an_2013,col="green")
lines(mois,data_mensuelles$an_2014,col="yellow")
lines(mois,data_mensuelles$an_2015,col="purple")
lines(mois,data_mensuelles$an_2016,col="cyan")
lines(mois,data_mensuelles$an_2017,col="orange")
lines(mois,data_mensuelles$an_2018,col="pink")
lines(mois,data_mensuelles$an_2019,col="brown")
legend(x="topright", legend=c("2011","2012","2013","2014","2015","2016","2017","2018","2019"),col=c("blue","red","green","yellow","purple","cyan","orange","pink","brown"), lty=1:1)

#Calcul moyenne mobile
p<-12
y<-data_tot$val
T<-length(y)
k<-p/2
ti<-k+1
tf<-T-k
mm<-rep(0,(tf-ti+1))
mm[1]<-(y[(ti-k)]/2 + sum(y[(ti-k+1):(ti+k-1)]) + y[(ti+k)]/2)/p
for (kk in 2 : (tf-ti+1)){
  mm[kk]<- mm[(kk-1)]-y[(ti+kk-2-k)]/(2*p)-y[(ti+kk-2-k+1)]/(2*p)+y[(ti+kk-2+k)]/(2*p)+y[(ti+kk-2+k+1)]/(2*p)
}
moyenne_mobile<-mm
moyenne_mobile


#Calcul des coefficients saisonniers
ST<-rep(0,(tf-ti+1))
for(i in 7:tf-ti+1)
{
  ST[i]<-y[i]-moyenne_mobile[i]
}
ST
length(ST)
ymed<-rep(0,12)
ymed[1]<-0
for(j in 1:12){
  cpt<-0
  for (i in seq(j,length(ST),by=12)){
    ymed[j]<-ymed[j]+ST[i]
    cpt<-cpt+1
  }
  ymed[j]<-ymed[j]/cpt
}
ymed

#calcul de l'effet saisonnier moyen
effet_saisonnier_moyen<-0
for (i in 1:length(ymed)) {
  effet_saisonnier_moyen<- effet_saisonnier_moyen + ymed[i]
}
effet_saisonnier_moyen<-effet_saisonnier_moyen
effet_saisonnier_moyen

#ycvs
ycvs<-rep(0,length(y))
cpt<-0
for (i in 1:length(y)) {
  cpt<-cpt+1
  ycvs[i]<-y[i]-ymed[cpt]
  if(cpt==12){cpt<-0}
}
ycvs
#graph moyenne corriger
plot(y,type="l", ylab="Nombre de touristes(millions)",xlab="Periode Etudiée",main = "Evolution de la frequentation touristique entre 2011 et 2019")
lines(ycvs,col="red")
legend(x="bottomleft",legend=c("donnees","ycvs"),col=c("black","red"),lty=1:1)

#Lissage Exponentiel

a<-0.9
serie<-ycvs

fitLES <- ets(serie,model="ANN")
predict<-forecast(fitLES,h=12)


m<-rep(serie,length(serie))
m[1]<-for(i in 2:length(serie)) {m[i]=a*serie[i]+(1-a)*m[i-1]}
m

plot(y,type="l")
lines(m,col="green")
lines(ycvs,col="red")
legend(x="bottomleft",legend=c("donnees","lissage exponnentiel","ycvs","predict"),col=c("black","green","red","blue"),lty=1:1)


plot(predict)
#Etude des residus 

T<-1:108
length(ycvs)
modele<-lm(ycvs ~ T)
modele$coefficients
ychap<-modele$fitted.values
ei<-modele$residuals

plot(ychap,ei)


