# Leo M�hlenweg 503247
#Meike Rudolph 505939

#Packages####
library(usethis)
?use_github
library(tidyverse)
library(AER)
library(readr)
library(gridExtra)
install.packages("~/R/EmpiricalEconomics_0.1.0.tar.gz", repos = NULL, type = "source")
library(EmpiricalEconomics)

use_github(protocol = "https",auth_token=Sys.getenv("GITHUB_PATH"))
rm(list=ls())
#1. Laser printers####

#Data
laser<-read_delim("Daten/laser.csv",delim = ",")


#Regression of price on cost

regr<-lm(price~cost,data = laser)

summary(regr,vcov=vcovHC)

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

edit_r_environ()

# Die Regression kann wie folgt interpretiert werden:
# Es besteht eine negative Korrelation zwischen Benutzungskosten (BK) pro Seite und dem Anschaffungspreis (AP).
# Hieraus l�sst sich allerdings noch kein direkter kausaler Zusammenhang  ableiten.
# Insbesondere die Richtung eines m�glichen kausalen Effekts ist unklar.
# Des Weiteren besteht ein m�glicher ommitted variable bias. 

#Intercept: 1249.46
#Slope: -33653.63

# Es bietet sich nicht an den Intercept zu interpretieren, da dieser nicht innerhalb des Datenbereichs liegt
# Theoretisch w�re dieser der anzunehmende Preis eines Druckers, dessen cost=0 w�ren
# Der Steigungsparameter l�sst sich wie folgt interpretieren:
# Wenn die BK um einen Euro steigen, dann sinkt der AP um 33653.63???.
# Beziehungsweise, wenn die Betriebskosten um einen Cent steigen, dann sinkt der AP um 336.5363???
# Dieser Zusammenhang ist wie gesagt nicht ohne Weiteres kausal zu interpretieren


#b
#Scatterplots

p1<-laser%>%ggplot(aes(cost,price))+geom_point()+theme_classic()
p2<-laser%>%ggplot(aes(log(cost),price))+geom_point()+theme_classic()
p3<-laser%>%ggplot(aes(cost,log(price)))+geom_point()+theme_classic()
p4<-laser%>%ggplot(aes(log(cost),log(price)))+geom_point()+theme_classic()

grid.arrange(p1,p2,p3,p4,nrow=2)

# Bei Betrachtung der Scatterplots scheint der Zusammenhang zwischen log(cost) zund log(price) 
# am ehesten linear zu sein.
# F�r die Sch�tzung eines linearen Zusammenhangs bieten sich folglich die logarithmierten Werte der
# beiden Variablen an.
# Der Steigungsparameter w�re in diesem Fall als Elastizit�t zu interpretieren.


#c

# Wir regressieren zun�chst auf cost und die weiteren erkl�renden Variabeln wie aus der Aufgabe hervorgeht
# in einem zweiten Schritt nutzen wir log(cost) auf Grund der Ergebnisse von b.

regr2<-lm(log(price)~cost+speed+hp+memory+paper+net+duplex, data=laser)

summary(regr2,vcov=vcovHC)

#Interpretation:
#Die Steigungsparameter sind als Semi-Elastizit�ten zu interpretieren:

#Cost
#Der Zusammenhang zwischen Betriebskosten und Anschaffungskosten ist weiterhin signifikant negativ
#Ein Anstieg der Betriebskosten um 1??? h�ngt mit um ca. 1440 % niedrigeren Kosten zusammen.

#speed
#Der Zusammenhang zwischen der Druckgeschwindigkeit von Seiten pro Minute und Anschaffungskosten ist signifikant positiv.
#Ein Anstieg der Druckgeschwindigkeit um eine Seite mehr pro Minute erh�ht den Preis des Druckers um 3,23%.
#Das hei�t je effizienter der Drucker arbeitet, umso h�her ist der Preis.

#hp
#Der Koeffizient hp ist eine Markenvariable und gleichzeitig eine dummy variable.
#Ist der Drucker von der Marke hp, so nimmt die Variable den Wert 1 an und der Steigungsparameter erlangt f�r die Regression von Bedeutung
#Dies l�sst sich wie folgt erl�utern:
#Ist der Drucker von der Marke hp, so steigt der Preis des Druckers um 17,31%.
#Es ist ein signifikant positiver Zusammenhang zwischen der Marke hp und dem Preis des Druckers erkennbar.

#memory
#Der Zusammenhang zwischen dem Speicherplatz und dem Preis des Druckers ist signifikant positiv.
#Steigt der Speicherplatz um 1 MB so erh�ht sich der Preis des Druckers um 0,2526 %.

#paper
#Der Zusammenhang zwischen der Papierkapazit�t des Papierfachs und dem Preis des Druckers ist signifikant positiv.
#Steigt die Papierkapazit�t des Papierfachs um 1 Blatt Papier, so erh�ht dies den Preis des Druckers um 0,06217 %.

#net
#Der Koeffizient net ist ebenfalls eine dummy variable.
#Kann der Drucker eine Netzwerkverbindung aufbauen, so nimmt der Koeffizient net den Wert 1 an, sodass der Steigungsparameter relevant wird.
#Besitzt der Drucker folglich diese F�higkeit, so steigt der Preis des Druckers um 16,96 %.
#Es liegt ein signifikant positiver Zusammenhang zwischen net und dem Preis des Druckers vor.

#duplex
#Es besteht ebenfalls ein signifikant positiver Zusammenhang zwischen beidseitig bedrucken und dem Preis des Druckers.
#Besitzt der Drucker die F�higkeit beidseitig zu bedrucken, nimmt die dummy variable duplex den Wert 1 an und der Preis des Druckers steigt um 14,91 %.

#Zusammenfassung der Interpretation Ergebnisse
#Es zeigt sich allgemein, dass je wertiger der Drucker ist, desto h�her ist sein Preis.
# Die Ergebnisse sind mit dem vereinbar, was wir theoretisch erwarten w�rden. 
# Wir sollten dennoch vorsichtig sein mit m�glichen kausalen Interpretationen

#Generell w�re es sinnvoll auf die log-log-Regression zur�ck zu greifen (vgl.regr3), welche die Elastizit�t des Preises im Zusammenhang zu der der Kosten besser verdeutlicht.
#Hier besteht nur noch ein schwach signifikanter Zusammenhang zwischen den Betriebskosten und dem Preis des Druckers.
#Steigen die Betriebskosten um 1% so mindert dies den Preis des Druckers um 0,14 %.



regr3<-lm(log(price)~log(cost)+speed+hp+memory+paper+net+duplex, data=laser)


summary(regr3,vcov=vcovHC)


#d


newv<-data.frame(hp=1,
                 speed=45,
                 memory=200,
                 paper=500,
                 cost=0.01,
                 net=1,
                 duplex=0)

log_price<-predict(regr2,newv)

log_price%>%exp()   

#Wir w�rden einen log-price von 7.35 erwarten. Der erwarte Preis w�re folglich 1553,66???


  

#Wir nutzen einen T-test 
#zumal die Funktion linearHypothesis den folgen Error ausgibt: 
#Fehler in solve.default(vcov.hyp) : 
 # Lapackroutine dgesv: System ist genau singul�r: U[1,1] = 0

linearHypothesis(regr2,c("hp=0.2"),vcov=vcovHC)

# Bei der Funktion linearHypothesis wird ein F-Test durchgef�hrt, was nicht ganz ideal ist, da wir nur einen Parameter testen
# Da wir einen nicht zu kleinen Datensatz haben und das Ergebnis eindeutig ist, sollte es allerdings kein Problem darstellen
# Wir lehnen die Hypothese, dass der Steigungsparameter von HP gleich 0.2 ist auf keinem sinnvollen Signifikanzniveau ab
# Die Daten widersprechen folglich nicht der Aussage, dass ein Drucker von HP c.p. 20% teurer ist

#T-test Funktion
t_test <- function(regression, coeficient, q = 0, homoskedasticity = F, significance = 0.05, typ = c("beidseitig", "rechtsseitig", "linksseitig")) {
  if (homoskedasticity == T) {
    coef <- regression %>%
      summary() %>%
      coef()
    r <- coef[coeficient, 1]
    se <- coef[coeficient, 2]
    names(r) <- paste("Estimate", coeficient)
    print(r)
    names(se) <- "Standard error"
    print(se)
    t <- (coef[coeficient, 1] - q) / coef[coeficient, 2]
    names(t) <- "t"
    print(t)
  } else if (homoskedasticity == F) {
    coefHC <- regression %>% coeftest(vcov = vcovHC)
    r <- coefHC[coeficient, 1]
    se <- coefHC[coeficient, 2]
    names(r) <- paste("Estimate", coeficient)
    print(r)
    names(se) <- "Standard error"
    print(se)
    t <- (coefHC[coeficient, 1] - q) / coefHC[coeficient, 2]
    names(t) <- "t"
    print(t)
  }
  
  
  v <- nrow(regression$model) - length(regression$coefficients) + 1 # Freiheitsgrade = Anzahl Beobachtungen - Anzahl Steigungsparameter +1
  
  if (typ == "beidseitig") {
    tw <- qt(1 - significance / 2, df = v)
    if (abs(t) > tw) {
      return(
        paste("Die Hypothese", coeficient, "=", q, "wird auf dem Signifikanzniveau", significance, "abgelehnt.")
      )
    } else if (abs(t) <= tw) {
      return(
        paste("Die Hypothese", coeficient, "=", q, "wird auf dem Signifikanzniveau", significance, "nicht abgelehnt.")
      )
    }
  }
  
  if (typ == "linksseitig") {
    tw <- qt(1 - significance, df = v)
    if (t < (-tw)) {
      return(
        paste("Die Hypothese", coeficient, ">=", q, "wird auf dem Signifikanzniveau", significance, "abgelehnt.")
      )
    } else if (t >= (-tw)) {
      return(
        paste("Die Hypothese", coeficient, ">=", q, "wird auf dem Signifikanzniveau", significance, "nicht abgelehnt.")
      )
    }
  }
  
  if (typ == "rechtsseitig") {
    tw <- qt(1 - significance, df = v)
    if (t > tw) {
      return(
        paste("Die Hypothese", coeficient, "<=", q, "wird auf dem Signifikanzniveau", significance, "abgelehnt.")
      )
    } else if (t <= tw) {
      return(
        paste("Die Hypothese", coeficient, "<=", q, "wird auf dem Signifikanzniveau", significance, "nicht abgelehnt.")
      )
    }
  }
}                    


regr2%>%t_test("hp",q=0.2,homoskedasticity = F,significance = 0.05,typ = "beidseitig")

# Auch mit einem T-Test erhalten wir das gleiche Ergebnis


# Wir testen die Null-Hypothese, dass speed=0 und memory=0
# Lehnen wir diese Hypothese ab, so k�nnen wir mit gro�er Sicherheit davon ausgehen,
# dass mindestens einer der beiden Koeffizientn ungleich null ist.
linearHypothesis(regr2,c("speed=0","memory=0"),vcov=vcovHC)

#Wir lehnen die Hypothese auf jedem �blichen Signifikanzniveau ab
# Die Koeffizienten sind folglich gemeinsam signifikant

#AfD voters####
rm(list=ls())

afd<-read_delim("Daten/afd.csv",delim = ",")
glimpse(afd)

p1<-afd%>%ggplot(aes(afd))+geom_histogram(fill="skyblue",color="Black")+theme_classic()
p1<-p1+labs(x="Stimmanteil AfD",y="Anzahl", title = "Histogram des Stimmanteils der AfD")
p1

regr1<-lm(log(afd)~log(auslaend),data = afd)
summary(regr1,vcov=vcovHC)

#Interpretation

afd%>%ggplot(aes(log(auslaend),log(afd)))+geom_point()+geom_smooth(method = lm,se=F)+theme_classic()


#d

regr2<-lm(log(afd)~log(auslaend)+log(akadem)+log(einkommen)+west,data=afd)
summary(regr2,vcov=vcovHC)















