#indagine sui bilanci delle famiglie italiane (2014)

library(ISLR2)
library(PRROC)
library(ROCR)
library(lmtest)
library(cluster)

load("DatiProgetto.rda")
str(m4)
head(m4)

# Seleziono solo colonne senza NA
dati_senza_na = m4[, colSums(is.na(m4)) == 0]
nomi_variabili_pulite = names(m4)[colSums(is.na(m4)) == 0]
print(nomi_variabili_pulite)
#TARGET YTP1 (PENSIONE)
#VAR X che abbiamo scelto: SEX, AREA3, STUDIO, ACOM4C,CFRED(CAPOFAMIGLIA),ANASC(ANNO DI NASCITA)
var_scelte = c("YTP1", "sex", "AREA3", "studio", "ACOM4C", "CFRED", "anasc")
dataset_progetto = dati_senza_na[, var_scelte]
str(dataset_progetto)
head(dataset_progetto)
#le numeriche sono ytp1 e anasc ma le altre sono categoriche quindi fare summary per loro non ha senso
#facciamo summary e deviaz standard per YTP1 e anasc
summary(dataset_progetto$YTP1)
sd_pensione = sd(dataset_progetto$YTP1)
print(sd_pensione)
# gran parte degli individui nel dataset hanno YTP1=0 quindi non hanno una pensione
#il fatto che alcuni abbiano YTP1 positivo  fa aumentare la media e il massimo.
summary(dataset_progetto$anasc)
sd_eta= sd(dataset_progetto$anasc)
print(sd_eta)
#per le categoriche/dicotom possiamo fare le frequenze
frequenze_assolute_sex = table(dataset_progetto$sex)
frequenze_relative_sex = frequenze_assolute_sex/sum(frequenze_assolute_sex)
frequenze_percentuali_sex = (frequenze_relative_sex * 100)

frequenze_assolute_AREA3 = table(dataset_progetto$AREA3)
frequenze_relative_AREA3 = frequenze_assolute_AREA3/sum(frequenze_assolute_AREA3)
frequenze_percentuali_AREA3 = (frequenze_relative_AREA3 * 100)

frequenze_assolute_studio = table(dataset_progetto$studio)
frequenze_relative_studio= frequenze_assolute_studio/sum(frequenze_assolute_studio)
frequenze_percentuali_studio = (frequenze_relative_studio * 100)

frequenze_assolute_ACOM4C = table(dataset_progetto$ACOM4C)
frequenze_relative_ACOM4C = frequenze_assolute_ACOM4C/sum(frequenze_assolute_ACOM4C)
frequenze_percentuali_ACOM4C = (frequenze_relative_ACOM4C * 100)

frequenze_assolute_CFRED = table(dataset_progetto$CFRED)
frequenze_relative_CFRED = frequenze_assolute_CFRED/sum(frequenze_assolute_CFRED)
frequenze_percentuali_CFRED = (frequenze_relative_CFRED * 100)
tabella_di_contingenza_AREA3_e_studio = table(dataset_progetto$AREA3, dataset_progetto$studio)
print(tabella_di_contingenza_AREA3_e_studio)
#osserviamo graficamente
barplot(table(dataset_progetto$sex), main = "Distribuzione per sesso",
        col = "orange")
barplot(table(dataset_progetto$AREA3),main = "Distribuzione per area geografica di residenza",
        col = "lightgreen")
barplot(table(dataset_progetto$studio),main = "Distribuzione per livello di studio",
        col = "lightpink")
barplot(table(dataset_progetto$ACOM4C), main = "Distribuzione per ampiezza demografica del comune di residenza",
        col ="blue")
barplot(table(dataset_progetto$CFRED), main = "Distribuzione per essere o meno capo famiglia",
        col = "coral")
# Istogrammi per le variabili pensione ed anno di nascita
options(scipen = 999)# penalità notazione scientifica molto alta altrimenti ci da i valori che non si leggono con notazione scientifica
hist(dataset_progetto$YTP1, main = "Distribuzione dei redditi pensionistici" , xlab = "Ammontare del reddito pensionistico in (€)",
     col = "magenta", border = "black")
media_YTP1 = mean(dataset_progetto$YTP1)
mediana_YTP1 = median(dataset_progetto$YTP1)
print(media_YTP1)
print(mediana_YTP1)
abline(v=media_YTP1, col="darkorange", lwd=4, lty=2) # linea arancione tratteggiata per la media
abline(v=mediana_YTP1, col="black", lwd=4, lty=2) #lwd spessore lty che tipo di linea

pensionati = subset(dataset_progetto, YTP1 > 0)

hist(pensionati$YTP1, main = "Distribuzione dei redditi pensionistici per chi ha pensione" , xlab = "Ammontare del reddito pensionistico in (€) per chi ha YTP1  > 0",
     col = "lightgrey", border = "black")
media_pensionati = mean(pensionati$YTP1)
mediana_pensionati = median(pensionati$YTP1)
print(media_pensionati)
print(mediana_pensionati)

hist(dataset_progetto$anasc, main = "Distribuzione degli anni di nascita", xlab = "Anno di nascita", col = "green", border = "black")
media_anasc = mean(dataset_progetto$anasc)
mediana_anasc = median(dataset_progetto$anasc)
print(media_anasc)
print(mediana_anasc)
abline(v=media_anasc, col="darkorange", lwd=4, lty=2)
abline(v=mediana_anasc, col="black", lwd=4, lty=2)

# Boxplot 

boxplot(dataset_progetto$anasc , main = "boxplot anno di nascita", col= "purple", ylab = "anno di nascita")
boxplot(dataset_progetto$YTP1 , main = "boxplot reddito pensionistico generale",
        col= "lightyellow", ylab = "(YTP1 in €)")
boxplot(pensionati$YTP1 , main = "boxplot reddito pensionistico per chi ha YTP1  > 0 ",
        col= "brown", ylab = "(YTP1 in €)")

boxplot(YTP1 ~ AREA3, data = pensionati, main="boxplot rispetto all'area geografica di residenza per chi ha YTP1 > 0",
        col= "gold", ylab = "(YTP1 in €)")
boxplot(YTP1 ~ sex, data= pensionati, main="boxplot rispetto al sesso per chi ha YTP1 > 0",
        col="coral", ylab="(YTP1 in €)" )
boxplot(YTP1 ~ studio, data = pensionati,main = "boxplot rispetto al livello di studio per chi ha YTP1 > 0",
        col = "lightblue", ylab = "(YTP1 in €)")
boxplot(YTP1 ~ ACOM4C, data = pensionati,main = "boxplot rispetto all'ampiezza demografica del comune di residenza per chi ha YTP1 > 0",
        col = "darkgreen", ylab = "(YTP1 in €)")
boxplot(YTP1 ~ CFRED, data = pensionati,main = "Boxplot pensione per categoria capo famiglia (YTP1 > 0)",
        col = "green", ylab = "(YTP1 in €)")
boxplot(YTP1 ~ anasc, data = pensionati, main="boxplot rispetto all'anno di nascita di coloro che hanno YTP1 > 0",
        col= "violet", ylab = "(YTP1 in €)")

# Intervallo di confidenza al 95% per verificare di quanto oscilla media reale anno di nascita (dati del 2014) con possibilita errore del 5%
media_anasc = mean(dataset_progetto$anasc)
nOSS_anasc = length(dataset_progetto$anasc)
sd_anasc = sd(dataset_progetto$anasc)
errore_standard_anasc = sd_anasc / sqrt(nOSS_anasc)
t_student = qt(0.975, df = nOSS_anasc - 1)

IC_inferiore_anasc = media_anasc - t_student * errore_standard_anasc
IC_superiore_anasc = media_anasc + t_student * errore_standard_anasc
c(IC_inferiore_anasc, IC_superiore_anasc)
#l'età media nel 2014 in Italia era circa 48 anni con margine di errore 5% (campione di circa 19 mila oss)
# Intervallo al 95% solo per chi ha pensione quindi solo per chi ha YTP1 maggiore di 0
nOSS_pensionati = length(pensionati$YTP1)
media_pensionati = mean(pensionati$YTP1)
sd_pensionati =sd(pensionati$YTP1)
errore_standard_pensionati = sd_pensionati / sqrt(nOSS_pensionati)
t_student_pensionati = qt(0.975, df = nOSS_pensionati - 1)
IC_inferiore_pensionati = media_pensionati - t_student_pensionati * errore_standard_pensionati
IC_superiore_pensionati = media_pensionati + t_student_pensionati * errore_standard_pensionati
print(c(IC_inferiore_pensionati, IC_superiore_pensionati))
#con il 95% di confidenza la vera media della pensione nel gruppo dei pensionati è compresa nel 2014 tra circa 18.480 € e 18.900 €.

# Modelli di regressione semplice
#per la regressione semplice utilizziamo il dataset dei soli pensionati per ottenere valori non distorti
pensionati$anasc_Xc = (pensionati$anasc - mean(pensionati$anasc))
regressione_anasc_Xc=lm(YTP1 ~ anasc_Xc, data = pensionati)
summary(regressione_anasc_Xc)

pensionati$sex = factor(pensionati$sex, labels = c("uomo", "donna"))  
pensionati$AREA3 = factor(pensionati$AREA3, labels = c("nord italia", "centro italia", "sud italia"))
pensionati$studio= factor(pensionati$studio, labels = c("Nessuno", "Licenza elementare", "Licenza media",
                                                        "Diploma prof. (3 anni)", "Superiori", "Laurea Triennale",
                                                        "Laurea Magistrale", "Specializzazione post laurea"))
pensionati$ACOM4C= factor(pensionati$ACOM4C, labels = c("da 0 a 20.000 abitanti", "da 20.000 a 40.000 abitanti",
                                                        "da 40.000 a 500.000 abitanti", "Oltre 500.000 abitanti"))
pensionati$CFRED = factor(pensionati$CFRED, labels = c("no capofamiglia", "si capofamiglia"))


regressione_AREA3 = lm(YTP1 ~ AREA3, data = pensionati)
summary(regressione_AREA3)

regressione_sex = lm(YTP1 ~ sex, data = pensionati)
summary(regressione_sex)

regressione_studio = lm(YTP1 ~ studio, data = pensionati)
summary(regressione_studio)

regressione_ACOM4C =lm(YTP1 ~ ACOM4C, data = pensionati)
summary(regressione_ACOM4C)

regressione_CFRED = lm(YTP1 ~ CFRED, data = pensionati)
summary(regressione_CFRED)


regressione_multipla = lm(YTP1 ~ anasc_Xc + sex + AREA3 + studio + ACOM4C + CFRED, data = pensionati)
summary(regressione_multipla)
residui_regmultipla =residuals(regressione_multipla)
plot(residui_regmultipla, main = "Residui della regressione multipla")
abline(h = 0, col = "darkorange", lty = 4)
#NORMALITA'no
plot(regressione_multipla,which = 2)#ma campione molto grande che tende alla normalità
#per la linearità
plot(regressione_multipla,which = 1) #ci sta linearità ma ci potrebbe essere eteroschedasticità
bptest(regressione_multipla) # se p value minore di 0.05 ci sta eteroschedasticità
#PROVO CON LA TRASFORMATA LOGARITMICA
trasformata_logar_regr_multipla = lm(log(YTP1) ~ anasc_Xc + sex + AREA3 + studio + ACOM4C + CFRED, data = pensionati)
summary(trasformata_logar_regr_multipla)
#NORMALITA'
plot(trasformata_logar_regr_multipla,which = 2)
#Linearità e eteroschedasticità
plot(trasformata_logar_regr_multipla,which = 1)
bptest(trasformata_logar_regr_multipla) 
#omoschedasticità? ancora no

#Per la regressione logistica dobbiamo usare il dataset completo di tutte le 19 mila osservazioni
dataset_progetto$anasc_Xc = (dataset_progetto$anasc - mean(dataset_progetto$anasc))
dataset_progetto$sex = factor(dataset_progetto$sex, labels = c("uomo", "donna"))  
dataset_progetto$AREA3 = factor(dataset_progetto$AREA3, labels = c("nord italia", "centro italia", "sud italia"))
dataset_progetto$studio= factor(dataset_progetto$studio, labels = c("Nessuno", "Licenza elementare", "Licenza media",
                                                                    "Diploma prof. (3 anni)", "Superiori", "Laurea Triennale",
                                                                    "Laurea Magistrale", "Specializzazione post laurea"))
dataset_progetto$ACOM4C= factor(dataset_progetto$ACOM4C, labels = c("da 0 a 20.000 abitanti", "da 20.000 a 40.000 abitanti",
                                                                    "da 40.000 a 500.000 abitanti", "Oltre 500.000 abitanti"))
dataset_progetto$CFRED = factor(dataset_progetto$CFRED, labels = c("no capofamiglia", "si capofamiglia"))


dataset_progetto$pensioneSI_oppure_NO = ifelse(dataset_progetto$YTP1 > 0, 1, 0)
regressione_logistica =glm(pensioneSI_oppure_NO ~ anasc_Xc + sex + AREA3 + studio + ACOM4C + CFRED, data = dataset_progetto,
                           family = binomial)
summary(regressione_logistica)
regressione_logistica$coefficients 
#(non vuol dire di quanto aumenta la probabilità, ci serve per capire se aumenta Y=1 oppure aumenta Y=0)
#quanto cambia l’odds di pensione si o no  con un’unità in più per ogni Xn
exp(regressione_logistica$coefficients) #per capire quanto varia

predict.probs = predict(regressione_logistica, type="response")
p.soglia = 0.5 # se la probabilità prevista è almeno 0.5 mettiamo Y= “1”
predict.class = ifelse(predict.probs>=p.soglia,1,0)
boxplot(predict.probs~predict.class, col="pink")

MATRICE_DI_CONFUSIONE= table(valori_predetti = predict.class, valori_osservabili= dataset_progetto$pensioneSI_oppure_NO)
print(MATRICE_DI_CONFUSIONE)
TN = MATRICE_DI_CONFUSIONE[1,1]
TP = MATRICE_DI_CONFUSIONE[2,2]
FN = MATRICE_DI_CONFUSIONE[1,2]
FP = MATRICE_DI_CONFUSIONE[2,1]

sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)
print(sensitivity)
print(specificity)
# creiamo e disegnamo la curva ROC
ROCRpred = prediction(predict.probs, dataset_progetto$pensioneSI_oppure_NO)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7), print.cutoffs.at = seq(0, 1, 0.1))

p.soglia.vec    = seq(from=1, to=0, length.out = 300)
sensitivity.vec = c()
specificity.vec = c()
#ciclo for per 300 soglie  da 1 a 0 per classificare la probabilità predetta come 0 o 1.
for(j in 1:length(p.soglia.vec)) {
  predict.class = ifelse(predict.probs >= p.soglia.vec[j], 1, 0)
  # Aggiungo 0 e 1 perche in una certa soglia potrebbe essere che stima tutti in 1 o tutti in 0 il che darebbe errore
  predict.class = c(0, 1, predict.class)
  predict.class = factor(predict.class, levels = c(0, 1))
  predict.class = predict.class[-c(1, 2)]  # rimuoviamo i due elementi aggiunti
  conf.mat = table(Predetti = predict.class, Osservati = factor(dataset_progetto$pensioneSI_oppure_NO, levels = c(0, 1)))
  TN = conf.mat[1,1]
  TP = conf.mat[2,2]
  FN = conf.mat[1,2]
  FP = conf.mat[2,1]
  
  sensitivity.vec[j] = TP / (TP + FN) #mettiamo tutte le sens e spec in vettori sensitivity.vec e specificity.vec.
  specificity.vec[j] = TN / (TN + FP)
}

plot(1 - specificity.vec, sensitivity.vec, type = "l",
     main = "Curva ROC")
plot(p.soglia.vec,sensitivity.vec,type = "l",col="yellow")
points(p.soglia.vec,specificity.vec,type = "l",col="purple")

res.mat = cbind(sensitivity.vec, specificity.vec, p.soglia.vec)
diff = abs(res.mat[,1] - res.mat[,2])  # differenza minima tra sensibilità e specificità
p.opt = p.soglia.vec[which.min(diff)]
abline(v=p.opt,col="darkorange")

p.soglia = p.opt
print(p.soglia)
predict.class = ifelse(predict.probs >= p.soglia, 1, 0)
conf.mat = table(Predetti = predict.class, Osservati = factor(dataset_progetto$pensioneSI_oppure_NO, levels = c(0, 1)))

TN = (conf.mat[1,1])
TP = (conf.mat[2,2])
FN = conf.mat[1,2]
FP = conf.mat[2,1]
print(TN)
print(TP)
print(FN)
print(FP)
sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)
print(sensitivity)
print(specificity)
#quindi alla fine la soglia ottimale era molto simile a quella standard 0.5 


set.seed(1680)

gower_dist = daisy(pensionati[, -c(1, 8)], metric = "gower") #togliamo sia ytp1 che anascxc mentre lascio anasc

sil.vec = c()
for(i in 2:10){
  pam.mod = pam(gower_dist, diss = TRUE, k = i)
  sil.vec[i] = pam.mod$silinfo$avg.width
}
plot(1:10, sil.vec,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil.vec)

k_opt = which.max(sil.vec)
print(k_opt)
mod.pam = pam(gower_dist, diss = TRUE, k = k_opt)

mod.pam$clustering

for(j in 1:k_opt){
  print(summary(pensionati[, -c(1, 8)][which(mod.pam$clustering == j), ]))
}

pensionati$cluster = factor(mod.pam$clustering)
#cluster analysis senza utilizzare anno di nascita  
set.seed(1680)
gower_dist_senza_anno = daisy(pensionati[, -c(1, 7, 8, 9 )], metric = "gower") 

sil.vec = c()
for(i in 2:10){
  pam.mod_senza_anno = pam(gower_dist_senza_anno, diss = TRUE, k = i)
  sil.vec[i] = pam.mod_senza_anno$silinfo$avg.width
}
plot(1:10, sil.vec,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil.vec)

k_opt_senza_anasc = which.max(sil.vec)
print(k_opt_senza_anasc)
mod.pam_senza_anno = pam(gower_dist_senza_anno, diss = TRUE, k = k_opt_senza_anasc)

mod.pam_senza_anno$clustering

for(j in 1:k_opt_senza_anasc){
  print(summary(pensionati[, -c(1, 7, 8, 9 )][which(mod.pam_senza_anno$clustering == j), ]))
}

pensionati$cluster_senza_anasc= factor(mod.pam_senza_anno$clustering)

pensionati$cluster_diverso= pensionati$cluster != pensionati$cluster_senza_anasc
subset(pensionati, cluster_diverso == TRUE)
#utilizzare anasc non è utile



