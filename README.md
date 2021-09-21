# Predicci-n-de-covid-
En base a información sobre características de los sistemas de salud para 450 regiones, se busca predecir los efectos de las variables sobre la probabilidad de que la región pertenezca al grupo por debajo del promedio de duración de todas las regiones.

#Instalamos librerias
library(readstata13)
library("ggplot2")
library(magrittr)
library("dplyr")
library("broom")
library("openintro")
library("plotly")
library("reshape2")
library("perturb")
library("lmtest")
library("olsrr")
library(tseries) 
library(aod)
library(mfx)
library(ROCR)
library(readr)
library(caret)
library(car)
library(MASS)
library(readstata13)
library(pROC)

#cargamos bases
base1 <- read.csv("C:/Users/rafae/OneDrive/Documentos/ORT/Regresion/Obligatorio/base_Obligatorio_DEAN.csv")
base2 <- read.csv("C:/Users/rafae/OneDrive/Documentos/ORT/Regresion/Obligatorio/base2_Obligatorio_DEAN.csv")

#seteamos escritorio de trabajo
setwd("C:/Users/rafae/OneDrive/Documentos/ORT/Regresion/Obligatorio")

#ponemos variable rank como variable categorica 
base1$rank<-factor(base1$rank) 


# 1a - regresamos con modelo de probabilidad lineal 

mod <- lm(base1$debajo~base1$visitas+base1$camas+base1$rank)
summary(mod)

# 1b - regresamos con modelo logit

modLog = glm(base1$debajo ~ base1$visitas+base1$camas+base1$rank,family=binomial(link="logit"), data=base1)
summary(modLog)

#comparamos con un regresor menos y vemos el AIC
modLog2 = glm(base1$debajo ~ base1$visitas+base1$rank,family=binomial(link="logit"), data=base1)
summary(modLog2)


#Odd ratio de debajo
logitor(base1$debajo~base1$visitas+base1$camas+base1$rank,data=base1)

# 1c - regresamos con modelo probit 

modProb = glm(base1$debajo ~ base1$visitas+base1$camas+base1$rank,family=binomial(link="probit"), data=base1)
summary(modProb)

modProb2 = glm(base1$debajo ~ base1$visitas+base1$rank,family=binomial(link="probit"), data=base1)
summary(modProb2)

#EPnP

efmgEPenPlog<-logitmfx(debajo ~ visitas+camas+rank, data=base1)
efmgEPenPlog$mfxest

efmgEPenPpro<-probitmfx(debajo ~ visitas+camas+rank, data=base1)
efmgEPenPpro$mfxest

tablaefmgsEPenP<-cbind(efmgEPenPlog$mfxest[1:4],efmgEPenPpro$mfxest[1:4])
tablaefmgsEPenP

#EPP promedio de todos los efectos marginales
efmgEPPlog<-logitmfx(debajo ~ visitas+camas+rank, data=base1, atmean = FALSE)
efmgEPPlog$mfxest

efmgEPPpro<-probitmfx(debajo ~ visitas+camas+rank, data=base1, atmean = FALSE)
efmgEPPpro$mfxest

tablaefmgsEPP<-cbind(efmgEPPlog$mfxest[1:4],efmgEPPpro$mfxest[1:4])
tablaefmgsEPP

#Tabla comaprativa EPP, EPenP y Modelo de probabilidad lineal

tablaecomp<-cbind(efmgEPPlog$mfxe[1:4],efmgEPPpro$mfxest[1:4], mod$coefficients[2:6])
tablaecomp

tablaecomp2<-cbind(efmgEPenPlog$mfxest[1:4],efmgEPenPpro$mfxest[1:4], mod$coefficients[2:6])
tablaecomp2

#precisiones de los modelos

#se utilizan los coeficientes de los modelos para predecir los casos exitosos.
#se incluyen nuevas variables en la base1

base1$predinmod = predict(mod, newdata=base1, type="response")
base1$predmodLog = predict(modLog, newdata=base1, type="response")
base1$predmodProb = predict(modProb, newdata=base1, type="response")

inobserv<-factor(base1$debajo, labels = c("No debajo", "Debajo"))

#predicciones con variables categÃ³ricas para las tablas
inclasifmod<-factor(as.numeric(base1$predinmod>=0.5), labels = c("NO","SI"))
inclasifmodLog<-factor(as.numeric(base1$predmodLog>=0.5), labels = c("NO","SI"))
inclasifmodProb<-factor(as.numeric(base1$predmodProb>=0.5), labels = c("NO","SI"))

#a partir de la confusion matrix se calcula la precisión de los modelos.

MPL<-table(inclasifmod, inobserv)
precisiónMPL<-(MPL[1,1]+MPL[2,2])/nrow(base1)
MPL
precisiónMPL

LOG<-table(inclasifmodLog, inobserv)
precisiónLOG<-(LOG[1,1]+LOG[2,2])/nrow(base1)
LOG
precisiónLOG

PRO<-table(inclasifmodProb, inobserv)
precisiónPRO<-(PRO[1,1]+PRO[2,2])/nrow(base1)
PRO
precisiónPRO

#TPR y FPR
MPL #primer objeto en columna (los predichos por el modelo)
sensMPL<-(MPL[2,2]/(MPL[2,2]+MPL[1,2]))
fprMPL<-(MPL[2,1]/(MPL[2,1]+MPL[1,1]))
sensMPL
fprMPL

LOG
sensLOG<-(LOG[2,2]/(LOG[2,2]+LOG[1,2]))
fprLOG<-(LOG[2,1]/(LOG[2,1]+LOG[1,1]))
sensLOG
fprLOG

PRO
sensPRO<-(PRO[2,2]/(PRO[2,2]+PRO[1,2]))
fprPRO<-(PRO[2,1]/(PRO[2,1]+PRO[1,1]))
sensPRO
fprPRO

# MPL curvas ROC y areas bajo las curvas
rocMPL<-roc(base1$debajo, base1$predinmod, plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, show.thres=TRUE)

rocMPL2<-roc(base1$debajo, base1$predinmod, percent = TRUE)

#MPL umbral
#obtener el mejor umbral de acuerdo a algún criterio determinado.

coords(rocMPL2, "best", ret="threshold", transpose = FALSE, best.method="youden") #es el método que se usa por defecto

coords(rocMPL2, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="youden")

coords(rocMPL2, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="closest.topleft")

coords(rocMPL2, "best", ret="all", transpose = TRUE)

#graficar las curvas ROC con los umbrales

plot(rocMPL2, col="blue", print.thres="best", print.thres.best.method="youden", print.thres.col="red", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

plot(rocMPL2, col="blue", print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19)

#si quiero superponer los umbrales

plot(rocMPL2, col="blue", add = TRUE, print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

##

# Probit curvas ROC y areas bajo las curvas
rocLog<-roc(base1$debajo, base1$predmodLog, plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, show.thres=TRUE)

rocLog2<-roc(base1$debajo, base1$predmodLog, percent = TRUE)

#Logit umbral
coords(rocLog2, "best", ret="threshold", transpose = FALSE, best.method="youden") #es el método que se usa por defecto

coords(rocLog2, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="youden")

coords(rocLog2, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="closest.topleft")

coords(rocLog2, "best", ret="all", transpose = TRUE)

#graficar las curvas ROC con los umbrales

plot(rocLog2, col="blue", print.thres="best", print.thres.best.method="youden", print.thres.col="red", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

plot(rocLog2, col="blue", print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

#si quiero superponer los umbrales

plot(rocLog2, col="blue", add = TRUE, print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

###

# Probit curvas ROC y areas bajo las curvas
rocProb<-roc(base1$debajo, base1$predmodProb, plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, show.thres=TRUE)

rocProb2<-roc(base1$debajo, base1$predmodProb, percent = TRUE)

#Probit umbral
coords(rocProb2, "best", ret="threshold", transpose = FALSE, best.method="youden") #es el método que se usa por defecto

coords(rocProb2, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="youden")

coords(rocProb2, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="closest.topleft")

coords(rocProb2, "best", ret="all", transpose = TRUE)

#graficar las curvas ROC con los umbrales

plot(rocProb2, col="blue", print.thres="best", print.thres.best.method="youden", print.thres.col="red", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

plot(rocProb2, col="blue", print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

#si quiero superponer los umbrales

plot(rocProb2, col="blue", add = TRUE, print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19, print.auc=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE)

