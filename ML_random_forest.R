##############################################################################################################
#
##############################################################################################################
load("WS.RData")
library(foreign)
library(reader)
library(tidyverse)
library(dplyr)
library(skimr)
library(readxl)
library(car)
#library(magrittr)
#rm(list=ls())
cat("\014")
##############################################################################################################
# Data cleaning
##############################################################################################################
Hogar<-read.csv("data/CS_HOGARES.csv", header=T,colClasses=c("ENT"="character"))
Residentes<-read.csv("data/CS_RESIDENTES.csv", header=T,colClasses=c("ENT"="character"))
Vivienda<-read.csv("data/CS_VIVIENDAS.csv", header=T,colClasses=c("ENT"="character"))


Poblacion %>% mutate(Obesidad=if_else(Obesidad==1,1,0),
                     Cardiova=if_else(Cardiova==1,1,0),
                     Presiona=if_else(Presiona==1,1,0),
                     Diabetis=if_else(Diabetis<=2,1,0),
                     Fuma=if_else(Fuma<=2,1,0),
                     SEXO=if_else(SEXO==1,1,0),
                     EDAD=as.numeric(EDAD)) ->Poblacion
 
Poblacion$EDAD  <-cut(Poblacion$EDAD,breaks= c(0, 20, 30, 40,50,60,70,80,Inf)) 
Poblacion$Diabetis %>% replace_na(0)->Poblacion$Diabetis 
Poblacion$Obesidad  %>% replace_na(0)->Poblacion$Obesidad 
Poblacion$Cardiova  %>% replace_na(0)->Poblacion$Cardiova 
Poblacion$Presiona  %>% replace_na(0)->Poblacion$Presiona 
Poblacion$Fuma  %>% replace_na(0)->Poblacion$Fuma 
Poblacion %>% drop_na(EDAD)->Poblacion

Poblacion %>% 
        group_by(EDAD) %>% 
        summarise(across(Obesidad:Fuma, mean), n = n())

# Factores
Factores<-c("P1_2", "P1_1", "P1_3", "P1_9","P1_12", "P1_13", "P1_15", "P1_19", "P1_23")
Vivienda<-cbind(as.data.frame(lapply(Vivienda[Factores], factor)),Vivienda %>% select(-Factores)) 
vars<- c(x1="P1_2", 
             x2="P1_1", 
             x3="P1_3", 
             x4="P1_9",
             x5="P1_12", 
             x6="P1_13", 
             x7="P1_15", 
             x8="P1_19", 
             x9="P1_23")
Vivienda %>%select(Factores,VIV_SEL,ENT,ESTRATO, REGION,DOMINIO) %>% rename(!!vars) ->H
rm(vars)

# Dicotomicas
Vivienda %>% group_by(P1_6) %>% tally()
Vivienda %>% mutate(P1_6=if_else(P1_6==2,0,1))

# Continuas
vars<-c("P1_4","P1_5","P2_4")
Vivienda <- Vivienda %>%naniar:: replace_with_na_at(.vars = vars,
                                                    condition = ~.x == 99)


Vivienda %>% group_by(P1_4) %>% tally

Poblacion %>% group_by(VIV_SEL, ENT, Urbano,ESTRATO) %>% tally() %>% ungroup %>%  summarise(mean=mean(n))




