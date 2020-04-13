##############################################################################################################
#
##############################################################################################################
library(foreign)
library(reader)
library(tidyverse)
library(dplyr)
library(skimr)
library(readxl)
rm(list=ls())
cat("\014")
##############################################################################################################

##############################################################################################################
# SSA 2018
##############################################################################################################

curl::curl_download("http://www.dgis.salud.gob.mx/descargas/datosabiertos/recursosSalud/Recursos_Salud_2018.zip",
                    destfile = "recursos.zip")

unzip(zipfile = "recursos.zip")

Recursos <- read_excel("Recursos.xlsx")

Recursos %>%select(`Nombre Estado`,`Nombre Municipio`,`Nombre Localidad`,`Total mÈdicos generales y especialistas`,`Total mÈdicos en formaciÛn`,
                    `Total medicos en otras labores`,`Total enfermeras en contacto con el paciente`,
                    `Total enfermeras en otras labores`,`TOTAL CAMAS AREA HOSPITALIZACI”N`,
                    `TOTAL CAMAS EN OTRAS AREAS (NO CONSIDERA HOSPITALIZACI”N)`,`Clave Estado`,
                    `Clave Municipio`,`Clave Localidad`) %>% 
              mutate(T_Medicos=`Total mÈdicos generales y especialistas`+
                                `Total mÈdicos en formaciÛn`+
                                `Total medicos en otras labores`,
                     T_Enfermeras=`Total enfermeras en contacto con el paciente`+
                                  `Total enfermeras en otras labores`,
                     T_Camas=`TOTAL CAMAS AREA HOSPITALIZACI”N`+
                              `TOTAL CAMAS EN OTRAS AREAS (NO CONSIDERA HOSPITALIZACI”N)`,
                     ENT=if_else(`Clave Estado`>9,as.character(`Clave Estado`),
                                paste0("0",as.character(`Clave Estado`))),
                     MUN=if_else(`Clave Municipio`<10,
                                 paste0("00",as.character(`Clave Municipio`)),
                                 if_else(`Clave Municipio`>99,
                                  as.character(`Clave Municipio`),
                                  paste0("0",as.character(`Clave Municipio`)))),
                     LOC= if_else(`Clave Municipio`<10,
                                          paste0("000",as.character(`Clave Municipio`)),
                          if_else(`Clave Municipio`<99,
                                          paste0("00",as.character(`Clave Municipio`)),
                          if_else(`Clave Municipio`<999,
                                          paste0("0",as.character(`Clave Municipio`)),
                                         as.character(`Clave Municipio`),
                                         ))))%>% rename(Entidad=`Nombre Estado`,
                                                        Municip=`Nombre Municipio`,
                                                        Localid=`Nombre Localidad`) %>%  
            select(T_Medicos,T_Enfermeras,T_Camas,ENT,MUN,LOC,Entidad,Municip,Localid)->Recursos
  
##############################################################################################################
# ENSANUT 2019
##############################################################################################################

Poblacion<-rbind(read.csv("CS_ADULTOS .csv", header=T,colClasses=c("ENT"="character")) %>% 
                   mutate(Pid=paste(VIV_SEL, paste(HOGAR,NUMREN))) %>%
                   rename(Obesidad=P1_1, Cardiova=P5_1, Presiona=P4_1,
                          Diabetis=P3_1, Fuma=P13_2, #1 y 2 si, 8 NA
                          Urbano=DOMINIO, Factor=F_20MAS) %>% 
                   select(Pid,UPM,HOGAR,VIV_SEL,Factor,Urbano,EDAD,SEXO,ENT,ESTRATO,Obesidad,Cardiova,Presiona,Diabetis,Fuma),
                 read_csv("CS_NINO.csv") %>% rename(Urbano=DOMINIO,Factor=F_NINO) %>% 
                   mutate(Pid=paste(VIV_SEL, paste(HOGAR,NUMREN)),Obesidad=NA,Cardiova=NA,Presiona=NA,Diabetis=NA,Fuma=NA) %>%
                   select(Pid,UPM,HOGAR,VIV_SEL,Factor,Urbano,EDAD,SEXO,ENT,ESTRATO,Obesidad,Cardiova,Presiona,Diabetis,Fuma),
                 read_csv("CS_ADOLESCENTES.csv") %>% rename(Urbano=DOMINIO,Factor=F_10A19) %>% 
                   mutate(Pid=paste(VIV_SEL, paste(HOGAR,NUMREN)),Obesidad=NA,Cardiova=NA,Presiona=NA,Diabetis=NA,Fuma=NA) %>%
                   select(Pid,UPM,HOGAR,VIV_SEL,Factor,Urbano,EDAD,SEXO,ENT,ESTRATO,Obesidad,Cardiova,Presiona,Diabetis,Fuma))

##############################################################################################################
# CONTEO 2015
##############################################################################################################


Entidades<-c("01","02","03","04","05","06","07","08","09","10",
                "11","12","13","14","15","16","17","18","19","20",
                "21","22","23","24","25","26","27","28","29","30",
                "31","32")

for (i in 1:length(Entidades)){         
        URL_INEGI<-paste0("https://www.inegi.org.mx/contenidos/programas/intercensal/2015/microdatos/eic2015_",paste0(Entidades[i],"_csv.zip"))
        File<-paste0(Entidades[i],"_csv.zip")
        curl::curl_download(URL_INEGI, destfile =File)
        unzip(zipfile =File)
        file.remove(File)
}

Entidades <- list.files(pattern = "TR_VIVIENDA")

Nation<-data.frame()
for (i in 1:length(Entidades)){         
  State<- read_csv(Entidades[i], col_types = cols(.default = "c"))%>% select(ENT, MUN, LOC50K, SEXO, EDAD,FACTOR,NOM_LOC, ID_VIV)
  Nation<- rbind(Nation,State)
  rm(State)
  #file.remove(Unziped,paste0("TR_VIVIENDA",paste0(Entidades[i],".CSV")))
  }

##############################################################################################################
# CONEVAL 2015
##############################################################################################################

curl::curl_download("https://www.coneval.org.mx/Medicion/Documents/Pobreza_urbana/Base_de_datos/Base_de_datos_de_pobreza_AGEB_segun_entidad_federativa_2015.zip",
                    destfile = "recursos.zip")

unzip(zipfile = "recursos.zip")

Recursos <- read_excel("Base de datos de pobreza AGEB segun entidad federativa 2015.xlsx")

Entidades<-c("Aguascalientes","Baja California  ","Baja California Sur ",
             "Campeche","Coahuila","Colima","Chiapas","Chihuahua",
"Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo",
"Jalisco","México","Michoacán","Morelos","Nayarit","Nuevo León",
"Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí",
"Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz",
"Yucatán","Zacatecas")


AGEB<-data.frame()

for (i in 1:length(Entidades)){         

  AGEB<-rbind(AGEB,read_excel("Base de datos de pobreza AGEB segun entidad federativa 2015.xlsx", 
                              sheet = Entidades[i], col_names = F,
                              skip = 6))
  
}

names(AGEB)<-c("Clave de entidad","Entidad federativa","Clave de municipio","Municipio",	
"Clave de AGEB","Rango de pobreza (%)",	"Rango de pobreza extrema (%)")

rm(i,Entidades)

##############################################################################################################
# GIS
##############################################################################################################

  stime <- system.time({
    curl::curl_download("http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292805_s.zip",
                        destfile = "GIS.zip")
    
    unzip(zipfile = "GIS.zip")
     })
  stime
  
  unzip("mgm2010v5_0.zip")
  unzip("mge2010v5_0.zip")
  
  
save.image(file = "WS.RData")
  
  

   
  
  