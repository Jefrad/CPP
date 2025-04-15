##Recomendaciones 
# borrar todos los objetos en el espacio de trabajo
rm(list=ls())      
# valores sin notación científica
options(scipen=999) 

##Cargar Datos latinobarometro
# Carga el archivo .RData desde la carpeta Input/datos/
load("Input/datos/Latinobarometro_2023_Esp_Rdata_v1_0.rdata")

# Reemplaza "Latinobarometro_2023_Esp_v1_0" por nombre más manejable
datos <- Latinobarometro_2023_Esp_v1_0

##Seleccionar las variables que vamos a a utilizar para operacionalizar 
#Instalar y cargar pacman
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
#Ubicar lo que buscamos 
"P13ST.G"#Confianza en paridos politicos 
"P18N.F" #Funcionan bien los partidos politicos
"Sexo" #Sexo
"Edad" #Edad
"Idenpa"#Pais
#separar variables de la base de datos
cpp <- datos %>% select("P13ST.G","P18N.F","sexo","edad","idenpa")
#Filtrar el idenpa al de chile
cpp <- cpp %>% dplyr::filter(idenpa==152)
#Hay que cambiar los nombres
sjlabelled::get_label(cpp) #Ver