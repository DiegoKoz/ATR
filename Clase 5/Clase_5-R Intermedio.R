# Reiniciar

library(tidyverse)
library(openxlsx)
library(ggthemes)

# Loops
for(i in 1:10){
  print(i^2)
  
}

for(Valores in 1:10){
  print(Valores^2)
  
}

Individual_t117 <- read.table("Fuentes/usu_individual_t117.txt",
                              sep=";", dec=",", header = TRUE, fill = TRUE)
Regiones <- read.xlsx("Fuentes/Regiones.xlsx")
Aglomerados <- read.xlsx("Fuentes/Aglomerados EPH.xlsx")

ggdata <- Individual_t117 %>% 
  left_join(Regiones) %>% 
  filter(P21>0, !is.na(NIVEL_ED)) %>% 
  mutate(NIVEL_ED = as.factor(NIVEL_ED),
         CH04     = as.factor(CH04))

unique(ggdata$Region)

pdf(file = "Resultados/Graficos_Region.PDF")
for(variable_itera in unique(ggdata$Region)){
  print(variable_itera)# No es necesario, permite ver por que Región estoy trabajando
  
  Base_Reg <- ggdata %>% # Aquí filtro la base cuando Region toma el valor de "variable_itera"
    filter(Region == variable_itera) 
  
  Graf_Reg <- ggplot(Base_Reg, 
                     aes(x= NIVEL_ED, y = P21, group = NIVEL_ED, fill = NIVEL_ED )) +
    geom_boxplot()+
    scale_y_continuous(limits = c(0, 40000))+
    labs(title = paste0("Region ", variable_itera))+
    facet_wrap(~ CH04, labeller = "label_both")
  
  print(Graf_Reg)
}
dev.off()

# Estructuras Condicionales

##if
if(2+2 == 4){
  print("Menos Mal")
}

if( 2+2 == 148.24){
  print("R, la estas pifiando")
}

##ifelse
resultado <- if_else(2+2==4, true = "Joya",false = "Error") #Versión dplyr

ABC_123 <- data.frame(Letras = LETTERS[1:20],Num = 1:20)
ABC_123 %>% 
  mutate(Mayor_o_Menor = ifelse(Num<=5,"Menor o igual que 5","Mayor que 5"))
# Lubridate
library(lubridate)
fecha  <- "04/12/92 17:35:16"
fecha

fecha  <- dmy_hms(fecha)
fecha

fecha2  <- "Dec-92"
fecha2 <- parse_date_time(fecha2, orders = 'my')
fecha2

## Extracción de información

year(fecha) # Obtener el año
month(fecha) #Obtener el mes
day(fecha) # Obtener el día
wday(fecha, label = TRUE) #Obtener el nombre del día
hour(fecha) #Obtener la hora

## Operaciones
# Sumo dos días 
fecha + days(2)
# Resto 1 semana y dos horas
fecha - (weeks(1) + hours(2))


# Funciones del Usuario

funcion_prueba <- function(parametro1,parametro2) {
  paste(parametro1, parametro2, sep = " <--> ")
}

funcion_prueba(parametro1 = "A ver", parametro2 = "Que pasa")

Otra_funcion_prueba <- function(parametro1 ,parametro2 = "Te colgaste en ingresar algo") {
  paste(parametro1, parametro2, sep = " <--> ")
  
}
Otra_funcion_prueba(parametro1 = "Valor 1 ")



##### map ####


resultado <- map2(ABC_123$Letras,ABC_123$Num,funcion_prueba)
resultado[1:3]


ABC_123 %>% 
  mutate(resultado= map2(Letras,Num,funcion_prueba))


ABC_123 %>% 
  mutate(resultado= unlist(map2(Letras,Num,funcion_prueba)))


map(ABC_123$Letras,funcion_prueba,ABC_123$Num)[1:2]


ABC_123 %>% 
  mutate(resultado= map(Letras,funcion_prueba,Num))



# Lectura y escritura de archivos intermedia
## RData
x <- 1:15
y <- list(a = 1, b = TRUE, c = "oops")

#Para guardar
save(x, y, file = "Clase 6a - Loops y funciones/xy.RData")

#Para leer
load('Clase 6a - Loops y funciones/xy.RData')
## __RDS__
x
saveRDS(x, "Clase 6a - Loops y funciones/x.RDS")

Z <- readRDS("Clase 6a - Loops y funciones/x.RDS")
Z
