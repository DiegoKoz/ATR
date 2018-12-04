#Reiniciar R


library(tidyverse)

INDICE  <- c(100,   100,   100,
             101.8, 101.2, 100.73,
             102.9, 102.4, 103.2)

FECHA  <-  c("Oct-16", "Oct-16", "Oct-16",
             "Nov-16", "Nov-16", "Nov-16",
             "Dic-16", "Dic-16", "Dic-16")


GRUPO  <-  c("Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado")

Datos <- data.frame(INDICE, FECHA, GRUPO)

Datos %>% 
  filter(INDICE>101 , GRUPO == "Privado_Registrado")

Datos %>% 
  filter(INDICE>101 | GRUPO == "Privado_Registrado")

Datos %>% 
  rename(Periodo = FECHA)

Datos <- Datos %>% 
  mutate(Doble=INDICE*2)

Datos <- Datos %>% 
  mutate(Caso_cuando = case_when(GRUPO == "Privado_Registrado"   ~ INDICE*2,
                                 GRUPO == "Público"              ~ INDICE*3,
                                 GRUPO == "Privado_No_Registrado"~ INDICE*5))

Datos2 <- Datos %>% 
  select(INDICE, FECHA, GRUPO)


Datos <- Datos %>% 
  select(-c(Doble,Caso_cuando))

Datos <- Datos %>% 
  arrange(GRUPO, INDICE)

Datos %>% 
  summarise(Indprom = mean(INDICE))

Datos %>% 
  group_by(FECHA) %>%
  summarise(Indprom = mean(INDICE))

Encadenado <- Datos %>% 
  filter(GRUPO == "Privado_Registrado") %>% 
  rename(Periodo = FECHA) %>% 
  mutate(Doble = INDICE*2) %>% 
  select(-INDICE)


Ponderadores <- data.frame(GRUPO = c("Privado_Registrado","Público","Privado_No_Registrado"),
                           PONDERADOR = c(50.16,29.91,19.93))

Datos_join <- Datos %>% 
  left_join(.,Ponderadores, by = "GRUPO")
Datos_join

Datos_Indice_Gral <- Datos_join %>% 
  group_by(FECHA) %>% 
  summarise(Indice_Gral = weighted.mean(INDICE,w = PONDERADOR))


#__Gather__ es una función que nos permite pasar los datos de forma horizontal a una forma vertical. 
#__spread__ es una función que nos permite pasar los datos de forma vertical a una forma horizontal.
Datos_Spread <- Datos %>% 
  spread(.,       # el . llama a lo que esta atras del %>% 
         key = GRUPO,    #la llave es la variable cuyos valores van a dar los nombres de columnas
         value = INDICE) #los valores con que se llenan las celdas

Datos_Spread  


Datos_gather <- Datos_Spread %>%  
  gather(.,         # el . llama a lo que esta atras del %>% 
         key   = GRUPO,   # como se llamará la variable que toma los nombres de las columnas 
         value = INDICE,  # como se llamará la variable que toma los valores de las columnas
         2:4)             #le indico que columnas juntar

Datos_gather

####EPH####

list.files("Fuentes/")
Individual_t117 <-
  read.table("Fuentes/usu_individual_t117.txt",
    sep = ";",
    dec = ",",
    header = TRUE,
    fill = TRUE )

Individual_t416 <-
  read.table("Fuentes/usu_individual_t416.txt",
             sep = ";",
             dec = ",",
             header = TRUE,
             fill = TRUE )

library(openxlsx) 

Aglom <- read.xlsx("Fuentes/Aglomerados EPH.xlsx")


###### Variables ###### 

# ESTADO: CONDICIÓN DE ACTIVIDAD 

# 0 = Entrevista individual no realizada ( no respuesta al Cuestionario Individual)
# 1 = Ocupado
# 2 = Desocupado
# 3 = Inactivo
# 4 = Menor de 10 años

# PONDERA: Ponderador 

###Defino variables de interes y acoto la base de datos a las mismas
Variables_interes <- c("ANO4","TRIMESTRE","ESTADO","PONDERA","REGION","AGLOMERADO")

Basesita_t416 <- Individual_t416 %>% select(Variables_interes)
Basesita_t117 <- Individual_t117 %>% select(Variables_interes)

#Uno las bases de ambos trimestres
Union_Bases <- bind_rows(Basesita_t416,Basesita_t117)
##alternativamente
Union_Bases <- Basesita_t416 %>% 
  bind_rows(Basesita_t117)

###Calculo los numeradores y denominadores de los cocientes
Poblacion_ocupados <- Union_Bases %>% 
  group_by(ANO4,TRIMESTRE) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]))

#Calculo la tasa de empleo, dentro del mismo summarise
Empleo <- Union_Bases %>% 
  group_by(ANO4,TRIMESTRE) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Tasa_Empleo    = Ocupados/Poblacion)

#Desecho las variables de nivel
Empleo %>% 
  select(-(3:4))


Tasas_dos_trimestres <- Union_Bases %>% 
  group_by(ANO4,TRIMESTRE) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA) %>% 
  select(1:2,7:ncol(.))

#Calculo las tasas desagregadas por aglomerado
Tasas_dos_trimestres_AGLOM <- Union_Bases %>% 
  group_by(ANO4,TRIMESTRE,AGLOMERADO) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA) 

#Incorporo el nombre los aglomerados y emprolijo los resultados

Tasas_dos_trimestres_AGLOM_nombre <- Tasas_dos_trimestres_AGLOM %>% 
  select(-c(4:7)) %>%    # Eliminamos las variables de nivel
  left_join(.,Aglom) %>% # Agregamos el nombre de los aglomerados, que teniamos en otro DF
  select(Nom_Aglo,everything(.),-AGLOMERADO) #Eliminamos el código de los aglomerados

Lista_a_exportar <- list("Resultado1" = Tasas_dos_trimestres,
                         "Resultado2" = Tasas_dos_trimestres_AGLOM_nombre)

write.xlsx(Lista_a_exportar,"Resultados/Informe Mercado Trabajo.xlsx")


#### Exportar resultados a  Excel



