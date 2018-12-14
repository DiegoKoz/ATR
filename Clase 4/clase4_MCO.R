# Clase 4
# author: "Barriola, Kozlowski y Weskler"
# date: "14/12/2018"

#### Carga de librerías

library(tidyverse)
library(GGally)
library(cowplot)

# Dataset

estados <- state.x77 %>%
as.data.frame() %>% # Transformo la matriz a dataframe
rename(life_exp = `Life Exp`, hs_grad=`HS Grad`) # Renombro las variables para no tener problema con los espacios

# Para conocer el dataset utilicemos `?state.x77` para abrir la ayuda de R y la funcion `glimpse()` para ver la estructura de nuestros datos.

?state.x77
glimpse(estados)

#Veamos algunas estadisticas de resumen para conocer un poco más nuestras variables, usando la funcion `summary`:
  
summary(estados)

# Covarianza y correlación

## Covarianza

### Interpretacion grafica


#ASOCIACION LINEAL POSITIVA

ggplot(estados, aes(x=Murder,y=Illiteracy)) + geom_point(size=2) +
  geom_hline(yintercept = mean(estados$Illiteracy),color='steelblue', linetype='dashed', size=1) +
  geom_vline(xintercept = mean(estados$Murder), color='steelblue', linetype='dashed', size=1) +
  annotate(geom='text', x=c(6.5,8.3,15,15), y=c(2.9,2.9,1.5,0.8), label='+', colour='forestgreen', size=19) +
  annotate(geom='text', x=c(6.5,8.3,0,0), y=c(0.3,0.3,1.5,0.8), label='-', colour='firebrick', size=27) +
  annotate(geom='text', x=15, y=2.9, label='1', colour='steelblue', size=10) +
  annotate(geom='text', x=15, y=0.3, label='4', colour='steelblue', size=10) +
  annotate(geom='text', x=0, y=0.3, label='3', colour='steelblue', size=10) +
  annotate(geom='text', x=0, y=2.9, label='2', colour='steelblue', size=10) +
  labs(title='Asociacion lineal positiva', x='Asesinatos (Murder)', y='Analfabetismo (Illiteracy)')+
  theme_bw() + scale_y_continuous(limits = c(0,3)) + scale_x_continuous(limits = c(0,16))

# La funcion `cov` permite calcular la covarianza entre dos variables.

cov(x=estados$Murder, y=estados$Illiteracy)

#ASOCIACION LINEAL NEGATIVA

ggplot(estados, aes(x=Murder,y=life_exp)) + geom_point(size=2) +
  geom_hline(yintercept = mean(estados$life_exp), color='steelblue', linetype='dashed', size=1, alpha=0.7) +
  geom_vline(xintercept = mean(estados$Murder), color='steelblue', linetype='dashed', size=1, alpha=0.7) +
  annotate(geom='text', x=c(6.5,8.3,15,15), y=c(74.2,74.2,71.7,70.1), label='+', colour='forestgreen', size=19) +
  annotate(geom='text', x=c(6.5,8.3,0,0), y=c(67.9,67.9,71.7,70.1), label='-', colour='firebrick', size=27) +
  annotate(geom='text', x=15, y=74.2, label='1', colour='steelblue', size=10) +
  annotate(geom='text', x=15, y=67.9, label='4', colour='steelblue', size=10) +
  annotate(geom='text', x=0, y=67.9, label='3', colour='steelblue', size=10) +
  annotate(geom='text', x=0, y=74.2, label='2', colour='steelblue', size=10) +
  labs(title='Asociacion lineal negativa', x='Asesinatos (Murder)', y='Life Exp (Experanza Vida)')+
  theme_bw() +
  scale_x_continuous(limits = c(0,16)) + scale_y_continuous(limits = c(67.5,74.5))

# la covarianza es negativa:

cov(x=estados$Murder, y=estados$life_exp)

#SIN ASOCIACION LINEAL

ggplot(estados, aes(x=Area,y=Illiteracy)) + geom_point(size=2) +
  geom_hline(yintercept = mean(estados$Illiteracy),color='steelblue', linetype='dashed', size=1) +
  geom_vline(xintercept = mean(estados$Area), color='steelblue', linetype='dashed', size=1) +
  annotate(geom='text', x=c(-10000,150000,500000,500000), y=c(2.9,2.9,1.5,0.8), label='+', colour='forestgreen', size=19) +
  annotate(geom='text', x=c(0,150000,-400000,-400000), y=c(-0.6,-0.6,1.5,0.8), label='-', colour='firebrick', size=27) +
  annotate(geom='text', x=500000, y=2.9, label='1', colour='steelblue', size=10) +
  annotate(geom='text', x=500000, y=-0.6, label='4', colour='steelblue', size=10) +
  annotate(geom='text', x=-400000, y=-0.6, label='3', colour='steelblue', size=10) +
  annotate(geom='text', x=-400000, y=2.9, label='2', colour='steelblue', size=10) +
  labs(title='Sin asociacion lineal',y='Analfabetismo (Illiteracy)')+
  theme_bw() + scale_y_continuous(limits = c(-1,3)) + scale_x_continuous(limits = c(-400000,600000))

cov(x = estados$Area, y=estados$Illiteracy)

# La covarianza tiene una **CARACTERÍSTICA** que puede ser un **PROBLEMA** importante: se ve afectada por la unidad de medida de las variables.

# Covarianza entre area (medida en millas cuadradas) y analfabetismo
cov_1 = cov(x = estados$Area, y=estados$Illiteracy)
cov_1
# 1 milla cuadrada = 2.59 kilometros cuadrados
area_kilometros = estados$Area*2.59
# Covarianza entre area (medida en kilometros cuadradas) y analfabetismo
cov_2=cov(x = area_kilometros, y=estados$Illiteracy)
cov_2

## Correlación

#La funcion `cor` permite calcular la correlacion entre dos variables.

# Correlacion entre area (medida en millas cuadradas) y analfabetismo
corr_1 = cor(x = estados$Area, y=estados$Illiteracy)
corr_1
# 1 milla cuadrada = 2.59 kilometros cuadrados
area_kilometros = estados$Area*2.59
# Correlacion entre area (medida en kilometros cuadradas) y analfabetismo
corr_2=cor(x = area_kilometros, y=estados$Illiteracy)
corr_2

#Vemos que ambos coeficientes de correlación son iguales.

### Ejemplos

# Correlacion entre homicidios y analfabetismo
corr_positiva = cor(x = estados$Murder, y=estados$Illiteracy)
corr_positiva

# Correlacion entre homicidios y esperanza de vida
corr_negativa = cor(x = estados$Murder, y=estados$life_exp)
corr_negativa

# Covarianza entre area (medida en kilometros cuadradas) y analfabetismo
corr_nula = cor(x = estados$Area, y=estados$Illiteracy)
corr_nula

### GGAlly

ggpairs(estados) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Test de Hipótesis

#### Motivación

# Sacamos el promedio de días con temperatura mínima por debajo de los 0 grados para los 50 estados
mean(estados$Frost)
# Sacamos la correlacion entre dias debajo de los 0 grados (Frost) y el analfabetismo 
cor(estados$Frost, estados$Illiteracy)

#Seleccionamos los 10 estados mas frios 
frios= estados %>% arrange(desc(Frost)) %>% slice(1:10)
# Sacamos el promedio de días con temperatura mínima por debajo de los 0 grados para esos 10 estados
mean(frios$Frost)
# Sacamos la correlacion entre dias debajo de los 0 grados (Frost) y el analfabetismo para esos 10 estados
cor(frios$Frost, frios$Illiteracy)

# Ahora seleccionamos los 10 estados mas calidos 
calidos= estados %>% arrange(desc(Frost)) %>% slice(41:50)
# Sacamos el promedio de días con temperatura mínima por debajo de los 0 grados para esos 10 estados
mean(calidos$Frost)
cor(calidos$Frost, calidos$Illiteracy)

#### Test de Hipótesis

#Curvas
ggplot(data = data.frame(x = c(-1, 1)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 0.07, sd = .1), color="firebrick", size=1.5) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 0.71, sd = .1), color="forestgreen",size=1.5) +
  geom_vline(xintercept = 0, linetype='dashed', size=1) +
  theme_bw() +
  labs(title="Distribucion coeficientes de correlacion", x="Coeficiente de Correlacion", y="")+
  scale_y_continuous(breaks = NULL)

#La función que vamos a utilizar se llama `cor.test`

# Veamos la documentacion
?cor.test
# Al igual que para calcular el coeficiente de correlacion pasamos nuestras dos variables: X e Y
cor.test(x=estados$Murder, y=estados$Illiteracy)
#Veamos el test de la correlacion entre **Area y Analfabetismo**:
cor.test(x=estados$Area, y=estados$Illiteracy)

# Correlación vs causalidad

#Ejemplo de `estados` con Frost vs Illiteracy

ggplot(estados, aes(x=Frost,y=Illiteracy)) + geom_point(size=2) +
  geom_hline(yintercept = mean(estados$Illiteracy),color='steelblue', linetype='dashed', size=1) +
  geom_vline(xintercept = mean(estados$Frost), color='steelblue', linetype='dashed', size=1) +
  labs(title='¿Correlacion espuria?')+
  theme_bw() + scale_y_continuous(limits = c(-1,3)) + scale_x_continuous(limits = c(-10,200))

cor(x=estados$Frost, y=estados$Illiteracy)

# Modelo

## Modelo lineal simple

#Supongamos que nos interesa modelar la relación entre asesinatos y la esperanza de vida en los estados de Estados Unidos.

ggplot(estados, aes(x=Murder,y=life_exp)) +
  geom_point(size=2) +
  labs(title='Asesinatos y Esperanza de Vida', x='Asesinatos (Murder)', y='Life Exp (Experanza Vida)')+
  theme_bw()

### Rectas

#Consideremos la recta $Y= 6 - 2X$

# Creo la funcion
funcion <- function(x) {6 - 2*x}
# Grafico
ggplot(data.frame(x = c(-1, 3)), aes(x)) +
    stat_function(fun=funcion, colour='forestgreen', size=2)+
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_bw()

### Parámetros del modelo

#Probemos algunas rectas y veamos cual se ajusta mejor a nuestros datos
  
recta1 <- ggplot(estados, aes(x=Murder,y=life_exp)) +
    geom_point(size=2) +
    geom_abline(intercept = 73, slope = -0.3, colour='forestgreen', size=1.5) +
    labs(title='Recta 1', x='Asesinatos (Murder)', y='Life Exp (Experanza Vida)')+
    theme_bw()
  
recta2 <- ggplot(estados, aes(x=Murder,y=life_exp)) +
    geom_point(size=2) +
    geom_abline(intercept = 69, slope = 0.2, colour='firebrick', size=1.5) +
    labs(title='Recta 2', x='Asesinatos (Murder)', y='Life Exp (Experanza Vida)')+
    theme_bw()
  
recta3 <- ggplot(estados, aes(x=Murder,y=life_exp)) +
    geom_point(size=2) +
    geom_abline(intercept = 71, slope = -0.3, colour='steelblue', size=1.5) +
    labs(title='Recta 3', x='Asesinatos (Murder)', y='Life Exp (Experanza Vida)')+
    theme_bw()
  
plot_grid(recta1,recta2,recta3)

## Modelo en R

#Creo el modelo con life_exp (variable a predecir) ~ Murder (variable predictora)
modelo_asesinatos<-lm(formula = life_exp~Murder, data = estados)
  
modelo_asesinatos

### Interpretación

modelo_asesinatos$coefficients
  
beta_0 <- modelo_asesinatos$coefficients[[1]] # Guardamos el valor del intercepto
beta_1 <- modelo_asesinatos$coefficients[[2]] # Guardamos el valor de la pendiente

# Grafico
ggplot(estados, aes(x=Murder,y=life_exp)) +
    geom_point(size=2) +
    geom_abline(intercept = beta_0, slope = beta_1, colour='forestgreen', size=1.5, alpha=0.6) +
    geom_vline(xintercept = 0) +
    labs(title='Modelo lineal', x='Asesinatos (Murder)', y='Life Exp (Experanza Vida)')+
    theme_bw()

### Evaluación del modelo

# Resumen del modelo
summary(modelo_asesinatos)

#Creo el modelo con life_exp (variable a predecir) ~ Income (variable predictora)
modelo_ingreso<-lm(formula = life_exp~Income, data = estados)
# Resumen del modelo
summary(modelo_ingreso)

