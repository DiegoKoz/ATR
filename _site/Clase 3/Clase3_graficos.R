#RESTART R
library(tidyverse) # tiene ggplot, dplyr, tidyr, y otros
library(ggthemes)  # estilos de gráficos
library(ggrepel)   # etiquetas de texto más prolijas que las de ggplot


# iris es un set de datos clásico, que ya viene incorporado en R
iris
plot(iris)

data(iris)
plot(iris$Sepal.Length,type = "p")
plot(iris$Sepal.Length,type = "l")
plot(iris$Sepal.Length,type = "b")
hist(iris$Sepal.Length, col = "lightsalmon1", main = "Histograma")

archivo <- "Resultados/grafico1.PNG"
archivo
png(archivo)
plot(iris$Sepal.Length,type = "b")
dev.off()

ggplot(data = iris, aes(x = Sepal.Length, fill = Species))+
  geom_histogram(alpha=0.75, binwidth = .5)+
  facet_wrap(~Species)+
  labs("Histograma por especie")+
  theme(legend.position = 'none')

##"Capa del gráfico 1"
g <- ggplot(data = iris, aes(x = Petal.Length, Petal.Width, color = Species))
g
##"Capa del gráfico 2"
g <- g +  geom_point(alpha=0.25)
g

##"Capa del gráfico 3 a 5"
g <- g +
  labs(title = "Medidas de los pétalos por especie")+
  theme(legend.position = 'none')+
  facet_wrap(~Species)
g


iris_promedios <- iris %>% 
  group_by(Species) %>% 
  summarise_all(mean)

iris_promedios

ggplot(data = iris_promedios, aes(x = Species, Petal.Width, fill = Species))+
  geom_col(alpha=0.75)+
  labs(title = "Ancho promedio del pétalo por especie")+
  theme(legend.position = 'none')

####Levanto Bases####
Individual_t117 <- read.table("Fuentes/usu_individual_t117.txt",
                              sep=";", dec=",", header = TRUE, fill = TRUE)


#### Boxplot de ingresos de la ocupación principal, según nivel educativo
# Las variables sexo( CH04 ) y Nivel educativo están codificadas como números, y el R las entiende como numéricas.
class(Individual_t117$NIVEL_ED)
class(Individual_t117$CH04)

ggdata <- Individual_t117 %>% 
  filter(P21>0, !is.na(NIVEL_ED)) %>% 
  mutate(NIVEL_ED = as.factor(NIVEL_ED),
         CH04     = as.factor(CH04))

ggplot(ggdata, aes(x = NIVEL_ED, y = P21, fill = NIVEL_ED)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))#Restrinjo el gráfico hasta ingresos de $40000

#Si queremos agregar la dimensión _sexo_, podemos hacer un facet_wrap()
ggplot(ggdata, aes(x= NIVEL_ED, y = P21, group = NIVEL_ED, fill = NIVEL_ED )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))+
  facet_wrap(~ CH04, labeller = "label_both")

##Foco en las diferencias opr sexo
ggplot(ggdata, aes(x= CH04, y = P21, group = CH04, fill = CH04 )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))+
  facet_grid(~ NIVEL_ED, labeller = "label_both") +
  theme(legend.position = "none")

### [Histogramas]
hist_data <-Individual_t117 %>%
  filter(P21>0) 

ggplot(hist_data, aes(x = P21,weights = PONDIIO))+ 
  geom_histogram(col = "grey")+
  scale_x_continuous(limits = c(0,50000))

### [Kernels]

kernel_data <-Individual_t117 %>%
  filter(P21>0) 

ggplot(kernel_data, aes(x = P21,weights = PONDIIO))+ 
  geom_density()+
  scale_x_continuous(limits = c(0,50000))
##**El eje y no tiene demasiada interpretabilidad en los Kernel, porque hace a la forma en que se construyen las distribuciones

ggplot(kernel_data, aes(x = P21,weights = PONDIIO))+ 
  geom_density(adjust = 2)+
  scale_x_continuous(limits = c(0,50000))


ggplot(kernel_data, aes(x = P21,weights = PONDIIO))+ 
  geom_density(adjust = 0.01)+
  scale_x_continuous(limits = c(0,50000))

kernel_data_2 <- kernel_data %>% 
  mutate(CH04= case_when(CH04 == 1 ~ "Varon",
                         CH04 == 2 ~ "Mujer"))

ggplot(kernel_data_2, aes(x = P21,
                          weights = PONDIIO,
                          group = CH04,
                          fill = CH04)) +
  geom_density(alpha=0.7,adjust =2)+
  labs(x="Distribución del ingreso", y="",
       title=" Total según tipo de ingreso y género", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom",
        plot.title      = element_text(size=12))

ggsave(filename = "Resultados/Kernel_1.png",scale = 2)