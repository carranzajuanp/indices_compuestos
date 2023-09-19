rm(list=ls())
setwd("~/Documentos/GitHub/indices_compuestos/")
datos = read.csv("indice_caminabilidad.csv", header = T, sep = ",")
names(datos)
library(dplyr)
# Creamos la base de datos conforme lo pide la librería:
library(COINr)
datos$Área.Movilidad.Motorizada = as.numeric(gsub(",",".", datos$Área.Movilidad.Motorizada))
datos$IC = as.numeric(gsub(",",".",datos$IC))
datos = na.omit(datos)

datos <- datos %>%
  group_by(fid) %>% 
  mutate_if(is.integer, as.numeric) %>% 
  summarise_at(vars("Arbolado.y.Vegetación":"IC"), mean)
summary(datos)

datos$Distancias.Paradas.TP = NULL

iData = datos[,2:17]
iData = na.omit(iData)

iData$uCode = as.character(datos$fid)
names(iData)
iData = iData[,c("uCode","Arbolado.y.Vegetación","Limpieza.Viaria","Contaminación.Sonora","Señaización.......H.y.V","Sendas.Peatonales",
                 "Obras.en.Construcción","Presencia.de.Obstáculos","Área.Movilidad.Motorizada","Actividad.Comercial","Ancho.Vereda.Útil",
                 "Estado.de.Vereda","Mobiliario.Urbano","Pendientes","Movilidad.Reducida","Estado.Rampas",
                 "Guía.no.Videntes")]

# Creamos el bicho que le da estructura a los índices:
iCode = c("Arbolado.y.Vegetación","Limpieza.Viaria","Contaminación.Sonora","Señaización.......H.y.V","Sendas.Peatonales",
          "Obras.en.Construcción","Presencia.de.Obstáculos","Área.Movilidad.Motorizada","Actividad.Comercial","Ancho.Vereda.Útil",
          "Estado.de.Vereda","Mobiliario.Urbano","Pendientes","Movilidad.Reducida","Estado.Rampas",
          "Guía.no.Videntes","Index")
Level = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2)
# Direction = c(1, 1, -1, 1, 1, -1, -1, -1, -1, 1, 1, 1, -1, 1, 1, 1, 1)
# Direction is set to 1 if higher values of the indicator should result in higher values of the index, and -1 in the opposite case.
Direction = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
Weight = c(1, 1, 1, 1, 1, 1, 1,  1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
Parent = c("Index","Index", "Index", "Index", "Index", "Index", "Index", "Index", "Index", "Index", "Index", "Index", "Index","Index","Index","Index", NA)
Type = c("Indicator","Indicator","Indicator","Indicator","Indicator","Indicator", "Indicator", "Indicator", "Indicator", "Indicator", "Indicator", "Indicator", "Indicator", "Indicator", "Indicator", "Indicator","Aggregate")
iMeta = data.frame(cbind(iCode,
                         Level,
                         Direction,
                         Weight,
                         Parent,
                         Type))

iMeta$Level = as.numeric(Level)
iMeta$Direction = as.numeric(Direction)
iMeta$Weight = as.numeric(Weight)

check_iData(iData)
check_iMeta(iMeta)

iMeta = iMeta[,c("Level","iCode","Direction","Weight","Parent","Type")]
iMeta

iData <- iData %>% mutate_if(is.integer, as.numeric)
summary(iData)


# PONDERACIÓN DEL LOS COMPONENTES A PARTIR DE LA OPTIMIZACIÓN DE CORRELACIONES
coin <- new_coin(iData = iData, iMeta = iMeta)
coin <- Normalise(coin, dset = "Raw")
coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$Original)
# coin <- get_opt_weights(coin, dset = "Aggregated", Level = 2, optype = "infomax",
#                     weights_to = "pesos", out2 = "coin")
# Ponderaciones para el nivel 1:
coin <- get_opt_weights(coin, dset = "Aggregated", Level =  1, optype = "infomax",
                        weights_to = "pesos", out2 = "coin")
coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, ]
sum(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, 3])

coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$pesos)
dset_aggregated <- get_dset(coin, dset = "Aggregated")
dset_aggregated = dset_aggregated %>% 
  mutate(promedio = (Arbolado.y.Vegetación + Limpieza.Viaria + Contaminación.Sonora + Señaización.......H.y.V + Sendas.Peatonales +
                       Obras.en.Construcción + Presencia.de.Obstáculos + Área.Movilidad.Motorizada + Actividad.Comercial + Ancho.Vereda.Útil +
                       Estado.de.Vereda + Mobiliario.Urbano + Pendientes + Movilidad.Reducida + Estado.Rampas +
                       Guía.no.Videntes) / 16)
dset_aggregated

dset_aggregated = rename(dset_aggregated, index_pca = Index)

summary(dset_aggregated$index_pca)
summary(dset_aggregated$promedio)


plot(dset_aggregated$index_pca, dset_aggregated$promedio,
     main = "Comparación Promedio vs PCA",
     ylab = "Indice con promedio simple",
     xlab = "Indice ponderado vía PCA")
cor.test(dset_aggregated$index_pca, dset_aggregated$promedio)


# CON COMPONENTES PRINCIPALES
coin <- new_coin(iData = iData,
                 iMeta = iMeta)
coin <- Normalise(coin, dset = "Raw")
coin$Data$Normalised

coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$Original)
coin$Data$Aggregated
summary(coin$Data$Aggregated)
cor(rowMeans(coin$Data$Aggregated[,2:17]), coin$Data$Aggregated$Index)

# Ponderaciones para el nivel 2:
# coin <- get_PCA(coin, dset = "Aggregated", Level =  2,
#                 weights_to = "pesos", out2 = "coin")
# Ponderaciones para el nivel más bajo:
coin <- get_PCA(coin, dset = "Aggregated", Level =  1,
                weights_to = "pesos", out2 = "coin")
coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, ]

# Vemos el peso de cada variable
importancia = coin$Meta$Weights$pesos[1:nrow(coin$Meta$Weights$pesos)-1,]
importancia$importancia = paste0(round((abs(importancia$Weight)) / 
  sum(abs(importancia$Weight)) * 100, 1), " %")
importancia
# # Normalizamos los pesos, pero esto está mal porque algunos son negativos.
# coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] = (coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] - min(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3])) /
#   (max(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3]) - min(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3]))
# coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] = coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] / sum(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, 3])
# coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, ]


coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$pesos)
dset_aggregated <- get_dset(coin, dset = "Aggregated")
dset_aggregated = dset_aggregated %>% 
  mutate(promedio = (Arbolado.y.Vegetación + Limpieza.Viaria + Contaminación.Sonora + Señaización.......H.y.V + Sendas.Peatonales +
                     Obras.en.Construcción + Presencia.de.Obstáculos + Área.Movilidad.Motorizada + Actividad.Comercial + Ancho.Vereda.Útil +
                     Estado.de.Vereda + Mobiliario.Urbano + Pendientes + Movilidad.Reducida + Estado.Rampas +
                     Guía.no.Videntes) / 16)
dset_aggregated

dset_aggregated = rename(dset_aggregated, index_pca = Index)

summary(dset_aggregated$index_pca)
summary(dset_aggregated$promedio)


plot(dset_aggregated$index_pca, dset_aggregated$promedio)

ggplot(data = dset_aggregated, aes(x = index_pca, y = promedio)) +
  geom_point(
    # color="transparent",
    fill="#69b3a2",
    shape=21,
    alpha=0.5,
    size= 1.5,
    stroke = 1
  ) +
  geom_smooth(method=lm , color="black", se=FALSE) +
  hrbrthemes::theme_ipsum() +
  labs(title = "Comparación entre promedio simple \n y ponderación mediante PCA") +
  ylim(0,100) + xlim(0,100) +
  ylab("IC con promedio simple") +
  xlab("IC ponderado con PCA")

cor.test(dset_aggregated$index_pca, dset_aggregated$promedio)

grafico = coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, ]

library(sf)
map = st_read("~/Descargas/2022-IC_Centro_01-con-formulas-20230914T133051Z-001/2022-IC_Centro_01-con-formulas/2022-IMPAR.gpkg",
              fid_column_name = "uCode")
summary(map)
class(iData$uCode)
map = left_join(map, dset_aggregated[,c("uCode","index_pca","promedio")])
summary(map)
library(mapview)
map = st_cast(map,"LINESTRING")
mapview(map, zcol = "index_pca")
map = data.table::melt(map, measure.vars = c("index_pca","promedio"))
class(map)
map = st_as_sf(map, sf_column_name = "geom")
map$value = round(map$value, 2)
mapview_facet <- function(x,f) {
  
  criteria=split(x,x[[f]])
  nms = paste(deparse(substitute(x)), names(criteria), sep = "-")
  for (i in 1:length(criteria)) {
    map=mapview::mapview(criteria[[i]], zcol = "value", layer.name = nms[i])
    assign(paste0("map_",i), map)
  }
  set=list(map_1)
  for (i in 2:length(criteria)) {set=append(set, get(paste0("map_",i)))}
  leafsync::latticeView(set)
}

mapa = mapview_facet(x = map, f = "variable")
mapa 


st_write(map, "~/Descargas/2022-IC_Centro_01-con-formulas-20230914T133051Z-001/indice.gpkg")


### PCA
# Elbow Method
names(datos)
summary(datos)

set.seed(7)
wss <- sapply(1:15,function(k){kmeans(coin$Data$Normalised[,c("Arbolado.y.Vegetación","Limpieza.Viaria","Contaminación.Sonora","Señaización.......H.y.V","Sendas.Peatonales",
                                                   "Obras.en.Construcción","Presencia.de.Obstáculos","Área.Movilidad.Motorizada","Actividad.Comercial","Ancho.Vereda.Útil",
                                                   "Estado.de.Vereda","Mobiliario.Urbano","Pendientes","Movilidad.Reducida","Estado.Rampas",
                                                   "Guía.no.Videntes")], k, nstart=50,iter.max = 16 )$tot.withinss})
plot(1:15, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

######################
grupo = 3
######################

set.seed (7)
library(e1071)
zona <- cmeans(coin$Data$Normalised[,c("Arbolado.y.Vegetación","Limpieza.Viaria","Contaminación.Sonora","Señaización.......H.y.V","Sendas.Peatonales",
                                       "Obras.en.Construcción","Presencia.de.Obstáculos","Área.Movilidad.Motorizada","Actividad.Comercial","Ancho.Vereda.Útil",
                                       "Estado.de.Vereda","Mobiliario.Urbano","Pendientes","Movilidad.Reducida","Estado.Rampas",
                                       "Guía.no.Videntes")], grupo, 100, method="cmeans", m=1.1)

zona$cluster
coin$Data$Normalised$cluster = as.character(zona$cluster)
library(factoextra)

res.pca <- prcomp(coin$Data$Normalised[,c("Arbolado.y.Vegetación","Limpieza.Viaria","Contaminación.Sonora","Señaización.......H.y.V","Sendas.Peatonales",
                                          "Obras.en.Construcción","Presencia.de.Obstáculos","Área.Movilidad.Motorizada","Actividad.Comercial","Ancho.Vereda.Útil",
                                          "Estado.de.Vereda","Mobiliario.Urbano","Pendientes","Movilidad.Reducida","Estado.Rampas",
                                          "Guía.no.Videntes")], scale = TRUE) # Se hace el an?lisis de CP y se los escala
eig.val <- get_eigenvalue(res.pca) # Se calculan los valores propios - Landa - que acopa?a a cada CP
round(eig.val, digits = 2) # se los redondea a dos decimales

# Se obtienen los valores para las variables 

res.var <- get_pca_var(res.pca) # Calcula las componentes principales
round(res.var$coord, digits = 2)  # Coordinates
round(res.var$contrib, digits = 2) # Contributions to the PCs
round(res.var$cos2, digits = 2)    # Quality of representation 

round(res.var$coord[,1], digits = 2) 
round(res.var$coord[,2], digits = 2)

fviz_eig(res.pca, ylab= "% CP", xlab= "Comp. Principales", main = "Componentes Principales", font.tickslab = c(12, "bold", "black"), font.title= 20,font.y=15, font.x=15)
fviz_contrib(res.pca, choice = "var",  axes = 1, fill="#06623b", top = 10, font.tickslab = c(12, "bold", "black"), font.title= 20,font.y=15, title=" Contribuci?n CP 1") 
fviz_contrib(res.pca, choice = "var", axes = 2,  fill="#6f0000", top = 10, font.tickslab = c(14, "bold", "black"), font.title= 20,font.y=15, title=" Contribuci?n CP 2") 
fviz_contrib(res.pca, choice = "var", axes = 3,  fill="#00263b", top = 10,font.tickslab = c(14, "bold", "black"), font.title= 20,font.y=15,  title=" Contribuci?n CP 3") 

# Graficar las variables y las CP


col<-c("#000000") # color hunt -https://colorhunt.co/ -

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = col,
             axes=c(1, 2),
             title="Comp. Princ. Variables Economicos",
             repel = TRUE    # Avoid text overlapping
)

# Se obtienen los valores para las observaciones - individuos-

res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

groups <- as.factor(coin$Data$Normalised$cluster)

col <- RColorBrewer::brewer.pal(grupo, "Set1")


fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = col,
             addEllipses = TRUE,
             legend.title = "Grupos",
             axes=c(1, 2),
             geom = c("point"),
             title="CP observaciones",
             alpha=1 ) # Concentration ellipses 

## Observar tanto varables como observaciones en las CP

fviz_pca_biplot(res.pca,
                col.ind = groups, # color by groups
                palette = col,
                col.var = "#000000",
                gradient.cols = "fff3af",
                addEllipses = TRUE,
                legend.title = "Grupos",
                axes=c(1, 2),
                geom = c("point"),
                jitter = list(what = "label", width = NULL, height = NULL),
                title="BI - Plot variables - Individuos",
                alpha=1 )

