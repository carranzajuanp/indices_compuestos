rm(list=ls())
datos = read.csv("~/Descargas/indice_caminabilidad_impar.csv", header = T, sep = ";")
names(datos)
library(dplyr)
# Creamos la base de datos conforme lo pide la librería:
library(COINr)
iData = datos[,2:18]
iData = na.omit(iData)

iData$uCode = as.character(c(1:nrow(iData)))

iData = iData[,c("uCode","Arbolado.y.Vegetacion","Limpieza.Viaria","Contaminacion.Sonora","Senializacion",
                 "Sendas.Peatonales","Obras.en.Construccion","Presencia.de.Obstaculos",
                 "Actividad.Comercial","Area.Movilidad.Motorizada","Ancho.Vereda.util","Estado.de.Vereda",
                 "Mobiliario.Urbano","Pendientes","Movilidad.Reducida","Estado.Rampas",
                 "Guia.no.Videntes")]

# Creamos el bicho que le da estructura a los índices:
iCode = c("Arbolado.y.Vegetacion","Limpieza.Viaria","Contaminacion.Sonora","Senializacion",
          "Sendas.Peatonales","Obras.en.Construccion","Presencia.de.Obstaculos",
          "Actividad.Comercial","Area.Movilidad.Motorizada","Ancho.Vereda.util","Estado.de.Vereda",
          "Mobiliario.Urbano","Pendientes","Movilidad.Reducida","Estado.Rampas",
          "Guia.no.Videntes","Index")
Level = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2)
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
# coin <- new_coin(iData = iData, iMeta = iMeta)
# coin <- Normalise(coin, dset = "Raw")
# coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$Original)
# # coin <- get_opt_weights(coin, dset = "Aggregated", Level = 2, optype = "infomax",
# #                     weights_to = "pesos", out2 = "coin")
# # Ponderaciones para el nivel 1:
# coin <- get_opt_weights(coin, dset = "Raw", Level = 1, optype = "infomax",
#                        weights_to = "pesos", out2 = "coin")
# coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 2, ]
# sum(coin$Meta$Weights$pesos[2, 3])
# coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$pesos)
# dset_aggregated <- get_dset(coin, dset = "Aggregated")
# dset_aggregated = dset_aggregated %>% 
#   mutate(promedio = (sobreocupado_demandante + subocupado_demandante + sobreocupado + vacaciones + 
#                        aguinaldo + obra_social + dias_enferm + jubilacion + ingreso_hora + 
#                        cantidad_ocupaciones + p_25) / 11)
# dset_aggregated
# plot(dset_aggregated$Index, dset_aggregated$promedio)
# cor.test(dset_aggregated$Index, dset_aggregated$promedio)
# 
# plot_framework(coin)
# plot_framework(coin, type = "stack", colour_level = 2)
# 
# dset_aggregated = rename(dset_aggregated, index_opt = Index)
# 
# indice = left_join(iData, dset_aggregated[,c("uCode","index_opt")])

# CON COMPONENTES PRINCIPALES
coin <- new_coin(iData = iData,
                 iMeta = iMeta)
coin <- Normalise(coin, dset = "Raw")
coin$Data$Normalised

coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$Original)
coin$Data$Aggregated
cor(rowMeans(coin$Data$Aggregated[,2:17]), coin$Data$Aggregated$Index)

# Ponderaciones para el nivel 2:
# coin <- get_PCA(coin, dset = "Aggregated", Level =  2,
#                 weights_to = "pesos", out2 = "coin")
# Ponderaciones para el nivel más bajo:
coin <- get_PCA(coin, dset = "Aggregated", Level =  1,
                weights_to = "pesos", out2 = "coin")
coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, ]

# Normalizamos los pesos, pero esto está mal porque algunos son negativos.
coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] = (coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] - min(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3])) /
  (max(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3]) - min(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3]))
coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] = coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1,3] / sum(coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, 3])

coin <- Aggregate(coin, dset = "Normalised", w = coin$Meta$Weights$pesos)
dset_aggregated <- get_dset(coin, dset = "Aggregated")
dset_aggregated = dset_aggregated %>% 
  mutate(promedio = (Arbolado.y.Vegetacion + Limpieza.Viaria + Contaminacion.Sonora + Senializacion +
                     Sendas.Peatonales + Obras.en.Construccion + Presencia.de.Obstaculos +
                     Actividad.Comercial + Area.Movilidad.Motorizada + Ancho.Vereda.util + Estado.de.Vereda +
                     Mobiliario.Urbano + Pendientes + Movilidad.Reducida + Estado.Rampas +
                     Guia.no.Videntes) / 17)
dset_aggregated

dset_aggregated = rename(dset_aggregated, index_pca = Index)

summary(dset_aggregated$index_pca)
summary(dset_aggregated$promedio)


plot(dset_aggregated$index_pca, dset_aggregated$promedio)
cor.test(dset_aggregated$index_pca, dset_aggregated$promedio)

grafico = coin$Meta$Weights$pesos[coin$Meta$Weights$pesos$Level == 1, ]

# plot_framework(coin)
# plot_framework(coin, type = "stack", colour_level = 2)
# 
# 
# 
# 
# indice = left_join(indice, dset_aggregated[,c("uCode","index_pca","promedio")])
# 
# sexo = indice %>% 
#   group_by(sexo) %>% 
#   summarise(indice_pca = mean(index_pca),
#             indice_opt = mean(index_opt),
#             indice_promedio_simple = mean(promedio))
# sexo
# 
# informalidad = indice %>% 
#   group_by(informal) %>% 
#   summarise(indice_pca = mean(index_pca),
#             indice_opt = mean(index_opt),
#             indice_promedio_simple = mean(promedio))
# informalidad
# 
# educacion = indice %>% 
#   group_by(educacion) %>% 
#   summarise(indice_pca = mean(index_pca),
#             indice_opt = mean(index_opt),
#             indice_promedio_simple = mean(promedio))
# educacion
# 
# q_itf = indice %>% 
#   group_by(q_itf) %>% 
#   summarise(indice_pca = mean(index_pca),
#             indice_opt = mean(index_opt),
#             indice_promedio_simple = mean(promedio))
# q_itf
# 
