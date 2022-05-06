# ¿Hay diferencia en los rasgos funcionales foliares, composición química
# foliar, y rasgos alométricos en función de la condición de crecimiento
# (sombra o sol) para cada especie (Arrayan, Urapan, y Roble)?
# Aumentamos la RAM
memory.limit(size=15000)
Datos_California <- read.table("E:/Research and Study/Estadística/R/Curso R y Scripts/Tareas/Trabajo Datos_California/Datos_California.txt", h = T,  dec = ",")
head(Datos_California)
names(Datos_California)[5]="NO3"
str(Datos_California)
# Estimación de datos faltantes
library(missForest)
Datos_California_Imputados <- missForest(Datos_California[ ,3:21])
Datos_Cal_Imputados <- Datos_California_Imputados$ximp
library(openxlsx)
write.xlsx (Datos_Cal_Imputados, "Datos_Cal_Imputados.xlsx")

# Test de distribución normal de las variables de respuesta
Normalidad_Shapiro <- apply(Datos_Cal_Imputados, MARGIN = 2, FUN =  shapiro.test) 
# el Margin = 2 significa que se aplica a columnas y FUN la función
Normalidad_Shapiro # Buscamos los valores > 0.05 porque no tienen distribución normal
# Siendo las variables que no tienen normalidad son:
# AC - NO3 - AO - CST - FEN - EF - Af - AT - AF - DAP - DBT - IE
# No se realiza test de homogeneidad de varianzas dado que ya sabemos que los datos
# se comportan de manera NO Paramétrica

# Realizamos un PCA sobre los Datos_Cal_Imputados para encontrar las variables
# más relevantes sobre las especies y su condición de crecimiento
library(FactoMineR)
Datos_PCA <- PCA(X = Datos_Cal_Imputados, scale.unit = TRUE, ncp = 999, graph = TRUE)
# Extraemos la información sobre las variables y las observaciones del PCA
library(factoextra)
Info_PCA <- get_pca(Datos_PCA)
Info_PCA
summary(Datos_PCA) # Resultados del PCA

# Graficamos los eigenvalues o porcentages de explicación de las variables sobre cada dimensión sobre
Plot_EigenValues <- fviz_screeplot(Datos_PCA, addlabels = TRUE) # Seleccionamos las 3 dimensiones más importantes
Plot_EigenValues
# Generamos un gráfico biplot para ver las relaciones con las variables
# Valores de cos2 bajos o cercanos al centro son menos importantes para la correlación
PCA_Biplot <- fviz_pca_biplot(Datos_PCA,
                # Individuos por Especie
                geom.ind = "point",
                fill.ind =  Datos_California$Especie, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco", 
                addEllipses = TRUE,
                # Variables
                alpha.var = "contrib", col.var = "contrib",
                gradient.cols = "rickandmorty",
                legend.title = list(fill = "Especie", color = "Contrib",
                                    alpha = "Contrib"), repel = TRUE)
PCA_Biplot                
#  Visualizamos cuáles variables son las que más contribuyen a las dimensiones
# y todas aquellas que estén sobre la media o línea roja son importantes
fviz_contrib(Datos_PCA, choice = "var", axes = 1, top = 10)
fviz_contrib(Datos_PCA, choice = "var", axes = 2, top = 10)
fviz_contrib(Datos_PCA, choice = "var", axes = 3, top = 10)
Info_PCA$contrib # Identificamos la contribución de la variable a la dimensión

# Exportamos las gráficas en forma to PDF o también puede hacerse en "png()"
pdf("PCA_Full.pdf")
print(PCA_Full)
pdf("Plot_EigenValues.pdf")
print(Plot_EigenValues)
# En caso de que se genere un error en la exportación se puede exportar desde
# el panel gráfico en: save as PDF y en formato A4

# Realizamos un PerMANOVA para establecer la diferencia en las varianzas
# de aquellas variables importantes identificadas en el PCA
# Se realiza PERMANOVA porque es un análisis de la varianza para datos NO PARAMÉTRICOS
library(vegan)
library(tidyverse)
Distance_Matrix <- vegdist(Datos_Cal_Imputados, method = "bray") # Se crea un matriz
# basada en distancias, dado que PerMANOVA lo requiere
Adonis_Run <- adonis(formula = Distance_Matrix ~ Datos_California$Especie*Datos_California$Condicion, 
       data = Datos_Cal_Imputados,  permutations = 999)
Adonis_Run
# En esta fórmula comparamos la Especie con interacción de la Condición como covariable
# respecto a las distancias de agrupamiento de las variables con 999 permutaciones
# Los valores críticos señalan que la relación no es por la interacción de las categorías
# si no que cada una en sí misma es significativa, siendo el factor Especie determinante

# Ahora vamos a hacer las comparaciones con un post hoc test de Tukey
