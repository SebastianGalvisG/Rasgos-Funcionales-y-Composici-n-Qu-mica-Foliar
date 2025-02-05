# �Hay diferencia en los rasgos funcionales foliares, composici�n qu�mica
# foliar, y rasgos alom�tricos en funci�n de la condici�n de crecimiento
# (sombra o sol) para cada especie (Arrayan, Urapan, y Roble)?
# Aumentamos la RAM
memory.limit(size=15000)
Datos_California <- read.table("E:/Research and Study/Estad�stica/R/Curso R y Scripts/Tareas/Trabajo Datos_California/Datos_California.txt", h = T,  dec = ",")
head(Datos_California)
names(Datos_California)[5]="NO3"
str(Datos_California)
# Estimaci�n de datos faltantes
library(missForest)
Datos_California_Imputados <- missForest(Datos_California[ ,3:21])
Datos_Cal_Imputados <- Datos_California_Imputados$ximp
library(openxlsx)
write.xlsx (Datos_Cal_Imputados, "Datos_Cal_Imputados.xlsx")

# Test de distribuci�n normal de las variables de respuesta
Normalidad_Shapiro <- apply(Datos_Cal_Imputados, MARGIN = 2, FUN =  shapiro.test) 
# el Margin = 2 significa que se aplica a columnas y FUN la funci�n
Normalidad_Shapiro # Buscamos los valores > 0.05 porque no tienen distribuci�n normal
# Siendo las variables que no tienen normalidad son:
# AC - NO3 - AO - CST - FEN - EF - Af - AT - AF - DAP - DBT - IE
# No se realiza test de homogeneidad de varianzas dado que ya sabemos que los datos
# se comportan de manera NO Param�trica

# Realizamos un PCA sobre los Datos_Cal_Imputados para encontrar las variables
# m�s relevantes sobre las especies y su condici�n de crecimiento
library(FactoMineR)
Datos_PCA <- PCA(X = Datos_Cal_Imputados, scale.unit = TRUE, ncp = 999, graph = TRUE)
# Extraemos la informaci�n sobre las variables y las observaciones del PCA
library(factoextra)
Info_PCA <- get_pca(Datos_PCA)
Info_PCA
summary(Datos_PCA) # Resultados del PCA

# Graficamos los eigenvalues o porcentages de explicaci�n de las variables sobre cada dimensi�n sobre
Plot_EigenValues <- fviz_screeplot(Datos_PCA, addlabels = TRUE) # Seleccionamos las 3 dimensiones m�s importantes
Plot_EigenValues
# Generamos un gr�fico biplot para ver las relaciones con las variables
# Valores de cos2 bajos o cercanos al centro son menos importantes para la correlaci�n
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
#  Visualizamos cu�les variables son las que m�s contribuyen a las dimensiones
# y todas aquellas que est�n sobre la media o l�nea roja son importantes
fviz_contrib(Datos_PCA, choice = "var", axes = 1, top = 10)
fviz_contrib(Datos_PCA, choice = "var", axes = 2, top = 10)
fviz_contrib(Datos_PCA, choice = "var", axes = 3, top = 10)
Info_PCA$contrib # Identificamos la contribuci�n de la variable a la dimensi�n

# Exportamos las gr�ficas en forma to PDF o tambi�n puede hacerse en "png()"
pdf("PCA_Full.pdf")
print(PCA_Full)
pdf("Plot_EigenValues.pdf")
print(Plot_EigenValues)
# En caso de que se genere un error en la exportaci�n se puede exportar desde
# el panel gr�fico en: save as PDF y en formato A4

# Realizamos un PerMANOVA para establecer la diferencia en las varianzas
# de aquellas variables importantes identificadas en el PCA
# Se realiza PERMANOVA porque es un an�lisis de la varianza para datos NO PARAM�TRICOS
library(vegan)
library(tidyverse)
Distance_Matrix <- vegdist(Datos_Cal_Imputados, method = "bray") # Se crea un matriz
# basada en distancias, dado que PerMANOVA lo requiere
Adonis_Run <- adonis(formula = Distance_Matrix ~ Datos_California$Especie*Datos_California$Condicion, 
       data = Datos_Cal_Imputados,  permutations = 999)
Adonis_Run
# En esta f�rmula comparamos la Especie con interacci�n de la Condici�n como covariable
# respecto a las distancias de agrupamiento de las variables con 999 permutaciones
# Los valores cr�ticos se�alan que la relaci�n no es por la interacci�n de las categor�as
# si no que cada una en s� misma es significativa, siendo el factor Especie determinante

# Ahora vamos a hacer las comparaciones con un post hoc test de Tukey
