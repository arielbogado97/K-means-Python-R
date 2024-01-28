#Instalamos librerias necesarias
install.packages("tidyverse") 
install.packages("corrplot")
install.packages("gridExtra")
install.packages("GGally")
install.packages("cluster")
install.packages("factoextra")
install.packages("openxlsx")

#Importamos librerías instaladas
library(tidyverse) 
library(corrplot)
library(gridExtra)
library(GGally)
library(cluster)
library(factoextra)
library(openxlsx)
library(ggplot2)


#Cargamos los datos
setwd("C:/Users/ariel/Desktop/Data Sciencts/Machine Learning A-Z (Codes and Datasets)/Part 4 - Clustering/Section 24 - K-Means Clustering/Python")
df <- read.xlsx("Mall_CustomersR.xlsx")
head(df)


#Estadísticos principales
summary(df)


#Matriz de correlación para comprender la relación entre las variables
corrplot(cor(df), type = 'upper', method = 'number', tl.cex = 0.9)

#Se observa que a priori no habría correlación entre las variables


#Dado que las variables están en escalas diferentes, necesitamos escalar los datos o normalizarlos.
dfNorm <- as.data.frame(scale(df))
head(dfNorm)

.01#Estadísticos principales de los datos ya normalizados
summary(dfNorm)


#Agrupamos los datos en dos grupos (centros = 2)
df_Kmedias <- kmeans(dfNorm, centers = 6, nstart = 25)
print(df_Kmedias)


#Visualización de los clúster
fviz_cluster(df_Kmedias, data = dfNorm)

#Vemos 
names(df_Kmedias)


#Definimos varios K para analizarlos mejor
df_K2 <- kmeans(dfNorm, centers = 2, nstart = 25)
df_K3 <- kmeans(dfNorm, centers = 3, nstart = 25)
df_K4 <- kmeans(dfNorm, centers = 4, nstart = 25)
df_K5 <- kmeans(dfNorm, centers = 5, nstart = 25)

#Graficamos
p1 <- fviz_cluster(df_K2, geom = "point", data = dfNorm) + ggtitle(" K = 2")
p2 <- fviz_cluster(df_K3, geom = "point", data = dfNorm) + ggtitle(" K = 3")
p3 <- fviz_cluster(df_K4, geom = "point", data = dfNorm) + ggtitle(" K = 4")
p4 <- fviz_cluster(df_K5, geom = "point", data = dfNorm) + ggtitle(" K = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)


#Requererimos de un gráfico que nos muestre las sumas de cuadrados totales en el conglomerado k
fviz_nbclust(x = dfNorm,FUNcluster = kmeans, method = 'wss' )

#Determinando el número óptimo de clúster usando Average Silhouette Method:
fviz_nbclust(x = dfNorm, FUNcluster = kmeans, method = 'silhouette' )


#Número óptimo, a partir del estadístico GAP:
gap_stat <- clusGap(x = dfNorm, FUN = kmeans, K.max = 15, nstart = 25, B = 50 )
fviz_gap_stat(gap_stat)

#Através del analisis estadístico de GAP, nos arroja que el numero óptimo de cluster es el 6
Cluster_final <- kmeans(dfNorm, centers = 6, nstart = 25)
print(Cluster_final)


#
fviz_cluster(Cluster_final, data = dfNorm)

#Conclusión
# podemos concluir que k=6 es el número óptimo de clusters.



