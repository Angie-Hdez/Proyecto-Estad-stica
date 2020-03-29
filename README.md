# Proyecto-Estad-stica
#PROYECTO ESTADÍSTICA
##¿En qué tipo de sustrato (corteza o roca) los líquenes foliosos presentan mayor área?

#_______________
# conocer el directorio de trabajo 

getwd()

# escoger directorio de trabajo en donde se encuentre el documento con los datos 

setwd("C:/Users/Angie Hernandez/Documents/Programacion en R/")

## Leer los datos con la funcion read.csv()
liquen <- read.csv("liquen.csv")

## ver los datos 

head(liquen)

    Sitio    Sustrato área..cm2.
##1       1   saxicola        15.2
##2       1   saxicola        15.2
##3       1   saxicola        13.2
##4       1   saxicola         8.0
##5       1   saxicola         9.1
##6       1   saxicola         9.6
.....


View(liquen)

attach(liquen)


# Análisis visual e interpretacion de los datos 

# Gráficos de Q-Qplot 

qqPlot(liquen$área..cm2.[Sustrato=="saxicola "], main="SAXÍCOLA ", ylab= "Área (cm2)", xlab = "Cuantiles Norm")
[1] 10 28

![qqplotsax](https://user-images.githubusercontent.com/55472011/77813230-08d6c980-7075-11ea-836e-475c38e01ad7.png)

qqPlot(liquen$área..cm2.[Sustrato=="corticicola"],main="CORTICÍCOLA", ylab= "Área (cm2)", xlab = "Cuantiles Norm")
[1]  3 11

![qqplotcor](https://user-images.githubusercontent.com/55472011/77825667-c0e98e00-70d8-11ea-90e8-84e2d9e0dff6.png)

# Histogramas de frecuencia

hist(liquen$área..cm2.[Sustrato=="saxicola "], main="SAXÍCOLA", ylab= "Frecuencia", xlab = "Área (cm2)", col= "dodgerblue2") 

![histsax](https://user-images.githubusercontent.com/55472011/77825762-6d2b7480-70d9-11ea-9399-5bd48cac441b.png)


hist(liquen$área..cm2.[Sustrato=="corticicola"], main="CORTICÍCOLA", ylab= "Frecuencia", xlab = "Área (cm2)", col= "dodgerblue2")

![histcor](https://user-images.githubusercontent.com/55472011/77825765-73b9ec00-70d9-11ea-8a33-5ee819eef07a.png)

## Asimetría y curtosis 

# líquenes corticícolas 

skewness(liquen$área..cm2.[Sustrato=="corticicola"])
kurtosis(liquen$área..cm2.[Sustrato=="corticicola"])

plot(density(liquen$área..cm2.[Sustrato=="corticicola"]), main= 'CORTICÍCOLA', ylab = "Densidad", col = "RED", lwd = 3)
abline(v= mean(liquen$área..cm2.[Sustrato=="corticicola"]), col = "blue")

![plotdencor](https://user-images.githubusercontent.com/55472011/77826034-0018de80-70db-11ea-85c6-fff3196c9cc1.png)

summary(liquen$área..cm2.[Sustrato=="corticicola"])

 ##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 #   7.1    11.9    14.5    15.8    18.9    37.4 

# líquenes saxícolas 

skewness(liquen$área..cm2.[Sustrato=="saxicola "])
kurtosis(liquen$área..cm2.[Sustrato=="saxicola "])


plot(density(liquen$área..cm2.[Sustrato=="saxicola "]), main= 'SAXÍCOLA', ylab = "Densidad", col = "RED", lwd = 3)
abline(v= mean(liquen$área..cm2.[Sustrato=="saxicola "]), col = "blue")

![plotdenssax](https://user-images.githubusercontent.com/55472011/77826038-073fec80-70db-11ea-85d4-b4df4f185401.png)

summary(liquen$área..cm2.[Sustrato=="saxicola "])

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.90    7.50    9.60   10.53   12.60   21.20 
   
#Revisando los supuestos de normalidad

# Ho: Los datos siguen una Distribucion Normal 

by(área..cm2.,Sustrato,lillie.test)
Sustrato: corticicola

	Lilliefors (Kolmogorov-Smirnov) normality test

data:  dd[x, ]
D = 0.1391, p-value = 0.001834

------------------------------------------------------------------- 
Sustrato: saxicola 

	Lilliefors (Kolmogorov-Smirnov) normality test

data:  dd[x, ]
D = 0.12573, p-value = 0.007936

## por separado cada sustrato 

lillie.test(liquen$área..cm2.[Sustrato=="corticicola"])

Lilliefors (Kolmogorov-Smirnov) normality test

data:  liquen$área..cm2.[Sustrato == "corticicola"]
D = 0.10785, p-value = 0.04235

lillie.test(liquen$área..cm2.[Sustrato=="saxicola "])

Lilliefors (Kolmogorov-Smirnov) normality test

data:  liquen$área..cm2.[Sustrato == "saxicola "]
D = 0.12573, p-value = 0.007936

##  P-value <0.05 por lo tanto los datos en ambos sustratos no siguen una distribucion normal

# Revisando la Homogeneidad de varianzas 

library("car")

## Ho: las varianzas de los dos grupos son homogeneneas

leveneTest(liquen$área..cm2. ~ liquen$Sustrato,center=mean)
Levene's Test for Homogeneity of Variance (center = mean)
       Df F value   Pr(>F)   
group   1  7.8308 0.005872 **
      138                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

leveneTest(liquen$área..cm2. ~ liquen$Sustrato,center=median)

Levene's Test for Homogeneity of Variance (center = median)
       Df F value  Pr(>F)  
group   1  6.5534 0.01154 *
      138                  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##  P-value< 0.05 por lo tanto no hay homogeneidad de varianzas 

#Hacer transformación de las variables (los datos no siguen una distribución normal)

# Transformacion de datos para sustrato corteza
T_log = log(liquen$área..cm2.[Sustrato=="corticicola"])

# transformacion de datos para sustrato roca
T_log = log(liquen$área..cm2.[Sustrato=="saxicola "])

# Evaluando la normalidad luego de la transformación 

lillie.test(log(liquen$área..cm2.[Sustrato=="corticicola"]))

Lilliefors (Kolmogorov-Smirnov) normality test

data:  log(liquen$área..cm2.[Sustrato == "corticicola"])
D = 0.067433, p-value = 0.5992

##  P-value>O.O5 se asume normalidad

qqPlot(log(liquen$área..cm2.[Sustrato=="saxicola "]), main="CORTICÍCOLA", ylab= "Área (cm2)", xlab = "Cuantiles Norm")

![qqtranscot](https://user-images.githubusercontent.com/55472011/77826116-89c8ac00-70db-11ea-84d3-7f15024933c1.png)


lillie.test(log(liquen$área..cm2.[Sustrato=="saxicola "]))

Lilliefors (Kolmogorov-Smirnov) normality test

data:  log(liquen$área..cm2.[Sustrato == "saxicola "])
D = 0.086682, p-value = 0.2155
## P-value>O.O5 se asume normalidad

qqPlot(log(liquen$área..cm2.[Sustrato=="saxicola "]), main="SAXÍCOLA ", ylab= "Área (cm2)", xlab = "Cuantiles Norm")

![qqtranssax](https://user-images.githubusercontent.com/55472011/77826120-8e8d6000-70db-11ea-9b0c-072904cb5b45.png)


# Evaluando Homgeneidad de varianzas después de transformacion de los datos 

leveneTest(log(área..cm2.+1) ~Sustrato,center=mean)

Levene's Test for Homogeneity of Variance (center = mean)
       Df F value Pr(>F)
group   1  0.3319 0.5655
      138 
      
leveneTest(log(área..cm2.+1) ~Sustrato,center=median)

Levene's Test for Homogeneity of Variance (center = median)
       Df F value Pr(>F)
group   1  0.3842 0.5364
      138 

## P-value>O.O5 se asume homogeneidad de varianzas 

# Después de la transformación logarítmica, los datos siguen los supuestos de normalidad y homogeneidad 
# de varianzas, se puede realizar la prueba de T-test. 

# prueba T-test

# Ho: El tamaño promedio (área) de los líquenes cortícicolas es menor o igual que el tamaño promedio (área) de los saxícolas 
# Ha: El tamaño promedio (área) de los líquenes cortícicolas es mayor que el tamaño promedio (área)de los saxícolas 

t.test(log(liquen$área..cm2.[Sustrato=="corticicola"]),log(liquen$área..cm2.[Sustrato=="saxicola "]), alternative = "greater")

Welch Two Sample t-test

data:  log(liquen$área..cm2.[Sustrato == "corticicola"]) and log(liquen$área..cm2.[Sustrato == "saxicola "])
t = 6.6764, df = 137.86, p-value = 2.759e-10
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
 0.302226      Inf
sample estimates:
mean of x mean of y 
 2.695425  2.293510 
 
 ## P-value < O.O5 no se acepta la hipotesís nula (Ho)
 
 #Gráficas para la distribucion de los datos 

 #Histograma con líneas de densidad, media y mediana para sustrato "roca"

hist(liquen$área..cm2.[Sustrato=="saxicola "],prob= T,main="SAXÍCOLA", ylab= "Frecuencia", xlab = "Área (cm2)", col= "gold1")
lines(density(liquen$área..cm2.[Sustrato=="saxicola "]),lwd= 2,col= "red")

abline(v = mean(liquen$área..cm2.[Sustrato=="saxicola "]),col = "green4",lwd = 2)
abline(v = median(liquen$área..cm2.[Sustrato=="saxicola "]),col = "blue",lwd = 2)
legend(x = "topright",c("Density plot", "Mean", "Median"),col = c("red", "green4", "blue"),lwd = c(2, 2, 2))


![arrreglado](https://user-images.githubusercontent.com/55472011/77840072-a9e28480-7148-11ea-93bc-a3ca588778ce.png)


#Histograma con líneas de densidad, media y mediana para sustrato "corteza"

hist(liquen$área..cm2.[Sustrato=="corticicola"],prob= T,main="CORTICÍCOLA", ylab= "Frecuencia", xlab = "Área (cm2)", col= "chartreuse2")
lines(density(liquen$área..cm2.[Sustrato=="corticicola"]),lwd= 2,col= "red")

abline(v = mean(liquen$área..cm2.[Sustrato=="corticicola"]),col = "green4",lwd = 2)
abline(v = median(liquen$área..cm2.[Sustrato=="corticicola"]),col = "blue",lwd = 2)
legend(x = "topright",c("Density plot", "Mean", "Median"),col = c("red", "green4", "blue"),lwd = c(2, 2, 2))

![corarre](https://user-images.githubusercontent.com/55472011/77840077-b2d35600-7148-11ea-960c-daa41f1fe4dc.png)


## Presentando los resultados en un boxplot

boxplot(liquen$área..cm2.~liquen$Sustrato, col= c("chartreuse2", "gold1"), names= c("Corticícola","Saxícola"), main="Tamaño de líquenes foliosos",xlab= "Sustrato",ylab= "Área (cm2)")
mtext(x = 1.1,y=20, "t = 6.6764, df = 137.86, p-value < 0.05 ",side=3)

![boxplotliquenes](https://user-images.githubusercontent.com/55472011/77826263-59354200-70dc-11ea-9162-caa47986b3c4.png)

## De acuerdo con los resultados obtenidos de la prueba T-test se concluye que la media del área de los líquenes foliosos corticícolas 
## es mayor que el área de los líquenes foliosos saxícolas.
