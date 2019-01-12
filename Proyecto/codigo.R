# Lo primero que tengo que hacer es importar el dataset que he creado

dataset <- read.csv("C:/Users/jorge/Desktop/Documentos Clase/Universidad/4ºCarrera/1er Cuatrimestre/Inteligencia Artificial/Trabajo Fin de Asignatura/datos.txt", header = TRUE)

# Ahora lo que hago es pasarlo a una matriz, quitando tanto el nombre (que no me interesa) como la etiqueta (que no la necesito por ahora)

matriz.pacientes.etiquetas <- dataset[, -1]
matriz.pacientes.datos <- matriz.pacientes.etiquetas[, -25]

# Primero compruebo que todos los datos tienen un tupo correcto.

sapply(matriz.pacientes.datos, class)

# Veo la media de la edad de los pacientes y el rango en el que se mueve

mean(matriz.pacientes.datos[, 1])
range(matriz.pacientes.datos[, 1])

# Finalmente, veo un resúmen de cada columna

summary(matriz.pacientes.datos)

# Como se puede ver, los datos de los pacientes están muy distanciados, y además su media es muy alta.
# Así, la media de la edad difiere enormemente del resto de valores de la matriz.
# Debido a ello, debemos de hacer un preprocesado de los datos del problema.

# Antes que este preprocesado,voy a hacer la visualización de algunas relaciones entre variables, de tal manera que podamos ver
# gráficamente algunos aspectos interesantes:


# Ahora voy a sacar un plot para ver la relación entre la edad y el sexo de las personas que están en consulta

plot(matriz.pacientes.datos[,1], matriz.pacientes.datos[,2], xlab="Edad", ylab="Sexo (0 - mujer, 1 - hombre)", main="Edad & Sexo");

# Otro plot para ver la correlación entre ser agresivo y ser impulsivo

#install.packages("hexbin")
#install.packages("RColorBrewer")

library(hexbin)
library(RColorBrewer)

rf <- colorRampPalette(rev(brewer.pal(4,'Spectral')))
df <- data.frame(matriz.pacientes.datos[, 23], matriz.pacientes.datos[, 24])
h <- hexbin(df)
plot(h, colramp=rf, xlab="Agresivo", ylab="Impulsivo", main="Agresivo Vs Impulsivo")
axis(1, at=1:10)

# Otro plot similar para ver la relación de ser inhibido e impulsivo

df <- data.frame(matriz.pacientes.datos[, 21], matriz.pacientes.datos[, 24])
h <- hexbin(df)
plot(h, colramp=rf, xlab="Inhibido", ylab="Impulsivo", main="Inhibido Vs Impulsivo")

# Voy a ver la relación entre el razonamiento emocional (actuar según tus sentimientos) y la impulsividad

df <- data.frame(matriz.pacientes.datos[, 20], matriz.pacientes.datos[, 24])
h <- hexbin(df)
plot(h, colramp=rf, xlab="Razonamiento Emocional", ylab="Impulsivo", main="Razonamiento Emocional Vs Impulsivo")

# Ahora quiero sacar una relación entre ser agresivo y ver el grupo en el que están

rf <- colorRampPalette(rev(brewer.pal(4,'Spectral')))
df <- data.frame(matriz.pacientes.datos[, 23], matriz.pacientes.etiquetas[, 25])
h <- hexbin(df)
plot(h, colramp=rf, xlab="Agresivo", ylab="Grupo", main="Agresivo Y Grupo Real")

# Voy a hacer lo mismo con la impulsividad

rf <- colorRampPalette(rev(brewer.pal(4,'Spectral')))
df <- data.frame(matriz.pacientes.datos[, 24], matriz.pacientes.etiquetas[, 25])
h <- hexbin(df)
plot(h, colramp=rf, xlab="Impulsivo", ylab="Grupo", main="Impulsivo y Grupo Real")

# De estas gráficas estamos obteniendo información realmente interesante antes de la predicción de los datos.
# He preferido hacer gráficas en 2D porque las gráficas en 3D son mucho más difíciles de interpretar que estas bonitas gráficas en 2D

# Vamos a ver la correlación que tienen mis variables 

res <- cor(matriz.pacientes.datos[, 1:24], method = "spearman") # Por mi tipo de datos, hacemos la correlación por spearman
options(width = 100)
res.round <- round(res, 2)

# Como saca una tabla enorme, lo que voy a hacer es usar una librería que me da una función para
# sacar de una forma bonita las correlaciones entre las variables.

#install.packages("corrplot")
library(corrplot)
corrplot(res.round, method="circle")

# Como podemos ver, por ejemplo, resiliencia baja y media tienen una correlación de -1, ya que si hay una no hay la otra y viceversa.
# Esto pasa igual con las relaciones entre contexto, ya que buena - trauma, trauma - mala, mala - buena tienen que ser inversas.


# Como he comentado antes, Lo que voy a hacer ahora es un centrado y escalado de los datos de la matriz. De esta manera, la red neuronal no tendrá
# ningún valor que destaque especialmente y con ello no dará de inicio más peso a unos valores que a otros, ya que no lo buscamos.

#install.packages("caret")
library(caret)

preObjeto <- preProcess(matriz.pacientes.datos, method=c("center", "scale"))  # Quiero hacer un centrado y escalado
matriz.pacientes.datos.centscal <- predict(preObjeto, matriz.pacientes.datos) # Obtengo los valores en la matriz centscal

#install.packages("nnet")
library(nnet)

# Ahora lo que hago es coger un conjunto muy grande de los datos para hacer el entrenamiento

conjuntoEntrenamiento <- sample(1:67, 55)




###############################################################
#################### 1 NEURONA ###############################
###############################################################

# Lo que voy a hacer ahora es entrenar la red neuronal con diferente cantidad de neuronas,y voy a ir comparando el resultado...

###################### SIN SOFTMAX #############################

pacientes.1neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=1 )


# Lo voy a entrenar también con el SOFTMAX = true. Esto optimiza la verosimilitud, no el error cuadrático medio...
###################### CON SOFTMAX ##############################

pacientes.1neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=1, softmax = T )

######################

# Una vez que lo tengo entrenado, lo que voy a hacer es calcular el error tanto en el entrenamiento como en el test de cada uno

pacientes.prediccion.1neu <- predict( pacientes.1neu, matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.1neu # Vemos las probabilidades de pertenencia de cada valor

#Ahora que los tengo todos entrenados, Determinamos cual es la máxima, es decir, la clase a la que hay que asignar los objetos

pacientes.prediccion.1neu.class <- apply( pacientes.prediccion.1neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.1neu.class

#Lo visualizo en forma de tabla para ir viendo el error

table( pacientes.prediccion.1neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] )  # Lo vemos en forma de tabla.

# Calculo el acierto

sum( diag( table( pacientes.prediccion.1neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) ) )/55 # Esta cuenta nos da el índice de acierto

#### TEST

pacientes.prediccion.test.1neu <- predict( pacientes.1neu, matriz.pacientes.datos.centscal[-conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.test.1neu

pacientes.prediccion.test.1neu.class <- apply( pacientes.prediccion.test.1neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.test.1neu.class

table( pacientes.prediccion.test.1neu.class , matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] )
sum( diag( table( pacientes.prediccion.test.1neu.class, matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] ) ) )/12




###############################################################
#################### 2 NEURONAS ###############################
###############################################################

# Lo que voy a hacer ahora es entrenar la red neuronal con diferente cantidad de neuronas,y voy a ir comparando el resultado...

###################### SIN SOFTMAX #############################

pacientes.2neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=2 )


# Lo voy a entrenar también con el SOFTMAX = true. Esto optimiza la verosimilitud, no el error cuadrático medio...
###################### CON SOFTMAX ##############################

pacientes.2neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=2, softmax = T )

######################

# Una vez que lo tengo entrenado, lo que voy a hacer es calcular el error tanto en el entrenamiento como en el test de cada uno

pacientes.prediccion.2neu <- predict( pacientes.2neu, matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.2neu # Vemos las probabilidades de pertenencia de cada valor

#Ahora que los tengo todos entrenados, Determinamos cual es la máxima, es decir, la clase a la que hay que asignar los objetos

pacientes.prediccion.2neu.class <- apply( pacientes.prediccion.2neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.2neu.class

#Lo visualizo en forma de tabla para ir viendo el error

table( pacientes.prediccion.2neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] )  # Lo vemos en forma de tabla.

# Calculo el acierto

sum( diag( table( pacientes.prediccion.2neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) ) )/55 # Esta cuenta nos da el índice de acierto

#### TEST

pacientes.prediccion.test.2neu <- predict( pacientes.2neu, matriz.pacientes.datos.centscal[-conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.test.2neu

pacientes.prediccion.test.2neu.class <- apply( pacientes.prediccion.test.2neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.test.2neu.class

table( pacientes.prediccion.test.2neu.class , matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] )
sum( diag( table( pacientes.prediccion.test.2neu.class, matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] ) ) )/12






###############################################################
#################### 3 NEURONAS ###############################
###############################################################

# Lo que voy a hacer ahora es entrenar la red neuronal con diferente cantidad de neuronas,y voy a ir comparando el resultado...

###################### SIN SOFTMAX #############################

pacientes.3neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=3)


# Lo voy a entrenar también con el SOFTMAX = true. Esto optimiza la verosimilitud, no el error cuadrático medio...
###################### CON SOFTMAX ##############################

pacientes.3neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=3, softmax = T)

######################

# Una vez que lo tengo entrenado, lo que voy a hacer es calcular el error tanto en el entrenamiento como en el test de cada uno

pacientes.prediccion.3neu <- predict( pacientes.3neu, matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.3neu # Vemos las probabilidades de pertenencia de cada valor

#Ahora que los tengo todos entrenados, Determinamos cual es la máxima, es decir, la clase a la que hay que asignar los objetos

pacientes.prediccion.3neu.class <- apply( pacientes.prediccion.3neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.3neu.class

#Lo visualizo en forma de tabla para ir viendo el error

table( pacientes.prediccion.3neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] )  # Lo vemos en forma de tabla.

# Calculo el acierto

sum( diag( table( pacientes.prediccion.3neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) ) )/55 # Esta cuenta nos da el índice de acierto

#### TEST

pacientes.prediccion.test.3neu <- predict( pacientes.3neu, matriz.pacientes.datos.centscal[-conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.test.3neu

pacientes.prediccion.test.3neu.class <- apply( pacientes.prediccion.test.3neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.test.3neu.class

table( pacientes.prediccion.test.3neu.class , matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] )
sum( diag( table( pacientes.prediccion.test.3neu.class, matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] ) ) )/12







###############################################################
#################### 3 NEURONAS ###############################
###################  Con Decay  ###############################
###############################################################

# Lo que voy a hacer ahora es entrenar la red neuronal con diferente cantidad de neuronas,y voy a ir comparando el resultado...

###################### SIN SOFTMAX #############################

pacientes.3neu.decay <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=3, decay = 0.2)


# Lo voy a entrenar también con el SOFTMAX = true. Esto optimiza la verosimilitud, no el error cuadrático medio...
###################### CON SOFTMAX ##############################

#pacientes.3neu.decay <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=3, softmax = T, decay = 0.03)

######################

# Una vez que lo tengo entrenado, lo que voy a hacer es calcular el error tanto en el entrenamiento como en el test de cada uno

pacientes.prediccion.3neu.decay <- predict( pacientes.3neu.decay, matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.3neu.decay # Vemos las probabilidades de pertenencia de cada valor

#Ahora que los tengo todos entrenados, Determinamos cual es la máxima, es decir, la clase a la que hay que asignar los objetos

pacientes.prediccion.3neu.class.decay <- apply( pacientes.prediccion.3neu.decay, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.3neu.class.decay

#Lo visualizo en forma de tabla para ir viendo el error (correlación)

table( pacientes.prediccion.3neu.class.decay, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] )  # Lo vemos en forma de tabla.

# Calculo el acierto

sum( diag( table( pacientes.prediccion.3neu.class.decay, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) ) )/55 # Esta cuenta nos da el índice de acierto

#### TEST

pacientes.prediccion.test.3neu.decay <- predict( pacientes.3neu.decay, matriz.pacientes.datos.centscal[-conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.test.3neu.decay

pacientes.prediccion.test.3neu.class.decay <- apply( pacientes.prediccion.test.3neu.decay, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.test.3neu.class.decay

table( pacientes.prediccion.test.3neu.class.decay , matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] )
sum( diag( table( pacientes.prediccion.test.3neu.class.decay, matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] ) ) )/12








###############################################################
#################### 5 NEURONAS ###############################
###############################################################

# Lo que voy a hacer ahora es entrenar la red neuronal con diferente cantidad de neuronas,y voy a ir comparando el resultado...

###################### SIN SOFTMAX #############################

pacientes.5neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=5 )


# Lo voy a entrenar también con el SOFTMAX = true. Esto optimiza la verosimilitud, no el error cuadrático medio...
###################### CON SOFTMAX ##############################

pacientes.5neu <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=5, softmax = T )

######################

# Una vez que lo tengo entrenado, lo que voy a hacer es calcular el error tanto en el entrenamiento como en el test de cada uno

pacientes.prediccion.5neu <- predict( pacientes.5neu, matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.5neu # Vemos las probabilidades de pertenencia de cada valor

#Ahora que los tengo todos entrenados, Determinamos cual es la máxima, es decir, la clase a la que hay que asignar los objetos

pacientes.prediccion.5neu.class <- apply( pacientes.prediccion.5neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.5neu.class

#Lo visualizo en forma de tabla para ir viendo el error

table( pacientes.prediccion.5neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] )  # Lo vemos en forma de tabla.

# Calculo el acierto

sum( diag( table( pacientes.prediccion.5neu.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) ) )/55 # Esta cuenta nos da el índice de acierto

#### TEST

pacientes.prediccion.test.5neu <- predict( pacientes.5neu, matriz.pacientes.datos.centscal[-conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.test.5neu

pacientes.prediccion.test.5neu.class <- apply( pacientes.prediccion.test.5neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.test.5neu.class

table( pacientes.prediccion.test.5neu.class , matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] )
sum( diag( table( pacientes.prediccion.test.5neu.class, matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] ) ) )/12






###############################################################
#################### 5 NEURONAS ###############################
#################### CON DECAY  ###############################
###############################################################

# Lo que voy a hacer ahora es entrenar la red neuronal con diferente cantidad de neuronas,y voy a ir comparando el resultado...

###################### SIN SOFTMAX #############################

pacientes.5neu.decay <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=5, decay=0.1)


# Lo voy a entrenar también con el SOFTMAX = true. Esto optimiza la verosimilitud, no el error cuadrático medio...
###################### CON SOFTMAX ##############################

pacientes.5neu.decay <- nnet( matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], class.ind( matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) , size=5, softmax = T, decay = 0.05)

######################

# Una vez que lo tengo entrenado, lo que voy a hacer es calcular el error tanto en el entrenamiento como en el test de cada uno

pacientes.prediccion.5neu.decay <- predict( pacientes.5neu.decay, matriz.pacientes.datos.centscal[conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.5neu.decay # Vemos las probabilidades de pertenencia de cada valor

#Ahora que los tengo todos entrenados, Determinamos cual es la máxima, es decir, la clase a la que hay que asignar los objetos

pacientes.prediccion.5neu.decay.class <- apply( pacientes.prediccion.5neu.decay, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.5neu.decay.class

#Lo visualizo en forma de tabla para ir viendo el error

table( pacientes.prediccion.5neu.decay.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] )  # Lo vemos en forma de tabla.

# Calculo el acierto

sum( diag( table( pacientes.prediccion.5neu.decay.class, matriz.pacientes.etiquetas[conjuntoEntrenamiento, 25] ) ) )/55 # Esta cuenta nos da el índice de acierto

#### TEST

pacientes.prediccion.test.decay.5neu <- predict( pacientes.5neu.decay, matriz.pacientes.datos.centscal[-conjuntoEntrenamiento, 1:24], type="raw" )
pacientes.prediccion.test.decay.5neu

pacientes.prediccion.test.decay.5neu.class <- apply( pacientes.prediccion.test.decay.5neu, MARGIN=1, FUN='which.is.max')
pacientes.prediccion.test.decay.5neu.class

table( pacientes.prediccion.test.decay.5neu.class , matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] )
sum( diag( table( pacientes.prediccion.test.decay.5neu.class, matriz.pacientes.etiquetas[-conjuntoEntrenamiento, 25] ) ) )/12





### Importo los datos de los resultados finales

dataset.resultados <- read.csv2("C:/Users/jorge/Desktop/Documentos Clase/Universidad/4ºCarrera/1er Cuatrimestre/Inteligencia Artificial/Trabajo Fin de Asignatura/Resultados.txt")

# Ahora voy a sacar un gráfico donde comparo los resultados.

#install.packages("plotly")
library("plotly")

tipos = dataset.resultados[, 1]
real = dataset.resultados[, 2]
practico = dataset.resultados[, 3]

p<- plot_ly(dataset.resultados, x = ~tipos, y = ~real, type = 'bar', name = 'Real') %>% add_trace(y = ~practico, name = 'Práctico') %>% layout(yaxis = list(title = 'Porcentaje'), barmode = 'group')

p 

#Mostramos el gráfico interactivo




######################## Ahora vamos a hacer la predicción mediante KNN

#install.packages("class")
library("class")

# Para hacer la predicción con knn, voy a coger los grupos de una manera distinta:

conjuntoEntrenamiento = matriz.pacientes.datos.centscal[1:55, 1:24]
conjuntoTest = matriz.pacientes.datos.centscal[56:67, 1:24]
# Utilizo por supuesto la matriz de centrado y escalado

etiquetasEntrenamiento = matriz.pacientes.etiquetas[1:55, 25]
etiquetasTest = matriz.pacientes.etiquetas[56:67, 25]

conjuntoEntrenamiento
conjuntoTest
etiquetasEntrenamiento
etiquetasTest

# K = 8

prediccion.knn.8 <- knn(train = conjuntoEntrenamiento, test = conjuntoTest, cl = etiquetasEntrenamiento, k = 8)
prediccion.knn.8


#install.packages("gmodels")
library("gmodels")

CrossTable(x = etiquetasTest , y = prediccion.knn.8, prop.chisq = FALSE)


# K = 6

prediccion.knn.6 <- knn(train = conjuntoEntrenamiento, test = conjuntoTest, cl = etiquetasEntrenamiento, k = 6)
prediccion.knn.6


CrossTable(x = etiquetasTest , y = prediccion.knn.6, prop.chisq = FALSE)


# K = 10

prediccion.knn.10 <- knn(train = conjuntoEntrenamiento, test = conjuntoTest, cl = etiquetasEntrenamiento, k = 10)
prediccion.knn.10


CrossTable(x = etiquetasTest , y = prediccion.knn.10, prop.chisq = FALSE)



## Como se puede observar, la mejor predicción la hemos hecho con K = 8

