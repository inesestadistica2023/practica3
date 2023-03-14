#Ejercicio 1: crea un vector llamado num_Artefactos con los valores indicados
numArtefactos <- c( 17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
#con la funcion 'c' podemos crear un vector y seleccionar todos los valores que queremos que contenga
class(numArtefactos) #con la función 'class' podemos saber si nuestros valores son enteros o dobles. 'numeric' con indica que son dobles. Vamos a pasarlos a enteros con 'as.integer'.
numArtefactos_int <- as.integer(numArtefactos)
class(numArtefactos_int) #ahora vemos que cambia. En vez de decirnos 'numeric' nos dice 'integer'

#Ejercicio 2: Calcula la media del objeto ‘numArtefactos_int’.
media_numArtefactos_int <- mean(numArtefactos_int)
media_numArtefactos_int
#La media es 45.55. con la función 'mean' podemos saber la media aritmética de los valores de nuestro objeto

#Ejercicio 3: Calcula la mediana del objeto ‘numArtefactos_int’. Define brevemente la mediana: concepto y cálculo
mediana_numArtefactos_int <- median(numArtefactos_int) #la función 'median es la que nos permite calcular la mediana del vector
mediana_numArtefactos_int
#La mediana es 40.5. La mediana de un vector es un valor que divide el conjunto de datos ordenados en dos partes iguales. Es decir, la mitad de los datos están por encima de la mediana y la otra mitad están por debajo de ella.
#Para calcular la mediana se ordena el vector de menor a mayor o de mayor a menor. Si el número es impar, la mediana es el valor central. Si es par, la mediana es la media de los dos valores centrales.

#Ejercicio 4: Calcula la moda del objeto ‘numArtefactos_int’. Explica detalladamente el procedimiento para su cálculo: empleo de funciones, operadores etc.
moda_numArtefactos_int <- names(sort(table(numArtefactos_int), decreasing = TRUE))[1]
moda_numArtefactos_int
#La moda es 10. Para calcular la moda de nuestro vector (valor que aparece con mayor frecuencia). Con la función 'table' calculamos la frecuencia de cada valor de nuestro vector. La función 'sort' ordena los valores de la tabla en orden descendente.
#La función names devuelve los nombres de los valores ordenados, y el [1] se utiliza para seleccionar el valor con mayor frecuencia (en nuestro caso es 10, que es el único valor que se repite).

#Ejercicio 5: Calcula el número de veces que se repite el valor correspondiente con la moda.
tabla_freq <- table(numArtefactos_int) #creamos una tabla de frecuencia para cada valor del objeto
max(tabla_freq) #Se repite 2 veces. con la función 'max' y la tabla de frecuencia podemos saber el número de veces que se repite la moda

#Ejercicio 6: Calcula los cuartiles del objeto ‘numArtefactos_int’.
cuartiles <- quantile(numArtefactos_int, probs = c(0.25, 0.5, 0.75)) #para calcular los cuartiles usaremos la función 'quantile'. 'probs' es un vector que contiene los percentiles que deseas calcular. Para calcular los cuartiles, establecemos probs en c(0.25, 0.5, 0.75), que corresponde a los percentiles 25, 50 y 75.
cuartiles #cuartiles: 21.5 (25%), 40.5 (50%), 61.5 (75%)

#Ejercicio 7: Calcula el rango intercuartílico del objeto ‘numArtefactos_int’. Interpreta el resultado
rango_intercual <- IQR(numArtefactos_int)
rango_intercual #El rango intercualitico es 40. para calcular el rango intercualitico usaremos la funcion 'IQR'. el rango intercualitico (IQR)es una medida de dispersión utilizada en estadística que se define como la diferencia entre el tercer cuartil y el primer cuartil de un conjunto de datos. Nos da información sobre la dispersión de los valores que se encuentran en la "mitad central" del conjunto de datos

#Ejercicio 8: Calcula el rango del objeto ‘numArtefactos_int’. Almacena el rango en un vector denominado ‘rango_artefactos’.
rango <- sum(numArtefactos_int != 0)
rango #El rango es 20. para calcular el rango de un vector (número de componentes no nulos), usaremos la función 'sum', para sumar los elementos del vector que no son 0.

#Ejercicio 9: Calcula la varianza del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo
varianza <- var(numArtefactos_int)
varianza #para calcular la varianza (medida de dispersión que indica cuánto varían los valores de un conjunto de datos con respecto a su media), usaremos la función 'var'. Esta función es la más sencilla pero también podemos hacerla a mano 
longitud <- length(numArtefactos_int) #primero debemos calcular la longitud del vector
varianza2 <- sum((numArtefactos_int - media_numArtefactos_int)^2) / (longitud - 1)
varianza2 #vemos que ambas dan el mismo resultado 927.1026

#Ejercicio 10: 10.Calcula la desviación estándar del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo.
desv_estand <- sd(numArtefactos_int) #para calcular la desviación estándar, usaremos la función 'sd'
desv_estand
longitud
desv_estans_2 <- sqrt(sum((numArtefactos_int - media_numArtefactos_int)^2) / (longitud - 1))
desv_estans_2 #otra manera de calcular la desviación estándar es hacerlo a mano usando la fórmula de desviación estándar, donde incluimos la media del vector y su longitud.
#ambos dan el mismo resultado: 30.44836

#Ejercicio 11: ¿En qué se diferencia la desviación estándar de la varianza?
#La varianza y la desviación estándar son dos medidas de dispersión utilizadas comúnmente en estadística para describir la variabilidad de un conjunto de datos.
#la principal diferencia entre la varianza y la desviación estándar es que la varianza se expresa en unidades al cuadrado, mientras que la desviación estándar se expresa en las mismas unidades que los datos originales. Además, la desviación estándar es una medida más intuitiva de la variabilidad de los datos porque se encuentra en la misma escala que los datos originales.

#Ejercicio 12: 12.Visualiza gráficamente de manera horizontal la dispersión del objeto ‘numArtefactos_int’.
boxplot(numArtefactos_int, horizontal = TRUE) #con 'bloxplot' podemos crear un gráfico de dispersión, y marcando 'horiz = TRUE' lo tendremos de manera horizontal

#Ejercicio 13: Crea un vector llamado ‘vector3’
vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

#Ejercicio 14: 14.Calcula el coeficiente de variación de los objetos: 1)‘numArtefactos_int’ y 2)‘vector3’. Emplea 2 funciones para su cálculo. Compara e interpreta los resultados.
desvest_numArtefactos_int <- sd(numArtefactos_int)#calcular la desviacion estandar
desvest_vector3 <- sd(vector3)
media_numArtefactos_int <- mean(numArtefactos_int)#calcular la media de los vectores
media_vector3 <- mean(vector3)
coefvar_numArtefactos_int <- (desvest_numArtefactos_int / media_numArtefactos_int) *100
coefvar_vector3 <- (desvest_vector3 / media_vector3) *100 #calcular el coeficiente de variación

#Ejercicio 15: Genera una tabla-resumen de los estadísticos descriptivos expuestos: media, mediana, desviación estándar etc.
summary(media_numArtefactos_int)
summary(media_vector3)
summary(mediana_numArtefactos_int)
summary(desvest_numArtefactos_int) #con la función 'summary' podemos obetner estadísticas descriptivas básicas

#Ejercicio 16: 
library(e1071) #cargamos el paquete
skewness(vector3) #con 'skewness' podemos calcular el coeficiente de asimetría del vector: 0.3138528
#Si el valor es positivo, significa que la distribución tiene una cola hacia la derecha (es decir, está sesgada hacia la derecha), mientras que si el valor es negativo, significa que la distribución tiene una cola hacia la izquierda (es decir, está sesgada hacia la izquierda). Un valor cercano a cero indica que la distribución es aproximadamente simétrica.

#Ejercicio 17: 
library(e1071) #cargamos el paquete
kurtosis(vector3) #con la función 'kurtosis' podemos calcular la curtosis el vector: -1.237981
#Un valor negativo indica que la distribución es platicúrtica (aplanada) en comparación con una distribución normal. Un valor positivo indica que la distribución es leptocúrtica (puntiaguda) en comparación con una distribución normal. Un valor cercano a cero indica que la distribución tiene una forma similar a una distribución normal. 

