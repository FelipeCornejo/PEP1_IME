#Nombres:
#Estefan�a Alvarez 20.371.287-1
#Felipe Cornejo 20.427.782-6
#David Morales 19.881.480-6
#Claudio Mu�oz 20.003.395-7

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)
library(pwr)
library (tidyverse)

# ////////////////////////// PREGUNTA 1 //////////////////////////

#Se cree que la ardilla chilena (Octodon degus) emite un chillido cuando es 
#perseguido por un depredador, posiblemente para alertar a otros deg�s. Se hizo 
#un experimento en que 45 deg�s se liberaron a 10 o a 100 metros de su madriguera
#y luego se les persegu�a hasta que se met�an en ella, para simular la persecuci�n 
#de un depredador. De las 24 ardillas liberadas a 10 metros de la madriguera, 16
#emitieron el chillido esperado, mientras que solo 3 de  las 21 ardillas liberadas
#a 100 metros de la madriguera lo hicieron.�Influye la distancia a la madriguera
#en la emisi�n del chillido por parte de un deg�?

#Hip�tesis:
#H0: Las variables de emisi�n de chillidos por los deg�s y la distancia a la que 
#fueron liberados de su madriguera son independientes

#HA: Las variables de emisi�n de chillidos por los deg�s y la distancia a la que 
#fueron liberados de su madriguera son independientes est�n relacionadas


#Se construye la tabla con los valores de las variables
chillido_10 <- c(16, 8)
chillido_100 <- c(3, 18)
tabla <- as.table(rbind(chillido_10, chillido_100))


dimnames(tabla) <- list(c("Chilla", "No Chilla"),
                        c("10 metros", "100 metros"))


print(tabla)                        

# Hacer prueba chi -cuadrado de independencia.
prueba <- chisq.test(tabla)
cat("\nLa prueba internamente calcula los valores esperados :\n")
esperados <- round(prueba [["expected"]], 1)
print(esperados)
cat("\nResultado de la prueba :\n")
print(prueba)                  

#R: Utilizando la prueba chi-cuadrado de independencia, el valor p resultante es
#de 0.001167, que aun tomando un nivel de significancia muy exigente como 0,01,
#el valor p sigue siendo menor, por lo que esto nos permite tener evidencia suficiente
#para rechazar la hip�tesis nula en favor de la hip�tesis alternativa, concluyendo 
#as� que las variables de la emisi�n de chillido por los deg� y la distancia a la 
#que fueron liberados de su madriguera est�n relacionadas.


# ////////////////////////// PREGUNTA 2 //////////////////////////

#Un art�culo describe un estudio en que se compararon diferentes versiones de 
#algoritmos evolutivos para resolver variadas instancias de problemas de 
#clasificaci�n tomadas desde el UCI Machine Learning Repository. La siguiente 
#tabla muestra los resultados de la clasificaci�n (COR: correcta, INC: incorrecta) 
#hecha por dos versiones de un algoritmo gen�tico evaluado en el estudio para el 
#problema Breast Cancer. �Hay un algoritmo con mejor desempe�o que el otro?


#Formulaci�n de Hip�tesis
#H0: no hay cambios significativos entre las versiones de algoritmos evolutivos.
#HA: s� hay cambios significativos entre las versiones de algoritmos evolutivos.

# Construir la tabla de contingencia.
Instancias <- seq (1:14)
AGv1 <- c("Incorrecta", "Correcta", "Incorrecta", rep("Correcta", 2), rep("Incorrecta", 2), rep("Correcta", 3), "Incorrecta", "Correcta", rep("Incorrecta", 2))
AGv2 <- c(rep("Correcta", 4), rep("Incorrecta", 2),rep("Correcta", 2), rep("Incorrecta", 2),"Correcta","Incorrecta",rep("Correcta", 2))
datos <- data.frame(Instancias , AGv2, AGv1)
tabla <- table(AGv2, AGv1)
print(tabla)

# Aplicar prueba de McNemar.
prueba <- mcnemar.test(tabla)
print(prueba)

#Como el valor p obtenido es igual a 0.7518 y considerando un nivel de significancia 
#de 0.05 no existe suficiente evidencia para rechazar la hip�tesis nula en favor de la
#hipotesis alternativa. Por lo tanto no se concluir que un algoritmo sea mejor que otro.



# ////////////////////////// PREGUNTA 3 //////////////////////////

#Una investigaci�n monitore� a m�s de 50 mil mujeres adultas durante 10 a�os (Lucas et al., 2011. Coffee,
#Caffeine, and Risk of Depression Among Women. Archives of Internal Medicine, 171(17), 1571-1578) con
#la intenci�n de identificar factores de riesgo de desarrollar un cuadro de depresi�n. Entre otras cosas, se
#registr� el consumo de cafe�na, cuyos datos se resumen en la siguiente tabla. �Existe alguna asociaci�n
#entre la cantidad de caf� que se bebe y la depresi�n?

#Formulcaci�n de Hip�tesis
#H0: Las variables de depresi�n y la cantidad de caf� que se bebe no est�n relacionadas, es decir son independientes.
#HA: Las variables de depresi�n y la cantidad de caf� que se bebe si est�n relacionadas.

#Se construye la tabla con los valores de las variables
depresion_si <- c(670, 373, 905, 564, 95)
depresion_no <- c(11545, 6244, 16329, 11726, 2288)
tabla <- as.table(rbind(depresion_si, depresion_no))


dimnames(tabla) <- list(Depresi�n=c("S�", "No"),
                        Consumo_caf�=c("<= 1 taza por semana", "2-6 tazas por semana", "1 taza al d�a", "2-3 tazas al d�a", ">= 4 tazas al d�a"))

print(tabla)                        

# Hacer prueba chi -cuadrado de independencia.
prueba <- chisq.test(tabla)
cat("\nLa prueba internamente calcula los valores esperados :\n")
v = (2-1)*(5-1) #Grados de libertad
esperados <- round(prueba [["expected"]], v)
print(esperados)
cat("\nResultado de la prueba :\n")
print(prueba)     

#R: como el valor p (0.0003267) es menor a un valor de significancia de 0,01
#La hip�tesis nula se rechaza a favor de la hip�tesis alternativa

# ////////////////////////// PREGUNTA 4 //////////////////////////

# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con las preferencias
#de los chilenos en plataformas de redes sociales durante la etapa escolar y luego de comenzar la
#educaci�n superior, que requiera utilizar una prueba Q de Cochran. Identifique las variables involucradas y
#las hip�tesis a contrastar.

#Considerando que la prueba Q de Cochran tiene las limitaciones:
# 1. La variable de respuesta es dicot�mica.
# 2. La variable independiente es categ�rica.
# 3. Las observaciones son independientes entre s�.
# 4. El tama�o de la muestra es lo suficientemente grande. Glen (2016a) sugiere que n � k ??? 24, donde n es
#    el tama�o de la muestra (la cantidad de instancias, para el ejemplo) y k, la cantidad de niveles en la
#    variable independiente.

#entonces se procede a crear 2 tablas distintas, una para los estudiantes escolares y otra para los de educaci�n superior
#Lo cual cada columna de esta tabla ser� la red social de preferencia y fila cada estudiante de su respectivo nivel de educaci�n

#Enunciado: Un estudiante de sociolog�a de la gloriosa universidad de santiago de chile, esta preparando su tesis acerca
# del efecto de las redes sociales en estudiantes escolares y estudiantes universitarios. Lleg� a la duda si
# existe un cambio en las preferencias (de almenos una, a la generalizaci�n de las preferencias o viceversa)
# sobre las redes sociales, identificando como variables las redes sociales m�s ocupadas (feibu, yutu, twit, insta, tiktok, wsp)
# y los estudiantes ser� cada instancia de las tablas

#Hipotesis
#H0 : No existe ning�n cambio entre la distribuci�n de preferencias en ambas muestras
#Ha : Existe alg�n cambio entre la distribuci�n de preferencias en ambas muestras

#Cabe destacar que para solucionar esto, hay que resolver las siguientes sub-hip�tesis para cada tabla
#H0 : No existe preferencia entre redes sociales
#Ha : Existe por lo menos una red social que destaca sobre las otras



