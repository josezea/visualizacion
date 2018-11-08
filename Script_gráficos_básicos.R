
# Lectura de la base de atos
rm(list = ls())

# Lectura de la base de datos de estaturas
url<-"https://raw.githubusercontent.com/jfzeac/analisis_datos_r/master/NCD_RisC_eLife_2016_height_age18_countries.csv"
datos<-read.csv(url)

# Lectura de medidas antropometrícas escolares en 8 municipios del país

url2<-"https://raw.githubusercontent.com/jfzeac/analisis_datos_r/master/medidas_escolares.csv"
medidas<-read.csv(url2)

################################## Gráfico de serie de tiempo ################################## 

# La evolucion de las estuturas de las colombianos
names(datos)
# Correr esto: table(datos$Country)
table(datos$Sex)
estat_mujeres <- subset(datos, Sex == "Women" &   Country == "Colombia",
                        select = c(Country, Sex, Year.of.birth, Mean.height..cm.))

colnames(estat_mujeres)
colnames(estat_mujeres) <- c("País", "Sexo", "Año_Nacim", "Estatura")

plot(estat_mujeres$Año_Nacim, estat_mujeres$Estatura)

plot(estat_mujeres$Año_Nacim, estat_mujeres$Estatura, pch = 20, xlab = "Año de nacimiento",
     ylab = "Estatura (cms)", main = "Evolución de las estaturas de las mujeres (1896-1996)")


# Hombres y mujeres simultanemente
estat <- subset(datos, Country == "Colombia",
                        select = c(Country, Sex, Year.of.birth, Mean.height..cm.))
colnames(estat)
colnames(estat) <- c("País", "Sexo", "Año_Nacim", "Estatura")


table(estat$Sexo)

x11()
plot(estat$Año_Nacim, estat$Estatura, pch = 20, xlab = "Año de nacimiento",
     ylab = "Estatura (cms)", main = "Evolución de las estaturas (1896-1996)", 
     type = "n")

points(estat$Año_Nacim[estat$Sexo == "Men"], estat$Estatura[estat$Sexo == "Men"], 
       col = "darkblue", pch = 18)

points(estat$Año_Nacim[estat$Sexo == "Women"], estat$Estatura[estat$Sexo == "Women"], 
       col = "pink", pch = 20)

legend(1960,150, # Coordenadas donde va la leyenda
       c("Hombre", "Mujer"), # Texto
       lty=c(1,2), # Tipo de lineas
       lwd=c(2.5,2.5), # Grosor de la linea
       col=c("darkblue","pink"), # Color de la línea
       cex = 0.9) # Tamaño de la leyenda)

################################## Diagrama de dispersión ################################## 
# Correlación

# Diagrama de dispersion de peso vs estatura y linea recta de regresión

# Correlación del peso y la estatura
cor(medidas$PESO, medidas$ESTATURA)
# Modelo de regresión
(modelo <- lm(ESTATURA ~ PESO, data = medidas))

# Diagrama de dispersión
plot(medidas$PESO, medidas$ESTATURA)

plot(medidas$PESO, medidas$ESTATURA, xlab = "Peso", ylab = "Estatura", pch = 20, 
     col = "darkblue")
abline(modelo, col = "red")


################################## Diagrama de dispersión ################################## 

# Crear IMC y graficar unos boxplot por genero de las estaturas de los niños de 14 años
summary(medidas$EDA_DECI)
# Niños de 14 años (que ya cumplieron 14 y o han cumplido 14)
table(14 <= medidas$EDA_DECI  & medidas$EDA_DECI < 15 )

niños14 <- subset(medidas, 14 <= EDA_DECI  & EDA_DECI < 15,
                  select = c(ID, SEXO, ESTATURA ))
boxplot(niños14$ESTATURA)

boxplot(niños14$ESTATURA, col = "darkgreen", ylab = "Estatura (cm)",
        main = "Diagrama de caja estatura de niños de 14 años", pch = 20)

# Ver colores y parámetros gráficos en: http://www.statmethods.net/advgraphs/parameters.html

# Boxplot agrupado

boxplot(niños14$ESTATURA ~ niños14$SEXO , col = c("pink", "darkblue"),
        ylab = "Estatura (cm)",
        main = "Diagrama de caja estatura de niños de 14 años", pch = 20)

# Diagrama de caja y jitter
boxplot(niños14$ESTATURA ~ niños14$SEXO , col = c("pink", "darkblue"),
        ylab = "Estatura (cm)",
        main = "Diagrama de caja estatura de niños de 14 años", pch = 20)

stripchart(niños14$ESTATURA ~ niños14$SEXO, vertical = TRUE, 
           method = "jitter", add = TRUE, pch = 20, col = 'red')

# Ejercicio comparar las estaturas de los niños y niñas de 9 años

################################## Histograma ################################## 

# Un histograma de la variable edad
names(medidas)
summary(medidas$EDA_DECI)
hist(medidas$EDA_DECI)

hist(medidas$EDA_DECI, xlab = "Edad", ylab = "Frecuencia", main = "", col = "red")

hist(medidas$EDA_DECI, xlab = "Edad", ylab = "Frecuencia", main = "", col = "red",
     xaxt="n" )
axis(side = 1, at=seq(4, 20, by=2))
# xaxt es el tipo de eje, con n se suprime el eje x
# con la función axis (side = 1) se construye el nuevo eje x, en at se indican los valores
# http://www.statmethods.net/advgraphs/axes.html


# Histograma y gráfico de densidad (aproximada)
hist(medidas$EDA_DECI, xlab = "Edad", ylab = "Densidad", main = "", col = "plum",
     xaxt="n", prob = TRUE )
axis(side = 1, at=seq(4, 20, by=2))
lines(density(medidas$EDA_DECI), col = "blue")

################################## Diagrama de barras ################################## 

# Hacer un gráfico de barras del sexo en la institución educativa
(tf_sexo <- table(medidas$SEXO))
barplot(tf_sexo)

barplot(tf_sexo, main="Frecuencias absolutas sexo", 
        xlab="Sexo", ylab = "Frecuencias absolutas",  space = 0.5, col = c("pink", "blue"), ylim = c(0, 5000))

barplot(tf_sexo, main="Frecuencias absolutas sexo", 
        xlab="Sexo", ylab = "Frecuencias absolutas", space = 0.5, col = c("pink", "blue"), ylim = c(0, 5000),
        yaxt="n")

axis(side = 2, at=seq(0, 5000, by=500))
# side = 2 para el eje x, en at se colocan los valores 

#Tabla de frecuencias relativas (%)
tfr_sexo <- prop.table(tf_sexo) * 100
barplot(tfr_sexo, main="Frecuencias absolutas sexo", 
        xlab="Sexo", ylab = "Frecuencias relativas (%)", space = 0.5, col = c("pink", "blue"), ylim = c(0, 100),
        yaxt="n")
axis(side = 2, at=seq(0, 100, by=10))


# Gráfico de barras apilado de ciudad y sexo
tf_CS <- table(medidas$COD_CIUDAD, medidas$SEXO)
barplot( tf_CS)

tf_CS <- table(medidas$SEXO, medidas$COD_CIUDAD)

barplot( tf_CS, xlab = "Sexo", ylab = "Frecuencias absolutas", 
         col = c("pink", "blue"))

x11()
barplot( tf_CS, xlab = "Sexo", ylab = "Frecuencias absolutas", 
         col = c("pink", "blue")) 

legend("topright", legend = rownames(tf_CS),fill = c("pink", "blue"), 
       cex=0.8, title = "Sexo" )

# Diagrama de barras agrupados
x11()
barplot( tf_CS, xlab = "Sexo", ylab = "Frecuencias absolutas", 
         col = c("pink", "blue"), beside = T) 

legend("topright", legend = rownames(tf_CS),fill = c("pink", "blue"), 
       cex=0.8, title = "Sexo" )

################################## Gráfico de perfiles ################################## 

x11()
tfr_CS <- prop.table(tf_CS) * 100
barplot( tfr_CS, xlab = "Sexo", ylab = "Frecuencias relativas (%)", 
         col = c("pink", "blue")) 

legend("topright", legend = rownames(tf_CS),fill = c("pink", "blue"), 
       cex=0.8, title = "Sexo" )

# Exportación de base de datos 

write.csv(estat, "estat.csv")
