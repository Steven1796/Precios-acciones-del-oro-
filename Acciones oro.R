library(dplyr)
##Para omitir valores nulos
Precios <- na.omit(Precios_oro)

##Calculo la regresión lineal que decido nombrar como log1
log1 <- lm(High + Low ~ Open + Close, data = Precios_oro)
summary(log1)


library(ggplot2)

sqrt(0.999)
##Esta sería la fórmula de regresión lineal aplicada al modelo plantado
##donde 1.03 es la variable dependiente y la Bera (variable independiente son los valores de apertura y cierre del modelo)
Precios = 1.03 + Beta (0.87+ 1.12) + e
Precios = 1.03 + (0.87+1.12) Apertura + Cierre

nuevos_datos <- Precios_oro(Open = c(0.87277, ...), Close = c(1.12645, ...))


predicciones <- predict(log1, data = Precios_oro(Open = 0.87277, Close = 1.12645), interval = "confidence", level = 0.95)
print(predicciones)

plot(Precios_oro$Date, predicciones[, "fit"], type = "l", col = "blue", lwd = 2, ylim = range(c(predicciones[, "lwr"], predicciones[, "upr"])), xlab = "Fecha", ylab = "High + Low")

lines(Precios_oro$Date, predicciones[, "lwr"], col = "red", lty = 2)
lines(Precios_oro$Date, predicciones[, "upr"], col = "red", lty = 2)

legend("topleft", legend = c("Predicción", "Intervalo de Confianza"), col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))


gráfico1= ggplot(Precios_oro, aes(High + Low, Open+Close))
gráfico1 + geom_point()

gráfico1 <- ggplot(Precios_oro, aes(High + Low, Open + Close)) +
  geom_point() +  # Agregar puntos
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Agregar línea de regresión
  labs(x = "Suma de valores máximos y mínimos", y = "Suma de valores de apertura y cierre") +
  ggtitle("Relación entre las sumas de valores") +
  theme_minimal()
print(gráfico1)


log2 <- lm(Open ~ High + Low, data = Precios_oro)
summary(log2)
log3 <- lm(Volume ~ High + Low, data = Precios_oro)
summary(log3)

##Vamos a analizar la variante de correlación en el modelo
##Para lo cuál usamos la siguiente función

correlation_matrix <- cor(Precios_oro[, c("Open", "High", "Low", "Close", "Volume")])


library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("corrplot")
library(corrplot)
# Convierte la matriz de correlación en un formato adecuado para ggplot
correlation_melted <- melt(correlation_matrix)
corrplot(correlation_matrix, method = "number")

ggplot(correlation_melted, aes(Var1, Var2, fill= value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) + 
  theme_minimal() +
  labs(title = "Mapa de calor para la correlación entre precios de apertura, máximos y mínimos",
       x= "Máximos y mínimos", y= "Precios de apertura")
