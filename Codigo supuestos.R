library(ggplot2)
library(forecast)
library(tseries)
#install.packages("desk")<- por si no esta instalado
library(desk) #necesario para la base de datos

# Crear serie temporal
temp_ts <- ts(data.spurious$temp, start = 1880, frequency = 1)

# Ajustar modelo ARIMA automáticamente
modelo <- auto.arima(temp_ts)
res <- residuals(modelo)

# Obtener raíces inversas
ar_roots <- 1 / polyroot(c(1, -modelo$model$phi))
ma_roots <- 1 / polyroot(c(1, modelo$model$theta))

# Test de normalidad (Shapiro-Wilk)
shapiro_result <- shapiro.test(res)
pval_shapiro <- round(shapiro_result$p.value, 4)

# Test ADF antes de diferenciar
adf_result_raw <- adf.test(temp_ts)
pval_adf_raw <- round(adf_result_raw$p.value, 4)
stat_adf_raw <- round(adf_result_raw$statistic, 3)

# Diferenciar serie
temp_ts_diff <- diff(temp_ts)

# Test ADF después de diferenciar
adf_result_diff <- adf.test(temp_ts_diff)
pval_adf_diff <- round(adf_result_diff$p.value, 4)
stat_adf_diff <- round(adf_result_diff$statistic, 3)
png("Supuestos.png", width = 2000, height = 1800, res = 300)
# Gráfico 2x2
par(mfrow = c(2, 2))

# (gráfico aquí)
# 1. Residuos del modelo
plot(res, main = "Residuos del modelo",
     ylab = "Residuos", xlab = "Tiempo", col = "steelblue")

# 2. Gráfico de raíces inversas
plot(NA, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5),
     xlab = "Real", ylab = "", yaxt = "n",
     main = "Raíces inversas AR/MA", asp = 1)
grid(col = "lightgray", lty = "dotted")
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray")
points(Re(ar_roots), Im(ar_roots), pch = 19, col = "blue")
points(Re(ma_roots), Im(ma_roots), pch = 19, col = "red")
legend("topright",
       legend = c("AR", "MA"),
       col = c("blue", "red"),
       pch = 19,
       cex = 0.9,         # Tamaño del texto
       pt.cex = 0.5,      # Tamaño de los puntos
       x.intersp = 1,   # Espacio horizontal
       y.intersp = 1,   # Espacio vertical
       box.lty = 1)       # Sin borde

# 3. Test de estacionariedad ADF (antes y después)
# 3. ADF test
plot.new()
text(0.5, 0.90, "Test ADF \nEstacionariedad", cex = 1.5, font = 2)
text(0.5, 0.65, "Antes de diferenciar:", cex = 1.3)
text(0.5, 0.50, paste("p-valor =", pval_adf_raw), cex = 1.5)
text(0.5, 0.38, "Después de diferenciar:", cex = 1.3)
text(0.5, 0.25, paste("p-valor =", pval_adf_diff), cex = 1.5)
text(0.5, 0.08,
     if (pval_adf_diff < 0.05) "Serie estacionaria \ndespués de diff"
     else "Serie aún no estacionaria",
     cex = 1.1, col = if (pval_adf_diff < 0.05) "darkgreen" else "red")

# 4. Shapiro-Wilk
plot.new()
text(0.5, 0.75, "Test de Normalidad \n(Shapiro-Wilk)", cex = 1.4, font = 2)
text(0.5, 0.50, paste0("p-valor = ", pval_shapiro), cex = 1.3)
text(0.5, 0.3,
     if (pval_shapiro > 0.05) "No se rechaza normalidad"
     else "Se rechaza normalidad",
     cex = 1.2, col = if (pval_shapiro > 0.05) "darkgreen" else "red")

dev.off()
par(mfrow=c(1,1))
