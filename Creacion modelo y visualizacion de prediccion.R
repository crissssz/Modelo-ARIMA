# --- PASO 0: INSTALAR Y CARGAR PAQUETES NECESARIOS ---
# Si no tienes instalados las librerias, descomenta las líneas de instalación
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("tseries")
# install.packages("desk")
library(ggplot2)
library(forecast)
library(tseries)
library(desk)
# --- PASO 1: CARGAR Y PREPARAR TUS DATOS DE SERIES DE TIEMPO ---

temp_ts <- ts(data.spurious$temp, start = 1880, frequency = 1)

# --- PASO 2: ANÁLISIS EXPLORATORIO DE LA SERIE DE TIEMPO ---
autoplot(temp_ts) +
  ggtitle("Serie de Tiempo Original de Anomalías de Temperatura") +
  xlab("Año") +
  ylab("Anomalía de temperatura (°C)") +
  theme_minimal(base_size = 14)

# --- PASO 3: PRUEBAS DE ESTACIONARIEDAD  ---
cat("--- Prueba ADF para estacionariedad ---\n")
adf_test_result <- adf.test(temp_ts)
print(adf_test_result)

cat("\n--- Prueba KPSS para estacionariedad ---\n")
kpss_test_result <- kpss.test(temp_ts)
print(kpss_test_result)


# --- PASO 4: AJUSTAR EL MODELO ARIMA AUTOMÁTICAMENTE ---
cat("\n--- Ajuste del Modelo ARIMA con auto.arima() ---\n")
modelo_arima <- auto.arima(temp_ts,
                           trace = TRUE) # Mostrar el proceso en la consola

print(modelo_arima)


# --- PASO 5: DIAGNÓSTICO DE RESIDUOS DEL MODELO ---
cat("\n--- Diagnóstico de Residuos del Modelo ---\n")
checkresiduals(modelo_arima)


# --- PASO 6: REALIZAR PREDICCIONES ---
num_años_a_predecir <- 10 # Para 10 años futuros

cat(paste("\n--- Realizando predicciones para los próximos", num_años_a_predecir, "años ---\n"))
predicciones_arima <- forecast(modelo_arima, h = num_años_a_predecir)


# --- PASO 7: VISUALIZAR LAS PREDICCIONES Y GUARDAR EL GRÁFICO PARA EL PÓSTER ---
modelo_arima_order <- modelo_arima$arma # Extrae los órdenes (p,d,q)
titulo_modelo <- paste0("ARIMA(",
                        modelo_arima_order[1], ",", modelo_arima_order[6], ",", modelo_arima_order[2], ")")

if (modelo_arima_order[7] > 0) { # Si hay componente estacional (s > 0)
  titulo_modelo <- paste0(titulo_modelo,
                          "(", modelo_arima_order[3], ",", modelo_arima_order[7], ",", modelo_arima_order[4], ")[", modelo_arima_order[5], "]")
}

plot_final <- autoplot(predicciones_arima) +
  ggtitle(paste0("Pronóstico de Anomalías \nde Temperatura con Modelo ", titulo_modelo)) +
  xlab("Año") +
  ylab("Anomalía de temperatura (°C)") +
  theme_minimal(base_size = 14) +
  # Personalización visual
  geom_point(data = data.frame(time = time(temp_ts), value = temp_ts),
             aes(x = time, y = value), color = "darkblue", size = 0.8) + # Puntos para datos históricos
  geom_line(data = data.frame(time = time(predicciones_arima$mean), value = predicciones_arima$mean),
            aes(x = time, y = value), color = "red", linetype = "dashed", size = 1) + # Línea de pronóstico
  geom_vline(xintercept = time(temp_ts)[length(temp_ts)], linetype = "dotted", color = "darkgray") # Línea divisoria

print(plot_final)


# --- PASO 8: EXPORTAR EL GRÁFICO COMO IMAGEN PNG PARA EL PÓSTER ---
# Crea un nombre de archivo dinámico basado en el modelo
nombre_archivo_png <- paste0("prediccion_ARIMA_",
                             modelo_arima_order[1], modelo_arima_order[6], modelo_arima_order[2],
                             ifelse(modelo_arima_order[7]>0, paste0("_s", modelo_arima_order[3], modelo_arima_order[7], modelo_arima_order[4]), ""),
                             ".png")

# Ajusta el ancho y alto según se necesite
png(nombre_archivo_png, width = 1920, height = 1800, res = 300, bg= "#f2f2f2")
print(plot_final)
dev.off()

cat(paste0("\nGráfico exportado para tu póster como: ", nombre_archivo_png, "\n"))


# --- PASO 9: VISUALIZAR LOS VALORES NUMÉRICOS DE LAS PREDICCIONES ---
cat("\n--- Detalles de las Predicciones ---\n")
print(predicciones_arima)

cat("\nValores puntuales pronosticados:\n")
print(predicciones_arima$mean)

cat("\nLímites inferiores del 95% de confianza:\n")
print(predicciones_arima$lower[, "95%"])

cat("\nLímites superiores del 95% de confianza:\n")
print(predicciones_arima$upper[, "95%"])

