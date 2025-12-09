# CARGAR LIBRERÍAS
if (!require("EBImage")) {
  install.packages("BiocManager")
  BiocManager::install("EBImage")
}
library(EBImage)

# FUNCIÓN SIMPLIFICADA PARA PROCESAR IMAGEN HEMISFÉRICA
procesar_foto_hemisferica <- function(imagen_path, xc = 463.5, yc = 349.5, radius = 347.5) {
  
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESANDO:", basename(imagen_path), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cat("PARÁMETROS DE CALIBRACIÓN:\n")
  cat("  Centro X (xc):", xc, "\n")
  cat("  Centro Y (yc):", yc, "\n")
  cat("  Radio:", radius, "\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # 1. CARGAR IMAGEN
  cat("CARGANDO IMAGEN...\n")
  img <- readImage(imagen_path)
  rows <- dim(img)[1]
  cols <- dim(img)[2]
  
  cat("  Dimensiones:", rows, "x", cols, "píxeles\n")
  
  # 2. CREAR MÁSCARA CIRCULAR
  cat("CREANDO MÁSCARA CIRCULAR...\n")
  
  # Crear coordenadas
  x_coords <- matrix(1:cols, nrow = rows, ncol = cols, byrow = TRUE)
  y_coords <- matrix(1:rows, nrow = rows, ncol = cols)
  
  # Calcular distancia al centro
  distancia <- sqrt((x_coords - xc)^2 + (y_coords - yc)^2)
  
  # Crear máscara (TRUE = dentro del círculo)
  mascara <- distancia <= radius
  
  area_circulo <- sum(mascara)
  cat("  Área dentro del círculo:", area_circulo, "píxeles (", 
      round(100 * area_circulo / (rows * cols), 1), "%)\n")
  
  # 3. CONVERTIR A ESCALA DE GRISES (MÉTODO SIMPLE)
  cat("CONVIRTIENDO A GRISES...\n")
  
  if (length(dim(img)) == 3 && dim(img)[3] >= 3) {
    # Usar canal verde (mejor para vegetación)
    img_gris <- img[,,2]
  } else {
    # Ya es escala de grises
    img_gris <- img[,,1]
  }
  
  # Normalizar a 0-1
  img_gris <- (img_gris - min(img_gris)) / (max(img_gris) - min(img_gris))
  
  # Obtener valores dentro del círculo
  valores_circulo <- img_gris[mascara]
  
  cat("  Intensidad media dentro del círculo:", round(mean(valores_circulo), 4), "\n")
  
  # 4. BINARIZAR (MÉTODO SIMPLIFICADO)
  cat("BINARIZANDO...\n")
  
  # Usar umbral simple basado en la mediana
  umbral_simple <- median(valores_circulo)
  cat("  Umbral usado (mediana):", round(umbral_simple, 4), "\n")
  
  # Aplicar binarización
  img_bin <- matrix(FALSE, nrow = rows, ncol = cols)
  img_bin[mascara] <- img_gris[mascara] < umbral_simple
  
  # En fotos hemisféricas: TRUE = vegetación (oscuro), FALSE = cielo (claro)
  fraccion_vegetacion <- sum(img_bin[mascara]) / area_circulo
  
  # Si hay más del 50% de vegetación, probablemente está invertido
  if (fraccion_vegetacion > 0.5) {
    img_bin[mascara] <- !img_bin[mascara]
    fraccion_vegetacion <- sum(img_bin[mascara]) / area_circulo
    cat("  (Se invirtió la binarización)\n")
  }
  
  gap_fraction <- 1 - fraccion_vegetacion
  cat("  Gap Fraction (cielo):", round(gap_fraction, 4), "\n")
  cat("  Fracción vegetación:", round(fraccion_vegetacion, 4), "\n")
  
  # 5. CALCULAR GAP FRACTION POR ANILLOS
  cat("CALCULANDO POR ANILLOS...\n")
  
  n_anillos <- 10
  resultados_anillos <- data.frame(
    Anillo = 1:n_anillos,
    Gap_Fraction = NA,
    Angulo = NA,
    Area = NA
  )
  
  for (i in 1:n_anillos) {
    # Definir radios del anillo
    r_interno <- (i-1) * (radius/n_anillos)
    r_externo <- i * (radius/n_anillos)
    
    # Máscara para este anillo
    mascara_anillo <- distancia >= r_interno & distancia <= r_externo & mascara
    area_anillo <- sum(mascara_anillo)
    
    if (area_anillo > 0) {
      # Calcular gap fraction para este anillo
      gf_anillo <- sum(!img_bin[mascara_anillo]) / area_anillo
      angulo_anillo <- (i - 0.5) * (90/n_anillos)
      
      resultados_anillos$Gap_Fraction[i] <- gf_anillo
      resultados_anillos$Angulo[i] <- angulo_anillo
      resultados_anillos$Area[i] <- area_anillo
    }
  }
  
  # 6. CALCULAR LAI (ÍNDICE DE ÁREA FOLIAR)
  cat("CALCULANDO LAI...\n")
  
  # Filtrar anillos válidos
  validos <- !is.na(resultados_anillos$Gap_Fraction) & 
    resultados_anillos$Gap_Fraction > 0 & 
    resultados_anillos$Gap_Fraction < 1
  
  if (sum(validos) >= 3) {
    gf_validos <- resultados_anillos$Gap_Fraction[validos]
    angulos_validos <- resultados_anillos$Angulo[validos] * pi / 180  # Radianes
    
    # Método 1: Lang & Xiang
    peso <- sin(angulos_validos) * cos(angulos_validos)
    lai_lang <- -2 * sum(log(gf_validos) * peso) / sum(peso)
    
    # Método 2: Norman
    gf_promedio <- mean(gf_validos)
    angulo_promedio <- mean(angulos_validos)
    lai_norman <- -log(gf_promedio) * cos(angulo_promedio) * 2
    
    cat("  LAI (Lang & Xiang):", round(lai_lang, 4), "\n")
    cat("  LAI (Norman):", round(lai_norman, 4), "\n")
    
  } else {
    cat("  No hay suficientes anillos válidos para calcular LAI\n")
    lai_lang <- NA
    lai_norman <- NA
  }
  
  # 7. RESULTADOS FINALES
  cat("\nRESULTADOS FINALES:\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("  Archivo:", basename(imagen_path), "\n")
  cat("  Dimensiones:", rows, "x", cols, "px\n")
  cat("  Centro (X,Y):", xc, ",", yc, "\n")
  cat("  Radio:", radius, "px\n")
  cat("  Gap Fraction total:", round(gap_fraction, 4), "\n")
  cat("  LAI (Lang & Xiang):", ifelse(is.na(lai_lang), "N/A", round(lai_lang, 4)), "\n")
  cat("  LAI (Norman):", ifelse(is.na(lai_norman), "N/A", round(lai_norman, 4)), "\n")
  cat("  Anillos válidos:", sum(validos), "/", n_anillos, "\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # 8. CREAR DATAFRAME CON RESULTADOS
  resultados_finales <- data.frame(
    Archivo = basename(imagen_path),
    XC = xc,
    YC = yc,
    Radio = radius,
    Ancho = cols,
    Alto = rows,
    Area_Total = rows * cols,
    Area_Circulo = area_circulo,
    Porcentaje_Circulo = round(100 * area_circulo / (rows * cols), 2),
    Intensidad_Media = round(mean(valores_circulo), 4),
    Umbral = round(umbral_simple, 4),
    Gap_Fraction_Total = round(gap_fraction, 4),
    Fraccion_Vegetacion = round(fraccion_vegetacion, 4),
    LAI_Lang = ifelse(is.na(lai_lang), NA, round(lai_lang, 4)),
    LAI_Norman = ifelse(is.na(lai_norman), NA, round(lai_norman, 4)),
    Anillos_Validos = sum(validos),
    Fecha_Procesamiento = as.character(Sys.time())
  )
  
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESAMIENTO COMPLETADO\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  return(list(
    resumen = resultados_finales,
    anillos = resultados_anillos
  ))
}

# ============================================================================
# EJECUTAR EL PROCESAMIENTO
# ============================================================================

# Ruta de la imagen
ruta_imagen <- "C:/Users/rcejudo/Downloads/ejemplo180.jpg"

# Verificar si el archivo existe
if (file.exists(ruta_imagen)) {
  cat("Archivo encontrado:", ruta_imagen, "\n\n")
  
  # Procesar la imagen
  resultado <- procesar_foto_hemisferica(
    imagen_path = ruta_imagen,
    xc = 463.5,
    yc = 349.5,
    radius = 347.5
  )
  
  # Mostrar resumen
  cat("\nTABLA DE RESULTADOS:\n")
  print(resultado$resumen)
  
  # Guardar resultados en CSV
  fecha_hora <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Guardar resumen
  archivo_resumen <- paste0("C:/Users/rcejudo/Downloads/resultados_", fecha_hora, ".csv")
  write.csv(resultado$resumen, archivo_resumen, row.names = FALSE)
  cat("\nResumen guardado en:", archivo_resumen, "\n")
  
  # Guardar datos por anillo
  archivo_anillos <- paste0("C:/Users/rcejudo/Downloads/anillos_", fecha_hora, ".csv")
  write.csv(resultado$anillos, archivo_anillos, row.names = FALSE)
  cat("Datos por anillo guardados en:", archivo_anillos, "\n")
  
} else {
  cat("ERROR: No se encuentra el archivo\n")
  cat("Ruta buscada:", ruta_imagen, "\n")
}

# ============================================================================
# VERSIÓN MÁS SIMPLE (SOLO CÁLCULOS BÁSICOS)
# ============================================================================

procesar_simple <- function(imagen_path, xc = 463.5, yc = 349.5, radius = 347.5) {
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("VERSIÓN SIMPLE\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Cargar imagen
  img <- readImage(imagen_path)
  rows <- nrow(img)
  cols <- ncol(img)
  
  # Crear coordenadas
  x <- matrix(1:cols, nrow = rows, ncol = cols, byrow = TRUE)
  y <- matrix(1:rows, nrow = rows, ncol = cols)
  distancia <- sqrt((x - xc)^2 + (y - yc)^2)
  mascara <- distancia <= radius
  
  # Convertir a grises (canal verde)
  if (length(dim(img)) >= 3) {
    img_gris <- img[,,2]
  } else {
    img_gris <- img[,,1]
  }
  
  # Normalizar
  img_gris <- (img_gris - min(img_gris)) / (max(img_gris) - min(img_gris))
  
  # Valores dentro del círculo
  valores <- img_gris[mascara]
  
  # Umbral simple (percentil 50 = mediana)
  umbral <- quantile(valores, 0.5)
  
  # Binarizar
  img_bin <- img_gris < umbral
  img_bin[!mascara] <- FALSE  # Solo dentro del círculo
  
  # Calcular gap fraction
  gf_total <- sum(!img_bin[mascara]) / sum(mascara)
  
  # LAI simple
  lai_simple <- -log(gf_total) * cos(45 * pi/180) * 2
  
  # Resultados
  resultados <- data.frame(
    Archivo = basename(imagen_path),
    XC = xc,
    YC = yc,
    Radio = radius,
    Gap_Fraction = round(gf_total, 4),
    LAI_Simple = round(lai_simple, 4),
    Umbral = round(umbral, 4),
    Intensidad_Media = round(mean(valores), 4)
  )
  
  cat("RESULTADOS BÁSICOS:\n")
  cat("  Gap Fraction:", round(gf_total, 4), "\n")
  cat("  LAI aproximado:", round(lai_simple, 4), "\n")
  cat("  Umbral usado:", round(umbral, 4), "\n")
  
  return(resultados)
}

# Ejecutar versión simple
cat("\n")
resultado_simple <- procesar_simple(ruta_imagen)
print(resultado_simple)

# ============================================================================
# VERSIÓN PARA PROCESAR MÚLTIPLES ARCHIVOS
# ============================================================================

procesar_varias_fotos <- function(carpeta, xc = 463.5, yc = 349.5, radius = 347.5) {
  cat("Buscando imágenes en:", carpeta, "\n")
  
  # Listar archivos de imagen
  archivos <- list.files(carpeta, 
                         pattern = "\\.(jpg|jpeg|png|tif|tiff)$",
                         ignore.case = TRUE,
                         full.names = TRUE)
  
  if (length(archivos) == 0) {
    cat("No se encontraron imágenes\n")
    return(NULL)
  }
  
  cat("Encontradas", length(archivos), "imágenes\n")
  
  # Procesar cada imagen
  todos_resultados <- list()
  
  for (i in 1:length(archivos)) {
    cat("\n", i, "/", length(archivos), ":", basename(archivos[i]), "\n")
    
    tryCatch({
      resultado <- procesar_simple(archivos[i], xc, yc, radius)
      todos_resultados[[i]] <- resultado
    }, error = function(e) {
      cat("Error procesando", basename(archivos[i]), ":", e$message, "\n")
      # Crear registro de error
      todos_resultados[[i]] <- data.frame(
        Archivo = basename(archivos[i]),
        XC = xc,
        YC = yc,
        Radio = radius,
        Gap_Fraction = NA,
        LAI_Simple = NA,
        Umbral = NA,
        Intensidad_Media = NA,
        Error = e$message
      )
    })
  }
  
  # Combinar todos los resultados
  df_final <- do.call(rbind, todos_resultados)
  
  # Guardar resultados
  fecha <- format(Sys.time(), "%Y%m%d_%H%M")
  archivo_salida <- paste0("C:/Users/rcejudo/Downloads/resultados_multiples_", fecha, ".csv")
  write.csv(df_final, archivo_salida, row.names = FALSE)
  
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESAMIENTO COMPLETADO\n")
  cat("Resultados guardados en:", archivo_salida, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  return(df_final)
}

# Para procesar múltiples imágenes (descomenta si lo necesitas):
# resultados_lote <- procesar_varias_fotos("C:/Users/rcejudo/Downloads/fotos_hemisfericas/")