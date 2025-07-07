# run_dev_test.R (Versión con recuperación de progreso)

# --- 1. Cargar librerías ---
cat("Cargando librerías...\n")
library(httr)
library(jsonlite)
library(dplyr)
library(rvest)
library(stringr)

# --- 2. Cargar funciones ---
cat("Cargando funciones desde la carpeta /dev...\n")
source("dev/extraer_noticias_wp.R")
source("dev/scrape_body_wp.R")

# --- 3. Cargar datos (con lógica de recuperación) ---
progress_file <- "ciper_completo_progreso.rds"
metadata_file <- "ciper_completo.rds"
noticias_a_procesar <- NULL

if (file.exists(progress_file)) {
  cat(paste("¡SE ENCONTRÓ UN ARCHIVO DE PROGRESO! Cargando desde", progress_file, "\n"))
  noticias_a_procesar <- readRDS(progress_file)

} else if (file.exists(metadata_file)) {
  cat(paste("Cargando metadata inicial desde", metadata_file, "\n"))
  noticias_a_procesar <- readRDS(metadata_file)

} else {
  cat("No se encontró ningún archivo. Ejecutando la extracción de metadata desde cero...\n")
  noticias_a_procesar <- extraer_noticias_wp(
    base_url = "https://www.ciperchile.cl",
    max_results = Inf # Descargar todo
  )
  saveRDS(noticias_a_procesar, metadata_file)
}

# --- 4. Ejecutar el SCRAPEO ---
if (!is.null(noticias_a_procesar) && nrow(noticias_a_procesar) > 0) {
  cat("\n--- Iniciando/Reanudando Fase de Scrapeo del Cuerpo ---\n")

  selector_ciper <- "div.col-lg-9 p.texto-nota"

  noticias_completas <- scrape_body_from_urls(
    news_df = noticias_a_procesar,
    content_selector = selector_ciper
  )

  # --- 5. Guardar el resultado FINAL ---
  cat("\n--- ¡Proceso finalizado! ---\n\n")
  cat("Guardando el dataframe definitivo en 'ciper_final_con_cuerpo.rds'...\n")
  saveRDS(noticias_completas, "ciper_final_con_cuerpo.rds")
  cat("¡Guardado con éxito!\n")

  cat("\nRevisando las últimas filas para ver el contenido scrapeado:\n")
  print(tail(noticias_completas[, c("titulo", "url", "contenido_limpio")]))

} else {
  cat("\n--- No hay datos para procesar. ---\n")
}
