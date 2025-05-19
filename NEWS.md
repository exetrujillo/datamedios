# datamedios 1.2.1

### Corregido (Fixed)

*   Se mejoró significativamente la robustez de las funciones de extracción para Emol (`extraer_noticias_fecha_emol`, `extraer_noticias_max_res_emol`) al procesar la estructura de datos devuelta por la API, especialmente para campos como resumen (`_source.bajada`), URL de imagen (`_source.tablas.tablaMedios`, `_source.imagen`) y temas (`_source.temas`). Esto previene errores que causaban la pérdida de todos los datos extraídos de Emol.
*   Las funciones de extracción de Emol ahora utilizan funciones auxiliares (helpers) internas para procesar campos complejos, mejorando la legibilidad y el manejo de errores específicos de cada campo.
*   Se implementó un manejo de errores más informativo en las funciones de extracción de Emol: si ocurre un error crítico durante el procesamiento final de los datos, se guarda una muestra de los datos crudos problemáticos en un archivo `.rds` para facilitar la depuración.
*   Se mejoró la robustez de la función `limpieza_notas` para manejar correctamente el contenido de noticias que puede ser HTML crudo, texto plano, o valores `NA`.
*   Se solucionó un error en `limpieza_notas` que ocurría cuando el contenido de una noticia era `NA`, evitando la detención de la función.
*   La función `limpieza_notas` ahora maneja de forma más predecible la inicialización de la columna `contenido_limpio`, usando siempre la columna `contenido` como base en cada ejecución.

### Mejorado (Improved)

*   Se optimizó la función `limpieza_notas` para detectar si el contenido es HTML antes de intentar un parseo completo, mejorando la eficiencia para contenidos que ya son texto plano.
*   Se refinó la lógica de filtrado por términos de búsqueda y sinónimos en `limpieza_notas` para ser más consistente con la presencia de valores `NA` en el contenido.
*   Se estandarizó el uso de `NA_character_` en los helpers de extracción de Emol para asegurar consistencia en los datos procesados.

# datamedios 1.2.0

### Agregado (Added)

*   Nuevas fuentes para la extracción de noticias de medios chilenos.
*   Funciones auxiliares para utilizar un enfoque iterativo controlado.

### Mejorado (Improved)

*   Modificación por modularización de las funciones de extracción de datos por fecha o por cantidad máxima de resultados.
*   Se modifica la función de limpieza para que responda a los formatos de las noticias de las nuevas fuentes.

# datamedios 1.1.0

### Mejorado (Improved)

*   Mejor rendimiento de funciones.

# datamedios 1.0.0

*   Initial CRAN submission.
