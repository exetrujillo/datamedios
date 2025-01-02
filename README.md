# Paquete `datamedios` (Versión 0.3.1)

`datamedios` es un paquete de R diseñado para facilitar la extracción automatizada de noticias desde medios de comunicación chilenos, por el momento desde la API de BíoBío.cl. Este paquete permite realizar búsquedas de noticias y filtrarlas por rangos de fechas, entregando los resultados en un formato estructurado y listo para su análisis. Además, incluye funcionalidades para almacenar los datos extraídos en una base de datos por medio de una API.

------------------------------------------------------------------------

## 🔧 Instalación

Para instalar el paquete desde GitHub, sigue los siguientes pasos:

1.  Asegúrate de tener instalado el paquete `devtools`. Si no lo tienes, instálalo ejecutando:

    ``` r
    install.packages("devtools")
    ```

2.  Instala `datamedios` desde el repositorio de GitHub:

    ``` r
    devtools::install_github("exetrujillo/datamedios")
    ```

------------------------------------------------------------------------

## 🔄 Uso

### Función principal: `extraer_noticias_fecha`

Esta función permite filtrar noticias por un rango de fechas específico, además de aplicar una frase de búsqueda. Por defecto carga los datos automáticamente a la base de datos de api-datamedios.

#### **Parámetros:**

-   **`search_query`**: Frase de búsqueda (obligatorio).
-   **`fecha_inicio`**: Fecha de inicio en formato `"YYYY-MM-DD"` (obligatorio).
-   **`fecha_fin`**: Fecha de fin en formato `"YYYY-MM-DD"` (obligatorio).
-   **`subir_a_bd`**: Por defecto está seteado en TRUE, pero para los tests lo dejamos en FALSE (opcional).

#### **Valor devuelto:**

Un `data.frame` con las siguientes columnas:

-   **`ID`**: Identificador de la noticia.
-   **`post_title`**: Título de la noticia.
-   **`post_content`**: Contenido completo.
-   **`post_excerpt`**: Resumen o extracto.
-   **`post_URL`**: Enlace a la noticia.
-   **`post_categories`**: Categorías asociadas.
-   **`post_tags`**: Etiquetas relacionadas.
-   **`year, month, day`**: Fecha de publicación (año, mes y día).
-   **`post_category_primary.name`**: Categoría principal.
-   **`post_category_secondary.name`**: Categoría secundaria.
-   **`post_image.URL`**: URL de la imagen asociada.
-   **`post_image.alt`**: Texto alternativo de la imagen.
-   **`post_image.caption`**: Leyenda de la imagen.
-   **`author.display_name`**: Nombre del autor.
-   **`raw_post_date`**: Fecha cruda de publicación.
-   **`resumen_de_ia`**: Resumen generado por IA (si está disponible).
-   **`search_query`**: Palabra o frase de búsqueda por la que se obtuvo los datos.

#### **Ejemplo de uso:**

``` r
# Buscar noticias entre el 1 de enero y el 31 de diciembre de 2023
noticias <- extraer_noticias_fecha("estallido social", "2019-10-18", "2024-12-31")
```

------------------------------------------------------------------------

### Función secundaria: `extraer_noticias_max_res`

Esta función permite obtener noticias desde la API de BíoBío.cl utilizando una frase de búsqueda. Además, puedes limitar el número de resultados a extraer. Por defecto carga los datos automáticamente a la base de datos de api-datamedios.

#### **Parámetros:**

-   **`search_query`**: Frase de búsqueda (obligatorio).
-   **`max_results`**: Máximo número de resultados a extraer (opcional).
-   **`subir_a_bd`**: Por defecto está seteado en TRUE, pero para los tests lo dejamos en FALSE (opcional)

#### **Valor devuelto:**

Un `data.frame` similar al de `extraer_noticias_fecha`, pero filtrado por `max_results`.

#### **Ejemplo de uso:**

``` r
# Buscar noticias relacionadas con "inteligencia artificial"
noticias <- extraer_noticias_max_res("inteligencia artificial", max_results = 100)
```

------------------------------------------------------------------------

## 🔖 Documentación

Para acceder a la documentación completa de las funciones, usa el siguiente comando:

``` r
help(package = "datamedios")
```

------------------------------------------------------------------------

## 📊 Dependencias

Este paquete utiliza las siguientes dependencias de R para su correcto funcionamiento:

-   **`dplyr`**: Manipulación y análisis de datos.

-   **`httr`**: Realización de solicitudes HTTP para interactuar con APIs.

-   **`magrittr`**: Uso de operadores como `%>%` para flujos de trabajo más legibles.

-   **`jsonlite`**: Procesamiento y conversión de datos en formato JSON.

-   **`utils`**: Funciones utilitarias básicas incluidas en R.

-   **`tidyverse`**: Conjunto de paquetes para análisis de datos y visualización.

-   **`wordcloud2`**: Generación de nubes de palabras interactivas.

-   **`tidytext`**: Análisis de texto basado en datos ordenados.

-   **`lubridate`**: Manejo y análisis de datos temporales.

-   **`rvest`**: Web scraping de páginas HTML.

-   **`stringr`**: Manejo y manipulación de cadenas de texto.

-   **`xml2`**: Lectura y manejo de datos en formato XML.

-   **`purrr`**: Programación funcional con listas y vectores.

------------------------------------------------------------------------

## 🗂️ Licencia

`datamedios` está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.

------------------------------------------------------------------------

## 👤 Autoría

Este paquete fue desarrollado por:

-   **Ismael Aguayo**
-   **Exequiel Trujillo**

------------------------------------------------------------------------

## 📝 To Do

-   Incorporar soporte para otros medios de comunicación.

-   Crear un pipeline automatizado para la actualización de la base de datos.

-   Crear una tabla en la base de datos para almacenar búsquedas pendientes por realizar desde el json, para almacenarlas en la base de datos notas_biobio o en otras cuando se incluya el soporte a otros medios de comunicación.
