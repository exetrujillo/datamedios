# Paquete `datamedios` (Versión 1.0.0)

`datamedios` es un paquete de R diseñado para facilitar la extracción automatizada de noticias desde medios de comunicación chilenos, por el momento desde la API de BíoBío.cl. Este paquete permite realizar búsquedas de noticias y filtrarlas por rangos de fechas, entregando los resultados en un formato estructurado y listo para su análisis. Además, incluye funcionalidades para almacenar los datos extraídos en una base de datos por medio de una API.

------------------------------------------------------------------------

## 🔧 Instalación

Para instalarlo desde CRAN:

Instala `datamedios` desde la librería de CRAN:

``` r
install.packages("datamedios")
library(datamedios) 
```

Alternativamente, para instalar el paquete desde GitHub, sigue los siguientes pasos:

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
-   **`titulo`**: Título de la noticia.
-   **`contenido`**: Contenido completo.
-   **`contenido limpio`**: Resumen o extracto.
-   **`url`**: Enlace a la noticia.
-   **`url_imagen`**: Categorías asociadas.
-   **`autor`**: Etiquetas relacionadas.
-   **`fecha`**: Fecha cruda de publicación (formato 'YYYY-MM-DD').
-   **`resumen`**: Resumen de la IA o bajada de la nota, según disponibilidad.
-   **`search_query`**: Palabra o frase de búsqueda por la que se obtuvo los datos.
-   **`medio`**: Medio al que corresponde la noticia.
-   **`temas`**: Categorías de la noticia.

#### **Ejemplo de uso:**

``` r
# Buscar noticias entre el 1 de enero y el 31 de diciembre de 2023
noticias <- extraer_noticias_fecha("estallido social", "2019-10-18", "2020-10-18")
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

## 🔖 Documentación

**Adicionalmente, el paquete incluye funciones de limpieza, manejo y visualización de las noticias extraídas. Para visitar la documentación de todos los códigos haga click [aquí](https://exetrujillo.github.io/datamedios/reference/index.html).**

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

-   **`DT`**: Generación de tablas interactivas en HTML desde R.

-   **`ggplot2`**: Creación de gráficos sofisticados y personalizados basados en la gramática de los gráficos.

------------------------------------------------------------------------

## 🗂️ Licencia

`datamedios` está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.

------------------------------------------------------------------------

## 👤 Autoría

Este paquete fue desarrollado por:

-   **Exequiel Trujillo** (contacto: [exequiel.trujillo\@ug.uchile.cl](mailto:exequiel.trujillo@ug.uchile.cl){.email})
-   **Ismael Aguayo** (contacto: [ismael.aguayo\@ug.uchile.cl](mailto:ismael.aguayo@ug.uchile.cl){.email})
-   **Klaus Lehmann** (contacto: [klehmann\@fen.uchile.cl](mailto:klehmann@fen.uchile.cl){.email})

------------------------------------------------------------------------
