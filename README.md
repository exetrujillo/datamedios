# Paquete `datamedios` (Versión 0.2.2)

`datamedios` es un paquete de R diseñado para facilitar la extracción automatizada de noticias desde medios de comunicación chilenos, particularmente desde la API de BíoBío.cl por el momento. Este paquete permite realizar búsquedas de noticias y filtrarlas por rangos de fechas, entregando los resultados en un formato estructurado y listo para su análisis. Además, incluye funcionalidades para almacenar los datos extraídos en una base de datos MySQL.

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

Esta función permite filtrar noticias por un rango de fechas específico, además de aplicar una frase de búsqueda.

#### **Parámetros:**

-   **`search_query`**: Frase de búsqueda (obligatorio).
-   **`fecha_inicio`**: Fecha de inicio en formato `"YYYY-MM-DD"` (obligatorio).
-   **`fecha_fin`**: Fecha de fin en formato `"YYYY-MM-DD"` (obligatorio).

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

#### **Ejemplo de uso:**

``` r
# Buscar noticias entre el 1 de enero y el 31 de diciembre de 2023
noticias <- extraer_noticias_fecha("inteligencia artificial", "2023-01-01", "2023-12-31")
```

------------------------------------------------------------------------

### Función secundaria: `extraer_noticias`

Esta función permite obtener noticias desde la API de BíoBío.cl utilizando una frase de búsqueda. Además, puedes limitar el número de resultados a extraer.

#### **Parámetros:**

-   **`search_query`**: Frase de búsqueda (obligatorio).
-   **`max_results`**: Máximo número de resultados a extraer (opcional).

#### **Valor devuelto:**

Un `data.frame` similar al de `extraer_noticias_fecha`, pero filtrado por `max_results`.

#### **Ejemplo de uso:**

``` r
# Buscar noticias relacionadas con "inteligencia artificial"
noticias <- extraer_noticias("inteligencia artificial", max_results = 100)
```

------------------------------------------------------------------------

### Función `agregar_datos_unicos`

Esta función permite agregar datos a una tabla MySQL solo si los registros no están ya presentes, evitando duplicados mediante la columna `ID`.

#### **Parámetros:**

-   **`tabla`**: Nombre de la tabla en MySQL donde se insertarán los datos.
-   **`datos`**: Un data frame con los datos a insertar. Debe contener una columna `ID`.

#### **Valor devuelto:**

Un mensaje indicando cuántos registros nuevos se agregaron o si no hay datos nuevos para insertar.

#### **Dependencias:**

Esta función utiliza un archivo `credenciales.R` para gestionar las credenciales de conexión a la base de datos.

#### **Ejemplo de uso:**

``` r
# Agregar datos únicos a la tabla "notas_biobio"
agregar_datos_unicos("notas_biobio", noticias)
```

------------------------------------------------------------------------

### Conexión y desconexión a base de datos

Para almacenar los datos extraídos, el paquete incluye funciones para conectarse y desconectarse de una base de datos MySQL. Estas funciones dependen de un archivo `credenciales.R` donde se especifican los detalles de conexión.

#### **Ejemplo:**

``` r
# Conectar a la base de datos
con <- conectar_bd("credenciales.R")

# Realizar operaciones en la base de datos
# ...

# Desconectar de la base de datos
desconectar_bd(con)
```

------------------------------------------------------------------------

## 🔖 Documentación

Para acceder a la documentación completa de las funciones, usa el siguiente comando:

``` r
help(package = "datamedios")
```

------------------------------------------------------------------------

## 📊 Dependencias

Este paquete utiliza los siguientes paquetes de R:

-   `httr`: Realización de solicitudes HTTP.
-   `jsonlite`: Procesamiento de datos JSON.
-   `lubridate`: Manejo de fechas.
-   `dplyr`, `tidyverse`: Manipulación y análisis de datos.
-   `DBI`: Interfaz para bases de datos relacionales.
-   `RMySQL`: Conexión a bases de datos MySQL.

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

-   Crear funciones para limpiar datos extraídos de HTML.

-   Crear funciones para interactuar con la base de datos (como optimizar inserciones y consultas).

-   Incorporar soporte para otros medios de comunicación.

-   Optimizar las consultas a la API para mejorar el rendimiento.

-   Documentar ejemplos adicionales para las funciones de base de datos.

-   Crear un pipeline automatizado para la actualización de la base de datos.

-   Crear una tabla en la base de datos para almacenar búsquedas pendientes por realizar desde el json, para almacenarlas en la base de datos notas_biobio o en otras cuando se incluya el soporte a otros medios de comunicación.
