# Paquete `datamedios` (Versión 0.1.1)

`datamedios` es un paquete de R que facilita la extracción automatizada de noticias desde los medios de comunicación chilenos, en particular desde la API de BíoBío.cl. A través de sus funciones, puedes obtener noticias a partir de frases de búsqueda y filtrar los resultados por fechas específicas.

## Instalación

Para instalar el paquete desde GitHub, primero necesitas instalar el paquete `devtools` si no lo tienes. Luego, puedes instalar `datamedios` con el siguiente código:
```         
r
# Instalar devtools si no lo tienes
install.packages("devtools")
```

# Instalar el paquete desde GitHub

devtools::install_github("exetrujillo/datamedios")

## Uso

### `extraer_noticias`

Esta función permite realizar una extracción de noticias utilizando una frase de búsqueda y un número máximo de resultados a extraer.

#### Parámetros:

-   `search_query`: Una frase de búsqueda que será utilizada para consultar las noticias (obligatorio).

-   `max_results`: Número máximo de resultados a extraer. Si no se especifica, se extraen todos los resultados disponibles (opcional).

#### Valor:

La función devuelve un dataframe con las noticias extraídas, que incluye:

-   `ID`: ID de la noticia.

-   `post_title`: Título de la noticia.

-   `post_content`: Contenido completo de la noticia.

-   `post_excerpt`: Resumen o extracto de la noticia.

-   `post_URL`: Enlace a la noticia en el sitio web.

-   `post_categories`: Categorías de la noticia.

-   `post_tags`: Etiquetas de la noticia.

-   `year, month, day`: Fecha de la noticia en formato año, mes y día.

-   `post_category_primary.name`: Categoría principal de la noticia.

-   `post_category_secondary.name`: Categoría secundaria de la noticia.

-   `post_image.URL`: URL de la imagen asociada a la noticia.

-   `post_image.alt`: Texto alternativo de la imagen.

-   `post_image.caption`: Leyenda de la imagen.

-   `author.display_name`: Nombre del autor.

-   `raw_post_date`: Fecha de publicación de la noticia.

-   `resumen_de_ia`: Resumen generado por inteligencia artificial, si está disponible.

#### Ejemplo:

```         
r
# Buscar noticias relacionadas con "inteligencia artificial"
noticias <- extraer_noticias("inteligencia artificial", max_results = 100) 
```

### `extraer_noticias_fecha`

Esta función permite realizar una extracción de noticias desde la API de BíoBío.cl, filtrando los resultados por un rango de fechas especificado.

#### Parámetros:

-   `search_query`: Una frase de búsqueda que se usará para filtrar las noticias (obligatorio).

-   `fecha_inicio`: Fecha de inicio del rango de búsqueda en formato "YYYY-MM-DD" (obligatorio).

-   `fecha_fin`: Fecha de fin del rango de búsqueda en formato "YYYY-MM-DD" (obligatorio).

#### Valor:

La función devuelve un dataframe con las noticias extraídas dentro del rango de fechas especificado. Las columnas devueltas son las mismas que para `extraer_noticias`, pero las noticias estarán filtradas por las fechas de publicación.

#### Ejemplo:

```         
r
# Buscar noticias relacionadas con "inteligencia artificial" entre el 1 de enero y el 31 de diciembre de 2023
noticias <- extraer_noticias_fecha("inteligencia artificial", "2023-01-01", "2023-12-31") 
```

## Documentación

La documentación completa de las funciones está disponible en el paquete. Para acceder a ella, puedes usar `help()`:

```         
r
help(package = "datamedios")
```

## Dependencias

Este paquete requiere los siguientes paquetes:

-   `httr`: Para realizar solicitudes HTTP.

-   `jsonlite`: Para procesar datos en formato JSON.

-   `lubridate`: Para manejar fechas.

-   `dplyr`, `tidyverse`: Para el manejo de datos.

## Licencia

Este paquete está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.

## Autoría

Este paquete fue desarrollado por:

-   Exequiel Trujillo e Ismael Aguayo

## Agradecimientos

Este paquete utiliza varias librerías de R de código abierto, como `httr`, `jsonlite`, y `dplyr`, para realizar las tareas de scraping y manipulación de datos.
