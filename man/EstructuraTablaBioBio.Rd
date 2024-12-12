\name{notas_biobio_structure}
\alias{notas_biobio_structure}
\title{Estructura de la tabla notas_biobio}
\description{
La tabla \code{notas_biobio} es una estructura en la base de datos diseñada para almacenar información sobre noticias obtenidas del medio Biobío. A continuación, se detalla su propósito y la estructura de sus campos, indicando los tipos de datos asociados y su uso previsto.
}
\details{
La tabla contiene los siguientes campos:
\itemize{
\item{\code{ID (VARCHAR(255), PRIMARY KEY)}: Identificador único de la noticia.}
\item{\code{post_title (VARCHAR(255))}: Título de la noticia.}
\item{\code{post_content (TEXT)}: Contenido completo de la noticia.}
\item{\code{post_excerpt (TEXT)}: Resumen breve de la noticia.}
\item{\code{post_URL (TEXT)}: URL completa de la noticia.}
\item{\code{post_categories (JSON)}: Lista de categorías asociadas a la noticia, almacenadas en formato JSON (incluye ID, nombre y slug de cada categoría).}
\item{\code{post_tags (JSON)}: Lista de etiquetas asociadas a la noticia, almacenadas en formato JSON (incluye ID, nombre y slug de cada etiqueta).}
\item{\code{year (INT)}: Año de publicación de la noticia.}
\item{\code{month (INT)}: Mes de publicación de la noticia.}
\item{\code{day (INT)}: Día de publicación de la noticia.}
\item{\code{post_category_primary_name (VARCHAR(255))}: Nombre de la categoría primaria asignada a la noticia.}
\item{\code{post_category_secondary_name (VARCHAR(255))}: Nombre de la categoría secundaria asignada a la noticia.}
\item{\code{post_image_URL (TEXT)}: URL de la imagen destacada de la noticia.}
\item{\code{post_image_alt (TEXT)}: Descripción alternativa (\code{alt}) de la imagen destacada.}
\item{\code{post_image_caption (TEXT)}: Pie de foto de la imagen destacada.}
\item{\code{author_display_name (VARCHAR(255))}: Nombre del autor o creador de la noticia.}
\item{\code{raw_post_date (DATE)}: Fecha cruda de publicación en formato \code{YYYY-MM-DD}.}
\item{\code{resumen_de_ia (TEXT)}: Resumen de la noticia generado automáticamente por inteligencia artificial, si aplica.}
}
}
