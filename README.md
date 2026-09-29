# EDM_PetFinder

Sitio del proyecto **Predicción y explicación de la velocidad de adopción de mascotas
(PetFinder.my) mediante aprendizaje supervisado sobre datos secundarios**.

Asignatura: **EDM — Estadística Descriptiva Multivariada** · Universidad Nacional de Colombia
Autoría: Jared Mijail Ramírez Escalante · María Paula Silva Capera
Profesor: Camilo José Torres Jiménez

## Estructura del sitio

| Página | Contenido |
|:---------------------------------------------|:------------------------------------------------------------------|
| `index.qmd` | Portada del sitio |
| `proyecto/planteamiento.qmd` | El problema, la plataforma, preguntas, hipótesis y propósito |
| `proyecto/marco-conceptual.qmd` | Antecedentes, vacíos, marco teórico y marco propio |
| `proyecto/objetivos.qmd` | Objetivo general y objetivos específicos tipificados |
| `proyecto/justificacion.qmd` | Aporte social, científico, metodológico, institucional y personal |
| `proyecto/metodologia.qmd` | Diseño, unidades, variables, procesamiento, técnicas, ética y limitaciones |
| `proyecto/productos.qmd` | Entregables y estructura del informe final |
| `proyecto/bibliografia.qmd` | Bibliografía completa (generada desde `referencias.bib`) |
| `entregas/analisis-exploratorio.qmd` | Entrega 1 — **análisis univariado** completo (general, cuantitativo y cualitativo) |
| `entregas/dataset.qmd` | Entrega 1 — dataset, diccionario de variables y enriquecimiento |

Los componentes del proyecto siguen la estructura de Briones (ICFES, Módulo tres). El estudio
trabaja con **datos secundarios**, así que la metodología declara explícitamente qué
componentes de la plantilla se aplican tal cual y cuáles se redefinen (no hay diseño muestral
ni instrumentos primarios ni trabajo de campo).

## Análisis univariado (Entrega 1)

La página `entregas/analisis-exploratorio.qmd` tiene tres partes y analiza las 31 variables,
una por una:

1. **Análisis general:** dimensión de la base, tipos de variables, valores perdidos, duplicados,
   resumen global de las cuantitativas y distribución por tipo de variable.
2. **Análisis cuantitativo (12 variables):** tabla de estadísticos (n, perdidos, media, mediana,
   moda, mínimo, máximo, cuartiles, IQR, rango, varianza, desviación estándar, coeficiente de
   variación, asimetría y curtosis), tabla de detección de atípicos (1,5 × IQR y |z| > 3), cuatro
   gráficos por variable (histograma, caja, densidad y QQ-plot — o valores más frecuentes en las
   variables de conteo) y una interpretación escrita.
3. **Análisis cualitativo (19 variables):** frecuencias absolutas, relativas y acumuladas, moda,
   número de categorías, categorías poco frecuentes, gráfico de barras, pastel cuando aporta y una
   interpretación escrita. En las variables de alta cardinalidad (`Breed1`, `Breed2`, `State`,
   `Name`) se muestran las categorías más frecuentes y el resto se agrupa en «Otras».

Es un análisis **estrictamente univariado**: no hay cruces de variables, medidas de asociación,
correlaciones ni técnicas multivariadas (eso corresponde a las entregas siguientes).

Los ayudantes de cálculo y de gráficos están en el primer bloque de código de la página
(`fmt`, `ent`, `pct`, `moda`, `asimetria`, `curtosis`, `esc_num`, `esc_cat`,
`tabla_estadisticos`, `tabla_atipicos`, `tabla_frecuencias`, `tabla_cardinalidad`,
`graficos_cuanti`, `graficos_cuali`); las cifras que aparecen en los textos se calculan en el
momento de renderizar, de modo que no hay resultados transcritos a mano.

El código se muestra siempre en bloques con **fondo claro y texto oscuro** (palabras clave en
azul oscuro, comentarios en verde oscuro) tanto en el tema claro como en el oscuro; los estilos
están en `styles.css`.

## Requisitos

* R ≥ 4.4 (probado con 4.6.1)
* Quarto ≥ 1.5 (probado con 1.10.18). Si no está en el `PATH`, con RStudio instalado suele
  estar en `/usr/lib/rstudio/resources/app/bin/quarto/bin/quarto`.
* `renv` para restaurar las dependencias

```bash
# Dependencias de R
Rscript -e 'renv::restore()'

# Vista previa del sitio
quarto preview

# Construcción del sitio en _site/
quarto render
```

## Datos

Los datos **no se versionan** (ocupan ~550 MB) y están excluidos en `.gitignore`. Para
renderizar el sitio con resultados recalculados en local, la carpeta `data/` debe contener:

```
data/
├── train.csv                          # Anuncios originales (Kaggle 2019)
├── train_sentiment/                   # JSON de sentimiento por anuncio
├── train_metadata/                    # JSON de etiquetas/rostros/colores por imagen
├── petfinder_enriquecido_final.csv    # Base enriquecida (salida del script)
└── petfinder_enriquecido_final.rds    # La misma base en formato R
```

Los resultados ya ejecutados de cada bloque de código se guardan en `_freeze/`, que **sí** se
versiona: así el sitio se puede construir (y publicar) sin tener los datos crudos en disco.

## Flujo de trabajo

```bash
# 1. Construir la base enriquecida (requiere data/train.csv y las dos carpetas de JSON)
Rscript scripts/01_dataset_enriquecido.R

# 2. Reconstruir el sitio
quarto render
```

`scripts/01_dataset_enriquecido.R` hace ocho pasos: define rutas con `here`, lee el archivo
principal, extrae el sentimiento y las anotaciones de imagen de los JSON, une las tres fuentes
por `PetID`, convierte las categóricas a factores con etiquetas legibles, calcula la longitud
del relato y guarda la base en CSV y RDS con un diagnóstico de faltantes.

## Estructura del repositorio

```
├── _quarto.yml                    # Configuración del sitio (navbar, sidebar, temas)
├── styles.css                     # Estilos propios, tema claro/oscuro, impresión y pantallas anchas
├── referencias.bib                # Bibliografía en BibTeX
├── index.qmd                      # Portada
├── proyecto/                      # Componentes del proyecto de investigación
├── entregas/                      # Productos de la Entrega 1
├── scripts/                       # Código de procesamiento
├── data/                          # Datos (ignorados por Git: ~550 MB)
├── _borradores/                   # Material de trabajo: no se publica en el sitio
├── _freeze/                       # Resultados en caché de los bloques de R (versionado)
└── renv.lock                      # Versiones de las dependencias
```

## Notas de mantenimiento

* **`_borradores/`** guarda material que **no** se publica en el sitio: la primera versión del
  análisis exploratorio (con cruces y medidas de asociación, útil para las entregas siguientes) y
  los scripts de exploración usados para obtener las cifras (`explora_univariado.R`,
  `explora2.R`).
* `project.render` en `_quarto.yml` limita la construcción a los archivos `.qmd`, para que los
  intermedios de knitr (`*.knit.md`) no se publiquen como páginas del sitio.
* Las páginas del sitio llevan la hoja de estilos de impresión: con `Ctrl` + `P` (o
  «Guardar como PDF») cualquier página sale como documento, sin barras de navegación.
* Para publicar en GitHub Pages basta renderizar y hacer *push* de `_site/`.
