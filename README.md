---
title: 'Visualización de datos: PEC2'
author: "Autor: Gabriela Alejandra Perez"
date: "Fecha de entrega: 06 de Noviembre 2025"
output:
  html_document:
    highlight: default
    number_sections: yes
    theme: cosmo
    toc: yes
    toc_depth: 2
    includes:
      in_header: 75.584-PEC-header.html
  header-includes:
  - \usepackage{booktabs}
  - \usepackage{sectsty} \sectionfont{\centering \emph}
  pdf_document:
    highlight: zenburn
    toc: yes
  word_document: default
editor_options: 
  markdown: 
    wrap: 72
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

------------------------------------------------------------------------

# Técnica de visualización específica/infrecuente: Sparkline

------------------------------------------------------------------------

## Visualización práctica: SPARKLINE DE BARRAS - Evolución continua de una variable

### Descripción del Dataset Utilizado

Para esta visualización práctica, he seleccionado datos del **mercado de valores estadounidense**, específicamente las **10 principales empresas tecnológicas** por capitalización de mercado. Los datos provienen de **Yahoo Finance** y abarcan un período de **12 meses** (noviembre 2024 - noviembre 2025).

**Características del dataset:**

- **Fuente:** Yahoo Finance API  
- **Período:** 365 días (datos diarios)  
- **Variable:** Precio de cierre ajustado en dólares estadounidenses  
- **Empresas incluidas:** AAPL (Apple), MSFT (Microsoft), GOOGL (Alphabet/Google), AMZN (Amazon), TSLA (Tesla), META (Meta/Facebook), NVDA (NVIDIA), NFLX (Netflix), ADBE (Adobe), CRM (Salesforce)

```{r sparkline1, echo=FALSE, results='asis', message=FALSE, warning=FALSE}

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

# Configurar quantmod para evitar plots automáticos
options("getSymbols.warning4.0" = FALSE)
options("getSymbols.auto.assign" = FALSE)

# Cargar todas las librerías
library(readr)
library(dplyr)
library(visNetwork)
library(igraph)
library(knitr)
library(kableExtra)
library(leaflet)
library(geosphere)
library(quantmod)
library(sparkline)
library(htmltools)
library(zoo)


# ============================================
# 1. DEFINIR EMPRESAS
# ============================================
empresas <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", 
              "META", "NVDA", "NFLX", "ADBE", "CRM")

# ============================================
# 2. OBTENER DATOS Y CREAR SPARKLINES
# ============================================
resultados <- list()

for(ticker in empresas) {
  tryCatch({
    # Descargar datos
    datos <- getSymbols(ticker, 
                        src = "yahoo", 
                        from = Sys.Date() - 365, 
                        to = Sys.Date(), 
                        auto.assign = FALSE)
    
    # Extraer precios
    precios <- as.vector(Cl(datos))
    precios <- precios[!is.na(precios)]
    
    # Calcular métricas
    precio_actual <- tail(precios, 1)
    precio_inicial <- head(precios, 1)
    cambio_pct <- ((precio_actual - precio_inicial) / precio_inicial) * 100
    maximo <- max(precios)
    minimo <- min(precios)
    
    # Crear sparkline
    spark <- sparkline(
      precios,
      type = "line",
      width = 200,
      height = 50,
      lineColor = "#17a2b8",           # Celeste (color principal de la línea)
      fillColor = "#d1ecf1",           # Celeste muy claro (relleno)
      minSpotColor = "#dc3545",        # Rojo (punto mínimo) 
      maxSpotColor = "#28a745",        # Verde (punto máximo) 
      spotColor = "#007bff",           # Azul (punto actual)
      spotRadius = 3,                  # Tamaño de los puntos destacados
      lineWidth = 1                    
    )
    
    # Guardar resultado
    resultados[[ticker]] <- list(
      empresa = ticker,
      precio = round(precio_actual, 2),
      cambio = round(cambio_pct, 2),
      minimo = round(minimo, 2),
      maximo = round(maximo, 2),
      grafico = spark
    )
    
  }, error = function(e) {
    cat("Error con", ticker, ":", e$message, "\n")
  })
}

# ============================================
# 3. CREAR TABLA HTML
# ============================================

# Crear dataframe base
df <- data.frame(
  Empresa = character(),
  Precio = character(),
  Cambio = character(),
  Minimo = character(),
  Maximo = character(),
  stringsAsFactors = FALSE
)

# Llenar dataframe y guardar cambios numéricos
cambios_num <- numeric()

for(i in seq_along(resultados)) {
  res <- resultados[[i]]
  cambio_num <- res$cambio
  cambios_num <- c(cambios_num, cambio_num)
  signo <- ifelse(cambio_num >= 0, "+", "")
  
  df <- rbind(df, data.frame(
    Empresa = res$empresa,
    Precio = paste0("$", res$precio),
    Cambio = paste0(signo, cambio_num, "%"),
    Minimo = paste0("$", res$minimo),
    Maximo = paste0("$", res$maximo),
    stringsAsFactors = FALSE
  ))
}

tabla_html <- tags$table(
  class = "table table-striped table-hover",
  style = "width: 100%; max-width: 1000px; margin: 20px auto; border-collapse: collapse;",
  
  tags$thead(
    tags$tr(
      style = "background-color: #3498db; color: white;",
      tags$th(style = "padding: 12px; text-align: left; border: 1px solid #ddd;", "Empresa"),
      tags$th(style = "padding: 12px; text-align: right; border: 1px solid #ddd;", "Precio Actual"),
      tags$th(style = "padding: 12px; text-align: right; border: 1px solid #ddd;", "Cambio Anual"),
      tags$th(style = "padding: 12px; text-align: right; border: 1px solid #ddd;", "Mínimo"),
      tags$th(style = "padding: 12px; text-align: right; border: 1px solid #ddd;", "Máximo"),
      tags$th(style = "padding: 12px; text-align: center; border: 1px solid #ddd;", "Tendencia (12 meses)")
    )
  ),
  
  tags$tbody(
    lapply(1:nrow(df), function(i) {
      color <- ifelse(cambios_num[i] >= 0, "#28a745", "#dc3545")
      bg <- ifelse(cambios_num[i] >= 0, "#d4edda", "#f8d7da")
      
      tags$tr(
        tags$td(style = "padding: 10px; font-weight: bold; border: 1px solid #ddd;", df$Empresa[i]),
        tags$td(style = "padding: 10px; text-align: right; border: 1px solid #ddd;", df$Precio[i]),
        tags$td(style = paste0("padding: 10px; text-align: right; color: ", color, "; background: ", bg, "; font-weight: bold; border: 1px solid #ddd;"), 
                df$Cambio[i]),
        tags$td(style = "padding: 10px; text-align: right; border: 1px solid #ddd;", df$Minimo[i]),
        tags$td(style = "padding: 10px; text-align: right; border: 1px solid #ddd;", df$Maximo[i]),
        tags$td(style = "padding: 10px; text-align: center; border: 1px solid #ddd;", resultados[[i]]$grafico)
      )
    })
  )
)

# ============================================
# 4. MOSTRAR TABLA
# ============================================
browsable(tagList(tabla_html, sparkline(0)))

```

## Otras visualizaciones: SPARKLINE DE BARRAS - Volumen Mensual

Los sparklines de barras representan valores discretos mediante columnas verticales. Son ideales para mostrar:

- Volumen de transacciones por período
- Conteo de eventos o actividades
- Métricas que varían significativamente entre períodos

**Ventaja:** Facilitan la comparación de magnitudes relativas entre períodos.

```{r sparkline2, echo=FALSE, results='asis', message=FALSE, warning=FALSE}
## Ejemplo adicional: Sparkline de barras

### Volumen mensual de transacciones
library(quantmod)
library(sparkline)
library(htmltools)
library(zoo)

# Empresas para análisis de volumen
empresas_vol <- c("AAPL", "TSLA", "GME")  # GME para mostrar alta volatilidad

resultados_barras <- list()

for(ticker in empresas_vol) {
  cat("Procesando volumen de:", ticker, "\n")
  
  tryCatch({
    # Descargar datos del último año
    datos <- getSymbols(ticker, 
                        src = "yahoo", 
                        from = Sys.Date() - 365, 
                        to = Sys.Date(), 
                        auto.assign = FALSE)
    
    # Extraer volumen y agregar por mes
    volumen <- Vo(datos)
    volumen_mensual <- apply.monthly(volumen, sum)
    volumen_vec <- as.vector(volumen_mensual) / 1e6  # En millones
    
    # Tomar últimos 12 meses
    volumen_vec <- tail(volumen_vec, 12)
    
    # Crear sparkline de BARRAS
    spark_barras <- sparkline(
      volumen_vec,
      type = "bar",              
      width = 200,
      height = 50,
      barColor = "#17a2b8",      # Celeste
      barWidth = 10,
      barSpacing = 2
    )
    
    # Calcular métricas
    vol_promedio <- round(mean(volumen_vec), 1)
    vol_max <- round(max(volumen_vec), 1)
    
    resultados_barras[[ticker]] <- list(
      empresa = ticker,
      promedio = vol_promedio,
      maximo = vol_max,
      grafico = spark_barras
    )
    
  }, error = function(e) {
    cat("Error con", ticker, "\n")
  })
}

# Crear tabla
tabla_barras <- tags$table(
  class = "table table-striped",
  style = "width: 100%; max-width: 700px; margin: 20px auto; border-collapse: collapse;",
  
  tags$thead(
    tags$tr(
      style = "background-color: #17a2b8; color: white;",
      tags$th(style = "padding: 12px;", "Empresa"),
      tags$th(style = "padding: 12px; text-align: right;", "Volumen Promedio (M)"),
      tags$th(style = "padding: 12px; text-align: right;", "Volumen Máximo (M)"),
      tags$th(style = "padding: 12px; text-align: center;", "Volumen Mensual (12 meses)")
    )
  ),
  
  tags$tbody(
    lapply(1:length(resultados_barras), function(i) {
      res <- resultados_barras[[i]]
      tags$tr(
        tags$td(style = "padding: 10px; font-weight: bold;", res$empresa),
        tags$td(style = "padding: 10px; text-align: right;", paste0(res$promedio, "M")),
        tags$td(style = "padding: 10px; text-align: right;", paste0(res$maximo, "M")),
        tags$td(style = "padding: 10px; text-align: center;", res$grafico)
      )
    })
  )
)

browsable(tagList(tabla_barras, sparkline(0)))


```

## Otras visualizaciones: SPARKLINE WIN/LOSS - Rendimiento Mensual

Los sparklines de ganancia/pérdida (también llamados "tristate") representan valores positivos, negativos y neutros mediante barras de colores. Son útiles para:

- Identificar rachas de éxito o fracaso
- Visualizar consistencia de resultados
- Detectar patrones de alternancia

**Ventaja:** Resalta inmediatamente períodos favorables vs. desfavorables sin necesidad de leer números.

```{r sparkline3, echo=FALSE, results='asis', message=FALSE, warning=FALSE}
## Ejemplo adicional: Sparkline de ganancia/pérdida

### Rendimiento mensual positivo/negativo
library(quantmod)
library(sparkline)
library(htmltools)
library(zoo)

# Empresas para análisis mensual
empresas_mensual <- c("NVDA", "ADBE", "CRM")

resultados_winloss <- list()

for(ticker in empresas_mensual) {
  cat("Procesando rendimiento mensual de:", ticker, "\n")
  
  tryCatch({
    # Descargar datos del último año
    datos <- getSymbols(ticker, 
                        src = "yahoo", 
                        from = Sys.Date() - 365, 
                        to = Sys.Date(), 
                        auto.assign = FALSE)
    
    # Calcular rendimiento mensual
    precios_mensuales <- to.monthly(datos)
    precios_cierre <- Cl(precios_mensuales)
    
    # Calcular cambio porcentual mensual
    rendimiento_mensual <- diff(precios_cierre) / lag(precios_cierre) * 100
    rendimiento_mensual <- na.omit(rendimiento_mensual)
    rendimiento_vec <- as.vector(rendimiento_mensual)
    
    # Tomar últimos 12 meses
    rendimiento_vec <- tail(rendimiento_vec, 12)
    
    # Contar meses positivos
    meses_positivos <- sum(rendimiento_vec > 0)
    meses_negativos <- sum(rendimiento_vec < 0)
    
    # Crear sparkline de GANANCIA/PÉRDIDA
    spark_winloss <- sparkline(
      rendimiento_vec,
      type = "tristate",         
      width = 200,
      height = 50,
      posBarColor = "#28a745",   # Verde para ganancias
      negBarColor = "#dc3545",   # Rojo para pérdidas
      zeroBarColor = "#6c757d",  # Gris para neutro
      barWidth = 12,
      barSpacing = 2
    )
    
    resultados_winloss[[ticker]] <- list(
      empresa = ticker,
      positivos = meses_positivos,
      negativos = meses_negativos,
      grafico = spark_winloss
    )
    
  }, error = function(e) {
    cat("Error con", ticker, "\n")
  })
}

# Crear tabla
tabla_winloss <- tags$table(
  class = "table table-striped",
  style = "width: 100%; max-width: 700px; margin: 20px auto; border-collapse: collapse;",
  
  tags$thead(
    tags$tr(
      style = "background-color: #6c757d; color: white;",
      tags$th(style = "padding: 12px;", "Empresa"),
      tags$th(style = "padding: 12px; text-align: center;", "Meses ↑"),
      tags$th(style = "padding: 12px; text-align: center;", "Meses ↓"),
      tags$th(style = "padding: 12px; text-align: center;", "Rendimiento Mensual (12 meses)")
    )
  ),
  
  tags$tbody(
    lapply(1:length(resultados_winloss), function(i) {
      res <- resultados_winloss[[i]]
      tags$tr(
        tags$td(style = "padding: 10px; font-weight: bold;", res$empresa),
        tags$td(style = "padding: 10px; text-align: center; color: #28a745; font-weight: bold;", 
                paste0(res$positivos, " meses")),
        tags$td(style = "padding: 10px; text-align: center; color: #dc3545; font-weight: bold;", 
                paste0(res$negativos, " meses")),
        tags$td(style = "padding: 10px; text-align: center;", res$grafico)
      )
    })
  )
)

browsable(tagList(tabla_winloss, sparkline(0)))
```

------------------------------------------------------------------------

# Técnica de visualización avanzada: Diagrama de Red

------------------------------------------------------------------------

## Visualización práctica

### Descripción del Dataset

Para esta visualización he utilizado el dataset **USA Airport Flight Routes** de Kaggle, que contiene información detallada sobre rutas aéreas comerciales entre aeropuertos de Estados Unidos, incluyendo número de pasajeros, vuelos y distancias.

**Fuente:** Kaggle - USA Airport Dataset  
**URL:** https://www.kaggle.com/datasets/flashgordon/usa-airport-dataset  
**Período:** 1990-2009  
**Registros:** Más de 3 millones de vuelos

```{r cargar-datos-red, message=FALSE, warning=FALSE, cache=TRUE}
# Instalar paquetes necesarios para el diagrama de red

library(readr)
library(dplyr)
library(dplyr)
library(visNetwork)
library(igraph)
library(knitr)
library(kableExtra)
library(leaflet)
library(geosphere)

# Leer el archivo (ajusta el nombre si es diferente)
datos_vuelos <- read_csv("Airports2.csv", show_col_types = FALSE)

# Ver estructura
cat("Total de registros de vuelos:", nrow(datos_vuelos), "\n")
cat("Columnas disponibles:", paste(names(datos_vuelos), collapse = ", "), "\n")

library(dplyr)
library(leaflet)
library(geosphere)
library(readr)
# ============================================
# 1. EXTRAER AEROPUERTOS ÚNICOS
# ============================================

# Obtener aeropuertos de origen
aeropuertos_origen <- datos_vuelos %>%
  select(
    codigo = Origin_airport,
    ciudad = Origin_city,
    lat = Org_airport_lat,
    lon = Org_airport_long,
    poblacion = Origin_population
  ) %>%
  distinct()

# Obtener aeropuertos de destino
aeropuertos_destino <- datos_vuelos %>%
  select(
    codigo = Destination_airport,
    ciudad = Destination_city,
    lat = Dest_airport_lat,
    lon = Dest_airport_long,
    poblacion = Destination_population
  ) %>%
  distinct()

# Combinar y eliminar duplicados
aeropuertos_todos <- bind_rows(aeropuertos_origen, aeropuertos_destino) %>%
  group_by(codigo) %>%
  summarise(
    ciudad = first(na.omit(ciudad)),
    lat = first(na.omit(lat)),
    lon = first(na.omit(lon)),
    poblacion = first(na.omit(poblacion)),
    .groups = "drop"
  ) %>%
  filter(!is.na(lat), !is.na(lon)) # Eliminar aeropuertos sin coordenadas

cat("\nTotal de aeropuertos únicos con coordenadas:", nrow(aeropuertos_todos), "\n")

# ============================================
# 2. AGREGAR RUTAS Y FILTRAR
# ============================================

# Agregar datos por ruta (sumar pasajeros, vuelos)
rutas_agregadas <- datos_vuelos %>%
  filter(!is.na(Org_airport_lat), !is.na(Dest_airport_lat)) %>%
  group_by(Origin_airport, Destination_airport) %>%
  summarise(
    total_pasajeros = sum(Passengers, na.rm = TRUE),
    total_vuelos = sum(Flights, na.rm = TRUE),
    distancia = first(Distance),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pasajeros))

# Seleccionar solo los TOP 15 aeropuertos por tráfico
top_aeropuertos <- rutas_agregadas %>%
  group_by(Origin_airport) %>%
  summarise(trafico = sum(total_pasajeros), .groups = "drop") %>%
  arrange(desc(trafico)) %>%
  head(15) %>%
  pull(Origin_airport)

cat("Top 15 aeropuertos por tráfico:", paste(top_aeropuertos, collapse = ", "), "\n")

# Filtrar aeropuertos
aeropuertos_viz <- aeropuertos_todos %>%
  filter(codigo %in% top_aeropuertos)

# Filtrar las 50 rutas más transitadas
rutas_viz <- rutas_agregadas %>%
  filter(
    Origin_airport %in% top_aeropuertos,
    Destination_airport %in% top_aeropuertos
    
  ) %>%
  arrange(desc(total_pasajeros)) %>%
  head(50) 

cat("\nAeropuertos para visualización:", nrow(aeropuertos_viz), "\n")
cat("Rutas para visualización:", nrow(rutas_viz), "\n")
```

```{r diagrama-red-usa, echo=FALSE, results='asis', message=FALSE, warning=FALSE, fig.width=10, fig.height=8}
library(visNetwork)
library(igraph)

# ============================================
# 3. PREPARAR NODOS
# ============================================

# Calcular número de conexiones por aeropuerto
conexiones <- rutas_viz %>%
  group_by(Origin_airport) %>%
  summarise(num_conexiones = n(), .groups = "drop")

# Crear dataframe de nodos
nodos <- aeropuertos_viz %>%
  left_join(conexiones, by = c("codigo" = "Origin_airport")) %>%
  mutate(
    id = codigo,
    label = codigo,
    value = ifelse(is.na(num_conexiones), 1, num_conexiones),
    title = paste0(
      "<b>", ciudad, "</b><br>",
      "Código: ", codigo, "<br>",
      "Conexiones: ", value, "<br>",
      "Población: ", format(poblacion, big.mark = ",")
    ),
    # Agrupar por región (simplificado por estado extraído de ciudad)
    group = ifelse(grepl("CA", ciudad), "California",
            ifelse(grepl("TX", ciudad), "Texas", 
            ifelse(grepl("FL", ciudad), "Florida",
            ifelse(grepl("NY", ciudad), "Nueva York", "Otro"))))
  ) %>%
  select(id, label, value, title, group)

# ============================================
# 4. PREPARAR ENLACES
# ============================================

# Normalizar valores para grosor de líneas
max_pasajeros <- max(rutas_viz$total_pasajeros)

enlaces <- rutas_viz %>%
  mutate(
    from = Origin_airport,
    to = Destination_airport,
    value = round(total_pasajeros / max_pasajeros * 10) + 1,  # Escala 1-10
    title = paste0(
      from, " → ", to, "<br>",
      "Pasajeros: ", format(total_pasajeros, big.mark = ","), "<br>",
      "Vuelos: ", format(total_vuelos, big.mark = ","), "<br>",
      "Distancia: ", distancia, " millas"
    ),
    color = "#17a2b8"
  ) %>%
  select(from, to, value, title, color)

# ============================================
# 5. CREAR VISUALIZACIÓN
# ============================================

visNetwork(nodos, enlaces, width = "100%", height = "600px") %>%
  visNodes(
    shape = "dot",
    scaling = list(min = 20, max = 50),
    font = list(size = 14, face = "arial", color = "#2c3e50", bold = TRUE),
    borderWidth = 2,
    color = list(
      background = "#e8f4f8",
      border = "#17a2b8",
      highlight = list(background = "#ffc107", border = "#ff9800")
    )
  ) %>%
  visEdges(
    smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2),
    color = list(color = "#17a2b8", highlight = "#ff9800"),
    arrows = list(to = list(enabled = FALSE))
  ) %>%
  visGroups(groupname = "California", color = "#e74c3c") %>%
  visGroups(groupname = "Texas", color = "#3498db") %>%
  visGroups(groupname = "Florida", color = "#2ecc71") %>%
  visGroups(groupname = "Nueva York", color = "#9b59b6") %>%
  visGroups(groupname = "Otro", color = "#95a5a6") %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1),
    nodesIdSelection = TRUE
  ) %>%
  visInteraction(
    navigationButtons = TRUE,
    hover = TRUE,
    tooltipDelay = 100
  ) %>%
  visLayout(randomSeed = 123) %>%
  visPhysics(
    stabilization = TRUE,
    barnesHut = list(gravitationalConstant = -8000, springLength = 200)
  ) %>%
  visLegend(
    width = 0.15,
    position = "right",
    main = "Región"
  )
```

## Otras visualizaciones: Con mapa

```{r mapa-red-usa, echo=FALSE}
library(dplyr)
library(leaflet)
library(geosphere)
library(readr)

# ============================================
# PREPARAR DATOS PARA MAPA
# ============================================

# Nodos con coordenadas
nodos_mapa <- aeropuertos_viz %>%
  left_join(conexiones, by = c("codigo" = "Origin_airport")) %>%
  mutate(
    num_conexiones = ifelse(is.na(num_conexiones), 1, num_conexiones)
  )

rutas_mapa <- rutas_viz

# ============================================
# CREAR PALETA DE COLORES
# ============================================

pal <- colorNumeric(
  palette = c("#3498db", "#17a2b8", "#f39c12", "#e74c3c"),
  domain = rutas_mapa$total_pasajeros
)

# ============================================
# CREAR MAPA BASE
# ============================================

mapa <- leaflet(width = "100%", height = "600px") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -95, lat = 39, zoom = 4)

# ============================================
# AÑADIR RUTAS CON COLORES SEGÚN TRÁFICO
# ============================================

for(i in 1:nrow(rutas_mapa)) {
  origen <- nodos_mapa %>% filter(codigo == rutas_mapa$Origin_airport[i])
  destino <- nodos_mapa %>% filter(codigo == rutas_mapa$Destination_airport[i])
  
  if(nrow(origen) > 0 & nrow(destino) > 0) {
    # Calcular línea curva (great circle)
    ruta_curva <- gcIntermediate(
      c(origen$lon, origen$lat),
      c(destino$lon, destino$lat),
      n = 50,
      addStartEnd = TRUE
    )
    
    # Grosor basado en pasajeros
    grosor <- 1 + (rutas_mapa$total_pasajeros[i] / max(rutas_mapa$total_pasajeros)) * 5
    
    # Añadir línea con color según tráfico
    mapa <- mapa %>%
      addPolylines(
        lng = ruta_curva[,1],
        lat = ruta_curva[,2],
        color = pal(rutas_mapa$total_pasajeros[i]),
        weight = grosor,
        opacity = 0.7,
        popup = paste0(
          "<b>", rutas_mapa$Origin_airport[i], " → ", 
          rutas_mapa$Destination_airport[i], "</b><br>",
          "Pasajeros: ", format(rutas_mapa$total_pasajeros[i], big.mark = ","), "<br>",
          "Vuelos: ", format(rutas_mapa$total_vuelos[i], big.mark = ","), "<br>",
          "Distancia: ", rutas_mapa$distancia[i], " millas"
        )
      )
  }
}

# ============================================
# AÑADIR AEROPUERTOS
# ============================================

mapa <- mapa %>%
  addCircleMarkers(
    data = nodos_mapa,
    lng = ~lon,
    lat = ~lat,
    radius = ~sqrt(num_conexiones) * 3,
    color = "#2c3e50",
    fillColor = "#e74c3c",
    fillOpacity = 0.7,
    weight = 2,
    popup = ~paste0(
      "<b>", ciudad, "</b><br>",
      "Código: ", codigo, "<br>",
      "Conexiones: ", num_conexiones, "<br>",
      "Población: ", format(poblacion, big.mark = ",")
    ),
    label = ~codigo,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      style = list(
        "font-weight" = "bold",
        "font-size" = "12px"
      )
    )
  )

# ============================================
# AÑADIR LEYENDA CON ESCALA
# ============================================

mapa <- mapa %>%
  leaflet::addLegend(
    position = "bottomright",
    pal = pal,
    values = rutas_mapa$total_pasajeros,
    title = "Volumen de<br>Pasajeros",
    opacity = 0.8,
    labFormat = leaflet::labelFormat(
      big.mark = ",",
      digits = 0
    )
  )

# Mostrar mapa
mapa
```

------------------------------------------------------------------------

# Técnica de visualización básica: Cartograma

------------------------------------------------------------------------

### Descripción del Dataset

Para esta visualización, crearemos un cartograma contiguo de los Estados Unidos, donde el área de cada estado será distorsionada para ser proporcional a su población.

- **Datos Geográficos:** Usaremos el paquete tigris de R para obtener los polígonos (shapefiles) de los 50 estados de EE. UU.

- **Datos Cuantitativos:** Usaremos el dataset state.x77 incorporado en R, que incluye la población de los estados (estimación de 1975).

Uniremos estos dos datasets por el nombre del estado y luego usaremos el paquete cartogram para generar la geometría distorsionada.

```{r cartograma-usa, message=FALSE, warning=FALSE, cache=TRUE}
# Cargar librerías necesarias
library(sf)           # Para manejar datos geoespaciales (simple features)
library(tigris)       # Para descargar las geometrías de los estados
library(cartogram)    # El paquete que crea los cartogramas
library(ggplot2)      # Para visualizar el resultado
library(dplyr)        # Para manipulación de datos
library(stringr)      # Para limpieza de texto

# ============================================
# 1. OBTENER Y PREPARAR DATOS
# ============================================
datos_poblacion <- as.data.frame(state.x77) %>%
  tibble::rownames_to_column("NAME") %>%
  select(NAME, Population)

# Obtener los datos geoespaciales
us_states <- tigris::states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "AS", "MP"))

# ============================================
# 2. UNIR DATOS
# ============================================
states_con_poblacion <- us_states %>%
  left_join(datos_poblacion, by = "NAME") %>%
  filter(!is.na(Population)) %>%
  st_transform(5070) 

# ============================================
# 3. CONSTRUIR EL CARTOGRAMA
# ============================================
# Usar el namespace explícito para evitar conflictos
carto_us_poblacion <- cartogram::cartogram_cont(
  states_con_poblacion, 
  "Population",     
  itermax = 10      
)

# ============================================
# 4. VISUALIZAR
# ============================================
ggplot(carto_us_poblacion) +
  geom_sf(aes(fill = Population), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    labels = scales::comma,
    option = "plasma"  
  ) +
  labs(
    title = "Cartograma de Población de EE.UU. (1975)",
    subtitle = "El área de cada estado es proporcional a su población",
    fill = "Población"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )
```



## Conclusiones sobre Cartogramas

Los cartogramas son una herramienta de visualización fundamental para **corregir la percepción geoespacial** y revelar la verdadera magnitud de una variable. Son la técnica superior cuando el objetivo no es mostrar dónde ocurre algo en un sentido geográfico estricto, sino cuál es el peso real de esa variable (como población, riqueza o poder electoral).

Su efectividad se maximiza cuando:

1. Se trabaja con **datos de totales absolutos** (ej. población total, PIB total, número de votos).
2. El objetivo es **desafiar el sesgo visual** que introducen los mapas tradicionales, donde áreas geográficas extensas pero con valores bajos (ej. estados rurales, países desérticos) dominan la visualización.
3. Se busca una **comunicación impactante** que muestre la escala "humana" o "económica" de un fenómeno.

La principal contrapartida del cartograma es el **sacrificio de la precisión geométrica**. Al distorsionar el área, las formas familiares de los países o estados pueden volverse irreconocibles, lo que requiere un esfuerzo cognitivo inicial por parte del espectador.

Sin embargo, este "costo" es precisamente su fortaleza: el cartograma obliga al usuario a dejar de ver la geografía (la tierra) y a empezar a ver los datos (las personas, el dinero, los votos), proporcionando una perspectiva más justa y precisa de la distribución del fenómeno estudiado.
