# Procesamiento y visualización de datos espaciales

# Preparativos
# Carga de paquetes
library(dplyr)
library(DT)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(raster)
library(sf)
library(spData)

# Carga de datos

# Carga de capas de orquídeas
orquideas <-
  sf::st_read(
  "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/orchidaceae-cr-registros.csv",
  options = c(
    "X_POSSIBLE_NAMES=decimalLongitude",
    "Y_POSSIBLE_NAMES=decimalLatitude"
  ),
  quiet = TRUE
)

# Carga de capa Areas Silvestres Protegidas (ASP)
asp <-
  sf::st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson",
    quiet = TRUE
  )

# Asignar proyecciones
sf::st_crs(asp) = 4326
sf::st_crs(orquideas) = 4326

# Limpieza
# Asignar los NA a una variable
orquideas$species[orquideas$species == ""] <- "orquideas"

# Conversion de los valores
orquideas <- 
  orquideas %>%
  mutate(coordinateUncertaintyInMeters = as.numeric(coordinateUncertaintyInMeters)) %>%
  mutate(eventDate = as.Date(eventDate, "%Y-%m-%d"))

cat("Cantidad original de registros:", nrow(orquideas))

# Limpieza de los valores de alta incertidumbre
orquideas <-
  orquideas %>%
  filter(!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= 1000)

cat("Cantidad de registros despues de limpiar los de alta incertidumbre:", nrow(orquideas))

# Limpieza de los NA en los registros de presencia
orquideas <-
  orquideas %>%
  filter(species!= "orquideas")

cat("Cantidad de registros despues de limpiar los de alta incertidumbre:", nrow(orquideas))

# Limpieza del data asp
asp <-
  asp %>%
  filter(descripcio!="Area Marina de Manejo" & descripcio!="Area marina protegida")

# Procesamiento y visualizacion
# Mapa de cantidad de registros por asp
# Creacion de un conjunto de datos con la cantidad de registros por provincia
registros_asp <-
  asp %>%
  sf::st_make_valid() %>%
  sf::st_join(orquideas) %>%
  group_by(nombre_asp) %>%
  summarize(especies = n())

# Asignacion de crs a la capa registros_asp
sf::st_crs(registros_asp) = 4326

# Asignación de la paleta de colores
colores <-
  colorNumeric(palette = "PuRd",
               domain = registros_asp$especies,
               na.color = "transparent")

# Mapeo 
leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, 
                   group = "Esri.WorldGrayCanvas") %>%
  addTiles(group = "OMS") %>%
  addPolygons(
    data = registros_asp,
    fillColor = ~ colores (registros_asp$especies),
    fillOpacity = 1,
    stroke = TRUE,
    color = "black",
    weight = 1,
    popup = paste(
      paste(
        "<strong>Área Silvestre Protegida:</strong>",
        registros_asp$nombre_asp
      ),
      paste(
        "<strong>Cantidad de orquídeas:</strong>",
        registros_asp$especies
        
      ),
      sep = '<br/>'
    ),
    group = "Cantidad de registros"
  ) %>%
  addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "OMS"),
                   overlayGroups = c("Cantidad de registros")) %>%
  addSearchOSM() %>%
  addMouseCoordinates() %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
  addLegend(
  position = "bottomleft",
  pal = colores,
  values = registros_asp$especies,
  group = "Cantidad de registros",
  title = "Cantidad orquídeas")