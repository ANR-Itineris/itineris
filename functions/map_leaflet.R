#' Create an HTML interactive map for the site distribution
#' @name map_leaflet
#' @description
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#' @param out.plot the path and name of the created map
#'
#' @return a leaflet interactive map
#'
#' @examples
#'
#' @export

# path.data <- "https://raw.githubusercontent.com/ANR-Itineris/itineris/main/data/"
# df <- read.table(paste0(path.data, "sites_coords.tsv"), sep = "\t", header = T)

map_leaflet <- function(df, out.plot = paste0(getwd(), "/data/map_sites.html")){
  #lbl <- '<a href = "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/EAMENA-0164997.geojson">EAMENA-0164997.geojson</a>'
  map.name <- "map_sites"
  ea.map <- leaflet::leaflet(df) %>%
    leaflet::addProviderTiles(leaflet::providers$"Esri.WorldImagery", group = "Ortho") %>%
    leaflet::addProviderTiles(leaflet::providers$"OpenStreetMap", group = "OSM") %>%
    leaflet::addCircleMarkers(
      lng = ~x,
      lat = ~y,
      weight = 1,
      radius = 5,
      popup = ~site,
      color = "red",
      fillOpacity = 1,
      opacity = 1) %>%
    leaflet::addLayersControl(
      baseGroups = c("Ortho", "OSM"),
      position = "topright") %>%
    leaflet::addScaleBar(position = "bottomright")
  print(ea.map)
  htmlwidgets::saveWidget(ea.map, out.plot)
  print(paste("interactive map created in:", out.plot))

}

