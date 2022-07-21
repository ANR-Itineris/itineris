#' Create an HTML interactive map for the site distribution
#' @name map_leaflet
#' @description
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#'
#' @return a leaflet interactive map
#'
#' @examples
#'
#' @export
map_leaflet <- function(df){
#lbl <- '<a href = "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/EAMENA-0164997.geojson">EAMENA-0164997.geojson</a>'
map.name <- "map_sites"
ea.map <- leaflet::leaflet(df) %>%
  leaflet::addProviderTiles(providers$"Esri.WorldImagery", group = "Ortho") %>%
  leaflet::addProviderTiles(providers$"OpenStreetMap", group = "OSM") %>%
  leaflet::addCircleMarkers(
    lng = ~x,
    lat = ~y,
    # lng = 58.36796164354272,
    # lat = 29.115391062920825,
    weight = 1,
    radius = 5,
    popup = ~site,
    color = "red",
    fillOpacity = 1,
    opacity = 1) %>%
  # leaflet::setView(lng = 58.36796164354272,
  #         lat = 29.11539106292082,
  #         zoom = 15) %>%
  leaflet::addLayersControl(
    baseGroups = c("Ortho", "OSM"),
    position = "topright") %>%
  leaflet::addScaleBar(position = "bottomright")
print(ea.map)
# htmlwidgets::saveWidget(ea.map, paste0(getwd(), "/data/geojson/maps/", map.name, ".html"))
}
