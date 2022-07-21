setwd("C:/Rdev-itineris/itineris/functions")

library(dplyr)

source("isotop_dataframe.R")
path.data <- "https://raw.githubusercontent.com/ANR-Itineris/itineris/main/data/"
df.isotop <- read.table(paste0(path.data, "isotop_results.tsv"), sep = "\t", header = T)
df.isotop <- isotop_dataframe(df.isotop)

source("isotop_datatable.R")
isotop_datatable(df.isotop, out.plot = "C:/Rdev-itineris/itineris/data/isotop_datatable.html")

source("isotop_3d.R")
isotop_3d(df.isotop, out.plot = "C:/Rdev-itineris/itineris/data/isotop_3d.html")



source("map_leaflet.R")
df.sites_coords <- read.table(paste0(path.data, "sites_coords.tsv"), sep = "\t", header = T)
map_leaflet(df.sites_coords, out.plot = "C:/Rdev-itineris/itineris/data/map_sites.html")

