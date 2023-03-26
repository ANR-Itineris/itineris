chm.sig  <- chm <- "C:/Rdev-itineris/itineris-anr/data/"

obj.colors <- data.frame(nom=c("Parure Golasecca",
                               "Vaisselle metallique",
                               "Ceramique grecque",
                               "Amphore"),
                         couleur=c("#9f9cf7", # bleu
                                   "#abffab", # vert
                                   "#f79cf4", # rose
                                   "#ffffab"), # jaune
                         rgb=c('rgb(159, 156, 247)',
                               'rgb(171, 255, 171)',
                               'rgb(247, 156, 244)',
                               'rgb(255, 255, 171)'),
                         stringsAsFactors = F)
site.type <- data.frame(contexte.site=c("funeraire","habitat","habitat-funeraire",
                                        "sanctuaire","depot","fluvial",
                                        "non_rens","indetermine"),
                        contexte.forme.ply=c("triangle-down","square","cross",
                                             "triangle","triangle","cross",
                                             "hexagon","hexagon"),
                        contexte.couleur=c("#848484","#ff0000","#808080",
                                           "#ffff00","#ffff00","#ffff00",
                                           "#00ff00","#00ff00"),
                        stringsAsFactors = F)

f.ggraph  <- function(df.per,obj.colors){
  ## nodes
  col.nds <- c("sites","num","contexte.site","contexte.forme.ply","contexte.couleur")
  # sites
  nds.sites <- df.per[ ,col.nds]
  # objets
  nds.objets <- data.frame(nom = unique(df.per$objets),
                           num = unique(df.per$objets),
                           contexte = rep("objet",length(unique(df.per$objets))),
                           forme = rep("circle",length(unique(df.per$objets))),
                           stringsAsFactors = F)
  nds.objets <- merge(nds.objets, obj.colors, by="nom")
  nds.objets$rgb <- NULL
  names(nds.objets) <- col.nds
  # merge
  nds <- rbind(nds.sites, nds.objets)
  # rm duplicated
  nds <- nds[!duplicated(nds[ ,"sites"]), ]
  ## edges
  eds <- df.per[,c("sites","objets")]
  # save sums
  df.eds <- table(eds)
  # rm duplicated
  eds <- eds[!duplicated(eds), ]
  g <- graph_from_data_frame(eds, directed=FALSE, vertices=nds)
  L <- layout.fruchterman.reingold(g) # spat
  g <- set.vertex.attribute(g, "site", value=V(g)$name)
  g <- set.vertex.attribute(g, "degree", value=degree(g))
  g <- set.vertex.attribute(g, "x", value=L[,1])
  g <- set.vertex.attribute(g, "y", value=L[,2])
  # igraph::as_data_frame(g, 'vertices') # check vertices
  es <- as.data.frame(get.edgelist(g),stringsAsFactors = F)
  # merge with color
  es <- merge(es, obj.colors, by.x = "V2", by.y="nom",all.x = T)
  Xn <- L[,1]
  Yn <- L[,2]
  df.nds <- igraph::as_data_frame(g, what="vertices") # to retrieve coordinates
  # edges
  edge_shapes <- list()
  for(i in 1:nrow(es)) {
    # i <- 48
    v0 <- es[i,]$V1
    v1 <- es[i,]$V2
    edge_shape = list(
      type = "line",
      line = list(color = es[i,]$couleur,
                  width = 1.5),
      layer='below', # plot below
      #♦ renversant... x~y
      x0 = df.nds[df.nds$name == v0,"y"],
      y0 = df.nds[df.nds$name == v0,"x"],
      x1 = df.nds[df.nds$name == v1,"y"],
      y1 = df.nds[df.nds$name == v1,"x"],
      opacity = 1
    )
    edge_shapes[[i]] <- edge_shape
  }
  set.seed(41)
  fig <- plot_ly() %>%
    # nodes
    layout(shapes = edge_shapes,
           xaxis = list(title = "",
                        showgrid = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE)
    ) %>%
    add_markers(
      y = ~V(g)$x,
      x = ~V(g)$y,
      mode = "markers",
      text = V(g)$name,
      hoverinfo = "text",
      # name = ~thm,
      # opacity = 1,
      marker = list(
        symbol = V(g)$contexte.forme.ply,
        # size = log2(V(g)$degree*1000),
        size = (log(V(g)$degree)+1)*20,
        color = V(g)$contexte.couleur,
        opacity = .8,
        line = list(
          color = "#000000",
          width = 1))) %>%
    # renversant
    add_annotations(x = ~V(g)$y,
                    y = ~V(g)$x,
                    text =  paste0("<b>",V(g)$num,"</b>"),
                    # text =  ~V(g)$num,
                    font=list(size=8),
                    # xref = "x",
                    # yref = "y",
                    # yanchor = 'center',
                    # xanchor = 'center',
                    showarrow = FALSE,
                    inherit = T)
  return(fig)
}

f.lflt.per <- function(chm.doss,tab,per){
  tab.per <- tab[tab[,per] > 0,]
  sit.per <- tab.per[, c("x", "y", "num","sites", "objets", "contexte.site")]
  sit.per <- sit.per[complete.cases(sit.per), ]
  sit.per <- sit.per[!duplicated(sit.per), ]
  sit.per <- na.omit(sit.per)
  # chemins
  chemins <- st_read(dsn = chm.sig, layer = "_chemins_merge", quiet = T)
  wgs84 <- CRS("+proj=longlat +datum=WGS84")
  chemins <- as(chemins, "Spatial")
  chemins <- spTransform(chemins, wgs84)
  chemins.per <- chemins[chemins$layer == per, ]
  # aire d'etude
  roi <- st_read(dsn = chm.sig, layer = "_aire_etude", quiet = T)
  roi <- as(roi, "Spatial")
  roi <- spTransform(roi, wgs84)


  leafIcons <- f.leafIcons(sit.per) # cr ions Lf

  # f.leafIcons(sit.per) # cr ions Lf
  # leaflet(width = "50%") %>%
  #   setView(lng = sit.per$x, lat = sit.per$y) %>%
  # aPeriod <- leaflet(data = sit.per,width = "75%") %>%
  #   addTiles() %>%
  #   addMarkers(~x, ~y, icon = leafIcons, popup=~sites)
  paste0(sit.per$num,". ",sit.per$sites,"<br>",sit.per$contexte.site)
  nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"
  aPeriod <- leaflet("map", data = sit.per, width = "100%", height = "600px") %>%
    # addTiles() %>%
    addTiles(group = 'OSM') %>%
    addProviderTiles(providers$Stamen.TerrainBackground, group='Stamen') %>%
    # addWMSTiles(nhd_wms_url, layers = "0", group='Topo')
    addProviderTiles(providers$Esri.WorldImagery, group='Topo') %>%
    addPolygons(data = roi, color = "black", fillOpacity = 0, opacity = .7, weight = 3) %>%
    addPolylines(data = chemins.per, color = "red", opacity = .2, weight = 2) %>%
    addMarkers(~x, ~y,
               icon = leafIcons,
               popup=paste0(sit.per$num,". <b>",sit.per$sites,"</b><br>",
                            sit.per$contexte.site),
               # popup=~sites,
               # opacity=0.8,
               label=~num,
               labelOptions = labelOptions(noHide = T,
                                           direction = "center",
                                           textOnly = TRUE,
                                           style = list(
                                             # "color" = "red",
                                             # "font-family" = "serif",
                                             "font-style" = "bold",
                                             # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                             "font-size" = "8px"
                                             # "border-color" = "rgba(0,0,0,0.5)"
                                           ))) %>%
    addControl(per, position = "topright") %>%
    addLayersControl(
      baseGroups = c('Stamen', 'Topo', 'OSM')) %>%
    addScaleBar(position = "bottomleft")
  return(aPeriod)
}

f.leafIcons <- function(sit.per){
  leafIcons <- icons(
    # leafIcons
    iconUrl = ifelse(sit.per$contexte.site %in% c('funeraire'),
                     paste0(getwd(),"/img/s_funeraire.png"),
                     ifelse(sit.per$contexte.site %in% c('indetermine','non_rens'),
                            paste0(getwd(),"/img/s_indet.png"),
                            ifelse(sit.per$contexte.site %in% c('depot','sanctuaire','fluvial'),
                                   paste0(getwd(),"/img/s_depot.png"),
                                   ifelse(sit.per$contexte.site %in% c('habitat'),
                                          paste0(getwd(),"/img/s_habitat.png"), paste0(getwd(),"/img/s_autres.png"))))
    ),
    iconWidth = 15, iconHeight = 15
  )
  return(leafIcons)
}
