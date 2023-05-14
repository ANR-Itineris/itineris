# Used for the CTHS and Pragua

library(googledrive)
library(openxlsx)
library(igraph)
library(ggplot2)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(ggrepel)
library(gridExtra)
library(ggmap)
library(magick)

`%notin%` <- Negate(`%in%`)


cwd <- dirname(rstudioapi::getSourceEditorContext()$path)

chm <- "pragua"
# /!\ online doc/data (ie, googledrive data), need to switch the first ws to read it
# Error in `[.data.frame`(`*tmp*`, , selected.col.chrono) :
#   undefined columns selected


ana.map <- F
ana.grh <- !ana.map
LD_OB_TY_ST <- T ; LD_OB_ST <- !LD_OB_TY_ST # selection on n-mode
only_Fibule <- F ; only_Others <- F # selection on objects
algos.community <- c("edge.betweenness.community") # "fastgreedy.community",
mandatory.fields <- c("x", "y", "Lieu-dit", "Contexte", "Objet", "type", "style")
WGS84 <- sf::st_crs(4326)[[2]]
# sp::CRS(wkt)
# WGS84 <- "+init=epsg:4326"
# out.path.gis <- paste0(cwd, "/borders/gis")
# out.path.imgs <- paste0(cwd, "/borders/imgs/")
out.path.gis <- paste0(cwd, "/gis")
out.path.imgs <- paste0(cwd, "/imgs/")
myH <- 15 # height of the Global plots
myW <- 7
# spatial
bbox <- c(left = 7,
          bottom = 44,
          right = 11.2,
          top = 46)
# the Stamen Terrain Background
bck.stamen.sp <- ggmap::get_stamenmap(bbox, 
                                      zoom = 8,
                                      maptype = "terrain-background")
xt <- ""
ggdocument.url <- "DATA_01_IT_VC_2.xlsx"
googledrive::drive_find(n_max = 30)
# https://docs.google.com/spreadsheets/d/14drbItddoWRMriy7bWVqs8tQUwGipA64/edit#gid=1105620675
unlink(ggdocument.url)
data.border <- drive_download(ggdocument.url)
data <- openxlsx::read.xlsx(ggdocument.url)
objets <- unique(data$Objet)
# on Fibules or on Others objects
if(only_Fibule){
  objets.to.suppress <- objets[objets != "Fib"]
}
if(only_Others){
  objets.to.suppress <- objets[objets == "Fib"]
}
if(only_Fibule | only_Others){
  data <- data[data$Objet %notin% objets.to.suppress, ]
}
colnames(data)


if(ana.map){
  # Analysis of the map
  # WS = "Sites_NMI_MET"
  # if(LD_OB_TY_ST){
  #   mandatory.fields <- c("X","Y", "Lieu-dit", "Context")
  # }
  # if(LD_OB_ST){
  #   mandatory.fields <- c("X","Y", "Lieu-dit", "Context")
  # }
  selected.col.tot <- c("Tot_Fibule", "Tot_Pendents", "Tot_Bottom",
                        "Tot_Belt", "Tot_Arm-rings", "Tot_Toilet-stick")
  data[ ,selected.col.tot][is.na(data[ ,selected.col.tot])] <- 0
  data <- data[, c("x", "y", "Lieu-dit", selected.col.tot)]
  data.tot <- data %>%
    rowwise() %>%
    mutate(tot = sum(c_across(any_of(selected.col.tot))))
  data.tot <- data.tot[data.tot$tot > 0, ]
  xy <- list(longitude=as.numeric(data.tot$x),
             latitude=as.numeric(data.tot$y))
  data.tot.sp <- SpatialPointsDataFrame(coords = xy,
                                        data = data.tot,
                                        proj4string = sp::CRS(WGS84))
  # write shapefile
  rgdal::writeOGR(obj = data.tot.sp,
                  dsn = out.path.gis,
                  layer = "counts",
                  driver = "ESRI Shapefile") # this is in geographical projection
  ## pie chart
  data.pie <- as.data.frame(data.tot)
  data.pie[, c("Lieu-dit", "tot", "y", "x")] <- NULL
  # colors (same as QGIS)
  c.fibulae <- "#f7e898"
  c.pendant <- "#e2d948"
  c.button <- "#f1c455"
  c.belt <- "#9b198d"
  c.armring <- "#5c2390"
  c.toilet <-  "#6ac1be"
  colors.pie <- c(c.fibulae, c.pendant, c.button, c.belt, c.armring, c.toilet)
  data.pie.tot <- data.frame(group = colnames(data.pie),
                             value = colSums(data.pie),
                             color = colors.pie)
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")
    )
  g.pie <- ggplot(data.pie.tot, aes(x="", y=value, fill=color))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = value),
              size = 3,
              position = position_stack(vjust = 0.5)) +
    #
    # geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]),
    #               label = value), size=3) +
    blank_theme +
    # theme_bw() +
    # scale_fill_manual(values = data.pie.tot$color)
    scale_fill_identity()
  name.out <- paste0(out.path.imgs, "totalpie.png")
  ggsave(name.out, g.pie)
}


### functions
## igraph shapes
# TODO: rm vertex black border, see:
# https://igraph.discourse.group/t/vertex-frame-color/1305/3
# https://stackoverflow.com/questions/58543917/in-igraph-in-r-is-it-possible-to-create-dotted-lines-around-the-vertex-objects
# triangle vertex shape
mytriangle <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  # # border width (not working)
  # vertex.frame.width <- params("vertex", "frame.width")
  # if (length(vertex.frame.width) != 1 && !is.null(v)) {
  #   vertex.frame.width <- 0 # vertex.frame.color[v]
  # }
  
  symbols(x = coords[ , 1], y = coords[ , 2], bg = vertex.color,
          stars = cbind(vertex.size, vertex.size, vertex.size),
          add = TRUE, inches = FALSE)
}



# generic star vertex shape, with a parameter for number of rays
mystar <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
         FUN = function(x, y, bg, size, nor) {
           symbols(x = x, 
                   y = y,
                   bg = bg,
                   stars = matrix(c(size, size/2), nrow = 1, ncol = nor*2),
                   add = TRUE, 
                   inches = FALSE)
         })
}
# clips as a circle
igraph::add_shape("triangle", clip = igraph::shapes("circle")$clip,
                  plot = mytriangle)
# no clipping, edges will be below the vertices anyway
igraph::add_shape("star", clip = igraph::shape_noclip,
                  plot = mystar, parameters = list(vertex.norays = 5))


## checks
# coordinates
# min(data$x) ; max(data$x) ; min(data$y) ; max(data$y)

# source("func/functions.R") # load data, functions

f.graph <- function(data.per, a.graph, ext.title){
  # data.per, a.graph, ext.title
  #######################################
  ## Create graph and community detection
  ########################################
  # a.graph <- "3_gr3"
  ### edges
  ## align Lieu-dit | Objet | type | style
  xt <- "allObj_"
  if(only_Fibule){
    xt <- "onlyFib_"
  }
  if(only_Others){
    xt <- "notFib_"
  }
  data.per <- as.data.frame(data.per)
  eds.sites_objets <- data.frame(a = data.per[,c("Lieu-dit")],
                                 b = data.per[,c("Objet")]
  ) # Lieu-dit | Objet
  eds.objets_types <- data.frame(a = data.per[,c("Objet")],
                                 b = data.per[,c("type")]
  )# Objet | type
  eds.objets_styles <- data.frame(a = data.per[,c("Objet")],
                                  b = data.per[,c("style")]
  )# Objet | style
  eds.types_styles <- data.frame(a = data.per[,c("type")],
                                 b = data.per[,c("style")]
  )# type | style
  
  ## Labels
  data.names <- data.per[ , c("Lieu-dit", "Objet", "type", "style")]
  
  # Listing sites
  labels.df.sites <- merge(data.names, data, by = "Lieu-dit", all.x = T)
  labels.df.sites <- labels.df.sites[ , c("Lieu-dit", "Lieu-dit-long")]
  labels.df.sites <- labels.df.sites[!duplicated(labels.df.sites), ]
  labels.df.sites <- labels.df.sites[order(labels.df.sites$`Lieu-dit`), ]
  # # export
  # write.csv(capture.output(cat(paste0(labels.df.sites$name, ": ", labels.df.sites$`Lieu-dit-long`), sep = "; ")),
  #           paste0(out.path.imgs, a.graph, "_sites.txt"),
  #           row.names = F)
  
  # Listing objets
  # data.names <- data[ , c("Lieu-dit", "Objet", "Objet-long")]
  labels.df.objets <- merge(data.names, data, by = "Objet", all.x = T)
  labels.df.objets <- labels.df.objets[ , c("Objet", "Objet-long")]
  labels.df.objets <- labels.df.objets[!duplicated(labels.df.objets), ]
  labels.df.objets <- labels.df.objets[order(labels.df.objets$Objet), ]
  # # export
  # write.csv(capture.output(cat(paste0(labels.df.objets$Objet, ": ", labels.df.objets$`Objet-long`), sep = "; ")),
  #           paste0(out.path.imgs, a.graph, "_objets.txt"),
  #           row.names = F)
  
  # Listing types
  # data.names <- data[ , c("Lieu-dit", "type", "type-long")]
  labels.df.types <- merge(data.names, data, by = "type", all.x = T)
  labels.df.types <- labels.df.types[ , c("type", "type_long")]
  labels.df.types <- labels.df.types[!duplicated(labels.df.types), ]
  labels.df.types <- labels.df.types[order(labels.df.types$type), ]
  # # export
  # write.csv(capture.output(cat(paste0(labels.df.types$type, ": ", labels.df.types$type_long), sep = "; ")),
  #           paste0(out.path.imgs, a.graph, "_types.txt"),
  #           row.names = F)
  
  styles.df <- data.frame(style = c("GOL", "VEN", "WEA", "LIG", "SPD", "HAL"),
                          style_long = c("Golasecca", "Vénétie", "Emilie Occidentale", "Ligurie", "Piémont méridional", "Hallstatt")
  )
  
  # Listing styles
  # data.names <- data[ , c("Lieu-dit", "type", "type-long")]
  labels.df.styles <- merge(data.names, styles.df, by = "style", all.x = T)
  labels.df.styles <- labels.df.styles[ , c("style", "style_long")]
  labels.df.styles <- labels.df.styles[!duplicated(labels.df.styles), ]
  labels.df.styles <- labels.df.styles[order(labels.df.styles$style), ]
  
  sites.lbl <- capture.output(cat(paste0(labels.df.sites$`Lieu-dit`, ": ", labels.df.sites$`Lieu-dit-long`), sep = "; "))
  sites.lbl <- paste0("Sites (cercles gris): ", sites.lbl)
  
  objets.lbl <- capture.output(cat(paste0(labels.df.objets$Objet, ": ", labels.df.objets$`Objet-long`), sep = "; "))
  objets.lbl <- paste0("Objets (carrés rouges): ", objets.lbl)
  
  types.lbl <- capture.output(cat(paste0(labels.df.types$type, ": ", labels.df.types$type_long), sep = "; "))
  types.lbl <- paste0("Types (triangles oranges): ", types.lbl)
  
  styles.lbl <- capture.output(cat(paste0(labels.df.styles$style, ": ", labels.df.styles$style_long), sep = "; "))
  styles.lbl <- paste0("Styles (étoiles jaunes): ", styles.lbl)
  
  
  write.csv(paste0(sites.lbl, ". ", objets.lbl, ". ", types.lbl, ". ", styles.lbl),
            paste0(out.path.imgs, a.graph, "_all.txt"),
            row.names = F)
  
  # merge
  if(LD_OB_TY_ST){
    eds.per.graph <- rbind(eds.sites_objets, eds.objets_types, eds.types_styles)
    xt <- paste0(xt, "LD_OB_TY_ST")
  }
  if(LD_OB_ST){
    eds.per.graph <- rbind(eds.sites_objets, eds.objets_styles)
    xt <- paste0(xt, "LD_OB_ST")
  }
  # if(only_Fibule | only_Others){
  #   eds.per.graph <- eds.per.graph[eds.per.graph$a %notin% objets.to.suppress & eds.per.graph$b %notin% objets.to.suppress, ]
  # }
  eds.per.graph <- eds.per.graph %>%
    tidyr::drop_na("a","b")
  # count same edges (column 'n')
  eds.per.graph <- eds.per.graph %>%
    count(a, b)
  ### nodes
  # class
  nds.sites <- data.frame(name = unique(data.per[,c("Lieu-dit")]),
                          shape = rep("circle", length(unique(data.per[,c("Lieu-dit")]))),
                          color = rep("grey", length(unique(data.per[,c("Lieu-dit")]))),
                          font = rep(2, length(unique(data.per[,c("Lieu-dit")])))
  )
  nds.objets <- data.frame(name = unique(data.per[,c("Objet")]),
                           shape = rep("square", length(unique(data.per[,c("Objet")]))),
                           color = rep("red", length(unique(data.per[,c("Objet")]))),
                           font = rep(1, length(unique(data.per[,c("Objet")])))
  )
  nds.types <- data.frame(name = unique(data.per[,c("type")]),
                          shape = rep("triangle", length(unique(data.per[,c("type")]))),
                          color = rep("orange", length(unique(data.per[,c("type")]))),
                          font = rep(1, length(unique(data.per[,c("type")])))
  )
  nds.styles <- data.frame(name = unique(data.per[,c("style")]),
                           shape = rep("star", length(unique(data.per[,c("style")]))),
                           color = rep("yellow", length(unique(data.per[,c("style")]))),
                           font = rep(3, length(unique(data.per[,c("style")])))
  )
  # merge
  if(LD_OB_TY_ST){
    nds.per.graph <- rbind(nds.sites, nds.objets, nds.types, nds.styles)
  }
  if(LD_OB_ST){
    nds.per.graph <- rbind(nds.sites, nds.objets, nds.styles)
  }
  # if(only_Fibule | only_Others){
  #   nds.per.graph <- nds.per.graph[nds.per.graph$name %notin% objets.to.suppress, ]
  # }
  nds.per.graph <- nds.per.graph %>%
    tidyr::drop_na("name")
  # TODO: find duplicated
  per.title <- paste0(a.graph, " ", ext.title)
  
  ### graph
  g <- graph_from_data_frame(eds.per.graph, 
                             vertices = nds.per.graph,
                             directed = T)
  # rm isolated nodes
  Isolated <- which(degree(g) == 0)
  g <- delete.vertices(g, Isolated)
  # plot(g)
  print("     - graph created")
  ## Global index
  # degree distribution
  df.ldegrees <- data.frame(n = names(degree(g)),
                            degrees = as.numeric(degree(g)))
  g.ldegrees <- ggplot(df.ldegrees, aes(x = degrees))+
    ggtitle(a.graph, subtitle = ext.title) +
    geom_histogram(colour = "black", fill = "grey", binwidth = 1) +
    # scale_x_continuous(breaks = seq(1, max(df.ldegrees$x), by = 1)) +
    theme_bw()
  ldegrees[[length(ldegrees) + 1]] <- g.ldegrees
  print(length(ldegrees))
  # density
  ldensities[[length(ldensities) + 1]] <- graph.density(g)
  # diameter
  ldiameter[[length(ldiameter) + 1]] <- diameter(g)
  ##################
  set.seed(124)
  layout.fr <- layout_with_fr(g)
  ################################################################
  ## raw graph
  name.out <- paste0(out.path.imgs, a.graph,"_graph_", xt,".png")
  png(name.out,
      width = 25,
      height = 14,
      units = "cm",
      res = 300)
  par(mar=c(0, 0, 2, 0))
  
  
  g.out <- plot(g,
                layout = layout.fr,
                labels = V(g)$name,
                vertex.label.color = "black",
                vertex.label.family = "sans",
                vertex.label.font = V(g)$font,
                vertex.label.cex = .5,
                vertex.shape = V(g)$shape,
                vertex.color = adjustcolor(V(g)$color, alpha.f = .5), # V(g)$color,
                vertex.size = 10,
                
                # vertex.frame.color = 0,
                vertex.frame.width = 0,
                # vertex.frame.color="red",
                # vertex.lwd = 0,
                
                # vertex.size = degree(g)+5,
                # vertex.frame.color = NA,
                # edge.label = E(g)$n,
                # edge.label.cex = (E(g)$n)/3,
                # edge.label.color = "black",
                # edge.label.family = "sans",
                edge.arrow.size = .5,
                edge.color = "darkgrey",
                edge.width = log(E(g)$n) + .5
  )
  title(per.title, font.main = 1.5)
  print(g.out)
  dev.off()
  print("     - graph output exported")
  # raw map
  sites.coordinates <- as.data.frame(unique(data[data$`Lieu-dit` %in% nds.sites$name,
                                                 c("Lieu-dit", "x", "y")]))
  names(sites.coordinates) <- c("name", "x", "y")
  sites.coordinates$x <- as.numeric(sites.coordinates$x)
  sites.coordinates$y <- as.numeric(sites.coordinates$y)
  sites.xy.community <- sites.coordinates
  sp.local <- bck.stamen.sp %>%
    ggmap() +
    ggtitle(per.title) +
    geom_point(data = sites.xy.community,
               aes(x = x, y = y), color = "black",
               size = 2) +
    geom_text_repel(data=sites.xy.community,
                    aes(x = x, y = y, label = name), color = "black", cex = 3.5)
  name.out <- paste0(out.path.imgs, a.graph, "_zCD_spat_", xt,".png")
  ggsave(name.out, sp.local)
  print(paste("     - map output exported", name.out))
  ######################################################################################
  ## community detection
  #
  for (algo.community in algos.community){
    # algo.community <- c("edge.betweenness.community")
    g <- as.undirected(g) # convert to undir
    op <- paste0(algo.community, "(g)")
    g_community <- eval(parse(text = op))
    n.membership <- length(unique(g_community$membership))
    # g_community <- fastgreedy.community(g)
    name.out <- paste0(out.path.imgs, a.graph, "_zCD_", algo.community, "_graph_", xt,".png")
    png(name.out,
        width = 25,
        height = 14,
        units = "cm",
        res = 300)
    par(mar=c(0, 0, 2, 0))
    # rcolors <- c("red", "blue", "darkpink", "orange", "darkgreen", "purple", "brown")
    nodes.colors <- rainbow(n.membership)[membership(g_community)]
    comm.colors <- rainbow(n.membership, alpha=.3)
    g.out <- plot(g_community,
                  layout = layout.fr,
                  g,
                  vertex.label.color = "black",
                  vertex.label.family = "sans",
                  vertex.label.font = V(g)$font,
                  vertex.label.cex = .5,
                  vertex.shape = V(g)$shape,
                  # vertex.color = V(g)$color,
                  vertex.size = 10,
                  # vertex.size = degree(g)+5,
                  vertex.frame.color = NA,
                  edge.color = "darkgrey",
                  # edge.width = E(g)$n,
                  col=nodes.colors,
                  mark.col=comm.colors,
                  mark.border = NA,
                  vertex.color = adjustcolor(V(g)$color, alpha.f = .5), # V(g)$color,
                  
                  # vertex.frame.color = 0,
                  # vertex.frame.color="red",
                  # vertex.lwd = 0,
                  
                  # vertex.size = degree(g)+5,
                  # vertex.frame.color = NA,
                  # edge.label = E(g)$n,
                  # edge.label.cex = (E(g)$n)/3,
                  # edge.label.color = "black",
                  # edge.label.family = "sans",
                  # edge.arrow.size = .5,
                  edge.width = .5
    )
    title(main = paste0(per.title, "\n", algo.community),
          font.main = 1.5)
    print(g.out)
    dev.off()
    print(paste("     - graph output exported", algo.community))
    
    # spatial
    # get index of sites (= first position)
    sites.idx <- match(nds.sites$name, V(g)$name)
    # use index to get communities
    sites.communities <- g_community$membership[sites.idx]
    nodes.colors <- rainbow(n.membership)[membership(g_community)]
    sites.colors <- nodes.colors[sites.idx] # sites colors
    sites.membership <- data.frame(name = nds.sites$name,
                                   community = sites.communities,
                                   color = sites.colors)
    sites.coordinates <- as.data.frame(unique(data[data$`Lieu-dit` %in% nds.sites$name,
                                                   c("Lieu-dit", "x", "y")]))
    names(sites.coordinates) <- c("name", "x", "y")
    sites.coordinates$x <- as.numeric(sites.coordinates$x)
    sites.coordinates$y <- as.numeric(sites.coordinates$y)
    # merge
    sites.xy.community <- merge(sites.coordinates, sites.membership, by = "name")
    sp.local <- bck.stamen.sp %>%
      ggmap() +
      ggtitle(per.title, subtitle = algo.community) +
      geom_point(data=sites.xy.community,
                 aes(x = x, y = y, color = color),
                 size = 2) +
      geom_text_repel(data=sites.xy.community,
                      aes(x = x, y = y, color = color, label = name), cex = 3.5) +
      scale_color_identity()
    name.out <- paste0(out.path.imgs, a.graph, "_zCD_", algo.community, "_spat_", xt,".png")
    ggsave(name.out, sp.local)
    print(paste("     - map community output exported", algo.community))
  }
  lG <- list("ldegrees" = ldegrees,
             "ldensities" = ldensities,
             "ldiameter" = ldiameter)
  
  # # Listing sites
  # data.names <- data[ , c("Lieu-dit", "Lieu-dit-long")]
  # labels.df <- merge(sites.coordinates, data.names, by.x = "name", by.y = "Lieu-dit", all.x = T)
  # labels.df <- labels.df[ , c("name", "Lieu-dit-long")]
  # labels.df <- labels.df[!duplicated(labels.df), ]
  # labels.df <- labels.df[order(labels.df$name), ]
  # # export
  # write.csv(capture.output(cat(paste0(labels.df$name, ": ", labels.df$`Lieu-dit-long`), sep = "; ")),
  #           paste0(out.path.imgs, a.graph, "_sites.txt"),
  #           row.names = F)
  # 
  # # Listing objets
  # data.names <- data[ , c("Lieu-dit", "Objet", "Objet-long")]
  # labels.df <- merge(sites.coordinates, data.names, by.x = "name", by.y = "Lieu-dit", all.x = T)
  # labels.df <- labels.df[ , c("Objet", "Objet-long")]
  # labels.df <- labels.df[!duplicated(labels.df), ]
  # labels.df <- labels.df[order(labels.df$Objet), ]
  # # export
  # write.csv(capture.output(cat(paste0(labels.df$name, ": ", labels.df$`Lieu-dit-long`), sep = "; ")),
  #           paste0(out.path.imgs, a.graph, "_objets.txt"),
  #           row.names = F)
  # 
  # # Listing types
  # data.names <- data[ , c("Lieu-dit", "type", "type-long")]
  # labels.df <- merge(sites.coordinates, data.names, by.x = "name", by.y = "Lieu-dit", all.x = T)
  # labels.df <- labels.df[ , c("type", "type-long")]
  # labels.df <- labels.df[!duplicated(labels.df), ]
  # labels.df <- labels.df[order(labels.df$Objet), ]
  # # export
  # write.csv(capture.output(cat(paste0(labels.df$name, ": ", labels.df$`Lieu-dit-long`), sep = "; ")),
  #           paste0(out.path.imgs, a.graph, "_types.txt"),
  #           row.names = F)
  return(lG)
}

if(ana.grh){
  # Analysis of the graphs
  # WS = "Ornament_IT_Archiv"
  ## chrono
  # mandatory.fields <- c("Lieu-dit", "Objet", "type", "style")
  # periods will be grouped
  selected.col.chrono <- c(#"GIIA.(600-550)",
    "GIIAB.(550-525)", 
    "GIIAB_IIB.(550-490)", 
    "GIIB.(525-490)",
    "GIIB_IIIA1.(525-450)",
    "GIIIA1.(490-450)", 
    "GIIIA1_GIIIA2.(490-420)",
    "GIIIA2.(450-420)")
  selected.col.groups <- c("gr1", "gr1", "gr1",
                           "gr2", 
                           "gr3", "gr3", "gr3")
  df.chrono <- data.frame(periods = selected.col.chrono,
                          groups = selected.col.groups,
                          stringsAsFactors = F)
  # replace NA values by 0 in chrono columns
  data[ , selected.col.chrono][is.na(data[ , selected.col.chrono])] <- 0
  ldegrees <- ldensities <- ldiameter <- lG <- list()
  # subset obj by period
  for(i in 1:length(unique(df.chrono$groups))){
    # i <- 1
    a.group <- unique(df.chrono$groups)[i]
    periods <- df.chrono[df.chrono[, "groups"] == a.group, "periods"]
    data.per <- data[, c(mandatory.fields, periods)]
    data.per.sum <- data.per %>%
      rowwise() %>%
      mutate(per = sum(c_across(any_of(periods))))
    # rename per
    names(data.per.sum)[names(data.per.sum) == 'per'] <- a.group
    data.per.sum <- data.per.sum[, c(mandatory.fields, a.group)]
    data.per <- data.per.sum[data.per.sum[ , a.group] > 0, ]
    # drop row with NA in mandatory columns
    a.graph <- paste0(as.character(i), "_", a.group)
    print(paste0("*read: ", a.graph))
    ext.title <- paste0(periods, collapse = " & ")
    aG <- f.graph(data.per, a.graph, ext.title)
    lG[[length(lG) + 1]] <- aG
  }
  # Global info
  # TODO: extract with a better method
  # Degrees distribution
  out.ldegrees <- paste0(out.path.imgs, "_ldegrees_", xt,".png")
  lG.deg <- c(lG[[1]]$ldegrees,
              lG[[2]]$ldegrees,
              lG[[3]]$ldegrees)
  ggsave(file = out.ldegrees,
         arrangeGrob(grobs = lG.deg,
                     nrow = length(lG.deg)),
         height = myH,
         units = "cm")
  # Density distribution
  lG.den <- c(lG[[1]]$ldensities,
              lG[[2]]$ldensities,
              lG[[3]]$ldensities)
  print(unlist(lG.den))
  df.den <- data.frame(per = unique(df.chrono$groups),
                       densities = unlist(lG.den))
  g.den <- ggplot(df.den, aes(x = per, y = densities, group = 1)) +
    # ggtitle("densities") +
    geom_line() +
    geom_point() +
    scale_x_discrete(limits=rev) +
    coord_flip() +
    theme_bw()
  out.ldensities <- paste0(out.path.imgs, "_ldensities_", xt,".png")
  ggsave(file = out.ldensities,
         g.den,
         height = myH,
         width = myW,
         units = "cm")
  # Diameter
  lG.diam <- c(lG[[1]]$ldiameter,
               lG[[2]]$ldiameter,
               lG[[3]]$ldiameter)
  print(unlist(lG.diam))
  df.diam <- data.frame(per = unique(df.chrono$groups),
                        diameters = unlist(lG.diam))
  g.diam <- ggplot(df.diam, aes(x = per, y = diameters, group = 1)) +
    # ggtitle("diameters") +
    geom_line() +
    geom_point() +
    scale_x_discrete(limits=rev) +
    coord_flip() +
    theme_bw()
  out.ldiameters <- paste0(out.path.imgs, "_ldiameters_", xt,".png")
  ggsave(file = out.ldiameters,
         g.diam,
         height = myH,
         width = myW,
         units = "cm")
}
img_trim <- F
if(img_trim){
  lf <- list.files(out.path.imgs, full.names = TRUE)
  lf <-lf[grep(".png",lf)]
  for (a.img in lf){
    img_trimed <- image_trim(image_read(a.img))
    image_write(img_trimed, a.img)
  }
}





