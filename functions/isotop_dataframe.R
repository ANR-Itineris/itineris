https://raw.githubusercontent.com/ANR-Itineris/itineris/main/data/isotop_results.tsv

source("setup.R")
setupKnitr(autoprint = TRUE)
set.seed(123)
library(rgl)
library(MASS)
library(ks)
library(misc3d)
library(knitr)
library(kableExtra)
library(dplyr)
library(DT)
library(openxlsx)
library(htmlwidgets)


# isotop dataset

isotop_datatable <- function(df){
  path.data <- "https://raw.githubusercontent.com/ANR-Itineris/itineris/main/data/"
  df.isotop <- read.table(paste0(path.data, "isotop_results.tsv"), sep = "\t", header = T)
  xyz.isotop <- c("var1", "var2", "var3")
  # color.isotop <- c("color")
  u.objects <- unique(df.isotop$object)
  df.colors <- data.frame(object = u.objects,
                          color = rainbow(length(u.objects)),
                          stringsAsFactors = F
  )
  isotop.color <- merge(df.isotop, df.colors, by = "object")
  return(isotop.color)
}

