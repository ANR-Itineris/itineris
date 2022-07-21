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

# isotop dataset
path.data <- "https://raw.githubusercontent.com/ANR-Itineris/itineris/main/data/"
df.isotop <- read.table(paste0(path.data, "isotop_results.tsv"), sep = "\t")
xyz.isotop <- c("var1", "var2", "var3")
color.isotop <- c("color")

# test
xyz.iris <- c("Sepal.Length", "Sepal.Width", "Petal.Length")
color.iris <- c("color")
n.sample.iris <- 20
iris.samp <- iris[sample(nrow(iris), n.sample.iris), c("Species", xyz.iris)]
