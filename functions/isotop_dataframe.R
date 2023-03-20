#' Create a simple dataframe
#' @name isotop_dataframe
#' @description
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#' @param color.column the name on which the different colors will be setup
#'
#' @return a dataframe with an hexadecimal value for the colors
#'
#' @examples
#'
#' path.data <- "https://raw.githubusercontent.com/ANR-Itineris/itineris/main/data/"
#' df.isotop <- read.table(paste0(path.data, "isotop_results.tsv"), sep = "\t", header = T)
#' df.isotop <- isotop_dataframe(df.isotop)
#'
#' @export
isotop_dataframe<- function(df,
                            color.column = "object"){
  u.colors <- unique(df[, color.column])
  df$type <- df[, color.column]
  df.colors <- data.frame(type = u.colors,
                          color = rainbow(length(u.colors)),
                          stringsAsFactors = F
  )
  isotop.color <- merge(df, df.colors, by = "type")
  return(isotop.color)
}

