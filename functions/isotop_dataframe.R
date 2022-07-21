#' Create a simple dataframe
#' @name isotop_3d
#' @description
#'
#' @param df the dataframe. The color is displayed on the 'object' field,
#'
#' @return a dataframe
#'
#' @examples
#'
#' @export
isotop_datatable <- function(df){
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

