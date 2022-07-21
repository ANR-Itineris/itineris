#' Create an HTML interactive chart with the values of 3 different isotops
#' @name isotop_3d
#' @description get a dataframe and create a 3D plot
#'
#' @param df the dataframe. The color is displayed on the 'object' field,
#' @param out.plot the path and name of the created 3D plot
#'
#' @return an HML 3D plot
#'
#' @examples
#'
#' @export
isotop_3d <- function(df, out.plot = paste0(getwd(), "/data/isotop_results.html")){
  dt <- DT::datatable(
    df[, c(1:7)],
    rownames = FALSE,
    width = "50%",
    editable = FALSE) %>%
    formatStyle("object",
                backgroundColor = styleEqual(df$object,
                                             toupper(df$color))
    )
  htmlwidgets::saveWidget(dt, out.plot)
}
