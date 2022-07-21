#' Create an HTML interactive table  with the values of 3 different isotops
#' @name isotop_datatable
#' @description get a simple dataframe and create an interactive datatable
#'
#' @param df the dataframe. The color is displayed on the 'object' field,
#' @param out.plot the path and name of the created intearctive data table
#'
#' @return an DT table
#'
#' @examples
#'
#' @export
isotop_datatable <- function(df, out.plot = paste0(getwd(), "/data/isotop_results.html")){
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
