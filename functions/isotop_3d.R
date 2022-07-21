#' Create an HTML interactive chart with the values of 3 different isotops
#' @name isotop_3d
#' @description get a dataframe and create a 3D plot
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#' @param colvar the names of the columns where the isotops measurments are
#' @param out.plot the path and name of the created 3D plot
#'
#' @return a plotly HML 3D plot
#'
#' @examples
#'
#' @export
isotop_3d <- function(df, colvar = c("var1", "var2", "var3"),
                      out.plot = paste0(getwd(), "/data/isotop_3d.html")){

  d3 <- plotly::plot_ly(df,
                        x = df[ , colvar[1]],
                        y = df[ , colvar[2]],
                        z = df[ , colvar[3]],
                        color = ~color)
  d3 <- d3 %>% plotly::add_markers()
  d3 <- d3 %>% plotly::layout(scene = list(xaxis = list(title = colvar[1]),
                                           yaxis = list(title = colvar[2]),
                                           zaxis = list(title = colvar[3])))
  htmlwidgets::saveWidget(d3, out.plot)
  print(paste("3D interactive plot created in:", out.plot))
}
