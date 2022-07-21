#' Create an HTML interactive chart with the values of 3 different isotops
#' @name isotop_3d
#' @description get a dataframe and create a 3D plot
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#' @param vars the names of the columns where the isotops measurments are
#' @param out.plot the path and name of the created 3D plot
#'
#' @return a plotly HML 3D plot
#'
#' @examples
#'
#' @export
isotop_3d <- function(df,
                      vars = c("var1", "var2", "var3"),
                      color.column = "object",
                      out.plot = paste0(getwd(), "/data/isotop_3d.html")){
  # print(colnames(df))
  d3 <- plotly::plot_ly(df,
                        x = df[ , vars[1]],
                        y = df[ , vars[2]],
                        z = df[ , vars[3]],
                        color = ~color,
                        hoverinfo = 'text',
                        text = paste0(df[ , color.column]))
  d3 <- d3 %>% plotly::add_markers()
  d3 <- d3 %>% plotly::layout(title = 'Isotop measures',
                              scene = list(xaxis = list(title = vars[1]),
                                           yaxis = list(title = vars[2]),
                                           zaxis = list(title = vars[3])))
  # print(d3)
  htmlwidgets::saveWidget(d3, out.plot)
  print(paste("interactive 3D plot created in:", out.plot))
}
