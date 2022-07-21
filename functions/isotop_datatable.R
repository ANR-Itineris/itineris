#' Create an HTML interactive table  with the values of 3 different isotops
#' @name isotop_datatable
#' @description get a simple dataframe and create an interactive datatable
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#' @param color.column the name on which the different colors will be setup
#' @param out.plot the path and name of the created intearctive data table
#'
#' @return an DT table with a confirmation message
#'
#' @examples
#'
#' @export
isotop_datatable <- function(df,
                             color.column = "object",
                             out.plot = paste0(getwd(), "/data/isotop_datatable.html")){
  # print(colnames(df))
  # df$type <- NULL
  dt <- DT::datatable(
    # df[, c(1:7)],
    df,
    rownames = FALSE,
    width = "90%",
    escape = c("color", "type"),
    editable = FALSE) %>%
    DT::formatStyle("object",
                    backgroundColor = DT::styleEqual(df[, color.column],
                                                     toupper(df$color))
    )
  htmlwidgets::saveWidget(dt, out.plot)
  print(paste("interactive datatable created in:", out.plot))
}
