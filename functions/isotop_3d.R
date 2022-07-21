isotop_3d <- function(df){
  dt <- datatable(
    df[, c(1:7)],
    rownames = FALSE,
    width = "50%",
    editable = FALSE) %>%
    formatStyle("object",
                backgroundColor = styleEqual(df$object,
                                             toupper(df$color))
    )
  saveWidget(dt, paste0(getwd(), "/data/isotop_results.html"))
}
