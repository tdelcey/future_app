# Helpers pour alléger le code
simple_datatable <- function(df, pageLength = 5, escape = TRUE) {
  DT::datatable(
    df,
    options = list(pageLength = pageLength),
    rownames = FALSE,
    escape = escape
  )
}
