#' xlsx
#'
#' @param table matrix or dataframe to write in a excel file
#' @param title_sheet character : title of the sheet in the excel file
#' @param file_name character : name of the file name
#'
#' @return
#' @export excel
#' @import xlsx
#'
#' @examples
excel <- function(table, title_sheet = NULL, file_name="results.xlsx") {
   write.xlsx(
      table,
      file_name,
      sheetName = title_sheet,
      append = FALSE,
      row.names = FALSE
   )
}
