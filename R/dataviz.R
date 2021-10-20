#' Makepalette
#'
#' @param palette 
#' @param show 
#'
#' @return
#' @export
#' @import ggthemes
#'
#' @examples
makepalette <- function(palette = few_pal(),
                        n=8,
                        type = c("Medium","Light","Dark"),
                        show=FALSE){
  
  if (is.vector(type))
    type = type[1]
  
  palette = few_pal(type)
  

  if (show){
    scales::show_col(palette(n))
  }
  
  return(palette(n))
}