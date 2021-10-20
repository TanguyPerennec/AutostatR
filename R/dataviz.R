#' Palette
#'
#' @param show (logical) : wether to show the palette 
#' @param pal (character) : name of the palette
#' @param n (numeric) : number of colors to display
#' @param type (character) : for few palette, Medium, Light or Dark
#'
#' @return
#' @export
#' @import ggthemes
#' @import RColorBrewer
#'
#' @examples
palette <- function(pal = c("few","RdBu"),
                    n=8,
                    type = c("Medium","Light","Dark"),
                    show=FALSE){
  ni <- n
  
  if (is.vector(type) & length(type) > 1)
    type = type[1]
  
  if (is.vector(pal) & length(pal) > 1)
    pal = pal[2]
  
  if (pal == "few"){
    pal = few_pal(type)
    if (show){
      scales::show_col(pal(n))
    }
  } else{
    if (n < 3){
      ni <- n
      n <- 3
    }

    palette = RColorBrewer::brewer.pal(n = n, name = pal)
    if (show){
      RColorBrewer::display.brewer.pal(n = n, name = pal)
    }
  }
  
  if (ni == 1){
    palette <- palette[1]
  } else if (ni == 2){
    palette <- palette[c(1,3)]
  } else {
    palette[1:n]
  }

  return(palette)
}

