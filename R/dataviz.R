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




#' Title
#'
#' @param groups 
#' @param labels 
#' @param titles 
#' @param values 
#'
#' @return
#' @export
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import rsvg
#'
#' @examples
initiate_flowchart <- function(groups = 1,
                               labels = c('Base protéomique','','Randomised',rep('Allocated to\nintervention',groups),rep('Lost to follow-up',groups),rep('Analysed',groups)),
                               titles = c('Base initiale', 'Allocation', 'Follow-Up', 'Analysis'),
                               values){
  
    # https://scriptsandstatistics.wordpress.com/2017/12/22/how-to-draw-a-consort-flow-diagram-using-r-and-graphviz/
    paste1 <- function(x, y){
      paste0(x, ' (n=', y, ')')
    }
    
    invisibles <- 4
    labels <- paste1(labels, values)
    LABS <- c(titles, labels, rep("", invisibles))
    
    ndf <-
      create_node_df(
        n = length(LABS),
        label = c(titles, labels, rep("", invisibles)),
        style = c(rep("solid", 13), rep('invis', invisibles)),
        shape = c(rep("plaintext", 4), 
                  rep("box", 9),
                  rep("point", invisibles)),
        width = c(rep(2, 4), rep(2.5, 9), rep(0.001, invisibles)),
        hight = c(rep(0.5, 13), rep(0.001, invisibles)),
        fontsize = c(rep(14, 4), rep(10, 17)),
        fontname = c(rep('Arial Rounded MT Bold', 4), rep('Courier New', 17)),
        penwidth = 2.0,
        fixedsize = "true")
    
    edf <-
      create_edge_df(
        arrowhead = c(rep('none', 3), rep("vee", 3), rep('none', 2), "vee", rep('none', 6),
                      rep("vee", 3), rep("none", 3), "vee", rep("none", 10)),
        color = c(rep('#00000000', 3), rep('black', 6), rep('#00000000', 6),
                  rep('black', 3), rep('#00000000', 3), rep('black', 1),
                  rep('#00000000', 2), rep('black', 2), 
                  rep('#00000000', 6)),
        constraint = c(rep("true", 18), rep('false', 14)),
        from = c(1, 19, 20, 16, 8, 10, # column 1
                 5, 14, 7, 15, 2, 3, # column 2
                 18, 6, 21, 17, 9, 11, # column 3
                 1, 5, # row 1
                 19, 14, # row 2
                 20, 7, # row 3
                 16, 15, # row 4
                 8, 2, # row 5
                 10, 3, # row 6
                 12, 4), # row 7
        to = c(19, 20, 16, 8, 10, 12, # column 1
               14, 7, 15, 2, 3, 4, # column 2
               6, 21, 17, 9, 11, 13, # column 3
               5, 18, # row 1
               14, 6, # row 2
               7, 21, # row 3
               15, 17, # row 4
               2, 9, # row 5
               3, 11, # row 6
               4, 13)) # row 7
    
    g <- create_graph(ndf, 
                      edf,
                      attr_theme = NULL)
    
    # Plotting ----------------------------------------------------------------
    render_graph(g)
  
}

