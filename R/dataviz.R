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
make_palette <- function(pal = c("few","RdBu"),
                    n=8,
                    type = c("Medium","Light","Dark"),
                    show=FALSE){
  ni <- n
  
  if (is.vector(type) & length(type) > 1)
    type = type[1]
  
  if (is.vector(pal) & length(pal) > 1)
    pal = pal[2]
  
  if (n < 3){
    ni <- n
    n <- 3
  }
  
  if (pal == "few"){
    palette = ggthemes::few_pal(type)(n)
    if (show)
      scales::show_col(palette)
  } else{
    

    palette = RColorBrewer::brewer.pal(n = n, name = pal)
    if (show)
      RColorBrewer::display.brewer.pal(n = n, name = pal)
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





#' Sankey plot
#' @param matrix : a dataframe with source, target and value columns
#' @param labels 
#' @param titles 
#' @param values 
#'
#' @import networkD3
#' @import dplyr
#' @return
#' @export
#' 
#' @examples
sankeyplot <- function(links,colors=F){
# Exemple
# links <- data.frame(
#   source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
#   target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
#   value=c(2,3, 2, 3, 1, 3)
# )

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=unique(c(as.character(links$source), 
         as.character(links$target)))
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

if (!is.logical(colors)){
domains <- c(levels(links[,"source"]),levels(links[,"target"]))
paste(domains,collapse = '","') -> domains
paste(colors,collapse = '","')-> colors
my_color <- paste('d3.scaleOrdinal() .domain(["',domains,'"]) .range(["',colors,'"])')

p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "value", NodeID = "name", colourScale=my_color,
                              sinksRight=FALSE)
} else{
  p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                                Source = "IDsource", Target = "IDtarget",
                                Value = "value", NodeID = "name",
                                sinksRight=FALSE)
}

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["HER2 0", "HER2 low","HER2 0 M+","HER2 low M+"]) .range(["#69b3a2", "steelblue","#69b3a2", "steelblue"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name")
p
return(p)
}




#### MODIFYING GGPUBR


#' Histogram
#'
#' @param data : dataframe
#' @param x : column use to plot the histogram
#' @param y : column that can be used to separate the data
#' @param colors : character or vector of colors
#' @param position : identity, stack or dodge
#' @param ... : other arguments to be passed through gghistogram
#'
#' @import ggpubr
#' @return
#' @export
#'
#' @examples
tphistogram <- function(data, x, y, colors, position="stack",...) {
  
  as.data.frame(data) -> data

  if(missing(y)){
    if(missing(colors)){
      colors = make_palette(type = "Medium",pal = "few")[1]
      gghistogram(data, x, color = colors, fill = colors,...) -> p
    } else {
      gghistogram(data,x,...) -> p
    }
  } else{
    data[,y] <- as.factor(data[,y])
    n <- length(levels(data[,y]))
    if(missing(colors)){
      colors <- make_palette(n = n,type = "Dark",pal = "few")
    }
    gghistogram(data, x, palette = colors, color = y, fill = y,
                 position=position,...) -> p
  }
  
 
  
  return(p)
}