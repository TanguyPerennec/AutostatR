#' Progressbar
#'
#' @param total : the number of iteration
#' @param i : iteration
#' @param range (optionnal) : the length of the bar in the progressbar
#' @param variable : name to display on the progressbar (must change for each iteration)
#' @param signe (optional) the signe to display for the progressbar. Put '-' to have ---> or ">" to have ">>>>"
#' @param text (optional) text to display befor 'variable'
#'
#' @return
#' @export
#'
#' @examples
progressbar <- function(
                     i,
                     total=FALSE,
                     range = 25,
                     variable = NULL,
                     signe = "=",
                     text = "Making calculs on ") {


   if (total != FALSE){

      if (!is.numeric(total)) stop("total should be numeric")

      percent <- round(100 * i / total, 0)
      space <- min(round(range * i / total, 0), range - 1)
      cat(
         '\r',
         percent,
         " % |",
         rep(signe, space),
         ">",
         rep(" ", range - space),
         " | ",
         text,
         variable,
         "                   "
      )
   }else{
      m=0
      if (i %% range == 0) m <- m + 1 #change the wait signe every 'range' iterations
      wait_sign <- c("-", "\\", "|", "/")
      cat(
         '\r',
         "   ",
         wait_sign[m %% 4 + 1],
         text,
         variable,
         "                   "
         )
   }
   }
