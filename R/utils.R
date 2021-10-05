
## Utils function
roundp <- function(x,n){
   x <- round(x,n)
   if(x == 0){
      x <- paste0("< 0.",strrep(0,n-1),"1")
   }
   return(x)
}





load_package <- function(pkgs,
                         verbose = TRUE)
{
   installed_packages <- pkgs %in% rownames(installed.packages())

   if (any(installed_packages == FALSE)) {
      install.packages(pkgs[!installed_packages])
   }

   unloaded_packages = pkgs[!(paste0("package:", pkgs) %in% search())]

   lapply(unloaded_packages, library, character.only = TRUE)

   if (verbose) {
      if (identical(pkgs[!installed_packages], character(0))) {
         install_packages = 'None'
      } else {
         install_packages = pkgs[!installed_packages]
      }
      cat("Installed packages :",
          install_packages,
          sep = "\n   - ")
      if (identical(unloaded_packages, character(0)))
         unloaded_packages = 'None'
      cat("\n\nLoaded packages :",
          unloaded_packages, sep = "\n   - ")
      if (!identical(pkgs[!pkgs %in% unloaded_packages], character(0))) {
         alp = pkgs[!pkgs %in% unloaded_packages]
      } else{
         alp = "None"
      }
      cat("\n\nAlready loaded packages :",
          alp, sep = "\n   - ")
   }


}


get_y_vector <- function(y,
                         DF = NULL,
                         name = y,
                         y_test=y,
                         names=NA) {

   y_vector <- c(y,name,y_test,names)
   names(y_vector) <- c("y","name","y_test","names")
   return(y_vector)
}




#' Explicatives matrix
#'
#' @param explicatives vector
#' @param names vector: names of the explicatives
#' @param complete_names vector :
#' @param one_lines vector of boolean : whether to display one or two lines for 2 levels factors
#' @param DF dataframe
#' @param make.names if names should be changed
#'
#' @return
#' @export
#'
#' @examples
get_explicatives_matrix <- function(explicatives,
                                    DF = NULL,
                                    names = explicatives,
                                    complete_names = TRUE,
                                    one_lines = TRUE,
                                    make.names = TRUE) {

   if (make.names){
      names <- colnames_prep(names, type = "presentation")
   }

   explicatives_matrix <- as.data.frame(matrix(
      c(
         explicatives,
         names,
         rep(complete_names, length(explicatives)),
         rep(one_lines, length(explicatives)),
         rep(NA, length(explicatives)),
         rep(NA, length(explicatives)),
         rep(NA, length(explicatives))
      ),
      nrow = length(explicatives),
      ncol = 7,
      dimnames = list(
         explicatives,
         c(
            "explicatives",
            "names",
            "complete_name",
            'one_lines',
            'factor',
            "levels",
            "labels"
         )
      )
   ))


   if (!is.null(DF)) {
      explicatives_matrix$factor <- sapply(DF[, explicatives],
                                           function(x) {
                                              if (is.numeric(x)) {
                                                 length(levels(as.factor(x))) < 5
                                              } else{
                                                 TRUE
                                              }
                                           })

      explicatives_matrix$levels <- explicatives_matrix$labels <-
         sapply(explicatives, function(x)
            if (explicatives_matrix[x, "factor"])
               levels(as.factor(DF[,x]))[1]
            else
               NA)


      if (length(complete_names) == 1) {
         explicatives_matrix[, 3] <-
            rep(complete_names, length(explicatives))
      } else {
         explicatives_matrix[, 3] <- complete_names
      }

      if (length(one_lines) == 1) {
         explicatives_matrix[, 4] <- ifelse(explicatives_matrix$factor,one_lines,NA)
      } else {
         explicatives_matrix[, 4] <- one_lines
      }
   }
   explicatives_matrix$names <- as.character(explicatives_matrix$names)
   return(explicatives_matrix)


}

