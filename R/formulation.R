#' Object to coerce objects to a formule object
#'
#' @param object object that can be coerced into a formula object
#' @param y (optional) character : the variable that will be the first term of the formula. If kept empty, y will be the first element in the object given to the function.
#'
#' @return formulation returns a formula object
#' @export
formulation <- function(object,y=NULL){


   # 1. Getting y
   #############################################
   if (is.null(y)){
      if (is.data.frame(object) || is.matrix(object))
      {
         object <- as.data.frame(object)
         y <- colnames(object)[1]
      }else
      {
         y <- object[1]
      }
   }
   #####


   # 2. Put y in first position
   #############################################
   if (is.data.frame(object))
   {
      object <- object[,c(y,colnames(object)[colnames(object) != y])]
   } else
   {
      object <- c(y,object[object != y])
   }
   #####


   # 2. Getting a vector with all explicatives into 'object'
   ###########################################################
   if (is.data.frame(object))
   {
      if (is.null(colnames(object)))
      {
         object <- object[1, ]
      } else
      {
         object <- colnames(object)
      }
   } else
   {
      if (length(object) == 0) stop("object is null")
      if (!(is.vector(object) || is.character(object)))
      {
         stop("the object is not coercible into formula")
      }
   }
   #####


   # 3. Coherce object to formula
   #############################################
   formule <- paste0(y, "~", object[2])
   for (explicative in object[-c(1, 2)])
   {
      formule <- paste0(formule, "+", explicative)
   }
   formule <- formula(formule)
   ###########

   return(formule)
}


