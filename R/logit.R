#' Function Logit
#'
#' @param DF a dataframe
#' @param y (optional) a variable to explain by logit function. If kept empty, y is the first element of DF.
#' @param auto : if TRUE, the decision of method regarding complete separation will be automatic
#' @param verbose : wheter explaination should be displayed in the console
#'
#' @return
#' @export
#'
#' @examples
logit <- function(DF,
            y = colnames(DF)[1],
            auto = FALSE,
            verbose = NULL)

{
   DF <- as.data.frame(DF)

   if (is.null(verbose))
   {
      if (auto)
      {
         verbose <- FALSE
      }else
      {
         verbose <- TRUE
      }
   }


   # 1) Getting the elements
   ##################################################
   formule <- formulation(DF,y)
   #####


   # 2) Verify conditions and transformation
   ##################################################
   if (!is.factor(DF[, y]))
   {
      DF[, y] <- tobinary(DF[, y])
   }else {
      if (!(levels(DF[,y]) == c(0,1) || levels(DF[,y]) == c(1,0)))
      {
         DF[,y] <- tobinary(DF[, y])
      }
   }
   #####


   # 3) Regression
   ##################################################
   error_glm <- TRUE
   firth_method <- FALSE

   while (error_glm)
   {
      tryCatch(
      {
         error_glm <- FALSE
         model <- safeBinaryRegression::glm(formule, data = DF, family = "binomial")
      },
      error = function(er)
      {
         error_glm <- TRUE
         if (grepl("separation", er)) # there is a complete separation
         {
            if (verbose) cat("\nComplete or quasi complete separation occured for : \n")
            for (var in colnames(DF)[-1])
            {
               if (verbose) cat(var, " ")
               DF <- complete_separation(var,y,DF)
               DF$continue <- NULL
               DF$method <- NULL
            }
         }
      })
   }

      #####

glm(formule, data = DF, family = "binomial") -> model

   return(model)
}



