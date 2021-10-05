#' Propensity score
#'
#' @param DF dataframe
#' @param y character
#' @param regression logical
#'
#' @return
#' @export
#' @import ROCR
#' @import dplyr
#' @import stringr
#'
#' @examples
propensity <- function(DF,
                       y,
                       regression)
{

      if (!is.matrix(regression))
         stop("regression must be a matrix with OR values")

   cols <- vector()
   level <- vector()
   make.names(colnames(DF)) -> colnames(DF)
   make.names(y) -> y
   i = 0
   for (col in regression[-1,1][regression[-1,5]!= "-"]){
      i + 1 -> i
      cols[i] <- str_split(col," ")[[1]][1]
   }

   DF[ ,c(y,cols)] -> DF_propensity
   DF_propensity[complete.cases(DF_propensity),] -> DF_propensity
   as.data.frame(DF_propensity) -> DF_propensity

   todelete <- vector()
   i = 0

   for (n in 1:length(DF_propensity))
   {
      if (is.character(DF_propensity[, n]))
      {
         DF_propensity[, n] <-  as.factor(DF_propensity[, n])
         levels(DF_propensity[, n]) -> levels_n
         DF_propensity[, n] <-  as.character(DF_propensity[, n])
         if (length(levels_n) < 3)
         {
            #replacing levels by 0 or 1
            DF_propensity[, n][DF_propensity[, n] == levels_n[1]] <- 0
            DF_propensity[, n][DF_propensity[, n] == levels_n[2]] <- 1
         } else {
            i <- 1 + i
            for (level in 2:length(levels_n))
            {
               nomcol <- paste0(colnames(DF_propensity)[n],levels_n[level])
               DF_propensity[ ,nomcol] <- DF_propensity[, n]
               DF_propensity[ ,nomcol][DF_propensity[,nomcol] != levels_n[level]] <- 0
               DF_propensity[ ,nomcol][DF_propensity[,nomcol] == levels_n[level]] <- 1
            }
            n -> todelete[i]
      }
      as.factor(DF_propensity[, n]) -> DF_propensity[, n]
      }else{
         DF_propensity[, n] <-  as.numeric(DF_propensity[, n])
      }
   }
   if (length(todelete) > 0){
      DF_propensity[, -todelete] -> DF_propensity
   }

   glm(DF_propensity,family = binomial) -> mod

   summary(mod)$coefficients[,1] -> logit_coeff
   DF_propensity$predicted <- as.numeric(DF_propensity[,1])
   for (patient in 1:nrow(DF_propensity))
   {
      names(logit_coeff) <- NULL
      predicted <- logit_coeff[1]#ordonnée à l'origine
      for (i in 1:(length(logit_coeff)-1))
      {
         predicted <- predicted + logit_coeff[i+1]*as.numeric(DF_propensity[patient,i+1])
      }
      DF_propensity$predicted[patient] <- predicted
   }

   Pred = prediction(DF_propensity$predicted,DF_propensity[,1])
   Perf = performance(Pred,'tpr','fpr')
   perftest <- performance(Pred,"auc")
   plot(Perf,colorize = TRUE, main = "courbe ROC",sub=paste("\nAUC = ", perftest@y.values[[1]]))

   return(perftest@y.values[[1]])
}
