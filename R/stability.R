#' Inclusion proportion by bootstrap
#'
#' @param DF dataframe
#' @param y character : variable to explain
#' @param nbre integer : number of iteration in the bootstrap process
#' @param criterias vector : criterias to test
#'
#' @return
#' @export
#'
#' @examples
stability_proportion <- function(DF,
                                 y = colnames(DF)[1],
                                 nbre=30,
                                 criterias = c("deviance"),
                                 methods = c("backward","forward"),
                                 keep_stability=NULL
                                 )
{
   n_criterias <- length(criterias)
   n_methods <- length(methods)
   n_col_matrix <- n_criterias * n_methods + 1
   stability_rslts <- matrix(ncol = n_col_matrix,nrow = length(colnames(DF)[colnames(DF) != y]))
   stability_rslts[ ,1] <- colnames(DF)[colnames(DF) != y]
   stability_rslts[,2:ncol(stability_rslts)] <- rep(0,nrow(stability_rslts))
   l <- 1
   for (method_i in methods)
   {
      for (critere in criterias)
      {
         l <- l + 1
         for (i in 1:nbre)
         {
            colDF <- sample(nrow(DF), nrow(DF), replace = TRUE)
            newDF <- DF[colDF, ]
            s_dev_back <- multivariate_selection(newDF,y,criteria = critere,method = method_i,verbose = FALSE,keep = keep_stability)
            #progressbar(i,total = nbre,variable = s_dev_back$vars_multi,text = "selected variables : ")
            for (var in colnames(DF)[colnames(DF) != y])
            {
               if (var %in% s_dev_back$vars_multi)
               {
                  stability_rslts[match(var,stability_rslts[,1]),l] <- as.numeric(stability_rslts[match(var,stability_rslts[,1]),l]) + 1
               }
            }
         }
      }
   }
   for (n in 2:ncol(stability_rslts))
   {
      round((as.numeric(stability_rslts[,n])/nbre)*100,1) -> stability_rslts[,n]
   }
   as.data.frame(stability_rslts[order(as.numeric(stability_rslts[,2])),]) -> stability_rslts
   colnames(stability_rslts) <- rep("variables",n_col_matrix)
   i = 1
   for (methode_i in methods)
   {
      for (critere in criterias)
      {
         i + 1 -> i
         colname <- paste0("% inclusion ",methode_i," ",critere)
         colnames(stability_rslts)[i] <- colname
      }

   }
   return(stability_rslts)
}


#' Selection of the meaningfull variables
#'
#' @param stability_object : an object with the % of variable selection by bootstrap
#' @param cutoff : the cutoff to select variable
#'
#' @return a vector with the meaningful variables
#' @export
#'
#' @examples
stability_select_meaningful <- function(stability_object,
                                        cutoff=30,
                                        criteria = "backward deviance")
{
   criteria_col <- paste0("% inclusion ",criteria)
   vars_selection <- stability_object[,c(1,match(criteria_col,colnames(stability_object)))]
   as.numeric(as.character(vars_selection[,2])) -> vars_selection[,2]
   as.character(vars_selection[,1]) -> vars_selection[,1]
   vars_selected <- vector()
   for (vars_i in 1:nrow(vars_selection))
   {
      if (vars_selection[vars_i,2] > cutoff)
      {
         vars_selected <- c(vars_selected,vars_selection[vars_i,1])
      }
   }
   return(vars_selected)
}



#' Mean coefficents on bootstrap
#'
#' @param DF : dataframe
#' @param y : variable to explain
#' @param meaningful_variables : variables pre-selected by bootstrap
#' @param nbre : number of iteration
#'
#' @return
#' @export
#'
#' @examples
variables_means <- function(DF,
                            y = colnames(DF)[1],
                            meaningful_variables = colnames(DF)[colnames(DF) != y],
                            nbre = 30)
{
   DF <- DF[,c(y,meaningful_variables)]
   variables_means_intermediate <- matrix(nrow = (length(meaningful_variables)+1),ncol = (1+nbre))
   variables_means_rslt <- matrix(nrow = (length(meaningful_variables)+1),ncol = 3)
   variables_means_rslt[1,] <- c("variables","mean coefficient","variance")
   variables_means_rslt[,1] <- c("variables",meaningful_variables)
   for (n in 1:nbre)
   {
      sample(nrow(DF), nrow(DF), replace = TRUE) -> colDF
      newDF <- DF[colDF, ]
      mod <- glm(newDF[,c(y,meaningful_variables)], family = "binomial")
      for (k in 1:length(meaningful_variables))
      {
         summary(mod)$coefficients[(k + 1), 1] -> variables_means_intermediate[k+1,n+1]
      }
   }
   for (k in 1:length(meaningful_variables))
   {
      variables_means_rslt[1+k,2] <- round(mean(variables_means_intermediate[1+k,2:nbre]),3)
      #variables_means_rslt[1+k,3] <- stats::var(variables_means_intermediate[1+k,2:nbre],na.rm=TRUE)
   }

   ORs <- round(exp((as.numeric(variables_means_rslt[,2]))),3)
   variables_means_rslt <- cbind(variables_means_rslt,ORs)
   as.data.frame(variables_means_rslt[-1,]) ->variables_means_rslt
   variables_means_rslt[,-3] -> variables_means_rslt
   colnames(variables_means_rslt) <- c("variables","mean coefficient","mean OR")
   return(variables_means_rslt)
}
