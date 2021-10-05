#' Logistic Regression Function
#'
#' @description reglog is able to perform logistic regression with variable selection and gives as a result a matrix with variable names, odds-ratios,
#'  confidence intervals and p-values of univariate and multivariate models.
#'
#' @param DF dataframe, matrix or tibble that contains all explicatives variables and the variable to explain
#' @param y character : name of the variable to explain
#' @param explicatives character vector : variables that should explain y in the logistic regression.
#' Takes all columns but y from the dataframe if kept empty.
#' @param method the method that will be used to select variables in the multivariate model.The default method is the backward elimination. See 'details' section for more informations.
#' @param alpha num : significance threeshold used to delete non-significant variables in the multivariate model.
#' @param verbose logical : if TRUE, explainations are displayed in the console while running the function.
#' @param alpha_max num : maximum threeshold used to select the minimum multivariate variables wanted.
#' @param round num : number of digits to display in the final table.
#' @param keep all the variables that should be kept in the multivariate results
#' @param exit specify where do you want to display the results : console (the default), excel (in a results.xlsx file), html (using kable)
#'
#'
#' @return reglog returns a matrix with all OR obtain from univariate model and OR obtain from the multivariate model
#' @export
#' @import dplyr
#' @import logistf
#' @import stringr
#' @import safeBinaryRegression
#' @import xlsx
#' @import MASS
#' @import flextable
#' @import readr
#'
#' @references Bursac, Z., Gauss, C.H., Williams, D.K. et al. Purposeful selection of variables in logistic regression. Source Code Biol Med 3, 17 (2008). https://doi.org/10.1186/1751-0473-3-17
#' @references Heinze G, Schemper M. A solution to the problem of separation in logistic regression. Stat Med. 2002;21(16):2409-2419. doi:10.1002/sim.1047
#' @examples
reglog <- function(DF,
            y,
            explicatives = colnames(DF)[colnames(DF) != y],
            alpha = 0.05,
            dataprep = TRUE,
            verbose = TRUE,
            alpha_max=0.2,
            round = 3,
            rowstimevariable = 10,
            confirmation=FALSE,
            keep=FALSE,
            exit = "html",
            stability=FALSE,
            equation = FALSE,
            title = TRUE,
            ...)
   {

   #To ignore warnings during usage
   options(warn = -1)
   options("getSymbols.warning4.0" = FALSE)


   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   # Y
   if (missing(y))
      y <- colnames(DF)[1]
   if (!is.character(y) || !(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")

   ## Explicatives
   if (!is.vector(explicatives))
      stop("explicatives should be a vector of characters")

   # Removes explicatives not in DF
   explicatives_out_DF <- explicatives[!(explicatives %in% colnames(DF))]
   if (length(explicatives_out_DF) > 0)
   {
      msg_error <- explicatives_out_DF[1]
      if (length(explicatives_out_DF) > 1)
      {
         for (expl_out_DF in explicatives_out_DF[-1])
         {
            msg_error <- paste0(msg_error, ", ", expl_out_DF)
         }
         msg_error <- paste0(msg_error, " are not part of DF columns")
      } else{
         msg_error <- paste0(msg_error, " is not part of DF columns")
      }
      msg_error <- paste(msg_error," ; maybe you should also check on wether the colnames are uniques")
      stop(msg_error)
   }

   # Removes y from explicatives
   if (y %in% explicatives)
   {
      message('y is part of "explicatives" and is removed from it')
      explicatives <- explicatives[explicatives != y]
   }
   explicative2 <- explicatives


   ## Dataframe
   if (is.data.frame(DF) || is.matrix(DF) || is.tbl(DF))
   {
      DF <- as.data.frame(DF,row.names = NULL)
      DF <- DF[,c(y,explicatives)]
      if (!setequal(make.names(colnames(DF)),colnames(DF)))
      {
         message("column names are not valid, 'make.names()' is used to have valid colnames")
         make.names(colnames(DF)) -> colnames(DF)
         make.names(explicatives) -> explicatives
         make.names(y) -> y
      }
   } else
   {
      stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")
   }


   if (!is.logical(verbose))
      stop("'verbose' must be logical")
   verbose -> verbose2

   if (!is.numeric(round) || round <= 0)
      stop("round must be numeric and positive")
   round -> round2


   if (!is.logical(confirmation))
      stop("'confirmation' must be logical")
   confirmation -> confirmation2


   if (!is.logical(keep))
   {
      if (!is.character(keep) & !is.vector(keep))
         stop("keep should be a vector of character or characters")
      if (FALSE %in% (keep %in% explicatives))
         stop("some of keep elements are not in 'explicatives'")
   }
   keep -> keep2

   ##################################################

   ##################################################
   #               1) DATA CLEANING                 #
   ##################################################
   if (dataprep == TRUE) {

      if (verbose) cat("
\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |      1) DATA CLEANING       |
     |                             |
     +-----------------------------+
                       \n")

      DF <- data_prep_complete(DF,y,verbose = TRUE,keep = keep2,...)

      ##################################################

   }


   # Constructing 'vect_explicative'
   #####
   vect_explicative <- vector()
   as.data.frame(DF) -> DF
   explicatives <- colnames(DF)[colnames(DF) != y]
   n = 1
   for (var in explicatives) {#making a vector with the name of the variable displayed as many times as (levels - 1)
      if (is.numeric(DF[, var])) {
         vect_explicative[n] <- var
         n <- n + 1
      } else{
         as.factor(DF[, var]) -> DF[,var]
         length(levels(DF[, var])) -> levels_var
         vect_explicative[n:(n + levels_var - 2)] <- rep(var, (levels_var - 1))
         n <- n + levels_var - 1
      }
   }
   #####



   ##################################################
   #            2) UNIVARIATE MODELS                 #
   ##################################################
   if (verbose) cat(
      "\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    2) UNIVARIATE MODEL      |
     |                             |
     +-----------------------------+\n
")


   rslt <- matrix(ncol = 7, nrow = (length(vect_explicative) + 1))
   rownames(rslt) <- c("",vect_explicative)

   getinfo_glm <- function(mod,k,var) {
      OR <- round(exp(as.numeric(mod$coefficients[k + 1])), round) # logit exponential
      pval <- summary(mod)$coefficients[k + 1, 4]
      IC <- paste0(round(suppressMessages(exp(confint(mod)))[k + 1, ], round),collapse = ";")
      IC <- paste0("[",IC,"]")
      name_var <- var
      name_level <- stringr::str_split(rownames(summary(mod)$coefficients)[k + 1], var)[[1]][2]
      name <- ifelse(name_level == "",name_var,(paste0(name_var," (",name_level ,")")))
      return(c(name,OR,IC,pval,name_level))
   }

   i = 0
   for (var_uni in explicatives) {
      progressbar(total = length(vect_explicative)-1,i,variable = var_uni)
      mod_uni <- glm(DF[,c(y,var_uni)], family = "binomial")
      vector_var = vector()
      k = 0
      if (is.numeric(DF[, var_uni])) {
         i <- i + 1
         k <- k + 1
         ligneR <- getinfo_glm(mod_uni,k,var_uni)
         vector_var[i] <- var_uni
         rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
         row.names(rslt)[i + 1] <- paste0(var_uni,ligneR[5])
      } else {
         while (k + 1 < length(levels(DF[, var_uni])))
         {
            i <- i + 1
            k <- k + 1
            ligneR <- getinfo_glm(mod_uni,k,var_uni)
            vector_var[i] <- var_uni
            rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
            row.names(rslt)[i + 1] <- paste0(var_uni,ligneR[5])
         }
      } #else of is.numeric
   } # end of loop

   ##################################################





   ##################################################
   #               MULTIVARIATE MODEL               #
   ##################################################
   if (verbose) cat("
\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    3) MULTIVARIATE MODEL    |
     |                             |
     +-----------------------------+\n")


   # Variable selection
   explicatives_multi <- multivariate_selection(DF[,c(y,explicatives)],y,keep = keep2)
   explicatives_multi <- explicatives_multi$vars_multi
   # Definitive model
   logit(DF[,c(y,explicatives_multi)]) -> mod_multi
   ##################################################



   if (verbose) cat("
\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    4) STABILITY ANALYSIS    |
     |                             |
     +-----------------------------+\n")



   if (stability) {

      stability_rslts <- stability_proportion(DF,y,keep_stability = keep)

      meaningful_variables <- stability_select_meaningful(stability_rslts)


      variables_means_rslt <- variables_means(DF[,c(y,meaningful_variables)])

      if (verbose) {

         cat("
\n
\n Percentage of inclusion in models are : \n")
         print(stability_rslts) #print compulsory
         cat("
\n
\n So meaningful variables are : \n")
         print(meaningful_variables,sep = " ; ")
         cat('
\n
\n Coefficents means are :
             ')
         print(variables_means_rslt)
      }
   }
   ##################################################






   ##################################################
   #              MATRICE DE RESULTATS              #
   ##################################################
   rslt -> rslt_stability
   OR <- exp(mod_multi$coefficients) #exp de la fonction logit
   pval <- summary(mod_multi)$coefficients[,4]
   IC <- round(suppressMessages(exp(confint(mod_multi))),round)
   i <- 0

   for (OR_var in names(OR)[-1]) {#-1 remove intercept
      i <- i + 1
      n_ligne <- match(OR_var,rownames(rslt))
      p <- round(as.numeric(pval[i + 1]), round)
      p <- ifelse(p == 0, "<0.001", p)
      IC_paste <- paste0("[",IC[i + 1, 1], ";", IC[i + 1, 2],"]")
      rslt[n_ligne, 5:7] <- c(signif(OR[i + 1], round), IC_paste, p)
   }

   for (n in 1:length(rslt[-1, 4])) {
      p = as.numeric(rslt[n + 1, 4])
      round(p, round) -> p
      ifelse(p == 0, "<0.001", p) -> rslt[n + 1, 4]
   }


   if (stability) {
      rslt_stability <- matrix(nrow = nrow(rslt),ncol = 1)
      i <- 0
      for (variable in variables_means_rslt[,1]) {
         i <- i + 1
         n_ligne <- match(grep(variable,rslt[,1],value = TRUE),rslt[,1])
         rslt_stability[n_ligne, 1] <- round(as.double(as.character(variables_means_rslt[i,3])),round)
      }
      rslt_stability[is.na(rslt_stability)] <- "-"
      rslt <- cbind(rslt, rslt_stability)
      rslt[1,] <- c("","OR","IC","pval","OR","IC","pval","mean OR")
   } else {
      rslt[1,] <- c("","OR","IC","pval","OR","IC","pval")
   }
   ##################################################

      row.names(rslt) <- NULL



if (FALSE){


      if (verbose & FALSE) cat("
\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    5) AUC                   |
     |                             |
     +-----------------------------+\n")


      scoreAUC <- propensity(DF,y,regression = rslt)

      cat('
\n
\n AUC = ', scoreAUC)

}

      if (verbose) cat("
\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    6) Results               |
     |                             |
     +-----------------------------+\n")




      if ("excel" %in% exit)
         write.xlsx(
            rslt,
            "rslt.xlsx",
            sheetName = "reglog",
            append = FALSE,
            row.names = FALSE
         )

      if ("console" %in% exit) {
         print(rslt)
      }

      if ("html" %in% exit) {
         rslt <- as.data.frame(rslt[-1,])
         rslt2 <- rslt
         if (stability){
            colnames(rslt) <-  c("variables","OR_univariate","IC_univariate","pval_univariate","OR_multivariate","IC_multivariate","pval_multivariate","Mean OR")
            rslt <- flextable(rslt, col_keys =  c("variables","OR_univariate","IC_univariate","pval_univariate","OR_multivariate","IC_multivariate","pval_multivariate","Mean OR"))
         } else {
            colnames(rslt) <-  c("variables","OR_univariate","IC_univariate","pval_univariate","OR_multivariate","IC_multivariate","pval_multivariate")
            rslt <- flextable(rslt, col_keys =  c("variables","OR_univariate","IC_univariate","pval_univariate","OR_multivariate","IC_multivariate","pval_multivariate"))
         }


         rslt <- delete_part(rslt,part = "header")
         rslt <- add_header(x = rslt, variable = "variables",OR_univariate = "OR",IC_univariate = "IC95%",pval_univariate = "p-value",OR_multivariate = "OR",IC_multivariate = "IC95%",pval_multivariate = "p-value", top = FALSE)
         rslt <- add_header(x = rslt, variable = "variables",OR_univariate = "univariate model",IC_univariate = "univariate model",pval_univariate = "univariate model",OR_multivariate = "multivariate model",IC_multivariate = "multivariate model",pval_multivariate = "multivariate model", top = TRUE)

         rslt <- merge_at(rslt, part = "header",i = 1:1,j = 2:4)
         rslt <- merge_at(rslt, part = "header",i = 1:1,j = 5:7)

         if (equation) {
         footer <- paste0("Equation for odds using adjusted odds ratios for each variable \n","odds of"," y = ")
         explicatives_multi <- colnames_prep(explicatives_multi,type = "presentation")
         superfp <- officer::fp_text(vertical.align = "superscript", font.size = 8)
         for (variable_multi in explicatives_multi) {
            n_ligne <- match(grep(variable_multi,rslt2[,1],value = TRUE),rslt2[,1])
            footer <- paste0(footer, rslt2[n_ligne,5] , as_paragraph("variable", as_chunk("1 if", props = superfp)))
         }
         rslt <- add_footer(rslt, variable = footer)
         rslt <- merge_at(rslt, j = 1:ncol(rslt2), part = "footer")
         rslt <- valign(rslt, valign = "bottom", part = "footer")
         }

         if (title) {
            rslt <- add_header_lines(rslt,paste0("Logistic regression for ", colnames_prep(y,type = "presentation")))
         }

         rslt <- theme_booktabs(rslt)
         rslt <- autofit(rslt)


      }

      cat('
          done !
          ')

      return(rslt)
}
