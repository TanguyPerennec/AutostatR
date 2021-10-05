#' Survivor
#'
#' @param DF dataframe : data to be analysed
#' @param explicatives vecor of character : columns of the dataframe to be analysed
#' @param time character : name of the column representing time to event
#' @param event character : name of the column representing the event
#' @param names vector of character : name of the explicatives variables as it will be display on table
#' @param keep vector of character : explicatives that should never be erased of thee model
#'
#' @return dataframe, excel file or html table depending on the exit parameter
#' @export
#' @import stringr
#' @import stats
#' @import flextable
#' @import pbapply
#' @import survival
#' @import MASS
survivor <- function(DF,
                     explicatives,
                     time = "time",
                     event = "event",
                     names = explicatives,
                     keep = 1,
                     verbose = TRUE,
                     round = 2)   {


   #To ignore warnings during usage
   options(warn = -1)
   options("getSymbols.warning4.0" = FALSE)


   DF_expl <- as.data.frame(DF)[,c(explicatives,time,event)]

   DF_expl <- NA_rm_for_glm(DF_expl,method_NA = "lessNA",EPV = 1)

   DF_expl[,"surv"] <- Surv(as.numeric(DF_expl[ ,time]),
                            as.numeric(DF_expl[ ,event]))

   as.data.frame(DF_expl) -> DF_expl


   if (verbose) cat(
      "\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    1) UNIVARIATE MODEL      |
     |                             |
     +-----------------------------+\n
")




   vect_explicative <- vector()
   n = 1

   for (var in explicatives) {#vector with the name of the variable displayed as many times as (levels - 1)
      if (is.numeric(DF_expl[, var])) {
         vect_explicative[n] <- var
         n <- n + 1
      } else{
         as.factor(DF_expl[, var]) -> DF_expl[,var]
         levels_var <- length(levels(DF_expl[, var]))
         vect <- sapply(levels(DF_expl[, var]),function(x) paste0(var,x))
         vect_explicative[n:(n + levels_var - 2)] <- vect[-1]
         n <- n + levels_var - 1
      }
   }
   #####



   getinfo_cox <- function(mod,k,var,cat) {
      HR <- round(exp(as.numeric(mod$coefficients[k + 1])), round)
      pval <- summary(mod)$coefficients[k + 1, 5]
      IC <- paste0(round(suppressMessages(exp(confint(mod)))[k + 1, ], round),collapse = ";")
      IC <- paste0("[",IC,"]")
      name_var <- name_level <- var
      if (cat != 'num') {
         name_level <- stringr::str_split(rownames(summary(mod)$coefficients)[k + 1], var)[[1]][2]
         name_var <- ifelse(name_level == "",name_var,(paste0(name_var," (",name_level ,")")))
      }
      return(c(name_var,HR,IC,pval,name_level))
   }



   results <- pbsapply(explicatives,
                       function(x,
                                DF = DF_expl) {
                          mod_uni <- survival::coxph(as.formula(DF[,c("surv",x)]),data = DF)

                          if (is.numeric(DF_expl[, x])) {
                             ligneR <- getinfo_cox(mod_uni,0,x,"num")[1:4]
                          } else {
                             ligneR <- matrix(ncol = length( levels(DF_expl[, x])) - 1,nrow = 4)
                             for (k in 1:(length( levels(DF_expl[, x])) - 1)) {
                                ligneR[,k] <- getinfo_cox(mod_uni,k - 1,x,'cat')[1:4]
                             }
                          }
                          return(ligneR)
                       })



   rslt <- matrix(ncol = 7,
                  nrow = (length(vect_explicative) + 1))

   rownames(rslt) <- c("",vect_explicative)

   rslt[2:nrow(rslt),1:4] <- t(matrix(unlist(results),
                                      nrow = 4))


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

   e <- new.env()
   e$DFm <-  DF_expl[,c("surv",explicatives)]

   e$f = as.formula(paste("surv", paste(explicatives,
                                        collapse = " + "),
                          sep = " ~ "),
                    env = e)

   e$mod = survival::coxph(e$f,data = e$DFm)

   e$fmin = as.formula(paste("surv", paste(keep, collapse=" + "), sep=" ~ "),env = e)
   e$modmin = survival::coxph(e$fmin,data = e$DFm)

   e$mod_multi <- stepAIC(e$mod, scope = list(lower=e$modmin, upper = e$mod),direction = "both" )

   mod_multi <- (e$mod_multi)
   if (length(mod_multi$coefficients) != 0) {
      prhz <- cox.zph(mod_multi) #test proportionnal hazards
      if (any(prhz$table[,3] < 0.05))
         message("proportionnal hazards are not assessed")
   } else {
      prhz <- 0
   }

   ##################################################


   ##################################################
   #              MATRICE DE RESULTATS              #
   ##################################################
   HR <- exp(mod_multi$coefficients)
   pval <- summary(mod_multi)$coefficients[,5]
   IC <- round(suppressMessages(exp(confint(mod_multi))),round)
   i <- 0
   for (HR_var in names(HR)) {
      i <- i + 1
      n_ligne <- match(HR_var,rownames(rslt))
      p <- round(as.numeric(pval[i]), round)
      p <- ifelse(p == 0, "<0.001", p)
      IC_paste <- paste0("[",IC[i, 1], ";", IC[i, 2],"]")
      rslt[n_ligne, 5:7] <- c(signif(HR[i], round), IC_paste, p)
   }

   for (n in 1:length(rslt[-1, 4])) {
      p = as.numeric(rslt[n + 1, 4])
      round(p, round) -> p
      ifelse(p == 0, "<0.001", p) -> rslt[n + 1, 4]
   }

   rslt[1,] <- c("","HR","IC","pval","HR","IC","pval")
   row.names(rslt) <- NULL

   rslt_matrix <- rslt

   rslt <- as.data.frame(rslt[-1,])
   colnames(rslt) <-  c("variables","HR_univariate","IC_univariate","pval_univariate","HR_multivariate","IC_multivariate","pval_multivariate")

   rslt %>%
      flextable(col_keys =  c("variables","HR_univariate",
                              "IC_univariate","pval_univariate","HR_multivariate",
                              "IC_multivariate","pval_multivariate")) %>%
      delete_part(part = "header") %>%
      add_header(variable = "variables",
                 HR_univariate = "HR",
                 IC_univariate = "IC95%",
                 pval_univariate = "p-value",
                 HR_multivariate = "HR",
                 IC_multivariate = "IC95%",
                 pval_multivariate = "p-value",
                 top = FALSE) %>%
      add_header(variable = "variables",
                 HR_univariate = "univariate model",
                 IC_univariate = "univariate model",
                 pval_univariate = "univariate model",
                 HR_multivariate = "multivariate model",
                 IC_multivariate = "multivariate model",
                 pval_multivariate = "multivariate model", top = TRUE) %>%
      merge_at(part = "header",i = 1:1,j = 2:4) %>%
      merge_at( part = "header",i = 1:1,j = 5:7) %>%
      theme_booktabs() %>%
      autofit() -> rslt


   resultats <- list()
   resultats$table <- rslt
   resultats$matrix <- rslt_matrix
   resultats$prhz <- prhz
   resultats$model <- mod_multi
   resultats$anova <- mod_multi$anova
   return(resultats)
}


