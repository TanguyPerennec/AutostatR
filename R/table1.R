#' Table 1
#'
#' @param DF dataframe : datas to be analysed
#' @param y character : column that will separate the dataframe
#' @param explicatives : columns of the dataframe to be analysed
#' @param round (optionnal) : number of digits to be display in the results. If a vector is provided, digits stands for p-value then variables.
#' @param overall booleen (optionnal) : TRUE if an "overall" column is wanted
#' @param mutation numeric : number of modalities to display for one variable. If there is more than "mutation" categories, the modalities after this threeshold are wrapped into an "others" categorie.
#' @param tests character / vector (optionnal) : classical (student, khi2...) or logistic regression. Specify if paired.
#' @param y_test character (optionnal) : name of the column that will be used for statistical test
#' @param exit character : 'html', 'console' or 'excel'
#' @param strata : columns with the number of the paires
#'
#' @return dataframe, excel file or html table depending on the exit parameter
#' @export
#' @import stringr
#' @import stats
#' @import flextable
#' @import MASS
#' @import pbapply
#' @import stats
table1 <- function(DF,
                   explicatives,
                   y = NULL,
                   overall = TRUE,
                   tests = "classic",
                   strata = NULL,
                   adjusted = NULL,
                   mutation = 10,
                   round = c(3,2),
                   exit = 'console',
                   num_function = "auto",
                   available_data = TRUE,
                   by_line = FALSE,
                   complete=F)
{

   version_pkg = '1.0.0_24/09/2021'



   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   ## EXPLICATIVES ## v1.0
   #####
   if (is.matrix(explicatives) | is.data.frame(explicatives)){
      explicatives_matrix <- explicatives
   } else {
      explicatives_matrix <- get_explicatives_matrix(explicatives,DF)
   }
   explicatives <- rownames(explicatives_matrix)
   if (! all(explicatives %in% colnames(DF)))
      stop(paste0(explicatives[!(explicatives %in% colnames(DF))], " not in DF"))
   #####


   ## DF
   #####
   if (is.data.frame(DF) || is.matrix(DF)) {
      DF <- as.data.frame(DF, row.names = NULL)
      if (any(make.names(colnames(DF)) != colnames(DF))) {
         message('Names are not fitted, names are generated')
         colnames(DF) <- colnames_prep(colnames(DF))
         explicatives <- explicatives_matrix[, 1] <- colnames_prep(explicatives)
      }
   } else
      stop("No dataframe DF has been provided")
   #autostats::format_data(DF) -> DF #plain and no plural factors
   #####

   ## y
   #####
   if (!is.null(y)) {

      if (!is.matrix(y)) {
         y <- colnames_prep(y)
         y_vector <- get_y_vector(y, DF)
      } else {
         y_vector <- y
         y <- y["y"] <- colnames_prep(y["y"])
      }
      y_test <- y_vector["y_test"]

      DF[, y] <- as.factor(DF[, y])
      levels_y <- length(levels(DF[, y]))
      DF_without_y_and_all <- DF_without_y <- DF[, -match(y, colnames(DF))]
      ynames <- y_vector["names"]
      if (is.na(ynames))
         ynames <- levels(as.factor(DF[,y]))

      if (!is.null(y_test) & (y != y_test))
            DF_without_y_and_all <- DF_without_y[, -match(y_test, colnames(DF_without_y))]

      if (!is.null(strata)) {
         paired = TRUE
         for (colstrata in strata) {
            DF_without_y_and_all <- DF_without_y_and_all[,-match(colstrata, colnames(DF_without_y_and_all))]
         }
      } else{
         paired = FALSE
      }

      if (!is.null(adjusted))
         DF_without_y_and_all <- DF_without_y_and_all[, -match(adjusted, colnames(DF_without_y_and_all))]

      if (!is.null(y_test)) {
         if(y == y_test){
            DF <- DF[,c(y, explicatives, strata)]
         } else {
            DF <- DF[,c(y, explicatives, strata, y_test)]
         }
      } else { DF <- DF[, c(explicatives)]}

   } else{ #y is null
         tests = ynames = NULL
         overall = TRUE
         levels_y = 0
         DF_without_y = DF_without_y_and_all = DF
   }



   # Options verification # 1.5

   ## Options
   #####
   if (!is.logical(overall))
      stop("'overall' should be a booleen")
   if (!is.numeric(mutation))
      stop("'mutation' should be numeric")
   if (any(c("IQR","quartiles","[Q1-Q3]","Q1Q3","IQQ","QQ") %in% num_function)){
      num_function <- c("median","quartile")
   }
   if (!is.null(tests)){
      method <- tests
      if("paired" %in% tests)
         paired = TRUE
      tests <- TRUE
   } else {
      tests <- paired <- method <- FALSE
   }
   #####


   # Round transformation # 1.0
   #####
   if (is.vector(round)) {
      round_p <- round[1]
      round <- round[2]
   } else if (!is.numeric(round)) {
      stop("'round' should be numeric or a vector of numerics")
   } else {
      round_p <- round
   }
   #####



   ##################################################



   ##################################################
   #                     HEADER                     #
   ##################################################
   tabf <- c("Characteristics")
   if (available_data)
      tabf <- c(tabf, "Available data")
   if (overall)
      tabf <- c(tabf, "Overall")

   tabf <- c(tabf, ynames)

   if (tests & (method == 'logistic'))
      tabf <- c(tabf, 'OR')
   if (tests)
      tabf <- c(tabf, 'p-value')
   first_column <- match(ynames[1], tabf)
   ligne2 <- rep("", length(tabf))

   if (!is.null(y)) {
      for (k in 1:levels_y) {
         ligne2[k + first_column - 1] <- paste0("N = ", table(DF[, y])[k])
      }
      numbers_observation <- ligne2
      if (overall) {
         overall_observations <- sum(table(DF[, y]))
         ligne2[first_column - 1] <- paste0("N = ", overall_observations)
      }
   } else {
      overall_observations <- sum(nrow(DF))
      ligne2[first_column - 1] <- paste0("N = ", overall_observations)
   }
   tabf <- rbind(tabf, ligne2)
   ##################################################




   ########################################################
   ###        Loop for each characteristics (var)       ###
   ########################################################
   lignes <- pblapply(as.data.frame(t(explicatives_matrix),row.names = colnames(explicatives_matrix)), function(X){
      varname <- as.character(X[1])
      var <- DF_without_y_and_all[,varname]
      ligne1 <- as.character(X[2])
      sign <- NULL # store a note if a special test is performed (fischer, Wilcoxon...)
      if (by_line) {
         expression = c(varname, "y_b", adjusted)
      } else {
         expression = c("y_b", varname, adjusted)
      }
      if (available_data)
         n_available <- sum(!is.na(var))
      #####################################






      #--------------------------------------------------------------------------------------------------------------------#
      #--  if numeric  ----------------------------------------------------------------------------------------------------#
      #--------------------------------------------------------------------------------------------------------------------#
      if (!as.logical(X[5])) {

         var <- as.numeric(var)
         if (available_data)
            ligne1 <- c(ligne1, n_available)

         ### column Legend
         if (length(num_function) < 3) {
            ligne2 <- ligne3 <- NULL
         } else {
            median_bis <- ifelse("quartile" %in% num_function, "[Q1-Q3]","[min - max]")
            ligne2 <- ifelse(exit == "html", "\t \t Mean (SD)", "      Mean (SD)")
            ligne3 <- ifelse(exit == "html",paste0("\t \t Median ", median_bis),paste0("      Median ", median_bis))
         }


         #### Overall calculation v1.5
         shapiro_overall <- !((shapiro.test(var)$p.value < 0.05) & length(var) < 30)
         ligne_mediane <- paste0(round(median(var, na.rm = TRUE), round),
                                 " [",
                                 ifelse(
                                    !any("quartile" %in% num_function) & !any("auto" %in% num_function),
                                    round(min(var, na.rm = TRUE), round),
                                    round(quantile(var, na.rm = TRUE)[2], round)
                                 ),
                                 " - ",
                                 ifelse(
                                    !any("quartile" %in% num_function) & !any("auto" %in% num_function),
                                    round(max(var, na.rm = TRUE), round),
                                    round(quantile(var, na.rm = TRUE)[4], round)
                                 ),
                                 "] "
                              )
         ligne_moyenne <- paste0(round(mean(var, na.rm = TRUE), round),
                                 " (", round(sd(var, na.rm = TRUE), round),")")

         if (overall) {
            if (length(num_function) < 3 & !any("auto" %in% num_function)) {
               ligne1 <- c(ligne1,
                           ifelse(any("mean" %in% num_function),ligne_moyenne,ligne_mediane))
            } else if ("auto" %in% num_function) {
               ligne1 <- c(ligne1,
                           ifelse(shapiro_overall,ligne_moyenne,ligne_mediane))
            } else {
               ligne1 <- c(ligne1, " ")
               ligne2 <- c(ligne2, ligne_moyenne)
               ligne3 <- c(ligne3, ligne_mediane)
            }
         }
         ####




         #### Calculation for variable ~ y v1.5
         if (!is.null(y)) {
            mean_vars <- aggregate(var ~ DF[, y], FUN = "mean")
            median_vars <- aggregate(var ~ DF[, y], FUN = "median")
            sd_vars <- aggregate(var ~ DF[, y], FUN = "sd")
            min_vars <- aggregate(var ~ DF[, y], FUN = "min")
            max_vars <- aggregate(var ~ DF[, y], FUN = "max")
            quartiles_vars <- aggregate(var ~ DF[, y], FUN = "quantile")
            length_vars <- aggregate(var ~ DF[, y], FUN = "length")
            if (any(length_vars$var < 6) | any(sd_vars$var == 0)) {
               normal <- FALSE
            } else {
               shapiro_vars <- aggregate(var ~ DF[, y], FUN = "shapiro.test")
               normal <- ! (any(shapiro_vars[, 2][, 2] < 0.05)) # si un test est positif => pas de normalit?
            }

            # round and save all results
            for (j in 1:levels_y) {
               mean_vars_level <- round(mean_vars[j, 2], round)
               sd_vars_level <- round(sd_vars[j, 2], round)
               median_vars_level <- round(median_vars[j, 2], round)
               min_vars_level <- round(min_vars[j, 2], round)
               max_vars_level <- round(max_vars[j, 2], round)
               quartiles_vars_level <- round(quartiles_vars[j, 2],round)

               ligne_moyenne <- paste0(mean_vars_level, " ? ", sd_vars_level)
               ligne_mediane <- paste0(
                  median_vars_level,
                  " [",
                  ifelse("quartile" %in% num_function,
                         quartiles_vars_level[2],
                         min_vars_level),
                  " - ",
                  ifelse(
                     "quartile" %in% num_function,
                     quartiles_vars_level[4],
                     max_vars_level),
                  "] "
                  )

               if (length(num_function) < 3 & !("auto" %in% num_function)) {
                  ligne1 <-
                     c(ligne1,
                       ifelse("mean" %in% num_function,ligne_moyenne,ligne_mediane)
                       )
               } else if ("auto" %in% num_function) {
                  ligne1 <- c(ligne1,ifelse(normal,ligne_moyenne,ligne_mediane))
               } else{
                  ligne1 <- c(ligne1, " ")
                  ligne2 <- c(ligne2,ligne_moyenne)
                  ligne3 <- c(ligne3,ligne_mediane)
               }
            }
         }
         ####


         ##### STATISTICAL TESTS ##### v1.5

         ### if not paired ###
         if (tests & !paired) {
            p <- "-" ## Control

            if (method == 'logistic') {
               regression <- glm(DF[, expression], family = "binomial")
               p <-  anova(regression, test = "LRT")$Pr[2]
               OR <- round(exp(summary(regression)$coefficients[2, 1]),round)
               IC <- round(exp(confint(regression)), round)
               OR <- paste0(OR, ' (', IC[2, 1], '-', IC[2, 2], ')')
               p <-  roundp(p,n = round_p)
               ligne1 <- c(ligne1, OR, p)

            } else{

               if (normal & levels_y < 3) {

                  if (nrow(mean_vars) == levels_y) {
                     variance_equal <- var.test(var ~ DF[, y])$p.value > 0.05
                  } else {
                     variance_equal <- FALSE
                  }

                  if (variance_equal) {
                     p <- roundp(t.test(var ~ DF[, y], var.equal = TRUE)$p.value, round_p)
                  } else{
                     p <- roundp(t.test(var ~ DF[, y], var.equal = FALSE)$p.value,round_p)
                     p <- paste0(p, " (a)")
                  }
               } else {
                  if (levels_y == 2){
                     withCallingHandlers(
                        warning = function(cnd) {
                           p <- roundp(kruskal.test(var ~ DF[, y])$p.value, round_p)
                           p <- paste0(p, " (d)")
                        },{
                           p <- roundp(wilcox.test(var ~ DF[, y])$p.value, round_p)
                           p <- paste0(p, " (c)")
                        }
                     )
                  }else{
                     p <- roundp(kruskal.test(var ~ DF[, y])$p.value, round_p)
                     p <- paste0(p, " (d)")
                  }
               }
            }
            ligne1 <- c(ligne1, p)


         } else if (tests & paired) {              ### if paired ###
               if (method == 'logistic') {
                  # Composition of the dataframe with each observations
                  Strate <- var_strat <- y_strat <- vector()
                  for (strate_i in strata) {
                     Strate <-  c(Strate, DF[, strate_i])
                     var_strat <- c(var_strat, var)
                     y_strat <- c(y_strat, DF[, y])
                  }
                  DF_var <-
                     as.data.frame(cbind(as.numeric(var_strat), y_strat - 1, Strate)) #y strat-1 cause the process changes the y_strat from 0/1 to 1/2
                  DF_var <- DF_var[complete.cases(DF_var), ]
                  colnames(DF_var) <- c("variable", "y", "Strate")
                  p <- anova(
                        survival::clogit(
                           expression + strata(Strate),
                           data = DF_var,
                           family = "binomial"),test = "LRT")$Pr[2]
                  OR <- exp(summary(
                        survival::clogit(
                           expression + strata(Strate),
                           data = DF_var,
                           family = "binomial"))$coefficients[2, 1])
                  OR <- round(OR, round)
                  p <- roundp(p, round_p)
                  ligne1 <- c(ligne1, OR, p)

               } else {
                  nrow = 0
                  DF_stratified <- as.data.frame(matrix(ncol = 2, nrow = 1))
                  colnames(DF_stratified) <- levels(DF$y_b)
                  for (strate_i in strata) {
                     for (a in 1:(length(DF[!is.na(DF[, strate_i]), strate_i]) / 2)) {
                        nrow  <- 1 + nrow
                        n <- DF[a, strate_i] #num de la paire
                        DF[DF[, strate_i] == n &
                              !is.na(DF[, strate_i]), c("y_b", varname)] -> tab_1 # resultat de la paire
                        c(tab_1[tab_1[, "y_b"] == levels(DF$y_b)[1], varname], tab_1[tab_1[, "y_b"] == levels(DF$y_b)[2], varname]) -> DF_stratified[nrow, ] 
                     }
                  }

                  p <- t.test(DF_stratified[, 1], DF_stratified[, 2], paired = TRUE)$p.value
                  p <- roundp(p, round_p)
                  ligne1 <- c(ligne1, p)
               }
            } else{ ligne1 <- c(ligne1, " ")}

         ligne <- ligne1
         if (!is.null(ligne2))
            ligne <- rbind(ligne, c(ligne2, " "))
         if (!is.null(ligne3))
            ligne <- rbind(ligne, c(ligne3, " "))
      } #  if numeric




      #--------------------------------------------------------------------------------------------------------------------#
      #--  if non numeric  ------------------------------------------------------------------------------------------------#
      #--------------------------------------------------------------------------------------------------------------------#
      else {
         clig <- NULL
         var <- sapply(var, ## convert modality's lenght is > 40 characters to first 40 chr
                       function(x) {
                          if (stringr::str_length(x) > 40 & !is.na(x)) {
                             paste0(substr(x, 1, 40), "...")
                          } else{x}
                       })

         #### Transformation into factor and mutation if needed
         relevel(as.factor(as.character(var)),as.character(X[6])) -> var
         var[var == "NA"] <- NA

         # if there is more than 'mutation' modalities, the last modalities are grouped in 'others' modality
         if (length(levels(var)) >= mutation) {
            nvar <- as.vector(var)
            for (other_levels_i in mutation:length(levels(var))) {
               other_levels <- levels(var)[other_levels_i]
               nvar[nvar == other_levels] <- "others"
            }
           as.factor(nvar) -> var
         }
         #---#


         if (length(levels(var)) >= 2) {

            if (!is.null(y)) {
               tb <- table(DF[, y],var, useNA = "no")
               tbm <- margin.table(tb, 1)

               # conditions verification
               verif_level <- margin.table(table(var, DF[, y]), 2)
               verif <- ! any(sapply(verif_level, function(lev) lev == 0)) # false if 0 elements in a group

               # Chi2 conditions
               if (verif) {
                  condition_chi2 <- 0
                  condition_chi2_B <- TRUE
                  for (m in chisq.test(var, DF[, y], simulate.p.value = TRUE)$expected) {
                     if (m < 5) { # counting EXPECTED modalities < 5. if one > 3 and only 2 column we can apply yate's correction otherwise we apply Fisher
                        condition_chi2_B <- FALSE
                        if (m < 3) {
                           condition_chi2_B <- FALSE
                           break
                        } else{
                           condition_chi2 <- condition_chi2 + 1
                        }
                     }
                  }
                  if (condition_chi2 > 1 | (condition_chi2 == 1 & length(levels(var)) > 2)) {
                     condition_chi2_B <- FALSE
                  }

                  ## if paired ##
                  if (paired & tests) {
                     Strate <- var_strat <- y_strat <- vector()
                     for (strate_i in strata) {
                        Strate <-  c(Strate, DF[, strate_i])
                        var_strat <- c(var_strat, var)
                        y_strat <- c(y_strat, DF[, y])
                     }
                     DF_var <-as.data.frame(cbind(
                           as.numeric(var_strat),
                           y_strat - 1,
                           Strate))
                     DF_var <- DF_var[complete.cases(DF_var), ]
                     colnames(DF_var) <- c("variable", "y", "Strate")

                     ## if not paired ##
                  } else if (tests) {
                     if (method == 'logistic') {
                        p <- anova(glm(DF[, expression], family = "binomial"), test = "LRT")$Pr
                        OR <- exp(summary(glm(DF[, expression], family = "binomial"))$coefficients[, 1])
                     } else {
                        if (condition_chi2_B) { # if verif ok chi2, else Fisher
                           clig <- roundp(chisq.test(var, DF[, y])$p.value,
                                         round_p)
                        } else {
                           clig <- roundp(fisher.test(var, DF[, y], simulate.p.value = T)$p.value,
                                 round_p)
                           sign <- " (b)"
                        }
                     }
                  }
               }

            } # if y is not null


            ## Variable with 2 levels #############################
            if ((length(levels(var)) == 2) & as.logical(X[4])) {
               if (toupper(levels(var)[1]) == "NON") {
                  if (toupper(levels(var)[2]) == "OUI")
                     var <- relevel(var, levels(var)[2])
               }

               if (tests) {
                  if (paired) {
                     if (method == 'logistic') {
                        clig <- anova(survival::clogit(
                                 expression + strata(Strate),
                                 data = DF_var,
                                 family = "binomial"),test = "LRT")$Pr[2]
                        OR <- exp(summary(
                              survival::clogit(
                                 expression + strata(Strate),
                                 data = DF_var,
                                 family = "binomial"
                              ))$coefficients[2, 1])
                        OR <- round(OR, round)
                        clig <- roundp(clig, round_p)
                        clig <- c(OR, clig)

                     } else{
                        nrow = 0
                        DF_stratified <- matrix(ncol = 2, nrow = 1)
                        DF_stratified <- as.data.frame(DF_stratified)
                        colnames(DF_stratified) <- levels(DF$y_b)

                        for (strate_i in strata) {
                           for (a in 1:(length(DF[!is.na(DF[, strate_i]), strate_i]) / 2)) {
                              nrow  <- 1 + nrow
                              n <- DF[a, strate_i] #num de la paire
                              DF[DF[, strate_i] == n & !is.na(DF[, strate_i]),
                                 c("y_b", varname)] -> tab_1 
                              c(tab_1[tab_1[, "y_b"] == levels(DF$y_b)[1], varname], tab_1[tab_1[, "y_b"] == levels(DF$y_b)[2], varname]) -> DF_stratified[nrow, ]                            }
                        }
                        p <- mcnemar.test(DF_stratified[, 1], DF_stratified[, 2])$p.value
                        clig <- roundp(p[2], round_p)
                     }

                  } else if (method == "logistic") {
                     OR <- round(OR[2], round)
                     IC <- round(exp(confint(glm(DF[, expression], family = "binomial"))),
                                 round)
                     OR <- paste0(OR, ' (', IC[2, 1], '-', IC[2, 2], ')')
                     clig <- roundp(p[2], round_p)
                     clig <- c(OR, clig)
                  }
               }

               if (available_data) {
                  if (as.logical(X[3])){
                     ligne <- c(paste0(ligne1, " (", levels(var)[1], ") - no. (%)"), n_available)
                  } else {
                     ligne <- c(ligne1, n_available)
                  }
               } else{
                  if (as.logical(X[3])) {
                     ligne <- c(paste0(ligne1, " (", levels(var)[1], ") - no. (%)"))
                  } else {
                     ligne <- c(ligne1)
                  }
               }

               if (overall) {
                  overall_count <- addmargins(table(DF[, varname], useNA = "no"))[levels(var)[1]]
                  percent_overall <- round(100 * overall_count / n_available, round)
                  ligne <- c(ligne, paste0(overall_count, "  (", percent_overall, "%)"))
               }

               if (!is.null(y)) {
                  ligne <- c(ligne,
                             sapply(1:levels_y, function(j) {
                                paste0(tb[j, 1],
                                       " (",
                                       round(100 * tb[j, 1] / tbm[j], round),
                                       "%)")
                             }))
               }

               if (tests)
                  ligne <- c(ligne, paste0(clig, sign))

            } else { ## Variable with more than 2 levels #############################

               ligne <- ifelse(as.logical(X[3]),
                               paste0(X[2], " - no. (%)"),
                               X[2])

               if (available_data)
                  ligne <- c(ligne, n_available)

                  ligne <- c(ligne,
                             rep("", ifelse(overall,levels_y+1,levels_y)))

                  if (tests){
                     ligne <- c(ligne,ifelse(method == 'logistic',
                                             c(" ", " "),
                                             paste0(clig, sign)))
                  }


               # other lines
               lignes_tot <- matrix(ligne, ncol = ncol(tabf))
               for (n in 1:length(levels(var))) {
                  if (tests) {
                     if (paired) {
                        if (n == 1) {
                           clig <- '-'
                           OR <- 1
                        } else {
                           clig <-
                              summary(
                                 survival::clogit(
                                    expression + strata(Strate),
                                    data = DF_var,
                                    family = "binomial"
                                 )
                              )$coefficients[n, 4]
                           OR <-
                              exp(summary(
                                 survival::clogit(
                                    expression + strata(Strate),
                                    data = DF_var,
                                    family = "binomial"
                                 )
                              )$coefficients[n, 1])
                           IC <-
                              exp(confint(
                                 survival::clogit(
                                    expression + strata(Strate),
                                    data = DF_var,
                                    family = "binomial"
                                 )
                              ))
                           OR <- round(OR, round)
                           OR <- paste0(OR, ' (', IC[2, 1], '-', IC[2, 2], ')')
                           clig <- roundp(clig, round_p)
                        }
                        clig <- c(OR, clig)
                     } else if (method == "logistic") {
                        if (n == 1) {
                           clig <- '-'
                           OR_clig <- 1
                        } else {
                           clig <- p[n]
                           OR_clig <- round(OR[n], round)
                           clig <- roundp(clig, round_p)
                        }
                        clig <- c(OR_clig, clig)
                     } else {
                        clig <- " "
                     }
                  } else {
                     clig <- " "
                  }

                  # For each modality
                  ligne <- ifelse(exit != "html",
                                  paste0("        ", levels(var)[n]),
                                  paste0(" \t \t  ", levels(var)[n]))

                  if (available_data)
                     ligne <- c(ligne, " ")

                  if (overall) {
                     overall_count <- addmargins(table(DF[,varname], useNA = "no"))[levels(var)[n]]
                     percent_overall <- round(100 * overall_count / length(var), round)
                     ligne <- c(ligne, paste0(overall_count," (", percent_overall," %)"))
                  }

                  if (!is.null(y)) {
                     tb <- table(DF[, y], var, useNA = "no")
                     tbm <- margin.table(tb, 1)
                     for (j in 1:levels_y) {
                        no <- tb[j, n]
                        pn <- round(100 * no / tbm[j], round)
                        ligne <- c(ligne, paste0(no, " (", pn, "%)"))
                     }
                  }
                  if (tests){
                     lignes_tot <- rbind(lignes_tot,c(ligne,clig))
                  } else {
                     lignes_tot <- rbind(lignes_tot,ligne)
                  }

               }
               lignes_tot -> ligne
            }
         }
      }
      return(ligne)
   })

   as.data.frame(do.call(rbind, lignes),stringsAsFactors = F) -> rslt
   row.names(rslt) <- NULL
   rslt <- rbind(tabf[2,],rslt)
   colnames(rslt) <- tabf[1,]

   if ("excel" %in% exit)
      xlsx::write.xlsx(
         rslt,
         paste0("table1", ".xlsx"),
         sheetName = "name_sheet",
         append = FALSE,
         row.names = FALSE
      )

   text_footer <- paste0(
      "Data shown are number (and percentage) for categorical data and ",
      ifelse("mean" %in% num_function,
             "mean and standard deviation ",
             ifelse('auto' %in% num_function,
                    "mean and standard deviation for numerical data if they were normaly distributed, median and interquartile range otherwise.",
                    ifelse('range' %in% num_function,
                           "median and range for numerical data.",
                           "median and interquartile range for numerical data."
                    )
             )
      ), ifelse(!tests,
                "",
                ifelse(
                   any("paired" %in% method),
                   " p-values have been obtained from paired t test for continuous variables and mac nemar test for categorical variables",
                   " p-values have been obtained from a two-sided student test for continuous variables and from a khi-2 test for categorical variables, unless specified otherwise (a: Student test with Welch approximation, b: Fisher's exact test, c: Wilcoxon test, d: Kruskal-Wallis Rank sum test)"
                )
      )
   )

   if ("html" %in% exit) {
      rslt %>%
         flextable(col_keys = colnames_rslt) %>%
         fontsize(i = 1, part = "header", size = 24) %>%
         bold(i = 1, part = "header", bold = TRUE) %>%
         theme_booktabs() %>%
         autofit() %>%
         align(j = 2:nb_colums, align = 'center') %>%
         align(j = 2:nb_colums, align = 'center', part = "header") -> rslt
   }

   version = paste0('version : ', version_pkg)
   return(list(version,rslt,text_footer))
}
