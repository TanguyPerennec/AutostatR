#' Variable selection for a multivariate logistic regression model
#'
#' @param DF dataframe
#' @param y character : variable to explain by 'explicatives'
#' @param explicatives vector of character : variables to be selected in the multivariate model
#' @param principal_factor (optional) : principal criteria to explain y. It will be th first variable to be selected and won't be removed from selection.
#' @param method character : can be backward, forward. You can pass other arguments such as augmented or stepwise as follow "backward stepwise augmented".
#' @param criteria character : the criteria that will be used to select models. Can be deviance, AIC and BIC.
#' @param check_interactions logical : wether interaction between the principal_factor and the other variables should be checked.
#' @param alpha num : the threesold that should be used to consider a test as significant
#' @param verbose logical : whether the details of the calculs should be displayed on the console
#' @param keep character or vector (optional) : variables that will be kept in the model no matter of its statistical importance Every variable known in the litterature to have interaction with y or other 'keep' variable should be listed in 'keep'.
#' @param delta num : threeshold of difference in BIC or AIC to consider a model as more informative
#'
#' @return
#' @export
#'
#' @examples
multivariate_selection <-
   function(DF,
            y,
            explicatives,
            keep,
            principal_factor=FALSE,
            method = "backward",
            criteria = "deviance",
            check_interactions = FALSE,
            alpha = 0.05,
            verbose=TRUE,
            delta=7)
   {

      #To ignore warnings during usage
      options(warn = -1)
      options("getSymbols.warning4.0" = FALSE)


      ##################################################
      #    Arguments verification / transformation     #
      ##################################################

      if (missing(explicatives))
         explicatives <- colnames(DF)[colnames(DF) != y]

      if (missing(keep))
      {
         keep <- FALSE
         if (verbose)
            message("no keep variables have been provided")
      }

      if (!(criteria  %in% c("deviance", "AIC", "BIC")))
         stop("criteria is not on the list")



      models_test <- function(model,
                              y,
                              var_diff,
                              ...){

         # Get the formula of the model to compare from the model and the variable that differ
         formule_diff_terms <- attr(model$terms,"term.labels")
         if (var_diff %in% formule_diff_terms){
            formule_diff_terms <- formule_diff_terms[formule_diff_terms != var_diff]
         }else {
            formule_diff_terms <- c(formule_diff_terms,var_diff)
         }
         formule_diff <- formulation(c(y,formule_diff_terms))


         if (criteria  == "deviance") {
            model2 <- stats::glm(formule_diff, data = DF, family = "binomial")
            anova_test <- anova(model, model2, test = "LRT")
            pval <- anova_test$Pr[nrow(anova_test)]
            val_test <- pval
         }

         if (criteria  == "AIC" || criteria  == "BIC"){
            k_val <- ifelse(criteria  == "AIC",2,log(nrow(DF)))
            model2 <- stats::glm(formule_diff,data = DF,family = "binomial")
            sens <- ifelse(extractAIC(model,k = k_val)[1] > extractAIC(model2,k = k_val)[1],1,-1)
            delta_AIC <- (extractAIC(model,k = k_val)[2] - extractAIC(model2,k = k_val)[2])*sens
            val_test <- delta_AIC
         }

         return(val_test)
      }


      response <- list()

      DF <- as.data.frame(DF)
      explicatives_multi_test <- c("null model",explicatives)
      matrix_rslt <- matrix(ncol = length(explicatives)*3,nrow = length(explicatives))
      rownames(matrix_rslt) <- explicatives
      nstep = 0



      # BACKWARD ELIMINATION
      ##############################
      if (grepl("backward",method)) {
         #Determination of the principal factor (specified or the variable for which the deviance is the smallest)
         if (!is.logical(principal_factor)) {
            if (!(principal_factor %in% explicatives))
               stop("the principal factor must be in 'explicatives'")
            if (verbose)
               cat("\nFirst variable set is <",principal_factor,">\n because it is the principal factor specifed")
            principal_factor -> keep[length(keep) + 1]
         }
         if (is.logical(principal_factor)) {
            # Determination of p-val with anova for all variables
            models1 <- data.frame(row.names = explicatives_multi_test)
            for (j in seq(explicatives_multi_test)){
               if (j == 1){
                  model1 <- stats::glm(DF[ ,y]~1, family = "binomial")
               } else {
                  model1 <- stats::glm(DF[ ,y]~DF[, explicatives_multi_test[j]], family = "binomial")
               }
               models1$deviance[j] <- model1$deviance
            }
            principal_factors <- models1[order(models1$deviance),,drop = FALSE]
            principal_factor <- rownames(principal_factors)[1]
            if (verbose)
               cat("\nFirst variable set is <",principal_factor,">\n because the model with this variable is better than all other models with 0 (null model) or 1 variable\n")
         }


         explicatives_remains <- explicatives[explicatives != principal_factor]
         formule <- formula(paste0(y,"~",principal_factor))
         model1 <- stats::glm(formule, data = DF, family = "binomial")


         # Determination of interactions with the principal factor
         if (check_interactions) {
            interact_table <-  data.frame(row.names = explicatives_remains)
            for (i in seq(explicatives_remains))
            {
               formule1 <- paste(y, "~", principal_factor, "+", explicatives_remains[i])
               formule2 <- paste(formule1, "+", principal_factor, ":", explicatives_remains[i])
               formule1 <- formula(formule1)
               formule2 <- formula(formule2)
               model_interact <- stats::glm(formule2,data = DF, family = "binomial")
               anova <- anova(model_interact,test = "LRT")
               pvalue <- anova$Pr[4]
               interact_table$pval[i] <- pvalue
            }
            interact_table <- interact_table[order(interact_table$pval),,drop = FALSE] #df with all p-values of likelihood ratio test between model with and without interaction
            signif_interact_table <- row.names(interact_table)[interact_table[,1] < 0.05]
            for (j in seq(signif_interact_table))
            {
               signif_interact_table[j] <- paste0(principal_factor,":",signif_interact_table[j])
            }
            interactives_remains <- signif_interact_table
         }



         # Backward selection for interaction
         if (check_interactions) {
            if (length(interactives_remains) > 0) {
               for (i in seq(interactives_remains)) {
                  formule <- formulation(DF[,c(y,explicatives_remains)])
                  for (i in seq(interactives_remains)) {
                     interactive_formule <-
                        ifelse(i == 1, interactives_remains[i], paste0("+", interactives_remains[i]))
                  }
                  newterms <- interactive_formule
                  formule <- update(formule, ~ . + newterms)
                  model0 <- stats::glm(formule, data = DF, family = "binomial")
                  pvals <- summary(model0)$coefficients[,4]

                  #Elimination one by one of all interaction variable based on PVALUE
                  interact_pval <- pvals[match(interactives_remains,names(pvals))]
                  interact_pval <- interact_pval[order(-interact_pval)]
                  if (interact_pval[1] > alpha)
                  {
                     interactives_remains <- interactives_remains[interactives_remains != names(interact_pval)]
                  }
               }
            }
         }


         vars_remainings <- explicatives
         formule_lastmodel <- formulation(object = DF[,c(y,vars_remainings)])
         last_model <- stats::glm(formule_lastmodel,data = DF, family = "binomial")
         vars_out <- NULL

         while (length(vars_remainings) > 1 & nstep < length(explicatives)*3)
         {
            nstep <- nstep + 1
            model_test_df <- matrix(nrow = length(vars_remainings), ncol = 2)
            for (i in seq(vars_remainings))
            {
               pval <- models_test(model = last_model, y, var_diff = vars_remainings[i])
               model_test_df[i, ] <- c(vars_remainings[i],pval)
            }
            model_test_df <- as.data.frame(model_test_df)
            col2 <- ifelse(criteria == "deviance","pval",paste0("delta ",criteria))
            colnames(model_test_df) <- c("name",col2)
            model_test_df[,2] <- as.numeric(as.character(model_test_df[,2]))
            model_test_df$name <- as.character(model_test_df$name)
            for (f in seq(model_test_df[,1]))
            {
               col <- model_test_df[f,1]
               matrix_rslt[match(col,rownames(matrix_rslt)),nstep] <- model_test_df[f,2]
            }
            model_test_df[order(-model_test_df[,2]),] -> model_test_df


            k <- 1
            while (model_test_df[k,1] %in% keep)
            {
               k <- k + 1
            }

            if (k > length(vars_remainings))
            {
               "no variable remainings but the variables to keep"
            }else {
               criteria_trigger <- ifelse(criteria == "deviance",alpha,delta)
               #print(model_test_df[k, 2] )
               #print(criteria_trigger)
               if (!is.na(model_test_df[k, 2]))
               {
                  if (model_test_df[k, 2] > criteria_trigger)
                  {
                     if (verbose)
                        cat("\nThe best model exclude <",model_test_df[k,1], "> (",ifelse(criteria == "deviance","with likelihood ratio test : p =","with delta AIC ="),round(as.numeric(model_test_df[k,2]),3),"), so this variable is now excluded")
                     vars_remainings <- vars_remainings[vars_remainings != model_test_df[k,1]]
                     vars_out <- c(vars_out,model_test_df[k,1])
                  }else
                  {
                     if (verbose)
                        cat("\nThe best model do not exclude any variable of the remainings (",ifelse(criteria == "deviance",paste0("with risk",alpha),paste0("with delta",criteria," threeshold = 10")),"):",vars_remainings)
                     break
                  }
               }
            }
            last_model <- stats::glm(formula = formulation(object = vars_remainings,y),family = "binomial",data = DF)



            ############
            if ((grepl("stepwise",method) || "stepwise" %in% method) & !is.null(vars_out))
            {
               model_test_df <- matrix(nrow = length(vars_out), ncol = 2)
               for (i in seq(vars_out))
               {
                  pval <- models_test(model = last_model, y, var_diff = vars_out[i])
                  model_test_df[i,] <- c(vars_out[i],pval)
               }
               model_test_df <- as.data.frame(model_test_df)
               col2 <- ifelse(criteria == "deviance","pval",paste0("delta ",criteria))
               colnames(model_test_df) <- c("name",col2)
               model_test_df[,2] <- as.numeric(as.character(model_test_df[,2]))
               model_test_df$name <- as.character(model_test_df$name)
               for (f in seq(model_test_df[,1]))
               {
                  col <- model_test_df[f,1]
                  matrix_rslt[match(col,rownames(matrix_rslt)),nstep] <- model_test_df[f,2]
               }
               model_test_df[order(model_test_df[,2]),] -> model_test_df

               k <- 1
               while (model_test_df[k,1] %in% keep)
               {
                  k <- k + 1
               }

               if (model_test_df[k,2] < ifelse(criteria == "deviance",alpha,7))
               {
                  if (verbose)
                     cat("\nThe best model now includes <",model_test_df[k,1], "> (",ifelse(criteria == "deviance","with likelihood ratio test : p =","with delta AIC ="),round(as.numeric(model_test_df[k,2]),3),"), so this variable is now excluded")
                  vars_remainings <- c(vars_remainings,model_test_df[k,1])
                  vars_out <- vars_out[vars_out != model_test_df[k,1]]
               }
               else
               {
                  if (verbose)
                     cat("\nThe best model does not include any variables out of the present model (",ifelse(criteria == "deviance",paste0("with risk",alpha),paste0("with delta ",criteria," threeshold = 10")),"):",vars_out)
               }
            }
            last_model <- stats::glm(formula = formulation(object = vars_remainings,y),family = "binomial",data = DF)

            }
         }




      #=========================================================================================#

      # FORWARD SELECTION
      ##############
      if (grepl("forward",method) || "forward" %in% method)
      {
         if (is.null(keep) || is.logical(keep))
         {
            vars_included <- principal_factor
         }else{
            vars_included <- keep
         }
         vars_remainings <- explicatives[explicatives != vars_included]
         formule <- formulation(vars_included,y)
         last_model <- stats::glm(formule,data = DF, family = "binomial")
         nstep <- 0

         while (length(vars_remainings) > 0 & nstep < length(explicatives)*3)
         {
            nstep <- nstep + 1
            model_test_df <- matrix(nrow = length(vars_remainings), ncol = 2)
            for (i in seq(vars_remainings))
            {
               pval <- models_test(model = last_model, y, var_diff = vars_remainings[i])
               model_test_df[i,] <- c(vars_remainings[i],pval)
            }
            model_test_df <- as.data.frame(model_test_df)
            col2 <- ifelse(criteria == "deviance","pval",paste0("delta ",criteria))
            colnames(model_test_df) <- c("name",col2)
            model_test_df[,2] <- as.numeric(as.character(model_test_df[,2]))
            model_test_df$name <- as.character(model_test_df$name)
            for (f in seq(model_test_df[,1]))
            {
               col <- model_test_df[f,1]
               matrix_rslt[match(col,rownames(matrix_rslt)),nstep] <- model_test_df[f,2]
            }
            model_test_df[order(model_test_df[,2]),] -> model_test_df

            if (model_test_df[1,2] < ifelse(criteria == "deviance",alpha,delta))
            {
               if (verbose)
                  cat("\nThe best model includes <",model_test_df[1,1], "> (",ifelse(criteria == "deviance","with likelihood ratio test : p =","with delta AIC ="),round(as.numeric(model_test_df[1,2]),3),"), so this variable is now included")
               vars_remainings <- vars_remainings[vars_remainings != model_test_df[1,1]]
               vars_included <- c(vars_included,model_test_df[1,1])
            }else
            {
               if (verbose)
                  cat("\nThe best model does not include any variables remaining (",ifelse(criteria == "deviance",paste0("with risk ",alpha),paste0("with delta ",criteria," threeshold = 10")),"):",vars_remainings)
               break
            }

            last_model <- stats::glm(formula = formulation(object = vars_included,y),family = "binomial",data = DF)


            if ((grepl("stepwise",method) || "stepwise" %in% method) & nstep > 1)
            {
               model_test_df <- matrix(nrow = length(vars_included), ncol = 2)
               vars_included <- vars_included[vars_included != FALSE]
               for (i in seq(vars_included))
               {
                  pval <- models_test(model = last_model, y, var_diff = vars_included[i])
                  model_test_df[i,] <- c(vars_included[i],pval)
               }
               model_test_df <- as.data.frame(model_test_df)
               col2 <- ifelse(criteria == "deviance","pval",paste0("delta ",criteria))
               colnames(model_test_df) <- c("name",col2)
               model_test_df[,2] <- as.numeric(as.character(model_test_df[,2]))
               model_test_df$name <- as.character(model_test_df$name)
               for (f in seq(model_test_df[,1]))
               {
                  col <- model_test_df[f,1]
                  matrix_rslt[match(col,rownames(matrix_rslt)),nstep] <- model_test_df[f,2]
               }
               model_test_df[order(-model_test_df[,2]),] -> model_test_df

               k <- 1
               while (model_test_df[k,1] %in% keep)
               {
                  k <- k + 1
               }

               if (model_test_df[k,2] > ifelse(criteria == "deviance",alpha,7))
               {
                  if (verbose)
                     cat("\nThe best model now exclude <",model_test_df[k,1], "> (",ifelse(criteria == "deviance","with likelihood ratio test : p =","with delta AIC ="),round(as.numeric(model_test_df[k,2]),3),"), so this variable is now excluded")
                  vars_remainings <- c(vars_remainings,model_test_df[k,1])
                  vars_included <- vars_included[vars_included != model_test_df[k,1]]
               }
               else
               {
                  if (verbose)
                     cat("\nThe best model do not exclude any variable of the present model (",ifelse(criteria == "deviance",paste0("with risk",alpha),paste0("with delta ",criteria," threeshold = 10")),"):",vars_included)
               }
               last_model <- stats::glm(formula = formulation(object = vars_included,y),family = "binomial",data = DF)
            }
         }
      }



         if (grepl("forward",method))
         {
            response$vars_multi <- vars_included[-1]
         }else{
            response$vars_multi <- vars_remainings
         }
         col <- ncol(matrix_rslt)

            while (col > 1)
            {
               if (ncol(matrix_rslt) < 3)
               {
                  matrix_rslt <- matrix_rslt[,-col]
                  break
               }
               if (!(FALSE %in% is.na(matrix_rslt[,col])))
               {
                  matrix_rslt <- matrix_rslt[,-col]
               }
               col <- col - 1
            }
            if (!is.null(ncol(matrix_rslt)))
            {
               for (col in 1:ncol(matrix_rslt))
               {
                  if (method == "backward")
                  {
                     matrix_rslt <- matrix_rslt[order(matrix_rslt[,col],na.last = TRUE),]
                  }else{
                     matrix_rslt <- matrix_rslt[order(-matrix_rslt[,col],na.last = TRUE),]
                  }
               }
            }

            response$all_values <- round(matrix_rslt,3)

            return(response)

         }

