########### REFERENCES ##############
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-complete-or-quasi-complete-separation-in-logisticprobit-regression-and-how-do-we-deal-with-them/
# https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression


#1) make sure that y is not a dichotomous version of a variable in the model

#' Check dichotomous distribution for one variable
#'
#' @param DF : a dataframe
#' @param var : the variable
#' @param y : the variable to explain
#'
#' @return check_dichotomous returns a boolean TRUE if y is a dichotomous variable of x
#' @export
#'
#' @examples
check_dichotomous <-
   function(var,y,DF)
   {

      table(DF[, y], DF[, var]) -> table
      table[1, ] -> table0
      table[2, ] -> table1
      table0[table0 != 0] -> value0
      table1[table1 != 0] -> value1
      names_value0 <- names(value0)
      names_value1 <- names(value1)
      as.numeric(names(value1)) -> names_value1
      check <- FALSE
      if (TRUE %in% c(is.na(names_value0), is.na(names_value1))) {
         cat("not all numerics")
      } else{
         names_value0[order(names_value0)] -> names_value0
         names_value1[order(names_value1)] -> names_value1
         if (max(names_value0) < min(names_value1)) {
            check <- TRUE
            cat(
               "\n\n",
               y,
               " is fully explicated by ",
               var,
               " :\n
             x = (",
               trunc(min(names_value0)),
               ":",
               trunc(min(names_value1)),
               "[     y = 0
             x = (",
               trunc(min(names_value1)),
               ":",
               (trunc(max(names_value1)) + 1),
               "[     y = 1"
            )
         }
         if (min(names_value0) > max(names_value1)) {
            check <- TRUE
            cat(
               "\n\n",
               y,
               " is fully explicated by ",
               var,
               " :\n
             x = (",
               trunc(min(names_value1)),
               ":",
               trunc(min(names_value0)),
               "[     y = 1
             x = (",
               trunc(min(names_value0)),
               ":",
               (trunc(max(names_value0)) + 1),
               "[     y = 0"
            )
         }
      }
      return(check)
   }



###############
#2) Data manipulation
clogg_transformation <- function(DF)
   {
      DF <- as.data.frame(DF)
      if (ncol(DF) > 2)
         stop("clogg transformation is only treated for univariate analysis")
      k = nrow(DF) #numberofparametre
      pi = vector()
      new_DF_uni <- as.matrix(DF)
      row.names(new_DF_uni) <- NULL
      table(DF) -> table_Clogg
      n_to_add <- table(DF)
      g_number <- length(colnames(table_Clogg))
      for (g in 1:length(colnames(table_Clogg)))
      {
         n = sum(table_Clogg[, g])
         pi[g] = table_Clogg[2, g] / n
         n_to_add[1, g] <- (1 - pi[g]) * k/g_number
         n_to_add[2, g] <- (pi[g]) * k/g_number
      }
      for (c in 1:2)
      {
         for (d in 1:2)
         {
            for (n in seq(n_to_add[c, d]))
            {
               DF[k + n, 1] <- rownames(n_to_add)[c]
               DF[k + n, 2] <- colnames(n_to_add)[d]
            }
            k <- nrow(DF)
         }
      }
   DF <- as.data.frame(DF)
   return(DF)
   }


###############
recast <- function(var,
                   DF,
                   y,
                   nb_breaks=5,
                   nb_entier = TRUE,
                   max_break = 9)
{

   DF <- as.data.frame(DF)
   if (is.character(DF[, var]) || is.factor(DF[, var]))
   {
      table(DF[,var],DF[,y]) -> table_var
         cat("
   Recast the model consists of re-grouping columns (variables) or modalities (if multimodal variable) :
      - The variable is :",var,"
      - modalities are : ")
         for (n in seq(table_var[,1]))
         {
            indice <- ifelse((table_var[n,1] < 3 || table_var[n,2] < 3)," ***"," ")
             cat(paste0("\n\t\t", n,". ",rownames(table_var)[n],indice))
         }
      cat(" ")
      modalities_to_bind <- readline("Which modalities should be bind ? (ex : 1+2+3=autres)    ")
      cat(" ")
      stringr::str_split(modalities_to_bind,"=")[[1]][1] -> variables
      stringr::str_split(variables,"[+]")[[1]] -> variables
      stringr::str_split(modalities_to_bind,"=")[[1]][2] -> new_name
      for (s in seq(variables))
      {
         DF[, var][DF[, var] == rownames(table_var)[as.numeric(variables[s])]] <- new_name
      }
   }else
   {
      trunc(max(DF[ ,var])) + 1 -> max_int
      trunc(min(DF[ ,var])) -> min_int
      range = max_int - min_int
      possible_breaks <- 1 + seq(1:min(max_break - 1, range - 1)) #max between set in function and possible
      for (b in possible_breaks[possible_breaks <= range])
      {
         if (range %% b != 0) #rm if not multiple
            possible_breaks <- possible_breaks[-(b - 1)]
      }

      result_break <- as.data.frame(DF[ ,var])
      i <- 2
      for (breaking in possible_breaks)
      {
         step <-  (max_int - min_int)/breaking
         breaks_test <-  min_int
         for (m in 1:breaking)
         {
            breaks_test[m + 1] = breaks_test[m] + step
         }
         new_var <- cut(DF[ ,var],breaks = breaks_test, include.lowest = TRUE)
         result_break[ ,i] <- new_var
         colnames(result_break)[i] <- breaking
         i = i + 1
      }
      # Check result
      sum_sq_diff = vector()
      sq_diff = vector()
      for (u in 1:(length(result_break) - 1))
      {
         sd(table(result_break[ ,u + 1])) -> sum_sq_diff[u]
         names(sum_sq_diff)[u] <- colnames(result_break)[u + 1]
      }

      names(sum_sq_diff[order(sum_sq_diff)][1]) -> best_breaks #cut with the more homogenous solution

      #final cut
      DF[ ,var] <- result_break[ ,best_breaks]

   }
      DF <- as.data.frame(DF)
      return(DF)
}





#  CHOICES
###################
#' Choice of complete separation method
#'
#' @param var variable to be evaluate
#' @param y the variable to explain
#' @param DF the dataframe
#' @param continue (optional) : whether the response should be apllied to all following variables.
#'
#' @return
#' @export
#'
#'
#' @examples
complete_separation <- function(var,
                                y,
                                DF,
                                continue = FALSE)
   {

   if (is.numeric(DF[,var]) || length(levels(as.factor(DF[,var]))) == 2)
   {
      #dichotomous <- check_dichotomous(var,y,DF)
      dicho_msg <- "(not advised)"
      #ifelse(
       #  dichotomous,
       #  paste0("(",var,"seems to be a dichotomous variation of ",y,")"),
       #  "(not advised)"
      #)
   }else
   {
      dicho_msg <- "(not advised)"
   }



   available <- c("1","2","6")
   availablec <- c("1c","2c","6c")
   if (!(continue %in% available))
   {
      cat("
      What method should be applied to",var," ?
       1. Omission of the variable.", dicho_msg,"
       2. Re-cast the model
       3. Use of an ad hoc adjustment (Clogg transformation)
       4. Exact logistic regression (not available)
       5. Standard analysis with beta set to a high value (not available)
       6. Firth's Bias-Reduced Logistic Regression"
      )
      choice = 0
      msg <- paste("Your choice (",paste(available,collapse = ", "),") ?     ")
      while (!(choice %in% available || choice %in% availablec))
      {
         cat(" ")
         choice <- readline(msg)
      }
   }else
   {
      choice <- continue
   }

   # Actions function of the choice
   if (grepl("1",choice))
   {
      DF[, var] <- NULL
      as.data.frame(DF) -> DF
      DF$method[1] <- "logit"
   }
   #####
   if (grepl("2",choice))
   {
      DF <- recast(var, DF, y)
      DF <- as.data.frame(DF)
      DF$method[1] <- "logit"
   }
   #####
   if (grepl("3",choice))
   {
      DF <- clogg_transformation(DF)
      DF <- as.data.frame(DF)
      DF$method[1] <- "logit"
   }
   #####
   if (grepl("6",choice))
   {
      DF$method[1] <- "firth"
   }

   # If a c is added
   #####
   if (grepl("c",choice))
   {
      continue <- strsplit(choice,"c")[[1]]
   }
   DF$continue[1] <- continue
   DF <- as.data.frame(DF)
   return(DF)
}
