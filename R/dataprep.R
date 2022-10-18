#' Colnames preparation
#'
#' @param object : vector of character or dataframe
#' @param type (character) : use 'makes.names' to create new names in correct format for R function or 'presentation' to put it in correct way to definitive presentation
#'
#' @return
#' @export
#'
#' @examples
colnames_prep <- function(object,type="make.names"){

   # Verification
   if (is.data.frame(object)) {
      names <- colnames(object)
   } else if (is.vector(object) || is.character(object)) {
      names <- object
   } else {
      stop("'object' is not a dataframe nor a vector")
   }

   if ("make.names" %in% type) {
      make.names <- TRUE
      presentation <- FALSE
   } else if (type == "presentation") {
      presentation <- TRUE
      make.names <- FALSE
   } else {
      stop("please make sure you provided a correct 'type' ")
   }

   if (make.names) {

      # No special characters
      for (i in seq(names)) {
         names[i] <- stringr::str_replace_all(names[i],"[\\u00e9\\u00e8\\u00ea\\u00eb]","e")
         names[i] <- stringr::str_replace_all(names[i],"[\\u00e0\\u00e2]","a")
         names[i] <- stringr::str_replace_all(names[i],"[\\u00ef]","i")
         names[i] <- stringr::str_replace_all(names[i],"[\\u00f9\\u00fa\\u00fc]","u")
         names[i] <- stringr::str_replace_all(names[i],"[\\u00f4\\u00f6]","o")
         names[i] <- stringr::str_replace_all(names[i],"[\\u00f1]","n")
         names[i] <- stringr::str_replace_all(names[i],"[\\u00e6]","ae")
      }

      # no spaces
      for (i in seq(names)) {
         names[i] <- stringr::str_replace_all(names[i],"[\ ]","_")
         names[i] <- stringr::str_replace_all(names[i],"[...]","_")
         names[i] <- stringr::str_replace_all(names[i],"[.]","_")
         names[i] <- stringr::str_replace_all(names[i],"[\']","_")
      }

   } else if (presentation) {
      for (i in seq(names)) {
         names[i] <- stringr::str_replace_all(names[i],"[_]"," ")
         names[i] <- stringr::str_to_sentence(names[i])
      }
   }

   return(names)

}



#' Detect outliers
#'
#' @param var : the variable to test
#' @param k : factor to apply to median absolute deviation to obtain the interval
#' @param inplace (boolean) : whether the variable should be returned without the outliers or not
#'
#' @return
#' @export
#'
#' @examples
detect_outliers <- function(var,
                            k=3,
                            inplace=TRUE)
{
   if (!is.numeric(var))
      stop("variable has to be numeric !")

   binf <- median(var,na.rm = TRUE) - k * mad(var,na.rm = TRUE) # calcule la borne inf de l'intervalle
   bsup <- median(var,na.rm = TRUE) + k * mad(var,na.rm = TRUE) # calcule la borne sup de l'intervalle

   outliers <- list()

   for (n in seq(length(var))){
      if (!is.na(var[n]))
      {
         values = var[n]
         if (values < binf | values > bsup)
         {
            outliers$values <- c(outliers$values,values)
            outliers$places <- c(outliers$places,n)
            if (inplace)
            {
               var[var == values] <- NA
            }
         }
      }
   }

   if (inplace)
   {
      response <- var
   } else {
      response <- outliers
   }


   return(response)

}





#' Coerce a column into a binary column
#'
#' @param col column to change into a binary column
#' @param verbose : TRUE to display some information in the console
#'
#' @return
#' @export
#' @import stringr
tobinary <- function(col,
                     verbose = TRUE)
{

   levels_col <- levels(as.factor(col)) #get the levels of col (that should be 0 or 1)

   if (length(levels_col) != 2)
      stop("the column provided should be a factor with 2 levels")

   if (FALSE %in% (levels_col %in% c(0, 1))) {
      col <- as.character(col) # prevents errors if the column is a already a factor
      stringr::str_detect(levels_col, "non") -> non_position
      stringr::str_detect(levels_col, "no ") -> no_position
      stringr::str_detect(levels_col, "not ") -> not_position
      if (non_position || no_position || not_position)
      {
         #if there is a level with "no " or "non" or "not " in it, it will be the 0 level
         if (!is.na(match(TRUE, non_position)))
            match(TRUE, non_position) -> level_non
         if (!is.na(match(TRUE, no_position)))
            match(TRUE, no_position) -> level_non
         if (!is.na(match(TRUE, not_position)))
            match(TRUE, not_position) -> level_non
         col[col == levels_col[level_non]] <- 0
         col[col != 0] <- 1
      } else
      {
         #else it will be the first level provided that will be 0
         col[col == levels_col[1]] <- 0
         col[col == levels_col[2]] <- 1
      }
      if (verbose)
         cat(
            'column has been changed to 0/1 factor with ',
            levels_col[1],
            ' = 0 and ',
            levels_col[2],
            ' = 1'
         )
   }else {}
   col <- as.factor(col)
   return(col)
}



#' Looking for complete NA separation
#'
#' @param DF dataframe
#' @param verbose logical : TRUE to display some information in the console
#'
#' @return
#' @export
#'
#' @examples
NA_separation <- function(DF, verbose=T)
{
   DF <- as.data.frame(DF)
   variables = colnames(DF)
   i_p = 0
   for (variable1 in variables) {
      #select variables with more than n/factor NAs
         for (variable2 in variables[variables != variable1]) {
            progressbar(i = i_p,total = length(variables) * (length(variables) - 1),
                        variable = variable1)
            i_p = i_p + 1
            DFtest <- DF[, c(variable1, variable2)]
            nb_NA <- sum(is.na(DFtest[, 2]))
            if (length(levels(as.factor(DF[, 1]))) == 2 &
                length(levels(as.factor(DF[, 2]))) == 2) {
               if (nb_NA > (nrow(DF)/length(levels(as.factor(DF[, 2])))))
                  {
                  table(DFtest[, 1], DFtest[, 2], useNA = "always") -> table
                  addmargins(table) -> table
                  for (i in 1:2){
                     if (table[i, 3] == table[i, 4]){
                        if (table[i, 3] == table[4, 3]){
                           cat("complete NA separation")
                        } else{
                           cat("uncomplete NA separation")
                        }
                     }
                  }
               }
            }
         }
      }
   }



#' Removing of all NAs and change the dataframe if too many rows are deleted
#'
#' @param DF : the dataframe
#' @param y : the variable to explain
#' @param EPV : the event-per-variable threeshold.
#' @param min_explicatives
#' @param floor_pval
#' @param verbose
#' @param method_NA
#' @param keep : the variable(s) to keep whathever are its significance and number of NA
#' @param imputed_variables  : variables to be imputed if imputed method is selected in 'method_NA'.
#'
#' @return
#' @export
#'
#' @examples
NA_rm_for_glm <- function(DF,
            y = colnames(DF)[1],
            EPV = 0,
            min_explicatives = 0.1*(ncol(DF)),
            floor_pval = 1,
            verbose = TRUE,
            method_NA = c("lessNA", "significance"),
            keep = FALSE,
            kNN = NULL,
            rf = NULL,
            median = NULL,
            mean=NULL,
            NA_as_level = NULL)
{
   DF <- as.data.frame(DF)
   explicatives1 <- colnames(DF)[colnames(DF) != y]
   DF_complete <- DF[complete.cases(DF),]
   explicatives <- colnames(DF)[colnames(DF) != y]

   if (!is.null(kNN))
   {
      DF_complete <- suppressWarnings(VIM::kNN(DF,variable = kNN))
   }
   if (!is.null(rf))
   {
      DF_complete <- suppressWarnings(missForest::missForest(DF))
   }
   if (!is.null(median))
   {
      DF -> DF_complete
      for (col in median)
      {
         DF_complete[,col][is.na(DF_complete[,col])] <- stats::median(DF[,col],na.rm = TRUE)
      }
   }
   if (!is.null(mean))
   {
      DF -> DF_complete
      for (col in mean)
      {
         DF_complete[,col][is.na(DF_complete[,col])] <- stats::mean(DF[,col],na.rm = TRUE)
      }
   }

   if (!is.null(NA_as_level))
   {
      DF -> DF_complete
      for (col in NA_as_level)
      {
         DF_complete[,col][is.na(DF_complete[,col])] <- "NC"
      }
   }

   if (!is.null(method_NA))
   {

      if (verbose)
      {
         cat("\n\n We delete all rows which are not complete :\n")
      }

      #Calculating EPV
      events <- min(table(DF[,y]))
      variables <- length(DF_complete)
      actualEPV <- events/variables

      if (actualEPV < EPV)
      {

         nb_NA <- apply(DF[, explicatives], 2, function(x) sum(is.na(x))) #nb NA by columns
         if (!is.logical(keep)) nb_NA <- nb_NA[!(keep %in% names(nb_NA))]
         nb_NA <- nb_NA[order(-nb_NA)]
         i = 1


         if ("significance" %in% method_NA)
         {
            for (var in names(nb_NA))
            {
               DF_uni <- DF[, c(y, var)]
               DF_uni <- DF_uni[complete.cases(DF_uni),]
               if (nrow(DF_uni) < 5 || length(levels(as.factor(DF_uni[,2]))) < 2 || 0 %in% table(DF_uni[ ,2]) )
               {
                  explicatives <- explicatives[explicatives != var]
               }else
               {
                  model <- stats::glm(formula = DF_uni[,y]~DF_uni[,var],family = "binomial")
                  p_valanova <- anova(model,formula(DF_uni[,y]~1),test = "Chisq")$Pr[2]
                  if (p_valanova > floor_pval)
                  {#if all pvals are > 0.5
                     explicatives <- explicatives[explicatives != var]
                     if (verbose)
                        cat("\n\n\n",var," is non significant with overall population (",floor_pval," threeshold) and is deleted")
                  }
               }
            }
         }

         DF <- DF[,c(y,explicatives)]
         DF_complete <- DF[complete.cases(DF),]




         if ("lessNA" %in% method_NA & sum(is.na(DF)) > 0 )
         {
            while (nrow(DF_complete) < EPV * length(DF_complete) & (length(explicatives) > min_explicatives) & i < length(nb_NA))
            {
               nb_NA <- apply(DF[, explicatives], 2, function(x) sum(is.na(x))) #nb NA by columns
               nb_NA <- nb_NA[!(keep %in% names(nb_NA))]
               nb_NA <- nb_NA[order(-nb_NA)]
               nb_NA <- nb_NA[nb_NA > 0]
               explicatives <- explicatives[explicatives != names(nb_NA[1])]
               progressbar(i = i,variable = names(nb_NA[1]),text = "Deleting columns... ",range = 5)
               DF <- DF[ ,c(y, explicatives)]
               DF_complete <- DF[complete.cases(DF), ]
               i <- i + 1
            }
         }
      }
   }


   DF_complete -> DF
   deleted_columns <-  explicatives1[!(explicatives1 %in% explicatives)]
   if (verbose & length(deleted_columns) != 0) cat("\n\n\nDeleted columns are :", deleted_columns)
   return(try(as.data.frame(DF_complete)))
}




#' Checking factors
#'
#' @param vars
#' @param DF dataframe : the dataframe to clean
#' @param verbose (optional) logical : TRUE will display some informations in the console
#' @return
#' @export
#'
#' @examples
checkforfactor <-
   function(DF,
            vars = colnames(DF),
            verbose = TRUE)
{
   DF <- as.data.frame(DF)
   for (var_i in length(vars):1) {
      var <- vars[var_i]
      if (is.numeric(DF[,var])) {
         as.factor(DF[,var]) -> DF[,var]
         if (length(levels(DF[, var])) < 2)
         {#removes factor with only one level
            if (verbose) message("\n", var, " is constant and is deleted")
            vars <- vars[-var_i]
            DF[,var] <- NULL
         } else {
            rep <- "N"
            if (length(levels(DF[, var])) < 7 & length(levels(DF[, var])) > 1) {
               message("\n", var, " is a numeric variable with only",length(levels(DF[, var]))," different values and could be considered as a factor")
              rep <- "O"
               if (rep != 'O') as.numeric(DF[,var]) -> DF[,var]
            }else
            {
               as.numeric(DF[,var]) -> DF[,var]
            }
         }
      }else {#convert each non numeric into a factor and check whether they have one level or not
         as.factor(DF[,var]) -> DF[,var]
         if (length(levels(DF[, var])) < 2)
         {
            #removes factor with only one level
            if (verbose)
               message("\n", var, " has only one level and is deleted")
            vars <- vars[-var_i]
            DF[,var] <- NULL
         }
      }
   }
   return(DF)
}


plain <- function(vec){
  if (is.character(vec)) {
    accents <- c("[\\u00e9\\u00e8\\u00ea\\u00eb]"="e",
                 "[\\u00e0\\u00e2]"="a",
                 "[\\u00ef]"="i",
                 "[\\u00f9\\u00fa\\u00fc]"="u",
                 "[\\u00f4\\u00f6]"="o",
                 "[\\u00f1]"="n",
                 "[\\u00e6]"="ae")
    vec <- stringr::str_to_lower(vec)
    vec <- stringr::str_replace_all(vec,accents)
  }
  return(vec)
}


#' Data formating
#'
#' @param DF : the dataframe to format
#' @param type (optional) : 'plain' for no accent nor capital letter, 'no-plural' for get rid of plural differences between factors
#'
#' @return the dataframe with no accent nor capital letters
#' @export
#'
#' @examples
format_data <- function(DF, type=c("plain")) {
  
   as.data.frame(DF) -> DF
   colnames(DF) -> colnamesDF
   
   
   if ("plain" %in% type){
     apply(DF,2,plain) -> DF
     as.data.frame(DF) -> DF
   }
   

      for (i in 1:length(DF)) {

         if ("no-plural" %in% type & is.character(DF[, i])) {
            string_levels <- levels(as.factor(DF[, i]))
            for (level in string_levels){
               words1 <- stringr::str_split(level," ")[[1]]
               for (level2 in string_levels[string_levels != level]){
                  real_diff <- FALSE
                  words2 <- stringr::str_split(level2," ")[[1]]
                  if (length(words1) == length(words2)){
                     for (l in seq(words1)) {
                        if (!(words1[l] == words2[l] || words1[l] == paste0(words2[l],"s"))) {
                           real_diff <- TRUE
                        }
                     }
                     if (! real_diff) {
                        DF[,i][DF[,i] == level] <- level2
                        break
                     }
                  }
               }
            }
         }

      if ("no-fem" %in% type & is.character(DF[, i])) {
            string_levels <- levels(as.factor(DF[, i]))
            for (level in string_levels) {
               words1 <- stringr::str_split(level," ")[[1]]
               for (level2 in string_levels[string_levels != level]){
                  real_diff <- FALSE
                  words2 <- stringr::str_split(level2," ")[[1]]
                  if (length(words1) == length(words2)){
                     for (l in seq(words1)){
                        if (!(words1[l] == words2[l] || words1[l] == paste0(words2[l],"e"))){
                           real_diff <- TRUE
                        }
                     }
                     if (! real_diff) {
                        DF[,i][DF[,i] == level] <- level2
                        break
                     }
                  }
               }
            }
      }

         if ("no-femplu" %in% type & is.character(DF[, i])){
            string_levels <- levels(as.factor(DF[, i]))
            for (level in string_levels) {
               words1 <- stringr::str_split(level," ")[[1]]
               for (level2 in string_levels[string_levels != level]) {
                  real_diff <- FALSE
                  words2 <- stringr::str_split(level2," ")[[1]]
                  if (length(words1) == length(words2)) {
                     for (l in seq(words1)) {
                        if (!(words1[l] == words2[l] || words1[l] == paste0(words2[l],"es"))) {
                           real_diff <- TRUE
                        }
                     }
                     if (! real_diff) {
                        DF[,i][DF[,i] == level] <- level2
                        break
                     }
                  }
               }
            }
         }
        
        if (is.character(DF[, i])){
          DF[,i] <- stringr::str_squish(DF[,i])
          DF[,i] <- stringr::str_trim(DF[,i])
        }
      }


      colnames(DF) <- colnamesDF

   return(DF)
}






######################################################
#########      DATA PREP COMPLETE            #########
######################################################


#' Preparing datas for logistic regression
#'
#' @param DF dataframe : the dataframe to clean
#' @param y (optional) character : the variable to explain
#' @param verbose (optional) logical : TRUE will display some informations in the console
#' @param keep (optional) vector : variables part of DF's columns that should not be deleted
#'   in the data cleaning process
#'
#' @return \code{data_prep_complete} returns a dataframe ready for a logistic regression
#' @export
#'
#' @examples
data_prep_complete <- function(DF,
                               y=colnames(DF)[1],
                               explicatives_matrix,
                               verbose = TRUE,
                               keep = FALSE)
{
   DF <- as.data.frame(DF)
   DF1 <- DF

   # Charactere / num preparation
   DF <- format_data(DF)
   explicatives_matrix[, 6] <- plain(explicatives_matrix[, 6])
   for (expl in explicatives_matrix$explicatives){
     if (!as.logical(explicatives_matrix[expl,5])) 
       DF[,explicatives_matrix[expl,1]] <- as.numeric(DF[,explicatives_matrix[expl,1]])
   }

   # Data prep of y
   DF[, y] <- tobinary(DF[, y])

   # get rid of NAs
   DF <- NA_rm_for_glm(DF,y,keep)
   DF <- as.data.frame(DF)

   # Clean constant variables
   #DF <- checkforfactor(DF)
   
   for (exp in explicatives_matrix$explicatives){
     exp <- explicatives_matrix[exp,]
     if (as.logical(exp[5])){
             tryCatch(relevel(as.factor(as.character(DF[,rownames(exp)])),
                              as.character(exp[6])) -> DF[,rownames(exp)],
                error = function(e) {
                  print(as.character(exp[6]))
                  print(DF[,rownames(exp)])
                  stop(paste0("Ho no ! you tried to assign <", as.character(exp[6]),
                              "> for the variable  <",rownames(exp),
                              "> but it doesn't exists"))
                }) -> var
     }
   }
   
   # Finally
   explicatives <- colnames(DF)[colnames(DF) != y]
   DF <- as.data.frame(DF)

   if (verbose) {
      cat("\n",(nrow(DF1) - nrow(DF)),
          " rows deleted (",
          round(100*(nrow(DF1) - nrow(DF))/(nrow(DF1)),0),"%)",
          "...........",nrow(DF),"rows remaining")
      cat("\n\nData cleaning is over.
          \n\nExplicatives variables remaining are :\n",
          explicatives,
          "\n\nIt remains ",length(explicatives),
          "variables and ",nrow(DF),"observations")
   }

   return(DF)
}




