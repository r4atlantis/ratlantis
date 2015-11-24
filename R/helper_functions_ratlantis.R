#helper functions for ratlantis

#' "%!in%" function
#'
#' This function allows you to check if an element is not in a larger group
#' @param x object you are trying to check for
#' @param table group you are checking against
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0


#' meannona function
#'
#' This function allows you to calculate the mean value of a set while excluuding
#' NA's
#' @param ... list for which you seeking a mean
#' @export
meannona=function(...){
  mean(..., na.rm=T)
}

#' maxnona function
#'
#' This function allows you to calculate the max value of a set while excluuding
#' NA's
#' @param ... list for which you seeking a maximum
#' @export
maxnona=function(...){
  max(..., na.rm=T)
}

#' minnona function
#'
#' This function allows you to calculate the mean value of a set while excluuding
#' NA's
#' @param ... list for which you seeking a minimum
#' @export
minnona=function(...){
  min(..., na.rm=T)
}


