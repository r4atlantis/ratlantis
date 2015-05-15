#helper functions for ratlantis

#' "Not in" function
#'
#' This function allows you to check if an element is not in a larger group
#' @param x object you are trying to check for
#' @param table group you are checking against
#' @export
#define not in function
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0

