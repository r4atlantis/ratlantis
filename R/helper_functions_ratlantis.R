#helper functions for ratlantis

#' "Not in" function
#'
#' This function allows you to check if an element is in a larger group
#' @param x
#' @param table
#' @export
#define not in function
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0
