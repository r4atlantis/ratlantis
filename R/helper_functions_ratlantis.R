#helper functions for ratlantis

#' not_in function
#'
#' This function allows you to check if an element is not in a larger group
#' @name not_in
#' @aliases not_in
#' @param x object you are trying to check for
#' @param table group you are checking against
#' @export
#' @details Also allows uses of "%!in%"
#define not in function
not_in <- function(x,table) match(x,table, nomatch = 0) == 0
"%!in%" <- not_in

