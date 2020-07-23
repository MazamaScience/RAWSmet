#' @export
#'
#' @title Hello
#'
#' @param name User to be greeted.
#' 
#' @description Returns a "Hello, <name>!" greeting.
#' 
#' @return Character string.

hello <- function(
  name = "Friend"
) {
  return(sprintf("Hello, %s!", name))
}
