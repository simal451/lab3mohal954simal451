#' Euclidean algorithm
#'
#' @param a An integer
#' @param b An integer
#' @return The greatest common divisor of \code{a} and \code{b}
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
euclidean <- function(a, b) {
  if(!is.numeric(a)) stop("a must be an integer or numeric scalar")
  if(!is.numeric(b)) stop("b must be an integer or numeric scalar")
  if(a %% 1 == 0 && b %% 1 == 0) { stop }
  while(b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
