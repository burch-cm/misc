#' ArgMax
#' @details Determines which given input maximizes a given function.
#' @param FUN a function over which to calculate the argmax value. This can be a named function, an anonymous function, or can use the formula `~ .x*2` form from library(purrr).
#' @param inputs a vector of values to pass to FUN
#' @section Future:
#' - allow for multiple input formats, and return type based on supplied format
#' - promote purrr to recommended cf required, and implement with base as a fallback
#' @return a vector of type double
#' @export
#'
#' @examples
#' arg_max(FUN = function(x) cos(x)**2, inputs = c(-2:2))
#' arg_max(~ .x ** 2, seq(-2, 2, by = 0.5))
arg_max <- function(FUN, inputs) {
    require(purrr)
    outs <- purrr::map_dbl(inputs, FUN)
    inputs[which.max(outs)]
}
