#' ifelse when used with Dates
#' 
#' @param cond condition
#' @param yes result when TrUE 
#' @param no result when FALSE
#' @return combinatino of yes and no depending on condition
#' 
#' @export
ifelse.Date <- function(cond, yes, no) {
  class.y <- class(yes)
  if (class.y == "factor") {
    levels.y = levels(yes)
  }
  X <- ifelse(cond,yes,no)
  if (class.y == "factor") {
    X = as.factor(X)
    levels(X) = levels.y
  } else {
    class(X) <- class.y
  }
  return(X)
}