#' Summarize regression output from multinomial generalized linear models.
#'
#' An alternative to the summary() function.
#'
#' @param in.object The regression output from multinom().
#' @param alpha A parameter defaulted to 0.05.
#' @return The output is a list.
#' @examples
#' data(primary)
#' attach(primary)
#' library(nnet)
#' primary.out <- multinom(PRIMVOTE ~ AGE + GENDER + EDUCATION + REGION +
#'                          RELIGIOSITY + IDEOLOGY + RWA + TRUMPWIN, data=primary)
#' glm.summary.multinom(primary.out)
#'
#' @export
glm.summary.multinom <- function (in.object, alpha = 0.05)
{
  numcats <- length(in.object$lev)-1
  coefs <- round(summary(in.object)$coefficients,3)
  std.errors <- round(summary(in.object)$standard.errors,3)
  lo <- round(coefs - qnorm(1-alpha/2) * std.errors,3)
  hi <- round(coefs + qnorm(1-alpha/2) * std.errors,3)
  out.ls <- lapply(1:numcats, function(x) cbind(coefs[x,], std.errors[x,], lo[x,], hi[x,]))
  out.ls <- lapply(out.ls, function(x){colnames(x) = c("Coefficient","Std. Error",
                                                       paste(1-alpha,"CI Lower"),
                                                       paste(1-alpha,"CI Upper"));return(x)})
  names(out.ls) <- in.object$lev[-1]
  return(out.ls)
}
