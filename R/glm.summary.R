#' Summarize regression output from generalized linear models.
#'
#' An alternative to the summary() function.
#'
#' @param in.object The regression output from glm().
#' @param alpha A parameter defaulted to 0.05.
#' @return The output is a matrix.
#' @examples
#' data(campaign)
#' attach(campaign)
#' cmpgn.out <- glm(TOTCONTR ~ CANDGENDER + PARTY + INCUMCHALL + HISPPCT,
#'               family=Gamma(link = 'log'), data=campaign)
#' glm.summary(cmpgn.out)
#'
#' @export
glm.summary <- function (in.object, alpha = 0.05){
  lo <- in.object$coefficient - qnorm(1-alpha/2) * sqrt(diag(summary(in.object)$cov.unscaled))
  hi <- in.object$coefficient + qnorm(1-alpha/2) * sqrt(diag(summary(in.object)$cov.unscaled))
  out.mat <- round(cbind(in.object$coefficient, sqrt(diag(glm.vc(in.object))), lo, hi),5)
  dimnames(out.mat)[[2]] <- c("Coefficient","Std. Error",
                              paste(1-alpha,"CI Lower"),paste(1-alpha,"CI Upper"))
  out.mat
}
