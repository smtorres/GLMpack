#' Compute variance-covariance matrix.
#'
#' Calculate the (unscaled) variance-covariance matrix from a generalized linear model regression output. Used in `GLMpack` within the function `glm.summary()`.
#'
#' @param obj The regression output from glm().
#' @return The output is a matrix.
#' @examples
#' data(campaign)
#' attach(campaign)
#' cmpgn.out <- glm(TOTCONTR ~ CANDGENDER + PARTY + INCUMCHALL + HISPPCT,
#'               family=Gamma(link = 'log'), data=campaign)
#' glm.vc(cmpgn.out)
#'
#' @export
glm.vc <- function(obj){
  summary(obj)$dispersion * summary(obj)$cov.unscaled
}
