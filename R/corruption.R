#' Data on censored corruption scale
#'
#' Data for the corruption example used in chapter 7
#'
#' @format A data frame with 83 rows and 7 variables:
#' \describe{
#'   \item{ticpi85b}{Country-level compilation of effects into a 0 to 10 scale of increasing government corruption with an adjustment that modifies this range slightly}
#'   \item{MSO}{Binary variable indicating whether the government owns a majority of key industries}
#'   \item{LOG.PC.GDP}{Log of the average per capita GDP from 1975 to 1983}
#'   \item{DEMOCRACY}{Polity IV democracy score from 1975 to 1983}
#'   \item{GOVGDT}{Average government spending as a percentage of GDP from 1980 to 1983}
#'   \item{ECONFREE}{Index of the ability of capitalists to invest and move money}
#'   \item{FEDERAL}{Binary variable indicating whether the government has a federal system during this period}
#'   ...
#' }
#'
#' @usage data(corruption)
#' @examples
#' data(corruption)
#' attach(corruption)
#' library(censReg)
#'
#' ## Table 7.5
#' corruption.tobit.out <- censReg( ticpi85b ~ MSO + LOG.PC.GDP + DEMOCRACY + GOVGDT +
#'     ECONFREE*FEDERAL,
#'     left = 2.6, right = 10.70, data=corruption, start = NULL, nGHQ = 8, logLikOnly = FALSE)
#' summary(corruption.tobit.out)
#' summary(corruption.tobit.out)$nObs
#' Coefs <- summary(corruption.tobit.out)$estimate[,1]
#' SEs <- summary(corruption.tobit.out)$estimate[,2]
#' CI.low <- Coefs - SEs*1.96
#' CI.hi <- Coefs + SEs*1.96
#' ( round(out.table <- cbind( Coefs, SEs, CI.low, CI.hi),4) )
"corruption"
