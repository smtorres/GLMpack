#' Data on the Scottish national parliament vote
#'
#' Data for the Scotland example used in chapters 4, 5, and 6
#'
#' @format A data frame with 32 rows and 7 variables:
#' \describe{
#'   \item{PerYesTax}{Percentage of population who granting Scottish parliament taxation powers}
#'   \item{CouncilTax}{Council tax collected}
#'   \item{PerClaimantFemale}{Female percentage of total claims for unemployment benefits as of January 1998}
#'   \item{StdMortalityRatio}{Standardized mortality rate}
#'   \item{Active}{Percentage of economically active individuals relative to the population of working age}
#'   \item{GDP}{GDP per council}
#'   \item{Percentage5to15}{Percentage of children aged 5 to 15}
#'   ...
#' }
#'
#' @usage data(scotvote)
#' @examples
#' data(scotvote)
#' attach(scotvote)
#'
#' ## Table 4.3
#' scotvote
#'
#' ## Table 5.3
#' scottish.vote.glm <- glm((PerYesTax) ~ CouncilTax*PerClaimantFemale+
#'                      StdMortalityRatio+Active+GDP+Percentage5to15,
#'                      family=Gamma,data=scotvote)
#' vote.sum <- summary(scottish.vote.glm)
#' round(cbind(
#'   vote.sum$coefficients[,1], vote.sum$coefficients[,2],
#'   confint(scottish.vote.glm)),4)
#' vote.sum
#'
#' ## Table 6.3
#' anova(scottish.vote.glm,test="F")
"scotvote"
