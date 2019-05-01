#' Data on the characteristics of peace agreement outcomes
#'
#' Data for the Peace example used in chapter 7
#'
#' @format A data frame with 216 rows and 9 variables:
#' \describe{
#'   \item{OUTISS}{Ordinal variable indicating the scale of outstanding issues that were not resolved during the peace negotiations with 30 percent zero values}
#'   \item{PP}{Binary variable indicating whether a rebel force is allowed to transform into a legal political party}
#'   \item{INTCIV}{Binary variable indicating whether members of the rebel group are to be integrated into the civil service}
#'   \item{AMN}{Binary variable indicating whether there is an amnesty provision in the agreement}
#'   \item{PRIS}{Binary variable indicating whether prisoners are released}
#'   \item{FED}{Binary variable indicating whether a federal state solution is included}
#'   \item{COMIMP}{Binary variable indicating whether the agreement establishes a commission or committee to oversee implementation}
#'   \item{REAFFIRM}{Binary variable indicating whether the agreement reaffirms earlier peace agreements}
#'   \item{PKO}{Binary variable indicating whether or not the peace agreement included the deployment of peacekeeping forces}
#'   ...
#' }
#'
#' @usage data(peace)
#' @examples
#' data(peace)
#' attach(peace)
#' require(pscl)
#'
#' ## Table 7.6
#' M3 <- zeroinfl(OUTISS ~ PP + INTCIV + AMN + PRIS + FED + COMIMP + REAFFIRM | PKO, data=peace)
#' summary(M3)
#' out.table.count <- cbind(summary(M3)$coef$count[,1:2],
#'     summary(M3)$coef$count[,1] - 1.96*summary(M3)$coef$count[,2],
#'     summary(M3)$coef$count[,1] + 1.96*summary(M3)$coef$count[,2])
#' out.table.zero <- cbind(summary(M3)$coef$zero[,1:2],
#'     summary(M3)$coef$zero[,1] - 1.96*summary(M3)$coef$zero[,2],
#'     summary(M3)$coef$zero[,1] + 1.96*summary(M3)$coef$zero[,2])
#' out.table.count
#' out.table.zero
"peace"
