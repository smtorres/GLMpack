#' Data on inflation increase in Africa
#'
#' Data for the Africa example used in chapter 7
#'
#' @format A data frame with 47 rows and 7 variables:
#' \describe{
#'   \item{INFLATN}{Inflation rates}
#'   \item{DICTATOR}{Number of years of personal dictatorship that occurred from independence to 1989}
#'   \item{SIZE}{Area at the end of the period, in thousand square kilometers}
#'   \item{GROWTH}{Average annual gross national product (GNP) rate of growth in percent from 1965 to 1989}
#'   \item{CHURMED}{Number of church-operated hospitals and medical clinics as of 1973}
#'   \item{CONSTIT}{the constitutional structure when not a dictatorship in ascending centrality (0 = monarchy, 1 = presidential, 2 = presidential/parliamentary mix, 3 = parliamentary)}
#'   \item{REPRESS}{Violence and threats of violence by the government against opposition political activity from 1990 to 1994}
#'   ...
#' }
#'
#' @usage data(africa)
#' @examples
#' data(africa)
#' attach(africa)
#' library(lmtest)
#' library(plm)
#'
#' ## Table 7.4
#' y <- (INFLATN/100)[-16]
#' y[y > 1] <- 1
#' X <- cbind(DICTATOR,SIZE,GROWTH,CHURMED,CONSTIT,REPRESS)[-16,]
#' X[,4] <- log(X[,4]+.01)
#' africa.glm <- glm(y ~ X[,1:6], family=quasibinomial('logit'))
#' out.mat <- coeftest(africa.glm, vcov.=vcovHC(africa.glm, type="HC0"))
#' ( out.mat <- round(cbind(out.mat[,1:2], out.mat[,1] - 1.96*out.mat[,2],
#'                          out.mat[,2] + 1.96*out.mat[,2]),4) )
"africa"
