#' Compute confidence intervals for predictions.
#'
#' Apply an exponential transformation to the confidence intervals and predictions from binomial and Poisson models.
#'
#' @param preds The predictions based on the additive linear component of the model.
#' @param ses The standard error(s) of the prediction.
#' @param alpha The desired confidence level.
#' @param df The desired degrees of freedom.
#' @return The output is a matrix.
#' @examples
#' data(campaign)
#' attach(campaign)
#' cmpgn.out <- glm(TOTCONTR ~ CANDGENDER + PARTY + INCUMCHALL + HISPPCT,
#'                  family=Gamma(link = 'log'), data=campaign)
#' newdat_gender <- data.frame(CANDGENDER = c('F','M'), PARTY= rep('Democrat',2),
#'                             INCUMCHALL=rep("C", 2), HISPPCT=rep(mean(campaign$HISPPCT),2))
#' preds_gender <- predict(cmpgn.out, newdata = newdat_gender, se.fit = TRUE)
#' glm.cis(preds_gender$fit, preds_gender$se.fit, 0.95,cmpgn.out$df.residual)
#'
#' @export
#' @import stats
glm.cis <- function(preds, ses, alpha, df){
  tval = qt((1-alpha)/2, df, lower.tail = F)
  raw_conf = cbind(preds-(tval*ses), preds+(tval*ses))
  trans_conf = t(apply(raw_conf, 1, exp))
  out <- cbind(preds, raw_conf, exp(preds), trans_conf)
  colnames(out) <- c('eta', 'low.eta', 'hi.eta', 'pred', 'low.pred', 'hi.pred')
  return(out)
}
