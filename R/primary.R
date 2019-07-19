#' Data on the 2016 Republican Presidentical Primaries
#'
#' Data for the primary example used in chapters 4 and 5
#'
#' @format A data frame with 706 rows and 9 variables:
#' \describe{
#'   \item{PRIMVOTE}{Vote intention}
#'   \item{AGE}{Age}
#'   \item{GENDER}{Gender}
#'   \item{EDUCATION}{Education}
#'   \item{REGION}{Region of the country in which the respondent lives}
#'   \item{RELIGIOSITY}{Religiosity}
#'   \item{IDEOLOGY}{Ideology}
#'   \item{RWA}{Right Wing Authoritarianism scale}
#'   \item{TRUMPWIN}{Perceptions of whether Trump could win}
#'   ...
#' }
#'
#' @usage data(primary)
#' @examples
#' opar = par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
#' data(primary)
#' attach(primary)
#' library(nnet)
#' library(pBrackets)
#' library(effects)
#'
#' ## Model
#' primary.out <- multinom(PRIMVOTE ~ AGE + GENDER + EDUCATION + REGION +
#'                         RELIGIOSITY + IDEOLOGY + RWA + TRUMPWIN, data=primary)
#' summ.primary.out <- glm.summary.multinom(primary.out)
#'
#' ## Figure 4.2
#' par(mfrow=c(3,3), mar=c(2.5,2,2,1))
#' # Plot 1: Electoral preference
#' countsPV0 <- barplot(table(primary$PRIMVOTE), main="Electoral Preference",
#'         xlab="Candidates", mgp=c(1.1, 0.2, 1))
#' text(countsPV0[,1], rep(10,4), as.numeric(table(primary$PRIMVOTE)), cex=1.5)
#' # Plot 2: Age
#' countsAGE <- barplot(table(primary$AGE), main="Age",
#'                      xlab="Age categories", mgp=c(1.1, 0.2, 0))
#' text(countsAGE[,1], rep(10,4), as.numeric(table(primary$AGE)), cex=1.5)
#' # Plot 3: Gender
#' countsGENDER <- barplot(table(primary$GENDER), main="Gender",
#'                      xlab="Gender categories", mgp=c(1.1, 0.2, 0), ylim=c(0,500))
#' text(countsGENDER[,1], rep(25,2), as.numeric(table(primary$GENDER)), cex=1.5)
#' # Plot 4: Education
#' countsEDUC <- barplot(table(primary$EDUCATION), main="Education",
#'                         xlab="Schooling level", mgp=c(1.1, 0.2, 0))
#' text(countsEDUC[,1], rep(10,4), as.numeric(table(primary$EDUCATION)), cex=1.5)
#' # Plot 5: Region
#' countsREG <- barplot(table(primary$REGION), main="Region",
#'                       xlab="Region", mgp=c(1.1, 0.2, 0))
#' text(countsREG[,1], rep(10,4), as.numeric(table(primary$REGION)), cex=1.5)
# # Plot 6: Religion
#' hist(primary$RELIGIOSITY,xlab="Religiosity Score",ylab="",
#'      xlim=c(-1.5,2), ylim=c(0, 225), main="Religiosity",
#'      col = "gray70", mgp=c(1.1, 0.2, 0))
#' # Plot 7: Ideology
#' hist(primary$IDEOLOGY,xlab="Ideology Score",ylab="",
#'      xlim=c(-2,1.5), ylim=c(0, 150), main="Ideology",
#'      col = "gray70", mgp=c(1.1, 0.2, 0))
#' # Plot 8: Right Wing Authoritarianism
#' hist(primary$RWA,xlab="Right Wing Authoritarianism Score",ylab="",
#'      xlim=c(-2.5,2), ylim=c(0, 200), main="Authoritarianism",
#'      col = "gray70", mgp=c(1.1, 0.2, 0))
#' colnames(primary)
#' # Plot 9: Could Trump Win?
#' countsWIN <- barplot(table(primary$TRUMPWIN), main="Trump's winnability",
#'                      xlab="Perceptions of whether Trump could win",
#'                      mgp=c(1.1, 0.2, 0), ylim=c(0,550))
#' text(countsWIN[,1], rep(30,3), as.numeric(table(primary$TRUMPWIN)), cex=1.5)
#' par(opar)
#'
#' ## Figure 5.3
#' layout(matrix(1:2, ncol = 1), heights = c(0.9,0.1))
#' par(mar=c(3,4,0,1),oma=c(1,1,1,1))
#' plot(summ.primary.out[[1]][,1], type = "n", xaxt="n", yaxt="n",
#'      xlim=c(-10,3), ylim=c(0,60), ylab="", xlab="")
#' abline(v=-5, h=c(12,16,28,40,44,48,52), lwd=2)
#' abline(h=c(4,8,20,24,32,36,56), lty=3, col="gray60")
#' text(rep(-7.5,15), seq(2,58,4),
#'      labels = c('30-44', '45-59', '60+',
#'                 'Male',
#'                 'High School','Some College','Bachelor\'s degree or higher',
#'                 'Northeast', 'South', 'West',
#'                 'Religiosity',
#'                 'Ideology',
#'                 'Authoritarianism',
#'                 'Yes', 'Don\'t know'))
#' segments(summ.primary.out[[1]][-1,3], seq(1,57,4), summ.primary.out[[1]][-1,4],
#'          seq(1,57,4), col="gray30", lwd=2)
#' points(summ.primary.out[[1]][-1,1], seq(1,57,4), pch=21, cex=1.4, bg="black")
#' text(summ.primary.out[[1]][-1,1], seq(1,57,4), labels = "C", cex = 0.7, col="white")
#' segments(summ.primary.out[[2]][-1,3], seq(3,59,4), summ.primary.out[[2]][-1,4],
#'          seq(3,59,4), col="gray30", lwd=2)
#' points(summ.primary.out[[2]][-1,1], seq(3,59,4), pch=21, cex=1.4, bg="white")
#' text(summ.primary.out[[2]][-1,1], seq(3,59,4), labels = "K", cex = 0.7, col="black")
#' abline(v=0, lty=2)
#' axis(1, tck=0.01, at = seq(-5,5,0.5),cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0,
#'      lwd.ticks = 1)
#' axis(2, tck=0.02, at = c(6,14,22,34,42,46,50,56), labels=c('AGE', 'GENDER',
#'      'EDUCATION', 'REGION', 'RELIGIOSITY', 'IDEOLOGY', 'RWA', 'TRUMPWIN'),
#'      cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 0, las=1)
#' title(xlab = "Coefficient",
#'       line = 1.7, cex.lab=1.3)
#' par(mar=c(0,4,0,0))
#' plot(0,0, type="n", axes = FALSE, xaxt="n", yaxt="n", xlab="", ylab = "")
#' legend("center", c("Cruz", "Kasich"), ncol=2, pch=c(21,21), pt.bg=c("black", "white"),
#'        pt.cex=rep(1.4,2), bty = "n")
#' par(opar)
#'
#' ## Figure 5.4
#' mygray = rgb(153, 153, 153, alpha = 200, maxColorValue = 255)
#' mygray2 = rgb(179, 179, 179, alpha = 150, maxColorValue = 255)
#' mygray3 = rgb(204, 204, 204, alpha = 150, maxColorValue = 255)
#' preds_win <- Effect("TRUMPWIN", primary.out)
#' preds_ideol <- Effect("IDEOLOGY", primary.out, xlevels=list(IDEOLOGY=100))
#' par(mfrow=c(1,2), mar=c(4,3,3,0),oma=c(1,1,1,1))
#' plot(1:3, preds_win$prob[,1], type="n",xlab="",ylab="",  yaxt="n", xaxt="n",
#'      xlim=c(0,4), ylim=c(0, 0.7))
#' segments(rep(1:3, 3), preds_win$lower.prob[,1:3], rep(1:3, 3), preds_win$upper.prob[,1:3],
#'          col=rep(c('black', 'black', 'gray60'), each=3), lty = rep(c(1,2,1), each=3))
#' points(rep(1:3,3), preds_win$prob[,1:3], pch=21, cex = 2,
#'        bg=rep(c("black", "white", "gray60"),each=3), col=rep(c("black", "black", "gray60"),each=3))
#' text(rep(1:3,3), preds_win$prob[,1:3], labels=rep(c("T", "C", "K"), each=3), cex = 0.8,
#'      bg=rep(c("black", "white", "gray60"),each=3), col=rep(c("white", "black", "black"),each=3))
#' axis(1, at=1:3, labels = c("No", "Yes", "DK"), tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0),
#'      lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = 'Perceptions of whether Trump could win the election',
#'       ylab="Probability of voting",
#'       line = 1.7, cex.lab=1)
#' title(line = 1, main="Winnability", font.main=3)
#' plot(preds_ideol$x$IDEOLOGY, preds_ideol$prob[,1], type="n",xlab="",ylab="",  yaxt="n", xaxt="n",
#'      xlim=c(-2,1.5), ylim=c(0, 0.7))
#' polygon(c(preds_ideol$x$IDEOLOGY, rev(preds_ideol$x$IDEOLOGY)),
#'         c(preds_ideol$lower.prob[,2], rev(preds_ideol$upper.prob[,2])), border = NA, col=mygray2)
#' polygon(c(preds_ideol$x$IDEOLOGY, rev(preds_ideol$x$IDEOLOGY)),
#'         c(preds_ideol$lower.prob[,1], rev(preds_ideol$upper.prob[,1])), border = NA, col=mygray)
#' polygon(c(preds_ideol$x$IDEOLOGY, rev(preds_ideol$x$IDEOLOGY)),
#'         c(preds_ideol$lower.prob[,3], rev(preds_ideol$upper.prob[,3])), border = NA, col=mygray3)
#' lines(preds_ideol$x$IDEOLOGY, preds_ideol$prob[,1], col="gray20", lwd=2)
#' lines(preds_ideol$x$IDEOLOGY, preds_ideol$prob[,2], col="gray40", lwd=2, lty=2)
#' lines(preds_ideol$x$IDEOLOGY, preds_ideol$prob[,3], col="black", lwd=2)
#' text(rep(1,3), c(min(preds_ideol$prob[,1]), min(preds_ideol$prob[,2]),
#'      max(preds_ideol$prob[,3]))+.05, labels = c('Trump', 'Cruz', 'Kasich'))
#' axis(1, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = 'Ideology scores',
#'       ylab="Probability of voting",
#'       line = 1.7, cex.lab=1)
#' title(line = 1, main="Ideology", font.main=3)
#' par(opar)
"primary"

