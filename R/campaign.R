#' Data on campaign contributions in California in the 2014 House elections
#'
#' Data for the campaign contributions example used in chapter 6
#'
#' @format A data frame with 180 rows and 16 variables:
#' \describe{
#'   \item{DTRCT}{California district}
#'   \item{CANDID}{FEC ID}
#'   \item{CYCLE}{Election cycle}
#'   \item{NAME}{Name of the candidate}
#'   \item{INCUMCHALL}{Incumbency status}
#'   \item{CFSCORE}{CFscore}
#'   \item{CANDGENDER}{Gender of the candidate}
#'   \item{PARTY}{Party of the candidate}
#'   \item{TOTCONTR}{Contributions to the 2014 electoral campaigns in the 53 districts of California in the U.S. House of Representatives}
#'   \item{TOTPOP}{Total state population}
#'   \item{FEMALE}{Number of female citizens in the state}
#'   \item{WHITE}{Number of white citizens in the state}
#'   \item{HISP}{Number of Hispanic citizens in the state}
#'   \item{FEMALEPCT}{Percentage of female citizens in the state}
#'   \item{WHITEPCT}{Percentage of white citizens in the state}
#'   \item{HISPPCT}{Percentage of Hispanic citizens in the state}
#'   ...
#' }
#'
#' @usage data(campaign)
#' @examples
#' data(campaign)
#' attach(campaign)
#' library(pBrackets)
#'
#' ## Gamma model
#' cmpgn.out <- glm(TOTCONTR ~ CANDGENDER + PARTY + INCUMCHALL + HISPPCT,
#'              family=Gamma(link = 'log'), data=campaign)
#'
#' ## Linear model
#' cmpgn.out_lm <- lm(TOTCONTR ~ CANDGENDER + PARTY + INCUMCHALL + HISPPCT, data=campaign)
#'
#' ## Table 6.8
#' round(glm.summary(cmpgn.out),4)
#' cmpgn.out$null.deviance
#' cmpgn.out$df.null
#' cmpgn.out$deviance
#' cmpgn.out$df.residual
#' logLik(cmpgn.out)
#' cmpgn.out$aic
#'
#' ## Table 6.9
#' summary(cmpgn.out_lm)
#' confint(cmpgn.out_lm)
#'
#' ## Figure 6.4
#' opar = par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
#' par(mar=c(4,3,3,0),oma=c(1,1,1,1))
#' hist(campaign$TOTCONTR,xlab="",ylab="", yaxt="n", xaxt="n",
#'      xlim=c(0,9000), ylim=c(0, 130), main="",
#'      col = "gray40")
#' axis(1, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=1, lwd.ticks = 1, las=2)
#' title(xlab = 'Total campaign contributions (thousands of dollars)',
#'       ylab= "Frequency",
#'       line = 1.7, cex.lab=1)
#' title(line = 1, main="Distribution of campaign contributions", font.main=1)
#' par(opar)
#'
#' ## Figure 6.5
#' campaign.mu <- predict(cmpgn.out_lm)
#' campaign.y <- campaign$TOTCONTR
#' par(mfrow=c(1,3), mar=c(3,3,2,1),oma=c(1,1,1,1))
#' plot(campaign.mu,campaign.y,xlab="",ylab="", yaxt='n', xaxt="n", pch="+")
#' axis(1, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = "Fitted values", ylab="Observed values",
#'       line = 1.7, cex.lab=1.3)
#' title(main="Model Fit Plot",
#'       line = 1, cex.main=1.7, font.main=1)
#' abline(lm(campaign.y~campaign.mu)$coefficients, lwd=2)
#' plot(fitted(cmpgn.out_lm),resid(cmpgn.out_lm,type="pearson"),xlab="",ylab="",
#'      yaxt='n', xaxt="n", pch="+")
#' axis(1, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = "Fitted values", ylab="Pearson Residuals",
#'       line = 1.7, cex.lab=1.3)
#' title(main="Residual Dependence Plot",
#'       line = 1, cex.main=1.7, font.main=1)
#' abline(0,0, lwd=2)
#' plot(cmpgn.out_lm,which=2, pch="+",
#'      sub.caption = "", caption = "", mgp=c(1.5, 0.3, 0),
#'      tck=0.02, cex.axis=0.9, cex.lab=1.3, lty=1)
#' title(main="Normal-Quantile Plot",
#'       line = 1, cex.main=1.7, font.main=1)
#'par(opar)
#'
#' ## Figure 6.6
#' mygray = rgb(153, 153, 153, alpha = 200, maxColorValue = 255)
#' newdat_gender <- data.frame(CANDGENDER = c('F','M'), PARTY= rep('Democrat',2),
#'                             INCUMCHALL=rep("C", 2), HISPPCT=rep(mean(campaign$HISPPCT),2))
#' newdat_party <- data.frame(CANDGENDER = rep('M', 3), PARTY= c('Democrat','Republican',
#'                            'Independent'), INCUMCHALL=rep("C", 3),
#'                            HISPPCT=rep(mean(campaign$HISPPCT),3))
#' newdat_incumchall <- data.frame(CANDGENDER = rep('M', 3), PARTY= rep('Democrat',3),
#'                                 INCUMCHALL=c('C', 'I', 'O'),
#'                                 HISPPCT=rep(mean(campaign$HISPPCT),3))
#' newdat_hisiq <- data.frame(CANDGENDER = rep('M', 2), PARTY= rep('Democrat',2),
#'                            INCUMCHALL=rep("C", 2),
#'                            HISPPCT=as.numeric(summary(campaign$HISPPCT)[c(2,5)]))
#' newdat_hispf <- data.frame(CANDGENDER = rep('M', 200), PARTY= rep('Democrat',200),
#'                            INCUMCHALL=rep("C", 200), HISPPCT=seq(.1, .9, length.out = 200))
#' preds_gender <- predict(cmpgn.out, newdata = newdat_gender, se.fit = TRUE)
#' preds_party <- predict(cmpgn.out, newdata = newdat_party, se.fit = TRUE)
#' preds_incumchall <- predict(cmpgn.out, newdata = newdat_incumchall, se.fit = TRUE)
#' preds_hispiq <- predict(cmpgn.out, newdata = newdat_hisiq, se.fit = TRUE)
#' preds_hispf <- predict(cmpgn.out, newdata = newdat_hispf, se.fit = TRUE)
#' cis_gender <- round(glm.cis(preds_gender$fit, preds_gender$se.fit, 0.95,cmpgn.out$df.residual),4)
#' cis_party <- round(glm.cis(preds_party$fit, preds_party$se.fit, 0.95,cmpgn.out$df.residual),4)
#' cis_incumchall <- round(glm.cis(preds_incumchall$fit, preds_incumchall$se.fit, 0.95,
#'                                 cmpgn.out$df.residual),4)
#' cis_hispiq <- round(glm.cis(preds_hispiq$fit, preds_hispiq$se.fit, 0.95,cmpgn.out$df.residual),4)
#' cis_hispf <- round(glm.cis(preds_hispf$fit, preds_hispf$se.fit, 0.95,cmpgn.out$df.residual),4)
#' iqrange = cbind(summary(campaign$HISPPCT)[c(2,5)],cis_hispiq[2,4] - cis_hispf[1,4])
#' par(mfrow=c(2,2), mar=c(4,3,3,0),oma=c(1,1,1,1))
#' plot(1:2, cis_gender[,4], type="n",xlab="",ylab="",  yaxt="n", xaxt="n",
#'      xlim=c(0,3), ylim=c(100, 700))
#' segments(1:2, cis_gender[,5], 1:2, cis_gender[,6], lwd=2, col="gray60")
#' points(1:2, cis_gender[,4], pch=16, cex=2.5)
#' text(1:2, cis_gender[,4], labels = c("F", "M"), col="white", cex=0.9)
#' segments(1.05, cis_gender[1,4], 1.95, cis_gender[2,4], lty=2)
#' brackets(1, cis_gender[1,4]+20, 2, cis_gender[1,4]+20, h = 45,  ticks = 0.5, lwd=2)
#' text(1.5, cis_gender[1,4]+100, bquote(hat(y)['F']-hat(y)['M'] ~ '='
#'      ~ .(cis_gender[2,4]-cis_gender[1,4])), cex=0.9)
#' axis(1, at=1:2, labels = c("Female", "Male"), tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0),
#'      lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = 'Gender of candidate',
#'       ylab="Total campaign contributions",
#'       line = 1.7, cex.lab=1)
#' title(line = 1, main="Gender of candidate", font.main=3)
#' plot(1:3, cis_party[,4], type="n",xlab="",ylab="",  yaxt="n", xaxt="n",
#'      xlim=c(0.5,3.5), ylim=c(0, 600))
#' segments(1:3, cis_party[,5], 1:3, cis_party[,6], lwd=2, col="gray60")
#' points(1:3, cis_party[,4], pch=15:17, cex=2.5)
#' text(1:3, cis_party[,4], labels = c("D", "R", "I"), col="white", cex=0.8)
#' segments(c(1.05,2.05), cis_party[1:2,4], c(1.95,2.95), cis_party[2:3,4], lty=2)
#' brackets(1, cis_party[2,4]+20, 2, cis_party[2,4]+20, h = 45,  ticks = 0.5, lwd=2)
#' brackets(3, cis_party[3,4]+20, 2, cis_party[3,4]+20, h = 45,  ticks = 0.5, lwd=2)
#' text(1.5, cis_party[1,4]+160, bquote(hat(y)['R']-hat(y)['D'] ~ '='
#'      ~ .(cis_party[2,4]-cis_party[1,4])), cex=0.9)
#' text(2.5, cis_party[3,4]-40, bquote(hat(y)['I']-hat(y)['R'] ~ '='
#'      ~ .(cis_party[3,4]-cis_party[2,4])), cex=0.9)
#' axis(1, at=1:3, labels = c("Democrat", "Republican", "Independent"), tck=0.03, cex.axis=0.9,
#'      mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = 'Party of candidate',
#'       ylab="Total campaign contributions",
#'       line = 1.7, cex.lab=1)
#' title(line = 1, main="Party of candidate", font.main=3)
#' plot(1:3, cis_incumchall[,4], type="n",xlab="",ylab="",  yaxt="n", xaxt="n",
#'      xlim=c(0.5,3.5), ylim=c(0, 3200))
#' segments(1:3, cis_incumchall[,5], 1:3, cis_incumchall[,6], lwd=2, col="gray60")
#' points(1:3, cis_incumchall[,4], pch=15:17, cex=1.5)
#' segments(c(1.05,2.05), cis_incumchall[1:2,4], c(1.95,2.95), cis_incumchall[2:3,4], lty=2)
#' brackets(1, cis_incumchall[2,4]+20, 2, cis_incumchall[2,4]+20, h = 105,  ticks = 0.5, lwd=2)
#' brackets(3, cis_incumchall[3,4]+20, 2, cis_incumchall[3,4]+20, h = 105,  ticks = 0.5, lwd=2)
#' text(1.5, cis_incumchall[2,4]+285, bquote(hat(y)['I']-hat(y)['C'] ~ '='
#'      ~ .(cis_incumchall[2,4]-cis_incumchall[1,4])), cex=0.9)
#' text(2.5, cis_incumchall[3,4]-200, bquote(hat(y)['O']-hat(y)['I'] ~ '='
#'      ~ .(cis_incumchall[3,4]-cis_incumchall[2,4])), cex=0.9)
#' axis(1, at=1:3, labels = c("Challenger", "Incumbent", "Open seat"), tck=0.03, cex.axis=0.9,
#'      mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = 'Status of candidate',
#'       ylab="Total campaign contributions",
#'       line = 1.7, cex.lab=1)
#' title(line = 1, main="Status of candidate", font.main=3)
#' plot(newdat_hispf$HISPPCT, cis_hispf[,4], type="n",xlab="",ylab="",  yaxt="n", xaxt="n",
#'      ylim = c(0,1100))
#' polygon(x = c(newdat_hispf$HISPPCT, rev(newdat_hispf$HISPPCT)),
#'         y = c(cis_hispf[,5], rev(cis_hispf[,6])), col = mygray, border = NA)
#' lines(newdat_hispf$HISPPCT, cis_hispf[,4], lwd=2)
#' rug(campaign$HISPPCT)
#' segments(iqrange[,1], cis_hispiq[,4], iqrange[,1], rep(500,2), lty=2)
#' segments(iqrange[1,1], 500, iqrange[2,1], 500, lty = 2)
#' brackets(iqrange[1,1], 510, iqrange[2,1], 510, h = 75,  ticks = 0.5, lwd=2)
#' text(abs((iqrange[2,1]-iqrange[1,1])/2)+iqrange[1,1], 450, 'Interquartile range', cex=0.8)
#' text(iqrange[,1], cis_hispiq[,4]-50, round(iqrange[,1],3), cex=0.8)
#' text(abs((iqrange[2,1]-iqrange[1,1])/2)+iqrange[1,1], 655,
#'           labels=bquote(hat(y)['Q3']-hat(y)['Q1'] ~ '=' ~ .(iqrange[1,2])), cex=0.9)
#' axis(1, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = '% Hispanic population in Congressional District',
#'       ylab="Total campaign contributions",
#'       line = 1.7, cex.lab=1)
#' title(line = 1, main="Hispanic constituency", font.main=3)
#' par(opar)
"campaign"
