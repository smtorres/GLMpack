#' Data on suicides in 2009 in OECD member states
#'
#' Data for the suicide example used in chapter 7
#'
#' @format A data frame with 32 rows and 7 variables:
#' \describe{
#'   \item{COUNTRYCODE}{Country code}
#'   \item{COUNTRYNAME}{Name of the country}
#'   \item{YEAR}{Year}
#'   \item{DEATHS}{Number of suicides in the country per 100,000 individuals}
#'   \item{GDP}{GDP in thousands of dollars}
#'   \item{SUBABUSE}{Share of the population with alcohol or drug use disorder}
#'   \item{TEMP}{Average temperature}
#'   ...
#' }
#'
#' @usage data(suicide)
#' @examples
#' opar = par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
#' data(suicide)
#' attach(suicide)
#'
#' ## Table 7.2
#' # Poisson model
#' suic.out.p <- glm(DEATHS ~ GDP + TEMP + SUBABUSE, family = poisson)
#' summary(suic.out.p)
#' round(confint(suic.out.p),3)
#' coefs_poisson <- summary(suic.out.p)$coefficients[1:4,]
#' coefs_poisson
# Quasi-model
#' suic.out.qp <- glm(DEATHS ~ GDP + TEMP + SUBABUSE, family = quasipoisson)
#' summary(suic.out.qp)
#' round(confint(suic.out.qp),3)
#' coefs_quasipoisson <- summary(suic.out.qp)$coefficients[1:4,]
#' coefs_quasipoisson
#'
#' ## Figure 7.1
#' layout(matrix(c(1,2,3,4), ncol=2, byrow = TRUE))
#' par(mar=c(4,3,2,0),oma=c(1,1,1,1))
#' # Histogram #1
#' hist(TEMP,xlab="",ylab="",  yaxt="n", xaxt="n", main="", col="gray50", border = "gray30",
#'     ylim=c(0,15))
#' axis(1, tck=0, mgp=c(0, 0, 0), lty=1, lwd=0, lwd.ticks = 0)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=1, lwd.ticks = 1, las=2)
#' title(xlab = 'Mean temperature (in Celsius)', ylab="",
#'       line = 1.7, cex.lab=1.2)
#' title(line = 1, main="Temperature", font.main=3)
#' # Histogram #2
#' hist(GDP,xlab="",ylab="",  yaxt="n", xaxt="n", main="", col="gray50", border = "gray30",
#'      ylim=c(0,15))
#' axis(1, tck=0, mgp=c(0, 0, 0), lty=1, lwd=0, lwd.ticks = 0)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=1, lwd.ticks = 1, las=2)
#' title(xlab = 'GDP per capita (in thousands of dollars)', ylab="",
#'       line = 1.7, cex.lab=1.2)
#' title(line = 1, main="Economic Conditions", font.main=3)
#' # Histogram #3
#' hist(SUBABUSE,xlab="",ylab="",  yaxt="n", xaxt="n", main="", col="gray50", border = "gray30",
#'      ylim=c(0,15))
#' axis(1, tck=0, mgp=c(0, 0, 0), lty=1, lwd=0, lwd.ticks = 0)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=1, lwd.ticks = 1, las=2)
#' title(xlab = '% of population with alcohol or drug use disorders', ylab="",line = 1.7, cex.lab=1.2)
#' title(line = 1, main="Substance abuse", font.main=3)
#' # Histogram #4
#' hist(DEATHS,xlab="",ylab="",  yaxt="n", xaxt="n", main="", col="gray10", border = "gray20",
#'      ylim=c(0,15))
#' axis(1, tck=0, mgp=c(0, 0, 0), lty=1, lwd=0, lwd.ticks = 0)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=1, lwd.ticks = 1, las=2)
#' title(xlab = 'Number of suicides per 100,000 people', ylab="",
#'       line = 1.7, cex.lab=1.2)
#' title(line = 1, main="Suicide rate", font.main=3)
#' par(opar)
#'
#' ## Figure 7.2
#' newdat1 <- data.frame(GDP=seq(13, 70.5, 1), TEMP=rep(mean(TEMP), 58),
#'                       SUBABUSE=rep(mean(SUBABUSE), 58))
#' newdat2 <- data.frame(GDP=rep(mean(GDP), 61), TEMP=rep(mean(TEMP), 61), SUBABUSE=seq(0,6,0.1))
#' preds.qp.gdp <- predict(suic.out.qp, newdata = newdat1, type = "link", se.fit = TRUE)
#' preds.qp.subabuse <- predict(suic.out.qp, newdata = newdat2, type = "link", se.fit = TRUE)
#' ilink.qp <- family(suic.out.qp)$linkinv
#' cis.p.preds.qp.gdp <- cbind(ilink.qp(preds.qp.gdp$fit - (2 * preds.qp.gdp$se.fit)),
#'                             ilink.qp(preds.qp.gdp$fit + (2 * preds.qp.gdp$se.fit)))
#' cis.p.preds.qp.subabuse <- cbind(ilink.qp(preds.qp.subabuse$fit - (2 * preds.qp.subabuse$se.fit)),
#'                             ilink.qp(preds.qp.subabuse$fit + (2 * preds.qp.subabuse$se.fit)))
#' mygray = rgb(153, 153, 153, alpha = 200, maxColorValue = 255)
#' par(mar=c(4,3,1,0),oma=c(1,1,1,1), mfrow=c(1,2))
#' plot(newdat1$GDP, ilink.qp(preds.qp.gdp$fit), type="n",xlab="",ylab="",  yaxt="n", xaxt="n", lwd=2,
#'      ylim = c(0,37))
#' polygon(c(newdat1$GDP,rev(newdat1$GDP)), c(cis.p.preds.qp.gdp[,1],rev(cis.p.preds.qp.gdp[,2])),
#'         border = NA, col = mygray)
#' lines(newdat1$GDP, ilink.qp(preds.qp.gdp$fit))
#' points(GDP, DEATHS, pch="+", col="gray20", cex=0.8)
#' axis(1, tck=0.03, cex.axis=0.9, at=seq(20,70,10), labels = seq(20,70,10), mgp=c(0.3, 0.3, 0),
#'      lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = 'GDP per capita (in thousands of dollars)',
#'       ylab="Number of suicides per 100,000 people", line = 1.7, cex.lab=1.2)
#' plot(newdat2$SUBABUSE, ilink.qp(preds.qp.subabuse$fit), type="n",xlab="",ylab="",
#'      yaxt="n", xaxt="n", lwd=2, ylim = c(0,37))
#' polygon(c(newdat2$SUBABUSE,rev(newdat2$SUBABUSE)), c(cis.p.preds.qp.subabuse[,1],
#'         rev(cis.p.preds.qp.subabuse[,2])), border = NA, col = mygray)
#' lines(newdat2$SUBABUSE, ilink.qp(preds.qp.subabuse$fit))
#' points(SUBABUSE, DEATHS, pch="+", col="gray20", cex=0.8)
#' axis(1, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = '% of population with alcohol or drug use disorders', ylab="",
#'       line = 1.7, cex.lab=1.2)
#' par(opar)
#' 
"suicide"
