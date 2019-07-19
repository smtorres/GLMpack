#' Data on capital punishment
#'
#' Data for the capital punishment example used in chapters 4, 5, and 6
#'
#' @format A data frame with 17 rows and 7 variables:
#' \describe{
#'   \item{EXECUTIONS}{The number of times that capital punishment is implemented on a state level in the United States for the year 1997}
#'   \item{INCOME}{Median per capita income in dollars}
#'   \item{PERPOVERTY}{Percent of the population classified as living in poverty}
#'   \item{PERBLACK}{Percent of Black citizens in the population}
#'   \item{VC100k96}{Rate of violent crimes per 100,000 residents for the year before (1996)}
#'   \item{SOUTH}{Dummy variable to indicate whether the state is in the South}
#'   \item{PROPDEGREE}{Proportion of the population with a college degree}
#'   ...
#' }
#'
#' @usage data(dp)
#' @examples
#' 
#' opar = par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
#' data(dp)
#' attach(dp)
#'
#' ## Table 4.2
#' dp
#'
#' ## Table 5.1
#' dp.out <- glm(EXECUTIONS ~ INCOME+PERPOVERTY+PERBLACK+log(VC100k96)+
#'               SOUTH+PROPDEGREE, family=poisson)
#' dp.cis <- round(glm.summary(dp.out, alpha = 0.05),4)
#' round(cbind(summary(dp.out)$coef[,1:2], dp.cis),4)
#' round(dp.out$null.deviance,4);round(dp.out$df.null,4)
#' round(dp.out$deviance,4);round(dp.out$df.residual,4)
#' round(logLik(dp.out),4)
#' round(dp.out$aic,4)
#' round(vcov(dp.out),4) # variance covariance matrix
#'
#' ## Table 5.2
#' k <- 200
#' b5 <- seq(0.1, 5.4,length=k)
#' w <- rep(0,k)
#' for(i in 1:k){
#'   mm <- glm(EXECUTIONS ~ INCOME+PERPOVERTY+PERBLACK+log(VC100k96)+PROPDEGREE,
#'             offset=b5[i]*SOUTH,family=poisson)
#'   w[i] <- logLik(mm)
#'   }
#'
#' f <- function(b5,x,y,maxloglik){
#'   mm <- glm(EXECUTIONS ~ INCOME+PERPOVERTY+PERBLACK+log(VC100k96)+PROPDEGREE,
#'             offset=b5*x,family=poisson)
#'   logLik(mm) - maxloglik + qchisq(.95,1)/2
#'   }
#' low.pll <- uniroot(f,interval=c(1.5,2), x=SOUTH, y=EXECUTIONS, maxloglik=logLik(dp.out))$root
#' high.pll <- uniroot(f,interval=c(3,4), x=SOUTH, y=EXECUTIONS, maxloglik=logLik(dp.out))$root
#' w[which.min(abs(w-low.pll))]
#' round(c(low.pll, high.pll),4)
#' cbind(round(dp.cis[,3:4],4),
#'       round(confint(dp.out),4))
#'
#' ## Table 6.2
#' resp <- resid(dp.out,type="response")
#' pears <- resid(dp.out,type="pearson")
#' working <- resid(dp.out,type="working")
#' devs <- resid(dp.out,type="deviance")
#' dp.mat <- cbind(rep(1,nrow(dp)), as.matrix(dp[,2:4]), as.matrix(log(dp[,5])),
#'                 as.matrix(dp[,6]), as.matrix(dp[,7]))
#' dp.resid.mat <- cbind(resp,pears,working,devs)
#' dimnames(dp.resid.mat)[[2]] <- c("response","pearson","working","deviance")
#' dimnames(dp.resid.mat)[[1]] <- rownames(dp)
#' dp.resid.mat2 <- round(dp.resid.mat,4)
#' resid.df <- data.frame(cbind(dp.resid.mat2[,1], dp.resid.mat2[,2],
#'       dp.resid.mat2[,3], dp.resid.mat2[,4]))
#' colnames(resid.df) <- dimnames(dp.resid.mat)[[2]]
#' resid.df
#'
#' ## Figure 5.1
#' dp.mat.0 <- cbind(dp.mat[,1:5],rep(0,length=nrow(dp.mat)),dp.mat[,7])
#' dimnames(dp.mat.0)[[2]] <- names(dp.out$coefficients)
#' dp.mat.1 <- cbind(dp.mat[,1:5],rep(1,length=nrow(dp.mat)),dp.mat[,7])
#' dimnames(dp.mat.1)[[2]] <- names(dp.out$coefficients)
#' tcks = list(seq(0,140,20), seq(0,12,2), seq(0,30,5), seq(0,10,2), seq(0,30,5))
#' layout(matrix(c(1,1,2,2,3,3,4,4,5,6,6,7,8,8,8,8), ncol=4, byrow = TRUE),
#'        heights = c(0.3,0.3,0.3,0.1))
#' par(mar=c(3,3,2,4),oma=c(2,1,1,3))
#' for (i in 2:(ncol(dp.mat.0)-1))  {
#'   j = i-1
#'   if (i==6){
#'     i <- i+1
#'     plot(0,0, type = "n", axes=FALSE, xlab = "", ylab="")
#'   }
#'   ruler <- seq(min(dp.mat.0[,i]),max(dp.mat.0[,i]),length=1000)
#'   xbeta0 <- exp(dp.out$coefficients[-i]%*%apply(dp.mat.0[,-i],2,mean)
#'                 + dp.out$coefficients[i]*ruler)
#'   xbeta1 <- exp(dp.out$coefficients[-i]%*%apply(dp.mat.1[,-i],2,mean)
#'                 + dp.out$coefficients[i]*ruler)
#'   plot(ruler,xbeta0,type="l", xlab="",ylab="", yaxt="n", xaxt="n",
#'        ylim=c(min(xbeta0,xbeta1)-2,max(xbeta0,xbeta1)), lwd=3)
#'   lines(ruler,xbeta1,lty=4, lwd=2)
#'   axis(1, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#'   axis(2, at=tcks[[j]],
#'        tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#'   title(xlab = paste("Levels of",dimnames(dp.mat.0)[[2]][i]), ylab="Expected executions",
#'         line = 1.7, cex.lab=1.2)
#' }
#' plot(0,0, type = "n", axes=FALSE, xlab = "", ylab="")
#' par(mar=c(0,1.5,1,1))
#' plot(0,0, type="n", axes = FALSE, xlab = "", ylab = "")
#' legend("center", ncol = 2,
#'        legend = c("South State", "Non-South State"),
#'        cex=1.1, lty=c(2,1), bty="n", lwd=c(2,3))
#' par(opar)
#'
#' ## Figure 5.2
#' par(mar=c(3,3,1,0),oma=c(1,1,1,1))
#' plot(b5,w,type="l",lwd=3, xaxt="n", yaxt="n", xlab="", ylab="")
#' abline(h=logLik(dp.out)-qchisq(.95,1)/2,lty=3, col="gray40")
#' segments(dp.cis[6,3], -45, dp.cis[6,4], -45, lty=6, col="black", lwd=2)
#' segments(dp.cis[6,3:4], c(-45,-45), dp.cis[6,3:4], c(-55,-55), lty=3, col="gray40")
#' text(3.5, y=-45, "Wald Test", cex=0.9)
#' segments(low.pll, -42.5, high.pll, -42.5, lty=2, lwd=2, col="black")
#' segments(c(low.pll, high.pll), c(-55,-55), c(low.pll, high.pll),
#'          rep(logLik(dp.out)-qchisq(.95,1)/2,2), lty=3, col="gray40")
#' text(3.25, y=-42.5, "Profile log-likelihood", cex=0.9, pos=4)
#' axis(1, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = 'Coefficient of SOUTH', ylab="Profile log-likelihood",
#'       line = 1.7, cex.lab=1.2)
#' par(opar)
#'
#' ## Figure 6.1
#' coef.vector <- NULL
#' for (i in 1:length(EXECUTIONS))  {
#'   dp.temp <- glm(EXECUTIONS[-i] ~ INCOME[-i]+PERPOVERTY[-i]+PERBLACK[-i]+log(VC100k96)[-i]+
#'                    SOUTH[-i]+PROPDEGREE[-i], family=poisson)
#'   coef.vector <- rbind(coef.vector,dp.temp$coefficients)
#' }
#' layout(matrix(c(1,2,3,4,5,6), ncol=2, byrow = TRUE), heights = c(0.33,0.33,0.33))
#' par(mar=c(3,4.5,2,4),oma=c(2,1,1,3))
#' for(i in 2:ncol(coef.vector))  {
#'   x=plot(coef.vector[,i],type="b",xlab="",ylab="",  yaxt="n", xaxt="n", lwd=2,
#'          ylim=c(min(coef.vector[,i])-abs(min(coef.vector[,i]))*0.25,
#'          max(coef.vector[,i])+abs(max(coef.vector[,i]))*0.25))
#'   abline(h=dp.out$coefficients[i])
#'   axis(1, at =seq(2,16,2), tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#'   if(i==2){
#'     axis(2, at = seq(5,35,5)/100000, labels = as.expression(paste(seq(5,35,5), "e(-5)", sep="")),
#'          tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#'   }
#'   else{
#'     axis(2, tck=0.03, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#'   }
#'   title(xlab = "Index number",
#'         line = 1.7, cex.lab=1.2)
#'   title(ylab=dimnames(dp.mat.0)[[2]][i],
#'         line = 3.25, cex.lab=1.2)
#' }
#' par(opar)
"dp"
