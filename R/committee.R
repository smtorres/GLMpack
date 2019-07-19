#' Data on bills assigned to House committees in the 103rd and 104th Houses of Representatives
#'
#' Data for the committees example used in chapters 6
#'
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{SIZE}{Number of members on the committee}
#'   \item{SUBS}{Number of subcommittees}
#'   \item{STAFF}{Number of staff assigned to the committee}
#'   \item{PRESTIGE}{Dummy variable indicating whether or not it is a high-prestige committee}
#'   \item{BILLS103}{Number of bills in the 103rd House}
#'   \item{BILLS104}{Number of bills in the 104th House}
#'   ...
#' }
#'
#' @usage data(committee)
#' @examples
#' data(committee)
#' attach(committee)
#' library(AER)
#' library(MASS)
#' library(pscl)
#'
#' ## Table 6.6
#' committee
#'
#' ## Table 6.7
#' committee.out <- glm.nb(BILLS104 ~ SIZE + SUBS * (log(STAFF)) + PRESTIGE + BILLS103)
#' summary.glm(committee.out)
#' data.frame(cbind(round(cbind(summary(committee.out)$coef[,1:2], confint(committee.out)),4)[,1],
#'        round(cbind(summary(committee.out)$coef[,1:2], confint(committee.out)),4)[,2],
#'        round(cbind(summary(committee.out)$coef[,1:2], confint(committee.out)),4)[,3],
#'        round(cbind(summary(committee.out)$coef[,1:2], confint(committee.out)),4)[,4]))
#'
#' ## Figure 6.3
#' z.matrix <- matrix(0,200,200)
#' for(i in 1:200)  {
#'         for(j in 1:200)  {
#'                 if(j < 70)    z.matrix[i,j] <- 1
#'                 if(j < 40)    z.matrix[i,j] <- 2
#'                 if(j < 10)    z.matrix[i,j] <- 3
#'                 if(j == 1)    z.matrix[i,j] <- 3.001
#'                 if(j > 130)   z.matrix[i,j] <- 1
#'                 if(j > 160)   z.matrix[i,j] <- 2
#'                 if(j > 190)   z.matrix[i,j] <- 3
#'                 if(j == 200)  z.matrix[i,j] <- 3.001
#'         }
#' }
#' pears <- resid(committee.out,type="pearson")
#' devs <- resid(committee.out,type="deviance")
#' x = seq(-2000,2000,length=200)
#' opar = par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
#' layout(matrix(c(1,2), ncol = 1), heights = c(0.9,0.1))
#' par(mar=c(3,4,2,4),oma=c(2,2,1,3))
#' image(seq(0,51,length=200), seq(-2000,2000,length=200),z.matrix,xlim=c(0,51),ylim=c(-2000,2000),
#' 	xaxt="n",yaxt="n",xlab="",ylab="", col=rev(c("white", "gray40", "gray60", "gray80")))
#' points(seq(1,50,length=20),(2000/3)*pears[order(BILLS104)],pch=15)
#' lines(seq(1,50,length=20),(2000/3)*devs[order(BILLS104)],type="h")
#' abline(0,0, lwd=2)
#' abline(h=c((x[10]+x[9])/2,(x[40]+x[39])/2,(x[70]+x[69])/2,(x[130]+x[131])/2,
#'            (x[160]+x[161])/2,(x[191]+x[190])/2), lty=2)
#' title(xlab = "Order of Fitted Outcome Variable", ylab="Residual Effect",
#'       line = 1.3, cex.lab=1.3)
#' title(main="Model Fit Plot",
#'       line = 1, cex.main=1.7, font.main=1)
#' par(mar=c(0,1.5,1,1))
#' plot(0,0, type="n", axes = FALSE, xlab = "", ylab = "")
#' legend("center", ncol = 2,
#'        legend = c("Pearson", "Deviances"),
#'        cex=1, lty=c(0,1), pch = c(15,NA))
#' par(opar)
"committee"
