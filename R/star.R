#' Data on California state data on educational policy and outcomes
#'
#' Data for the STAR program example used in chapter 6
#'
#' @format A data frame with 303 rows and 16 variables:
#' \describe{
#'   \item{LOWINC}{Proportion of low-income students}
#'   \item{PERASIAN}{Proportions of Asian students}
#'   \item{PERBLACK}{Proportions of African-American students}
#'   \item{PERHISP}{Proportions of Hispanic students}
#'   \item{PERMINTE}{Percentage of minority teachers}
#'   \item{AVYRSEXP}{Mean teacher experience in years}
#'   \item{AVSAL}{Median teacher salary, including benefits, in thousands of dollars}
#'   \item{PERSPEN}{Per-pupil expenditures in thousands of dollars}
#'   \item{PTRATIO}{Pupil/teacher ratio in the classroom}
#'   \item{PCTAF}{Percentage of students taking college credit courses}
#'   \item{PCTCHRT}{Percentage of schools in the district that are charter schools}
#'   \item{PCTYRRND}{Percent of schools in the district operating year-round programs}
#'   \item{READTOT}{Total number of students taking the reading exam in the 9th grade}
#'   \item{PR50RD}{Proportion of students scoring over the reading median in the 9th grade}
#'   \item{MATHTOT}{Total number of students taking the math exam in the 9th grade}
#'   \item{PR50M}{Proportion of students scoring over the math median in the 9th grade}
#'   ...
#' }
#'
#' @usage data(star)
#' @examples
#' opar = par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
#' data(star)
#' attach(star)
#'
#' ## MATH MODEL
#' star.logit.fit <- glm(cbind(PR50M,MATHTOT-PR50M) ~ LOWINC + PERASIAN + PERBLACK + PERHISP +
#'                   PERMINTE * AVYRSEXP * AVSAL + PERSPEN * PTRATIO * PCTAF +
#'                   PCTCHRT + PCTYRRND, family=binomial(link=logit),data=star)
#'
#' ## READING MODEL
#' star.logit.fit2 <- glm(cbind(PR50RD,READTOT-PR50RD) ~ LOWINC + PERASIAN + PERBLACK + PERHISP +
#'                    PERMINTE * AVYRSEXP * AVSAL + PERSPEN * PTRATIO * PCTAF +
#'                    PCTCHRT + PCTYRRND, family=binomial(link=logit),data=star)
#'
#' ## Table 6.4
#' star.summ.mat <- round(summary(star.logit.fit)$coef, 4)
#' data.frame(cbind(star.summ.mat[,1], star.summ.mat[,2], "[", round(confint(star.logit.fit)[,1],4),
#'       " ~", round(confint(star.logit.fit)[,2],4), "]"))
#'
#' ## Table 6.5
#' mean.vector <- apply(star,2,mean)
#' diff.vector <- c(1,mean.vector[1:12],mean.vector[5]*mean.vector[6],mean.vector[5]*mean.vector[7],
#'                  mean.vector[6]*mean.vector[7],mean.vector[8]*mean.vector[9],
#'                  mean.vector[8]*mean.vector[10],mean.vector[9]*mean.vector[10],
#'                  mean.vector[5]*mean.vector[6]*mean.vector[7],
#'                  mean.vector[8]*mean.vector[9]*mean.vector[10])
#' names(diff.vector) <- names(summary(star.logit.fit2)$coef[,1])
#' # PERMINTE FIRST DIFFERENCE ACROSS IQR
#' logit <- function(vec){return(exp(vec)/(1+exp(vec)))}
#' logit(c(diff.vector[1:5],6.329,diff.vector[7:13],6.329*mean.vector[6],6.329*mean.vector[7],
#'         diff.vector[16:19],6.329*mean.vector[6]*mean.vector[7],diff.vector[21])
#'       %*%summary.glm(star.logit.fit)$coef[,1]) -
#' logit(c(diff.vector[1:5],19.180,diff.vector[7:13],19.180*mean.vector[6],19.180*mean.vector[7],
#'           diff.vector[16:19],19.180*mean.vector[6]*mean.vector[7],diff.vector[21])
#'         %*%summary.glm(star.logit.fit)$coef[,1])
#' # First quartile information
#' q1.diff.mat <- q2.diff.mat <- q3.diff.mat <- q4.diff.mat <-
#'   matrix(rep(diff.vector,length(diff.vector)),
#'                       nrow=length(diff.vector), ncol=length(diff.vector),
#'                       dimnames=list(names(diff.vector),names(diff.vector)))
#' diag(q1.diff.mat)[2:13] <- apply(star,2,summary)[2,1:12]
#' q1.diff.mat[14,6] <- q1.diff.mat[6,6]*q1.diff.mat[7,6]
#' q1.diff.mat[15,6] <- q1.diff.mat[6,6]*q1.diff.mat[8,6]
#' q1.diff.mat[20,6] <- q1.diff.mat[6,6]*q1.diff.mat[7,6]*q1.diff.mat[8,6]
#' q1.diff.mat[14,7] <- q1.diff.mat[7,7]*q1.diff.mat[6,7]
#' q1.diff.mat[16,7] <- q1.diff.mat[7,7]*q1.diff.mat[8,7]
#' q1.diff.mat[20,7] <- q1.diff.mat[6,7]*q1.diff.mat[7,7]*q1.diff.mat[8,7]
#' q1.diff.mat[15,8] <- q1.diff.mat[8,8]*q1.diff.mat[6,8]
#' q1.diff.mat[16,8] <- q1.diff.mat[8,8]*q1.diff.mat[7,8]
#' q1.diff.mat[20,8] <- q1.diff.mat[6,8]*q1.diff.mat[7,8]*q1.diff.mat[8,8]
#' q1.diff.mat[17,9] <- q1.diff.mat[9,9]*q1.diff.mat[10,9]
#' q1.diff.mat[18,9] <- q1.diff.mat[9,9]*q1.diff.mat[11,9]
#' q1.diff.mat[21,9] <- q1.diff.mat[9,9]*q1.diff.mat[10,9]*q1.diff.mat[11,9]
#' q1.diff.mat[17,10] <- q1.diff.mat[10,10]*q1.diff.mat[9,10]
#' q1.diff.mat[19,10] <- q1.diff.mat[10,10]*q1.diff.mat[11,10]
#' q1.diff.mat[21,10] <- q1.diff.mat[9,10]*q1.diff.mat[10,10]*q1.diff.mat[11,10]
#' q1.diff.mat[18,11] <- q1.diff.mat[11,11]*q1.diff.mat[9,11]
#' q1.diff.mat[19,11] <- q1.diff.mat[11,11]*q1.diff.mat[10,11]
#' q1.diff.mat[21,11] <- q1.diff.mat[9,11]*q1.diff.mat[10,11]*q1.diff.mat[11,11]
#' # Third quartile
#' diag(q2.diff.mat)[2:13] <- apply(star,2,summary)[5,1:12]
#' q2.diff.mat[14,6] <- q2.diff.mat[6,6]*q2.diff.mat[7,6]
#' q2.diff.mat[15,6] <- q2.diff.mat[6,6]*q2.diff.mat[8,6]
#' q2.diff.mat[20,6] <- q2.diff.mat[6,6]*q2.diff.mat[7,6]*q2.diff.mat[8,6]
#' q2.diff.mat[14,7] <- q2.diff.mat[7,7]*q2.diff.mat[6,7]
#' q2.diff.mat[16,7] <- q2.diff.mat[7,7]*q2.diff.mat[8,7]
#' q2.diff.mat[20,7] <- q2.diff.mat[6,7]*q2.diff.mat[7,7]*q2.diff.mat[8,7]
#' q2.diff.mat[15,8] <- q2.diff.mat[8,8]*q2.diff.mat[6,8]
#' q2.diff.mat[16,8] <- q2.diff.mat[8,8]*q2.diff.mat[7,8]
#' q2.diff.mat[20,8] <- q2.diff.mat[6,8]*q2.diff.mat[7,8]*q2.diff.mat[8,8]
#' q2.diff.mat[17,9] <- q2.diff.mat[9,9]*q2.diff.mat[10,9]
#' q2.diff.mat[18,9] <- q2.diff.mat[9,9]*q2.diff.mat[11,9]
#' q2.diff.mat[21,9] <- q2.diff.mat[9,9]*q2.diff.mat[10,9]*q2.diff.mat[11,9]
#' q2.diff.mat[17,10] <- q2.diff.mat[10,10]*q2.diff.mat[9,10]
#' q2.diff.mat[19,10] <- q2.diff.mat[10,10]*q2.diff.mat[11,10]
#' q2.diff.mat[21,10] <- q2.diff.mat[9,10]*q2.diff.mat[10,10]*q2.diff.mat[11,10]
#' q2.diff.mat[18,11] <- q2.diff.mat[11,11]*q2.diff.mat[9,11]
#' q2.diff.mat[19,11] <- q2.diff.mat[11,11]*q2.diff.mat[10,11]
#' q2.diff.mat[21,11] <- q2.diff.mat[9,11]*q2.diff.mat[10,11]*q2.diff.mat[11,11]
#' # Minimum
#' diag(q3.diff.mat)[2:13] <- apply(star,2,summary)[1,1:12]
#' q3.diff.mat[14,6] <- q3.diff.mat[6,6]*q3.diff.mat[7,6]
#' q3.diff.mat[15,6] <- q3.diff.mat[6,6]*q3.diff.mat[8,6]
#' q3.diff.mat[20,6] <- q3.diff.mat[6,6]*q3.diff.mat[7,6]*q3.diff.mat[8,6]
#' q3.diff.mat[14,7] <- q3.diff.mat[7,7]*q3.diff.mat[6,7]
#' q3.diff.mat[16,7] <- q3.diff.mat[7,7]*q3.diff.mat[8,7]
#' q3.diff.mat[20,7] <- q3.diff.mat[6,7]*q3.diff.mat[7,7]*q3.diff.mat[8,7]
#' q3.diff.mat[15,8] <- q3.diff.mat[8,8]*q3.diff.mat[6,8]
#' q3.diff.mat[16,8] <- q3.diff.mat[8,8]*q3.diff.mat[7,8]
#' q3.diff.mat[20,8] <- q3.diff.mat[6,8]*q3.diff.mat[7,8]*q3.diff.mat[8,8]
#' q3.diff.mat[17,9] <- q3.diff.mat[9,9]*q3.diff.mat[10,9]
#' q3.diff.mat[18,9] <- q3.diff.mat[9,9]*q3.diff.mat[11,9]
#' q3.diff.mat[21,9] <- q3.diff.mat[9,9]*q3.diff.mat[10,9]*q3.diff.mat[11,9]
#' q3.diff.mat[17,10] <- q3.diff.mat[10,10]*q3.diff.mat[9,10]
#' q3.diff.mat[19,10] <- q3.diff.mat[10,10]*q3.diff.mat[11,10]
#' q3.diff.mat[21,10] <- q3.diff.mat[9,10]*q3.diff.mat[10,10]*q3.diff.mat[11,10]
#' q3.diff.mat[18,11] <- q3.diff.mat[11,11]*q3.diff.mat[9,11]
#' q3.diff.mat[19,11] <- q3.diff.mat[11,11]*q3.diff.mat[10,11]
#' q3.diff.mat[21,11] <- q3.diff.mat[9,11]*q3.diff.mat[10,11]*q3.diff.mat[11,11]
#' diag(q4.diff.mat)[2:13] <- apply(star,2,summary)[6,1:12]
#' q4.diff.mat[14,6] <- q4.diff.mat[6,6]*q4.diff.mat[7,6]
#' q4.diff.mat[15,6] <- q4.diff.mat[6,6]*q4.diff.mat[8,6]
#' q4.diff.mat[20,6] <- q4.diff.mat[6,6]*q4.diff.mat[7,6]*q2.diff.mat[8,6]
#' q4.diff.mat[14,7] <- q4.diff.mat[7,7]*q4.diff.mat[6,7]
#' q4.diff.mat[16,7] <- q4.diff.mat[7,7]*q4.diff.mat[8,7]
#' q4.diff.mat[20,7] <- q4.diff.mat[6,7]*q4.diff.mat[7,7]*q4.diff.mat[8,7]
#' q4.diff.mat[15,8] <- q4.diff.mat[8,8]*q4.diff.mat[6,8]
#' q4.diff.mat[16,8] <- q4.diff.mat[8,8]*q4.diff.mat[7,8]
#' q4.diff.mat[20,8] <- q4.diff.mat[6,8]*q4.diff.mat[7,8]*q4.diff.mat[8,8]
#' q4.diff.mat[17,9] <- q4.diff.mat[9,9]*q4.diff.mat[10,9]
#' q4.diff.mat[18,9] <- q4.diff.mat[9,9]*q4.diff.mat[11,9]
#' q4.diff.mat[21,9] <- q4.diff.mat[9,9]*q4.diff.mat[10,9]*q4.diff.mat[11,9]
#' q4.diff.mat[17,10] <- q4.diff.mat[10,10]*q4.diff.mat[9,10]
#' q4.diff.mat[19,10] <- q4.diff.mat[10,10]*q4.diff.mat[11,10]
#' q4.diff.mat[21,10] <- q4.diff.mat[9,10]*q4.diff.mat[10,10]*q4.diff.mat[11,10]
#' q4.diff.mat[18,11] <- q4.diff.mat[11,11]*q4.diff.mat[9,11]
#' q4.diff.mat[19,11] <- q4.diff.mat[11,11]*q4.diff.mat[10,11]
#' q4.diff.mat[21,11] <- q4.diff.mat[9,11]*q4.diff.mat[10,11]*q4.diff.mat[11,11]
#' first_diffs <- NULL
#' for (i in 2:13){
#'         temp1 <- logit(q2.diff.mat[,i]%*%summary.glm(star.logit.fit)$coef[,1]) -
#'                         logit(q1.diff.mat[,i]%*%summary.glm(star.logit.fit)$coef[,1])
#'         temp2 <- logit(q4.diff.mat[,i]%*%summary.glm(star.logit.fit)$coef[,1]) -
#'           logit(q3.diff.mat[,i]%*%summary.glm(star.logit.fit)$coef[,1])
#'         first_diffs <- rbind(first_diffs, c(temp1,temp2))
#' }
#' first_diffs <- round(first_diffs,4)
#' diffs_mat <- cbind(diag(q1.diff.mat)[2:13], diag(q2.diff.mat)[2:13],
#'                    first_diffs[,1],
#'                    diag(q3.diff.mat)[2:13], diag(q4.diff.mat)[2:13],
#'                    first_diffs[,2])
#' colnames(diffs_mat) <- c("1st quartile", "3rd quartile", "Interquartile 1st diff",
#'                          "Min", "Max", "Full range 1st diff")
#' diffs_mat
#'
## Figure 6.2
#' star.mu <- predict.glm(star.logit.fit,type="response")
#' star.y <- PR50M/MATHTOT
#' star.n <- length(star.y)
#' PR50M.adj <- PR50M
#' for (i in 1:length(PR50M.adj))  {
#'   if (PR50M.adj[i] > mean(PR50M)) PR50M.adj[i] <- PR50M.adj[i] - 0.5
#'   if (PR50M.adj[i] < mean(PR50M)) PR50M.adj[i] <- PR50M.adj[i] + 0.5
#' }
#' par(mfrow=c(1,3), mar=c(6,3,6,2),oma=c(4,1,4,1))
#' plot(star.mu,star.y,xlab="",ylab="", yaxt='n', xaxt="n", pch="+")
#' axis(1, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = "Fitted values", ylab="Observed values",
#'       line = 1.7, cex.lab=1.3)
#' title(main="Model Fit Plot",
#'       line = 1, cex.main=1.7, font.main=1)
#' abline(lm(star.y~star.mu)$coefficients, lwd=2)
#' plot(fitted(star.logit.fit),resid(star.logit.fit,type="pearson"),xlab="",ylab="",
#'      yaxt='n', xaxt="n", pch="+")
#' axis(1, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = "Fitted values", ylab="Pearson Residuals",
#'       line = 1.7, cex.lab=1.3)
#' title(main="Residual Dependence Plot",
#'       line = 1, cex.main=1.7, font.main=1)
#' abline(0,0, lwd=2)
#' qqnorm(resid(star.logit.fit,type="deviance"),main="",xlab="",ylab="",
#'        yaxt='n', xaxt="n", pch="+")
#' axis(1, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1)
#' axis(2, tck=0.02, cex.axis=0.9, mgp=c(0.3, 0.3, 0), lty=1, lwd=0, lwd.ticks = 1, las=2)
#' title(xlab = "Quantiles of N(0,1)", ylab="Deviance Residual Quantiles",
#'       line = 1.7, cex.lab=1.3)
#' title(main="Normal-Quantile Plot",
#'       line = 1, cex.main=1.7, font.main=1)
#' abline(-0.3,3.5, lwd=2)
#' par(opar)
#' 
"star"
