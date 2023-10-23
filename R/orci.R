#' Wald CI for odds ratio
#'
#' @param coef coefficient estimate from logistic regression model
#' @param se standard error from coefficient estimate of logistic regression model
#' @param siglevel significance level (coverage) for coefficient estimate
#' @param roundto number of decimals to print out
#'
#' @return OR (CI)
#' @export
#'
#' @examples
#' data(toydata)
#' model <- glm(y ~ x2, data = toydata, family = binomial)
#' beta <- summary(model)$coefficients["x2", "Estimate"]
#' SE.beta <- summary(model)$coefficients["x2", "Std. Error"]
#' OR_95CI(beta, SE.beta, 0.95, 2)
orci <- function(coef, se, siglevel, roundto) {
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nmsall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ",  ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
                     )
  return(ORresult)
}
