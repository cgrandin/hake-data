#' Fit a length-weight model
#'
#' @param d Dataframe containing the columns `length` and `weight`
#' @param tol See [stats::nls()]
#' @param maxiter See [stats::nls()]
#'
#' @return The [stats::coefficients()] of the model fit
#' @importFrom stats nls coefficients
fit_lw <- function(d,
                   tol = 0.1,
                   maxiter = 1000){
  d <- d |>
    filter(!is.na(length),
           !is.na(weight))
  if(!nrow(d)){
    return(c(NA, NA))
  }
  w <- d$weight
  l <- d$length
  fit <- nls(w ~ lw_alpha * l ^ lw_beta,
             start = c(lw_alpha = 0.5, lw_beta = 2.0),
             control = list(tol = tol, maxiter = maxiter))
  coefficients(fit)
}
