#' A function to estimate an inverse Burr regression model
#' 
#' This function allows you to fit an inverse Burr model of some outcome
#' either using constant parameter values for the distribution's scale and 
#' two shape parameters, or else using covariates to fit these parameters.
#' 
#' @param outcome The outcome being modeled.
#' @param mu The scale parameter. Default is a constant formula of `~ 1` or can be the right-hand side of a formula object with covariates.
#' @param alpha The first shape parameter. Default is a constant formula of `~ 1` or can be the right-hand side of a formula object with covariates.
#' @param theta The second shape parameter. Default is a constant formula of `~ 1` or can be the right-hand side of a formula object with covariates.
#' @param data The dataset containing the outcome and any covariates. At the moment, the function cannot handle data with missing values.
#' @param its The function uses bootstrapping for statistical inference. How many bootstrapped iterations do you want to use? The default is 2000.
#' @export
ibm <- function(
    outcome,     # the outcome
    mu = ~ 1,    # rhs for mu
    alpha = ~ 1, # rhs for alpha
    theta = ~ 1, # rhs for theta
    data = NULL, # data
    its = 2000) {# do inference with 2000 bootstraps
  
  ## The Data
  if(!is.null(data)) {
    y <- data |> dplyr::pull(!!enquo(outcome))
  } else {
    y <- outcome
  }
  x1 <- model.matrix(mu, data)
  x2 <- model.matrix(alpha, data)
  x3 <- model.matrix(theta, data)
  
  ## The likelihood
  inbur_lik <- function(x1, x2, x3, y, pars) {
    b <- matrix(
      data = pars[1:(ncol(x1) + ncol(x2) + ncol(x3))],
      nrow = length(y),
      ncol = 1
    )
    mu <- exp(x1 %*% b[1:ncol(x1)])
    alpha <- exp(x2 %*% b[ncol(x1) + 1:ncol(x2)])
    theta <- exp(x3 %*% b[ncol(x1) + ncol(x2) + 1:ncol(x3)])
    sum(
      - log(
        actuar::dinvburr(
          y, 
          shape1 = alpha,
          shape2 = theta,
          scale = mu
        )
      )
    )
  }
  
  ## Estimation
  optim(
    par = rep(0, len = ncol(x1) + ncol(x2) + ncol(x3)),
    fn = inbur_lik,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    y = y,
    hessian = F
  ) -> opt_out
  
  ## Bootstrapping
  tibble::tibble(
    its = 1:its,
    bout = furrr::future_map(
      its,
      ~ {
        bkeep <- sample(1:length(y), length(y), T)
        optim(
          par = rep(0, len = ncol(x1) + ncol(x2) + ncol(x3)),
          fn = inbur_lik,
          x1 = x1[bkeep, , drop = F],
          x2 = x2[bkeep, , drop = F],
          x3 = x3[bkeep, , drop = F],
          y = y[bkeep],
          hessian = F
        ) -> opt_out
        tibble::tibble(
          pars = 1:length(opt_out$par),
          vals = opt_out$par
        )
      },
      .options = furrr::furrr_options(seed = T)
    )
  ) |>
    tidyr::unnest(cols = bout) |>
    dplyr::group_by(pars) |>
    dplyr::summarize(
      std.error = sd(vals, na.rm=T)
    ) -> boot_se
  
  ## Return model output in a tidy tibble
  list(
    out = tibble::tibble(
      term = c(
        ifelse(rep(ncol(x1) == 1, len = ncol(x1)), 
               "mu: (Intercept)", 
               paste0("mu: ", colnames(x1))),
        ifelse(rep(ncol(x2) == 1, len = ncol(x2)), 
               "alpha: (Intercept)", 
               paste0("alpha: ", colnames(x2))),
        ifelse(rep(ncol(x3) == 1, len = ncol(x3)), 
               "theta: (Intercept)", 
               paste0("theta: ", colnames(x3)))
      ),
      estimate = opt_out$par,
      std.error = boot_se$std.error,
      statistic = estimate / std.error,
      p.value = 2*pnorm(
        -abs(statistic)
      ) |> round(3)
    ),
    logLik = -opt_out$value
  )
}