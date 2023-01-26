#' Custom tidy Method for Bayesian Model Presentation Using {modelsummary}
#' 
#' This function provides a custom tidy method for objects of class brmsfit
#' that, among other things, correctly handles multinomial and categorical 
#' logit models in a way that doesn't end up looking like shit.
#' 
#' @importFrom stringr str_extract_all, str_remove_all
#' @importFrom bayestestR describe_posterior
#' @importFrom brms ngrps
#'
#' @param x An object of class `brmsfit`
#' 
#' @param conf_level 1 - alpha level to use when calculating Bayesian credible
#' intervals for the posterior medians.
#' 
#' @param parameters A string representing a regular expression for the
#' parameters to include from the model. Defaults to 
#' `"^(b_|sd_|cor_|sigma|rescor|phi|ar)"` which should be appropriate for 
#' most common applications.
#' 
#' @param signif A string argument indicating whether to include a column 
#' for p-values. This is calculated based on the percentage of the posterior
#' distribution that falls in a specific direction. If not `NULL` (the default),
#' it must be one of either "one-sided" or "two-sided" depending on the desired
#' test direction.
#' 
#' @param ... Additional arguments passed down to `describe_posterior`.
#' See `?bayestestR::describe_posterior` for possible options
#'
#' @return A data frame containing the relevant model information to 
#' be used internally by {modelsummary}
#' 
#' @export tidy.brmsfit
#'
tidy.brmsfit <- function(x,
                         conf_level = 0.95,
                         parameters = "^(b_|sd_|cor_|sigma|rescor|phi|ar)",
                         signif = NULL,
                         ...) {

  ## Extract the model information
  posterior <- bayestestR::describe_posterior(
    x,
    ci = conf_level,
    parameters = parameters,
    effects = "all",
    component = "all",
    ...
  )

  # Build the data frame for {modelsummary}
  out <- data.frame(
    term = posterior$Parameter,
    estimate = posterior$Median,
    conf.level = posterior$CI,
    conf.low = posterior$CI_low,
    conf.high = posterior$CI_high,
    pd = posterior$pd,
    rhat = posterior$Rhat,
    ess = posterior$ESS,
    effect = posterior$Effects
  )

  # If model family is of class categorical or multinomial, terms need to be 
  # handled differently to make tables not look like shit
  if (isTRUE(grepl(x$family[1], "categorical|multinomial"))) {
    
    # Decompose the term column into a response category identifier and a
    # term name identifier
    out$response <- as.character(
      stringr::str_extract_all(out$term, "mu.*?(?=_)")
      )

    # Overwrite the original term column
    out$term <- stringr::str_remove_all(out$term, "_mu.*?(?=_)")
    
  }

  # Option to return stars if reviewer 2 is an idiot and demands you report
  # "statistical significance" for some weird reason. We're just going to 
  # use posterior tail probabilities for this and pretend that's the same 
  # thing
  if (!is.null(signif)) {
    
    # Check that signif is a valid argument
    signif_check <- grepl(signif, "one-sided|two-sided")
    stopifnot(
      "signif must be either 'one-sided' or 'two-sided'" = signif_check,
      signif_check
      )
    
    # Estimate p-values based on the inverted probability of direction
    out$p.value <- ifelse(
      out$effect != "random", 
      yes = bayestestR::pd_to_p(out$pd, direction = signif), 
      no = 1
      )
    
  }

  # Return the data frame object
  return(out)
}
