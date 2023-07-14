check_fit_BENS <- function (parameter_estimates, check_gradients = FALSE, quiet = FALSE) 
{
  problem_found = FALSE
  On_bounds = ifelse(parameter_estimates$par < (parameter_estimates$diagnostics[, 
                                                                                "Lower"] + 1e-04) | parameter_estimates$par > (parameter_estimates$diagnostics[, 
                                                                                                                                                               "Upper"] - 1e-04), TRUE, FALSE)
  if (any(On_bounds)) {
    problem_found = TRUE
    if (quiet == FALSE) {
      message("\nCheck bounds for the following parameters:")
      print(parameter_estimates$diagnostics[which(On_bounds), 
      ])
    }
  }
  At_zero = ifelse(abs(parameter_estimates$par) < 1e-04, TRUE, 
                   FALSE)
  if (any(At_zero)) {
    problem_found = TRUE
    if (quiet == FALSE) {
      message("\nThe following parameters appear to be approaching zero:")
      print(parameter_estimates$diagnostics[which(At_zero), 
      ])
      #bad_one = parameter_estimates$diagnostics[which(At_zero),]
     # stop(paste0("Parameters approaching zero are: ",bad_one))
      if (length(grep("L_", names(parameter_estimates$par[which(At_zero)])))) {  #ben altered this line to print the parameter going to 0
        stop(paste0("Please turn off factor-model variance parameters `L_` that are approaching zero and re-run the model, specifically", names(parameter_estimates$par[which(At_zero)])))
      }
    }
  }
  Bad_gradient = ifelse(abs(parameter_estimates$diagnostics[, 
                                                            "final_gradient"]) > 0.001, TRUE, FALSE)
  if (check_gradients == TRUE && any(Bad_gradient)) {
    problem_found = TRUE
    if (quiet == FALSE) {
      message("\nThe following parameters has a bad final gradient:")
      print(parameter_estimates$diagnostics[which(Bad_gradient), 
      ])
    }
  }
  return(invisible(problem_found))
}
