fit_model_BENS <- function (settings, Lat_i, Lon_i, t_i, b_i, a_i, c_iz = rep(0, 
                                                                              length(b_i)), v_i = rep(0, length(b_i)), working_dir = paste0(getwd(), 
                                                                                                                                            "/"), X1config_cp = NULL, X2config_cp = NULL, covariate_data, 
                            X1_formula = ~0, X2_formula = ~0, Q1config_k = NULL, Q2config_k = NULL, 
                            catchability_data, Q1_formula = ~0, Q2_formula = ~0, newtonsteps = 1, 
                            silent = TRUE, build_model = TRUE, run_model = TRUE, test_fit = TRUE, 
                            category_names = NULL, year_labels = NULL, framework = "TMBad", 
                            use_new_epsilon = TRUE, ...) 
{
  extra_args = list(...)
  extra_args = c(extra_args, extra_args$extrapolation_args, 
                 extra_args$spatial_args, extra_args$optimize_args, extra_args$model_args)
  start_time = Sys.time()
  data_frame = data.frame(Lat_i = Lat_i, Lon_i = Lon_i, a_i = a_i, 
                          v_i = v_i, b_i = b_i, t_i = t_i, c_iz = c_iz)
  if (is.null(year_labels)) 
    year_labels = paste0(seq(min(t_i), max(t_i)))
  if (is.null(category_names)) 
    category_names = paste0(1:(max(c_iz, na.rm = TRUE) + 
                                 1))
  years_to_plot = which(seq(min(t_i), max(t_i)) %in% t_i)
  message("\n### Writing output from `fit_model` in directory: ", 
          working_dir)
  dir.create(working_dir, showWarnings = FALSE, recursive = TRUE)
  capture.output(settings, file = file.path(working_dir, "settings.txt"))
  if (is.null(extra_args$extrapolation_list)) {
    message("\n### Making extrapolation-grid")
    extrapolation_args_default = list(Region = settings$Region, 
                                      strata.limits = settings$strata.limits, zone = settings$zone, 
                                      max_cells = settings$max_cells, DirPath = working_dir)
    extrapolation_args_input = combine_lists(input = extra_args, 
                                             default = extrapolation_args_default, args_to_use = formalArgs(make_extrapolation_info_BENS))
    extrapolation_list = do.call(what = make_extrapolation_info_BENS, 
                                 args = extrapolation_args_input)
  }
  else {
    extrapolation_args_input = NULL
    extrapolation_list = extra_args$extrapolation_list
  }
  if (is.null(extra_args$spatial_list)) {
    message("\n### Making spatial information")
    spatial_args_default = list(grid_size_km = settings$grid_size_km, 
                                n_x = settings$n_x, Method = settings$Method, Lon_i = Lon_i, 
                                Lat_i = Lat_i, Extrapolation_List = extrapolation_list, 
                                DirPath = working_dir, Save_Results = TRUE, fine_scale = settings$fine_scale, 
                                knot_method = settings$knot_method)
    spatial_args_input = combine_lists(input = extra_args, 
                                       default = spatial_args_default, args_to_use = c(formalArgs(make_spatial_info), 
                                                                                       formalArgs(INLA::inla.mesh.create)))
    spatial_list = do.call(what = make_spatial_info, args = spatial_args_input)
  }
  else {
    spatial_args_input = NULL
    spatial_list = extra_args$spatial_list
  }
  if (is.null(extra_args$data_list)) {
    message("\n### Making data object")
    if (missing(covariate_data)) 
      covariate_data = NULL
    if (missing(catchability_data)) 
      catchability_data = NULL
    data_args_default = list(Version = settings$Version, 
                             FieldConfig = settings$FieldConfig, OverdispersionConfig = settings$OverdispersionConfig, 
                             RhoConfig = settings$RhoConfig, VamConfig = settings$VamConfig, 
                             ObsModel = settings$ObsModel, c_iz = c_iz, b_i = b_i, 
                             a_i = a_i, v_i = v_i, s_i = spatial_list$knot_i - 
                               1, t_i = t_i, spatial_list = spatial_list, Options = settings$Options, 
                             Aniso = settings$use_anisotropy, X1config_cp = X1config_cp, 
                             X2config_cp = X2config_cp, covariate_data = covariate_data, 
                             X1_formula = X1_formula, X2_formula = X2_formula, 
                             Q1config_k = Q1config_k, Q2config_k = Q2config_k, 
                             catchability_data = catchability_data, Q1_formula = Q1_formula, 
                             Q2_formula = Q2_formula)
    data_args_input = combine_lists(input = extra_args, 
                                    default = data_args_default)
    data_list = do.call(what = make_data, args = data_args_input)
  }
  else {
    data_args_input = NULL
    data_list = extra_args$data_list
  }
  message("\n### Making TMB object")
  model_args_default = list(TmbData = data_list, RunDir = working_dir, 
                            Version = settings$Version, RhoConfig = settings$RhoConfig, 
                            loc_x = spatial_list$loc_x, Method = spatial_list$Method, 
                            build_model = build_model, framework = framework)
  model_args_input = combine_lists(input = extra_args, default = model_args_default, 
                                   args_to_use = formalArgs(make_model))
  tmb_list = do.call(what = make_model, args = model_args_input)
  if (run_model == FALSE | build_model == FALSE) {
    input_args = list(extra_args = extra_args, extrapolation_args_input = extrapolation_args_input, 
                      model_args_input = model_args_input, spatial_args_input = spatial_args_input, 
                      data_args_input = data_args_input)
    Return = list(data_frame = data_frame, extrapolation_list = extrapolation_list, 
                  spatial_list = spatial_list, data_list = data_list, 
                  tmb_list = tmb_list, year_labels = year_labels, 
                  years_to_plot = years_to_plot, category_names = category_names, 
                  settings = settings, input_args = input_args)
    class(Return) = "fit_model"
    return(Return)
  }
  if (silent == TRUE) 
    tmb_list$Obj$env$beSilent()
  if (test_fit == TRUE) {
    message("\n### Checking model at initial values")
    LogLike0 = tmb_list$Obj$fn(tmb_list$Obj$par)
    Gradient0 = tmb_list$Obj$gr(tmb_list$Obj$par)
    if (any(Gradient0 == 0)) {
      message("\n")
      stop("Please check model structure; some parameter has a gradient of zero at starting values\n", 
           call. = FALSE)
    }
    else {
      message("All fixed effects have a nonzero gradient")
    }
  }
  message("\n### Estimating parameters")
  optimize_args_default1 = list(lower = tmb_list$Lower, upper = tmb_list$Upper, 
                                loopnum = 1)
  optimize_args_default1 = combine_lists(default = optimize_args_default1, 
                                         input = extra_args, args_to_use = formalArgs(TMBhelper::fit_tmb))
  optimize_args_input1 = list(obj = tmb_list$Obj, savedir = NULL, 
                              newtonsteps = 0, bias.correct = FALSE, control = list(eval.max = 50000, 
                                                                                    iter.max = 50000, trace = 1), quiet = TRUE, getsd = FALSE)
  optimize_args_input1 = combine_lists(default = optimize_args_default1, 
                                       input = optimize_args_input1, args_to_use = formalArgs(TMBhelper::fit_tmb))
  parameter_estimates1 = do.call(what = TMBhelper::fit_tmb, 
                                 args = optimize_args_input1)
  if (exists("check_fit") & test_fit == TRUE) {
    problem_found = VAST::check_fit(parameter_estimates1)
    if (problem_found == TRUE) {
      message("\n")
      stop("Please change model structure to avoid problems with parameter estimates and then re-try; see details in `?check_fit`\n", 
           call. = FALSE)
    }
  }
  if ((use_new_epsilon == TRUE) & (settings$bias.correct == 
                                   TRUE) & (framework == "TMBad") & ("Index_ctl" %in% settings$vars_to_correct)) {
    settings$vars_to_correct = setdiff(settings$vars_to_correct, 
                                       c("Index_ctl", "Index_cyl"))
    if (length(settings$vars_to_correct) == 0) {
      settings$bias.correct = FALSE
    }
    settings$vars_to_correct = c(settings$vars_to_correct, 
                                 "eps_Index_ctl")
  }
  optimize_args_default2 = list(obj = tmb_list$Obj, lower = tmb_list$Lower, 
                                upper = tmb_list$Upper, savedir = working_dir, bias.correct = settings$bias.correct, 
                                newtonsteps = newtonsteps, bias.correct.control = list(sd = FALSE, 
                                                                                       split = NULL, nsplit = 1, vars_to_correct = settings$vars_to_correct), 
                                control = list(eval.max = 10000, iter.max = 10000, trace = 1), 
                                loopnum = 1, getJointPrecision = TRUE, start_time_elapsed = parameter_estimates1$time_for_run)
  optimize_args_input2 = combine_lists(input = extra_args, 
                                       default = optimize_args_default2, args_to_use = formalArgs(TMBhelper::fit_tmb))
  optimize_args_input2 = combine_lists(input = list(startpar = parameter_estimates1$par), 
                                       default = optimize_args_input2)
  parameter_estimates2 = do.call(what = TMBhelper::fit_tmb, 
                                 args = optimize_args_input2)
  if ((use_new_epsilon == TRUE) & (framework == "TMBad") & 
      ("eps_Index_ctl" %in% settings$vars_to_correct) & !is.null(parameter_estimates2$SD)) {
    message("\n### Applying faster epsilon bias-correction estimator")
    fit = list(parameter_estimates = parameter_estimates2, 
               tmb_list = tmb_list, input_args = list(model_args_input = model_args_input))
    parameter_estimates2$SD = apply_epsilon(fit)
  }
  if ("par" %in% names(parameter_estimates2)) {
    if (!is.null(tmb_list$Obj$env$intern) && tmb_list$Obj$env$intern == 
        TRUE) {
      Report = as.list(tmb_list$Obj$env$reportenv)
    }
    else {
      Report = tmb_list$Obj$report()
    }
    ParHat = tmb_list$Obj$env$parList(parameter_estimates2$par)
    Report = amend_output(Report = Report, TmbData = data_list, 
                          Map = tmb_list$Map, Sdreport = parameter_estimates2$SD, 
                          year_labels = year_labels, category_names = category_names, 
                          extrapolation_list = extrapolation_list)
  }
  else {
    Report = ParHat = "Model is not converged"
  }
  input_args = list(extra_args = extra_args, extrapolation_args_input = extrapolation_args_input, 
                    model_args_input = model_args_input, spatial_args_input = spatial_args_input, 
                    optimize_args_input1 = optimize_args_input1, optimize_args_input2 = optimize_args_input2, 
                    data_args_input = data_args_input)
  Return = list(data_frame = data_frame, extrapolation_list = extrapolation_list, 
                spatial_list = spatial_list, data_list = data_list, 
                tmb_list = tmb_list, parameter_estimates = parameter_estimates2, 
                Report = Report, ParHat = ParHat, year_labels = year_labels, 
                years_to_plot = years_to_plot, category_names = category_names, 
                settings = settings, input_args = input_args, X1config_cp = X1config_cp, 
                X2config_cp = X2config_cp, covariate_data = covariate_data, 
                X1_formula = X1_formula, X2_formula = X2_formula, Q1config_k = Q1config_k, 
                Q2config_k = Q1config_k, catchability_data = catchability_data, 
                Q1_formula = Q1_formula, Q2_formula = Q2_formula, total_time = Sys.time() - 
                  start_time)
  Return$effects = list()
  if (!is.null(catchability_data)) {
    catchability_data_full = data.frame(catchability_data, 
                                        linear_predictor = 0)
    Q1_formula_full = update.formula(Q1_formula, linear_predictor ~ 
                                       . + 0)
    call_Q1 = lm(Q1_formula_full, data = catchability_data_full)$call
    Q2_formula_full = update.formula(Q2_formula, linear_predictor ~ 
                                       . + 0)
    call_Q2 = lm(Q2_formula_full, data = catchability_data_full)$call
    Return$effects = c(Return$effects, list(call_Q1 = call_Q1, 
                                            call_Q2 = call_Q2, catchability_data_full = catchability_data_full))
  }
  if (!is.null(covariate_data)) {
    covariate_data_full = data.frame(covariate_data, linear_predictor = 0)
    X1_formula_full = update.formula(X1_formula, linear_predictor ~ 
                                       . + 0)
    call_X1 = lm(X1_formula_full, data = covariate_data_full)$call
    X2_formula_full = update.formula(X2_formula, linear_predictor ~ 
                                       . + 0)
    call_X2 = lm(X2_formula_full, data = covariate_data_full)$call
    Return$effects = c(Return$effects, list(call_X1 = call_X1, 
                                            call_X2 = call_X2, covariate_data_full = covariate_data_full))
  }
  Return$last.par.best = tmb_list$Obj$env$last.par.best
  class(Return) = "fit_model"
  return(Return)
}
