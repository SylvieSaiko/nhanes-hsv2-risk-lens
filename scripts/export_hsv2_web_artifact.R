#!/usr/bin/env Rscript

# Export the four prespecified population-weighted logistic models for the
# client-side research demonstrator. This script is intentionally a one-way
# privacy boundary: it reads ignored analysis objects but serializes only
# coefficients, preprocessing constants, aggregate validation summaries, and
# synthetic parity fixtures. It never writes participant rows, identifiers,
# survey design objects, fitted model frames, or row-level predictions.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Run scripts/install_dependencies.R first.")
}
if (!requireNamespace("xgboost", quietly = TRUE)) {
  stop("Package 'xgboost' is required. Run scripts/install_dependencies.R first.")
}

options(stringsAsFactors = FALSE)

script_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", script_args, value = TRUE)
if (length(script_arg) != 1L) {
  stop("Run this exporter with Rscript so its project-relative paths can be resolved.")
}
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
attempt_root <- dirname(dirname(dirname(script_path)))
analysis_root <- file.path(attempt_root, "outputs", "hsv2_prediction_1999_2016")
models_dir <- file.path(analysis_root, "models")
data_dir <- file.path(analysis_root, "data")
tables_dir <- file.path(analysis_root, "main_tables")
website_dir <- file.path(attempt_root, "website_hsv2")
artifact_dir <- file.path(website_dir, "artifacts")
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

model_manifest <- list(
  list(
    id = "model1", tier = 1L,
    name = "Baseline demographic model",
    shortName = "Baseline",
    bundle = "model1_demographics_models.rds",
    predictorTier = "Baseline Model (Model 1): sociodemographic",
    description = "Age and broad sociodemographic information."
  ),
  list(
    id = "model2", tier = 2L,
    name = "Lifestyle and access model",
    shortName = "Lifestyle",
    bundle = "model2_lifestyle_access_models.rds",
    predictorTier = "Model 2: lifestyle and access",
    description = "Baseline information plus healthcare access, smoking, alcohol, and drug-use history."
  ),
  list(
    id = "model3", tier = 3L,
    name = "Routine clinical model",
    shortName = "Routine clinical",
    bundle = "model3_routine_clinical_models.rds",
    predictorTier = "Model 3: routine clinical (sexual-history-sparing)",
    description = "Lifestyle/access information plus routine examination and laboratory measurements, without direct sexual-history questions."
  ),
  list(
    id = "model4", tier = 4L,
    name = "Sensitive-history model",
    shortName = "Sensitive history",
    bundle = "model4_sensitive_history_models.rds",
    predictorTier = "Model 4: clinical plus sensitive sexual history",
    description = "The full clinical model plus direct sexual-history and prior-STI information."
  )
)

required_files <- c(
  file.path(models_dir, vapply(model_manifest, `[[`, character(1), "bundle")),
  file.path(data_dir, "hsv2_no_prior_diagnosis_analysis.rds"),
  file.path(tables_dir, "table2_model_performance.csv"),
  file.path(tables_dir, "table7_adaptive_sensitive_question_gate.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Required analysis outputs are missing. Run scripts/run_all.R first:\n- ",
    paste(missing_files, collapse = "\n- ")
  )
}

round_number <- function(x, digits = 12L) {
  ifelse(is.finite(x), round(as.numeric(x), digits), NA_real_)
}

weighted_quantile <- function(x, w, probability) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  ord <- order(x, seq_along(x))
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) / sum(w) >= probability)[1]]
}

weighted_bin <- function(x, w, bins = 10L) {
  if (length(x) != length(w) || any(!is.finite(x)) || any(!is.finite(w) | w <= 0)) {
    stop("Weighted bins require finite predictions and strictly positive weights.")
  }
  ord <- order(x, seq_along(x))
  midpoint_cdf <- (cumsum(w[ord]) - 0.5 * w[ord]) / sum(w[ord])
  out <- integer(length(x))
  out[ord] <- pmin(bins, floor(midpoint_cdf * bins) + 1L)
  out
}

apply_preprocessor <- function(df, prep) {
  out <- df
  for (variable in prep$numeric_vars) {
    out[[paste0(variable, "_missing")]] <- as.integer(is.na(out[[variable]]))
    out[[variable]][is.na(out[[variable]])] <- prep$medians[[variable]]
  }
  for (variable in prep$categorical_vars) {
    value <- as.character(out[[variable]])
    value[is.na(value) | !value %in% prep$factor_levels[[variable]]] <- "Unknown"
    out[[variable]] <- factor(value, levels = prep$factor_levels[[variable]])
  }
  keep <- c(prep$numeric_vars, paste0(prep$numeric_vars, "_missing"), prep$categorical_vars)
  out[, keep, drop = FALSE]
}

make_matrix <- function(df) {
  matrix <- stats::model.matrix(~ ., data = df)
  matrix <- matrix[, colnames(matrix) != "(Intercept)", drop = FALSE]
  colnames(matrix) <- make.names(colnames(matrix), unique = TRUE)
  matrix
}

model_coefficients <- function(bundle) {
  beta <- bundle$glm$coefficients
  if (is.null(beta)) beta <- stats::coef(bundle$glm)
  beta[!is.finite(beta)] <- 0
  beta
}

case_to_raw_frame <- function(inputs, prep) {
  result <- list()
  for (variable in prep$numeric_vars) {
    if (identical(variable, "age_squared")) {
      age <- inputs[["age"]]
      result[[variable]] <- if (is.null(age) || length(age) == 0L || is.na(age)) {
        NA_real_
      } else {
        (as.numeric(age) - 35)^2
      }
    } else {
      value <- inputs[[variable]]
      result[[variable]] <- if (is.null(value) || length(value) == 0L || is.na(value)) {
        NA_real_
      } else {
        as.numeric(value)
      }
    }
  }
  for (variable in prep$categorical_vars) {
    value <- inputs[[variable]]
    result[[variable]] <- if (is.null(value) || length(value) == 0L || is.na(value)) {
      NA_character_
    } else {
      as.character(value)
    }
  }
  as.data.frame(result, check.names = FALSE)
}

score_fitted_bundle <- function(bundle, inputs) {
  prepared <- apply_preprocessor(case_to_raw_frame(inputs, bundle$prep), bundle$prep)
  matrix <- make_matrix(prepared)
  beta <- model_coefficients(bundle)
  design <- cbind("(Intercept)" = 1, matrix)
  missing_terms <- setdiff(names(beta), colnames(design))
  if (length(missing_terms) > 0L) {
    stop("Synthetic parity matrix is missing fitted terms: ", paste(missing_terms, collapse = ", "))
  }
  eta <- sum(design[1, names(beta)] * beta)
  list(logit = unname(eta), probability = unname(stats::plogis(eta)))
}

category_contribution <- function(bundle, variable, level) {
  prep <- bundle$prep
  reference_inputs <- list()
  for (numeric_variable in setdiff(prep$numeric_vars, "age_squared")) {
    reference_inputs[[numeric_variable]] <- prep$medians[[numeric_variable]]
  }
  for (categorical_variable in prep$categorical_vars) {
    reference_inputs[[categorical_variable]] <- prep$factor_levels[[categorical_variable]][[1]]
  }
  level_inputs <- reference_inputs
  level_inputs[[variable]] <- level
  reference_frame <- apply_preprocessor(case_to_raw_frame(reference_inputs, prep), prep)
  level_frame <- apply_preprocessor(case_to_raw_frame(level_inputs, prep), prep)
  reference_matrix <- make_matrix(reference_frame)
  level_matrix <- make_matrix(level_frame)
  missing_columns <- setdiff(colnames(reference_matrix), colnames(level_matrix))
  if (length(missing_columns) > 0L) {
    level_matrix <- cbind(
      level_matrix,
      matrix(0, nrow = 1, ncol = length(missing_columns), dimnames = list(NULL, missing_columns))
    )
  }
  level_matrix <- level_matrix[, colnames(reference_matrix), drop = FALSE]
  beta <- model_coefficients(bundle)
  matrix_beta <- beta[colnames(reference_matrix)]
  matrix_beta[!is.finite(matrix_beta)] <- 0
  sum((level_matrix[1, ] - reference_matrix[1, ]) * matrix_beta)
}

extract_numeric_coefficients <- function(bundle) {
  prep <- bundle$prep
  beta <- model_coefficients(bundle)
  input_variables <- setdiff(prep$numeric_vars, "age_squared")
  result <- lapply(input_variables, function(variable) {
    specification <- list(
      coefficient = round_number(beta[[variable]]),
      missingCoefficient = round_number(beta[[paste0(variable, "_missing")]]),
      median = round_number(prep$medians[[variable]])
    )
    if (identical(variable, "age") && "age_squared" %in% prep$numeric_vars) {
      specification$derivedTerms <- list(list(
        id = "age_squared",
        coefficient = round_number(beta[["age_squared"]]),
        missingCoefficient = round_number(beta[["age_squared_missing"]]),
        median = round_number(prep$medians[["age_squared"]]),
        transform = list(type = "centeredSquare", center = 35)
      ))
    }
    specification
  })
  names(result) <- input_variables
  result
}

extract_categorical_contributions <- function(bundle) {
  prep <- bundle$prep
  result <- lapply(prep$categorical_vars, function(variable) {
    levels <- prep$factor_levels[[variable]]
    contributions <- vapply(
      levels,
      function(level) category_contribution(bundle, variable, level),
      numeric(1)
    )
    contributions <- as.list(round_number(contributions))
    names(contributions) <- levels
    list(reference = levels[[1]], levels = contributions)
  })
  names(result) <- prep$categorical_vars
  result
}

score_exported_logistic_model <- function(model, inputs) {
  scoring <- model$scoring
  eta <- as.numeric(scoring$intercept)
  for (variable in names(scoring$numericCoefficients)) {
    specification <- scoring$numericCoefficients[[variable]]
    value <- inputs[[variable]]
    missing <- is.null(value) || length(value) == 0L || is.na(value)
    used_value <- if (missing) as.numeric(specification$median) else as.numeric(value)
    eta <- eta + as.numeric(specification$coefficient) * used_value
    if (missing) eta <- eta + as.numeric(specification$missingCoefficient)
    if (!is.null(specification$derivedTerms)) {
      for (term in specification$derivedTerms) {
        derived_value <- if (missing) {
          as.numeric(term$median)
        } else if (identical(term$transform$type, "centeredSquare")) {
          (used_value - as.numeric(term$transform$center))^2
        } else {
          stop("Unsupported derived-term transform in exported model.")
        }
        eta <- eta + as.numeric(term$coefficient) * derived_value
        if (missing) eta <- eta + as.numeric(term$missingCoefficient)
      }
    }
  }
  for (variable in names(scoring$categoricalContributions)) {
    specification <- scoring$categoricalContributions[[variable]]
    value <- inputs[[variable]]
    if (is.null(value) || length(value) == 0L || is.na(value) ||
        !as.character(value) %in% names(specification$levels)) {
      value <- "Unknown"
    }
    eta <- eta + as.numeric(specification$levels[[as.character(value)]])
  }
  list(logit = eta, probability = stats::plogis(eta))
}

to_float32 <- function(x) {
  readBin(
    writeBin(as.numeric(x), raw(), size = 4L),
    what = "numeric", n = length(x), size = 4L
  )
}

find_child_node <- function(node, child_id) {
  if (is.null(node$children) || length(node$children) == 0L) {
    stop("XGBoost split node has no children for child id ", child_id, ".")
  }
  child_ids <- vapply(node$children, function(child) as.integer(child$nodeid), integer(1))
  index <- match(as.integer(child_id), child_ids)
  if (is.na(index)) {
    stop("Could not find XGBoost child node ", child_id, " below node ", node$nodeid, ".")
  }
  node$children[[index]]
}

score_xgb_tree <- function(node, feature_row) {
  if (!is.null(node$leaf)) return(as.numeric(node$leaf))
  split_name <- as.character(node$split)
  feature_value <- feature_row[[split_name]]
  if (is.null(feature_value)) feature_value <- NA_real_
  feature_value <- suppressWarnings(to_float32(feature_value))
  split_value <- to_float32(node$split_condition)
  next_id <- if (is.na(feature_value)) {
    as.integer(node$missing)
  } else if (feature_value < split_value) {
    as.integer(node$yes)
  } else {
    as.integer(node$no)
  }
  score_xgb_tree(find_child_node(node, next_id), feature_row)
}

score_xgb_trees <- function(trees, feature_row) {
  sum(vapply(trees, score_xgb_tree, numeric(1), feature_row = feature_row))
}

align_matrix_columns <- function(matrix, feature_names) {
  missing_columns <- setdiff(feature_names, colnames(matrix))
  if (length(missing_columns) > 0L) {
    matrix <- cbind(
      matrix,
      base::matrix(
        0, nrow = nrow(matrix), ncol = length(missing_columns),
        dimnames = list(NULL, missing_columns)
      )
    )
  }
  unexpected_columns <- setdiff(colnames(matrix), feature_names)
  if (length(unexpected_columns) > 0L) {
    stop("Preprocessing produced unexpected feature columns: ", paste(unexpected_columns, collapse = ", "))
  }
  matrix[, feature_names, drop = FALSE]
}

make_bundle_matrix <- function(bundle, inputs, feature_names = NULL) {
  prepared <- apply_preprocessor(case_to_raw_frame(inputs, bundle$prep), bundle$prep)
  matrix <- make_matrix(prepared)
  if (!is.null(feature_names)) matrix <- align_matrix_columns(matrix, feature_names)
  matrix
}

make_profiles <- function(bundle) {
  prep <- bundle$prep
  numeric_variables <- setdiff(prep$numeric_vars, "age_squared")
  categorical_variables <- prep$categorical_vars

  reference <- list()
  missing <- list()
  lower <- list()
  elevated <- list()
  for (variable in numeric_variables) {
    reference[[variable]] <- as.numeric(prep$medians[[variable]])
    missing[[variable]] <- NA_real_
    lower[[variable]] <- as.numeric(prep$medians[[variable]])
    elevated[[variable]] <- as.numeric(prep$medians[[variable]])
  }
  for (variable in categorical_variables) {
    reference[[variable]] <- prep$factor_levels[[variable]][[1]]
    missing[[variable]] <- NA_character_
    lower[[variable]] <- prep$factor_levels[[variable]][[1]]
    elevated[[variable]] <- prep$factor_levels[[variable]][[1]]
  }

  lower_overrides <- list(
    age = 23, pir = 5, alcohol_drinks_day = 0, bmi = 22, waist = 75,
    sbp = 105, hba1c = 4.8, hdl = 65, total_chol = 160, alt = 15,
    ast = 18, alp = 55, ggt = 10, creatinine = 0.7, wbc = 5.5,
    hemoglobin = 14, platelets = 240, sex = "Male",
    race_ethnicity = "Non-Hispanic White", education = "College graduate or above",
    partnership = "Married/living with partner", insured = "Yes",
    current_smoker = "No", ever_hard_drug = "No", ever_injection_drug = "No",
    age_first_sex_group = "18 or older", partners_group = "1",
    condomless_sex = "None reported", prior_other_sti = "No",
    circumcision = "Circumcised"
  )
  elevated_overrides <- list(
    age = 47, pir = 0.8, alcohol_drinks_day = 4, bmi = 34, waist = 112,
    sbp = 148, hba1c = 6.8, hdl = 34, total_chol = 245, alt = 48,
    ast = 42, alp = 92, ggt = 85, creatinine = 1.3, wbc = 10.5,
    hemoglobin = 16, platelets = 340, sex = "Male",
    race_ethnicity = "Non-Hispanic Black", education = "High school/GED",
    partnership = "Not partnered", insured = "No", current_smoker = "Yes",
    ever_hard_drug = "Yes", ever_injection_drug = "Yes",
    age_first_sex_group = "Before 16", partners_group = "10 or more",
    condomless_sex = "Any", prior_other_sti = "Yes",
    circumcision = "Not circumcised"
  )
  for (variable in intersect(names(lower), names(lower_overrides))) {
    lower[[variable]] <- lower_overrides[[variable]]
  }
  for (variable in intersect(names(elevated), names(elevated_overrides))) {
    elevated[[variable]] <- elevated_overrides[[variable]]
  }

  list(
    synthetic_reference = reference,
    synthetic_missing = missing,
    synthetic_lower_pattern = lower,
    synthetic_higher_pattern = elevated
  )
}

export_preprocessor <- function(bundle, preprocessor_id, tier) {
  prep <- bundle$prep
  profiles <- make_profiles(bundle)
  reference_inputs <- profiles$synthetic_reference
  reference_matrix <- make_bundle_matrix(bundle, reference_inputs)
  feature_names <- colnames(reference_matrix)

  numeric_inputs <- lapply(setdiff(prep$numeric_vars, "age_squared"), function(variable) {
    specification <- list(
      valueFeature = variable,
      missingFeature = paste0(variable, "_missing"),
      median = round_number(prep$medians[[variable]])
    )
    if (!variable %in% feature_names || !specification$missingFeature %in% feature_names) {
      stop("Numeric preprocessing features are missing for ", variable, ".")
    }
    if (identical(variable, "age") && "age_squared" %in% prep$numeric_vars) {
      specification$derivedTerms <- list(list(
        feature = "age_squared",
        missingFeature = "age_squared_missing",
        median = round_number(prep$medians[["age_squared"]]),
        transform = list(type = "centeredSquare", center = 35)
      ))
    }
    specification
  })
  names(numeric_inputs) <- setdiff(prep$numeric_vars, "age_squared")

  categorical_inputs <- lapply(prep$categorical_vars, function(variable) {
    levels <- prep$factor_levels[[variable]]
    feature_by_level <- list()
    for (level in levels) {
      level_inputs <- reference_inputs
      level_inputs[[variable]] <- level
      level_matrix <- make_bundle_matrix(bundle, level_inputs, feature_names)
      changed <- which(abs(level_matrix[1, ] - reference_matrix[1, ]) > 0.5)
      if (length(changed) > 1L) {
        stop("Categorical level changed more than one encoded feature for ", variable, "/", level, ".")
      }
      if (length(changed) == 1L) {
        if (level_matrix[1, changed] != 1 || reference_matrix[1, changed] != 0) {
          stop("Unexpected treatment-contrast encoding for ", variable, "/", level, ".")
        }
        feature_by_level[[level]] <- colnames(level_matrix)[changed]
      }
    }
    list(
      reference = levels[[1]],
      unknownLevel = "Unknown",
      levels = unname(levels),
      featureByLevel = feature_by_level
    )
  })
  names(categorical_inputs) <- prep$categorical_vars

  list(
    id = preprocessor_id,
    tier = as.integer(tier),
    featureNames = unname(feature_names),
    numericInputs = numeric_inputs,
    categoricalInputs = categorical_inputs
  )
}

encode_exported_features <- function(preprocessor, inputs) {
  feature_names <- unlist(preprocessor$featureNames, use.names = FALSE)
  features <- stats::setNames(rep(0, length(feature_names)), feature_names)

  for (variable in names(preprocessor$numericInputs)) {
    specification <- preprocessor$numericInputs[[variable]]
    value <- inputs[[variable]]
    missing <- is.null(value) || length(value) == 0L || is.na(value)
    used_value <- if (missing) as.numeric(specification$median) else as.numeric(value)
    features[[specification$valueFeature]] <- used_value
    features[[specification$missingFeature]] <- as.integer(missing)
    if (!is.null(specification$derivedTerms)) {
      for (term in specification$derivedTerms) {
        derived_value <- if (missing) {
          as.numeric(term$median)
        } else if (identical(term$transform$type, "centeredSquare")) {
          (used_value - as.numeric(term$transform$center))^2
        } else {
          stop("Unsupported derived-term transform in exported preprocessor.")
        }
        features[[term$feature]] <- derived_value
        features[[term$missingFeature]] <- as.integer(missing)
      }
    }
  }

  for (variable in names(preprocessor$categoricalInputs)) {
    specification <- preprocessor$categoricalInputs[[variable]]
    value <- inputs[[variable]]
    levels <- unlist(specification$levels, use.names = FALSE)
    if (is.null(value) || length(value) == 0L || is.na(value) ||
        !as.character(value) %in% levels) {
      value <- specification$unknownLevel
    }
    feature <- specification$featureByLevel[[as.character(value)]]
    if (!is.null(feature)) features[[feature]] <- 1
  }
  features
}

score_fitted_xgb_bundle <- function(bundle, inputs, feature_names) {
  matrix <- make_bundle_matrix(bundle, inputs, feature_names)
  dmatrix <- xgboost::xgb.DMatrix(matrix)
  list(
    logit = unname(as.numeric(stats::predict(bundle$xgb, dmatrix, outputmargin = TRUE))),
    probability = unname(as.numeric(stats::predict(bundle$xgb, dmatrix)))
  )
}

score_exported_xgb_model <- function(model, preprocessors, inputs) {
  preprocessor <- preprocessors[[model$preprocessorId]]
  if (is.null(preprocessor)) stop("Missing exported preprocessor ", model$preprocessorId, ".")
  feature_row <- as.list(encode_exported_features(preprocessor, inputs))
  margin <- as.numeric(model$scoring$baseMargin) + score_xgb_trees(model$scoring$trees, feature_row)
  list(logit = margin, probability = stats::plogis(margin))
}

score_exported_model <- function(model, preprocessors, inputs) {
  kind <- as.character(model$scoring$kind)
  if (identical(kind, "logistic")) return(score_exported_logistic_model(model, inputs))
  if (identical(kind, "xgboost")) return(score_exported_xgb_model(model, preprocessors, inputs))
  stop("Unsupported exported scoring kind: ", kind)
}

make_validation_deciles <- function(bundle, validation_data, algorithm_name) {
  predictions <- bundle$predictions
  predictions <- predictions[
    predictions$set == "Temporal validation (2013-2016)" &
      predictions$algorithm == algorithm_name,
    c("SEQN", "prediction", "outcome"),
    drop = FALSE
  ]
  match_index <- match(predictions$SEQN, validation_data$SEQN)
  if (anyNA(match_index) || anyDuplicated(predictions$SEQN)) {
    stop("Could not align the locked validation predictions to survey weights uniquely.")
  }
  weights <- validation_data$pooled_mec_weight[match_index]
  if (any(!is.finite(weights) | weights <= 0)) {
    stop("Locked validation weights must be finite and strictly positive.")
  }
  bins <- weighted_bin(predictions$prediction, weights, 10L)
  thresholds <- vapply(
    seq(0.1, 0.9, by = 0.1),
    function(probability) weighted_quantile(predictions$prediction, weights, probability),
    numeric(1)
  )
  total_weight <- sum(weights)
  stats <- lapply(seq_len(10L), function(decile) {
    in_bin <- bins == decile
    list(
      decile = decile,
      lowerInclusive = round_number(min(predictions$prediction[in_bin])),
      upperInclusive = round_number(max(predictions$prediction[in_bin])),
      n = sum(in_bin),
      weightedShare = round_number(sum(weights[in_bin]) / total_weight),
      meanPredicted = round_number(stats::weighted.mean(predictions$prediction[in_bin], weights[in_bin])),
      observedRate = round_number(stats::weighted.mean(predictions$outcome[in_bin], weights[in_bin]))
    )
  })
  list(thresholds = unname(round_number(thresholds)), stats = stats)
}

performance_table <- utils::read.csv(
  file.path(tables_dir, "table2_model_performance.csv"),
  check.names = FALSE
)
adaptive_table <- utils::read.csv(
  file.path(tables_dir, "table7_adaptive_sensitive_question_gate.csv"),
  check.names = FALSE
)
if (!"algorithm" %in% names(adaptive_table)) {
  stop("Adaptive-gate table must identify the scoring algorithm.")
}
analysis_data <- readRDS(file.path(data_dir, "hsv2_no_prior_diagnosis_analysis.rds"))
validation_data <- analysis_data[analysis_data$cycle_index >= 8, c("SEQN", "pooled_mec_weight"), drop = FALSE]

make_performance <- function(predictor_tier, algorithm_name) {
  row <- performance_table[
    performance_table$predictor_tier == predictor_tier &
      performance_table$algorithm == algorithm_name &
      performance_table$set == "Temporal validation (2013-2016)",
    , drop = FALSE
  ]
  if (nrow(row) != 1L) stop("Expected one locked-validation performance row for ", predictor_tier)
  list(
    validationPeriod = "2013-2016",
    n = as.integer(row$n),
    events = as.integer(row$events),
    weightedPrevalence = round_number(row$weighted_prevalence),
    aurocWeighted = round_number(row$auroc_weighted),
    cIndexWeighted = round_number(row$auroc_weighted),
    aurocUnweighted = round_number(row$auroc_unweighted),
    aurocUnweightedCi = unname(round_number(c(row$auroc_unweighted_ci_low, row$auroc_unweighted_ci_high))),
    averagePrecisionWeighted = round_number(row$average_precision_weighted),
    brierWeighted = round_number(row$brier_weighted),
    calibrationIntercept = round_number(row$calibration_intercept),
    calibrationSlope = round_number(row$calibration_slope),
    top20Threshold = round_number(row$top20_threshold),
    top20Sensitivity = round_number(row$top20_sensitivity),
    top20Specificity = round_number(row$top20_specificity),
    top20Ppv = round_number(row$top20_ppv),
    top20Npv = round_number(row$top20_npv)
  )
}

attach_parity_cases <- function(model, bundle, preprocessors) {
  profiles <- make_profiles(bundle)
  # JSON tree dumps round leaf values, so accumulated XGBoost margins are
  # checked to two parts per million; logistic serialization remains tighter.
  tolerance <- if (identical(model$scoring$kind, "xgboost")) 2e-6 else 5e-9
  model$parityCases <- lapply(names(profiles), function(case_id) {
    fitted_score <- if (identical(model$scoring$kind, "xgboost")) {
      feature_names <- unlist(
        preprocessors[[model$preprocessorId]]$featureNames,
        use.names = FALSE
      )
      score_fitted_xgb_bundle(bundle, profiles[[case_id]], feature_names)
    } else {
      score_fitted_bundle(bundle, profiles[[case_id]])
    }
    exported_score <- score_exported_model(model, preprocessors, profiles[[case_id]])
    error <- max(abs(c(
      fitted_score$logit - exported_score$logit,
      fitted_score$probability - exported_score$probability
    )))
    if (!is.finite(error) || error > tolerance) {
      stop(
        "R-side parity failure before serialization for ", model$id, "/", case_id,
        ": error=", signif(error, 8)
      )
    }
    list(
      id = case_id,
      synthetic = TRUE,
      inputs = profiles[[case_id]],
      expectedLogit = round_number(fitted_score$logit),
      expectedProbability = round_number(fitted_score$probability)
    )
  })
  model
}

export_logistic_baseline <- function(manifest_row, bundle, preprocessors) {
  numeric_coefficients <- extract_numeric_coefficients(bundle)
  categorical_contributions <- extract_categorical_contributions(bundle)
  model <- list(
    id = "baseline_lr",
    name = "Baseline logistic model",
    shortName = "Baseline LR",
    tier = manifest_row$tier,
    algorithm = "Population-weighted logistic regression",
    role = "reference",
    description = "Reference logistic model using the six baseline demographic and socioeconomic inputs.",
    inputCount = length(numeric_coefficients) + length(categorical_contributions),
    preprocessorId = "tier1",
    scoring = list(
      kind = "logistic",
      intercept = round_number(model_coefficients(bundle)[["(Intercept)"]]),
      numericCoefficients = numeric_coefficients,
      categoricalContributions = categorical_contributions
    ),
    performance = make_performance(manifest_row$predictorTier, "Logistic regression"),
    deciles = make_validation_deciles(bundle, validation_data, "Logistic regression")
  )
  attach_parity_cases(model, bundle, preprocessors)
}

export_xgb_model <- function(manifest_row, bundle, preprocessors) {
  preprocessor_id <- paste0("tier", manifest_row$tier)
  preprocessor <- preprocessors[[preprocessor_id]]
  feature_names <- unlist(preprocessor$featureNames, use.names = FALSE)
  dump_text <- xgboost::xgb.dump(bundle$xgb, with_stats = FALSE, dump_format = "json")[[1]]
  trees <- jsonlite::fromJSON(dump_text, simplifyVector = FALSE)
  split_features <- unique(unlist(lapply(trees, function(tree) {
    collect <- function(node) {
      if (!is.null(node$leaf)) return(character())
      c(as.character(node$split), unlist(lapply(node$children, collect), use.names = FALSE))
    }
    collect(tree)
  }), use.names = FALSE))
  unexpected_splits <- setdiff(split_features, feature_names)
  if (length(unexpected_splits) > 0L) {
    stop("XGBoost dump refers to unknown features: ", paste(unexpected_splits, collapse = ", "))
  }

  profiles <- make_profiles(bundle)
  reference_features <- as.list(encode_exported_features(preprocessor, profiles$synthetic_reference))
  reference_native <- score_fitted_xgb_bundle(
    bundle, profiles$synthetic_reference, feature_names
  )
  base_margin <- reference_native$logit - score_xgb_trees(trees, reference_features)

  model <- list(
    id = paste0("xgb_model", manifest_row$tier),
    name = paste0("XGBoost Model ", manifest_row$tier, ": ", manifest_row$shortName),
    shortName = paste0("ML Model ", manifest_row$tier),
    tier = manifest_row$tier,
    algorithm = "XGBoost",
    role = "machine-learning",
    description = manifest_row$description,
    inputCount = length(preprocessor$numericInputs) + length(preprocessor$categoricalInputs),
    preprocessorId = preprocessor_id,
    scoring = list(
      kind = "xgboost",
      objective = "binary:logistic",
      baseMargin = round_number(base_margin),
      trees = trees
    ),
    training = list(
      bestIteration = as.integer(bundle$xgb_best_iteration),
      treeCount = length(trees),
      cycleGroupedCrossValidation = TRUE,
      parameters = list(
        evalMetric = "logloss", eta = 0.03, maxDepth = 3L,
        minChildWeight = 15, subsample = 0.8, colsampleBytree = 0.8,
        lambda = 1, alpha = 0, seed = 20260712L
      )
    ),
    performance = make_performance(manifest_row$predictorTier, "XGBoost"),
    deciles = make_validation_deciles(bundle, validation_data, "XGBoost")
  )
  attach_parity_cases(model, bundle, preprocessors)
}

input_metadata <- list(
  age = list(label = "Age", group = "Demographics", type = "number", unit = "years", min = 20, max = 49, step = 1, introducedIn = 1L, sensitivity = "standard"),
  pir = list(label = "Poverty-income ratio", group = "Demographics", type = "number", unit = "ratio", min = 0, max = 5, step = 0.1, introducedIn = 1L, sensitivity = "standard", help = "Family income divided by the federal poverty threshold; NHANES top-codes this measure at 5."),
  sex = list(label = "Sex", group = "Demographics", type = "select", introducedIn = 1L, sensitivity = "standard"),
  race_ethnicity = list(label = "Race and ethnicity", group = "Demographics", type = "select", introducedIn = 1L, sensitivity = "potentially-sensitive"),
  education = list(label = "Education", group = "Demographics", type = "select", introducedIn = 1L, sensitivity = "standard"),
  partnership = list(label = "Partner status", group = "Demographics", type = "select", introducedIn = 1L, sensitivity = "potentially-sensitive"),
  alcohol_drinks_day = list(label = "Alcohol on drinking days", group = "Lifestyle & access", type = "number", unit = "drinks/day", min = 0, max = 25, step = 1, introducedIn = 2L, sensitivity = "potentially-sensitive"),
  insured = list(label = "Health insurance", group = "Lifestyle & access", type = "select", introducedIn = 2L, sensitivity = "standard"),
  current_smoker = list(label = "Current smoking", group = "Lifestyle & access", type = "select", introducedIn = 2L, sensitivity = "potentially-sensitive"),
  ever_hard_drug = list(label = "Ever used cocaine, heroin, or methamphetamine", group = "Lifestyle & access", type = "select", introducedIn = 2L, sensitivity = "sensitive"),
  ever_injection_drug = list(label = "Ever injected non-prescribed drugs", group = "Lifestyle & access", type = "select", introducedIn = 2L, sensitivity = "sensitive"),
  bmi = list(label = "Body mass index", group = "Routine clinical", type = "number", unit = "kg/m²", min = 10, max = 80, step = 0.1, introducedIn = 3L, sensitivity = "clinical"),
  waist = list(label = "Waist circumference", group = "Routine clinical", type = "number", unit = "cm", min = 40, max = 200, step = 0.1, introducedIn = 3L, sensitivity = "clinical"),
  sbp = list(label = "Systolic blood pressure", group = "Routine clinical", type = "number", unit = "mm Hg", min = 70, max = 260, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  hba1c = list(label = "Hemoglobin A1c", group = "Routine clinical", type = "number", unit = "%", min = 3, max = 20, step = 0.1, introducedIn = 3L, sensitivity = "clinical"),
  hdl = list(label = "HDL cholesterol", group = "Routine clinical", type = "number", unit = "mg/dL", min = 5, max = 200, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  total_chol = list(label = "Total cholesterol", group = "Routine clinical", type = "number", unit = "mg/dL", min = 50, max = 500, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  alt = list(label = "Alanine aminotransferase (ALT)", group = "Routine clinical", type = "number", unit = "U/L", min = 1, max = 1000, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  ast = list(label = "Aspartate aminotransferase (AST)", group = "Routine clinical", type = "number", unit = "U/L", min = 1, max = 1000, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  alp = list(label = "Alkaline phosphatase", group = "Routine clinical", type = "number", unit = "U/L", min = 10, max = 1000, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  ggt = list(label = "Gamma-glutamyl transferase", group = "Routine clinical", type = "number", unit = "U/L", min = 1, max = 3000, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  creatinine = list(label = "Creatinine", group = "Routine clinical", type = "number", unit = "mg/dL", min = 0.1, max = 20, step = 0.1, introducedIn = 3L, sensitivity = "clinical"),
  wbc = list(label = "White blood cell count", group = "Routine clinical", type = "number", unit = "10³ cells/µL", min = 0.1, max = 100, step = 0.1, introducedIn = 3L, sensitivity = "clinical"),
  hemoglobin = list(label = "Hemoglobin", group = "Routine clinical", type = "number", unit = "g/dL", min = 3, max = 25, step = 0.1, introducedIn = 3L, sensitivity = "clinical"),
  platelets = list(label = "Platelet count", group = "Routine clinical", type = "number", unit = "10³ cells/µL", min = 10, max = 1000, step = 1, introducedIn = 3L, sensitivity = "clinical"),
  age_first_sex_group = list(label = "Age at first sex", group = "Sensitive history", type = "select", introducedIn = 4L, sensitivity = "sensitive"),
  partners_group = list(label = "Lifetime sexual partners", group = "Sensitive history", type = "select", introducedIn = 4L, sensitivity = "sensitive"),
  condomless_sex = list(label = "Condomless sex", group = "Sensitive history", type = "select", introducedIn = 4L, sensitivity = "sensitive"),
  prior_other_sti = list(label = "Prior non-HSV sexually transmitted infection", group = "Sensitive history", type = "select", introducedIn = 4L, sensitivity = "sensitive"),
  circumcision = list(label = "Circumcision", group = "Sensitive history", type = "select", introducedIn = 4L, sensitivity = "sensitive")
)

bundles <- lapply(
  model_manifest,
  function(manifest_row) readRDS(file.path(models_dir, manifest_row$bundle))
)
names(bundles) <- paste0("tier", seq_along(bundles))
preprocessors <- lapply(seq_along(model_manifest), function(index) {
  export_preprocessor(
    bundles[[index]],
    preprocessor_id = paste0("tier", index),
    tier = model_manifest[[index]]$tier
  )
})
names(preprocessors) <- paste0("tier", seq_along(preprocessors))

models <- c(
  list(export_logistic_baseline(model_manifest[[1]], bundles[[1]], preprocessors)),
  lapply(seq_along(model_manifest), function(index) {
    export_xgb_model(model_manifest[[index]], bundles[[index]], preprocessors)
  })
)

# Attach factor options to input metadata from the largest tier that contains
# each input. This avoids duplicating UI labels in the hand-written metadata.
for (preprocessor in rev(preprocessors)) {
  for (variable in names(preprocessor$categoricalInputs)) {
    if (is.null(input_metadata[[variable]]$options)) {
      input_metadata[[variable]]$options <- unname(
        unlist(preprocessor$categoricalInputs[[variable]]$levels, use.names = FALSE)
      )
    }
  }
}

development_row <- performance_table[
  performance_table$predictor_tier == model_manifest[[1]]$predictorTier &
    performance_table$algorithm == "Logistic regression" &
    performance_table$set == "Development (1999-2012)",
  , drop = FALSE
]
validation_row <- performance_table[
  performance_table$predictor_tier == model_manifest[[1]]$predictorTier &
    performance_table$algorithm == "Logistic regression" &
    performance_table$set == "Temporal validation (2013-2016)",
  , drop = FALSE
]
xgb_adaptive <- adaptive_table[adaptive_table$algorithm == "XGBoost", , drop = FALSE]
adaptive_row <- xgb_adaptive[
  which.min(abs(xgb_adaptive$target_sensitive_question_fraction - 0.6)),
  , drop = FALSE
]
if (nrow(adaptive_row) != 1L ||
    abs(adaptive_row$target_sensitive_question_fraction - 0.6) > 1e-8) {
  stop("Could not identify the prespecified XGBoost 60% adaptive-gate row.")
}

artifact <- list(
  schemaVersion = "2.0.0",
  metadata = list(
    title = "HSV-2 five-model information-tier risk models",
    artifactVersion = "attempt7-five-model-2026-07",
    algorithms = c("Population-weighted logistic regression", "XGBoost"),
    outcome = "HSV-2 seropositivity on NHANES serology",
    eligibility = list(
      ageMin = 20L,
      ageMax = 49L,
      geography = "United States",
      priorGenitalHerpesDiagnosis = "Excluded from the primary analysis",
      publicDataYears = "1999-2016"
    ),
    cohort = list(
      totalN = as.integer(development_row$n + validation_row$n),
      totalEvents = as.integer(development_row$events + validation_row$events),
      development = list(period = "1999-2012", n = as.integer(development_row$n), events = as.integer(development_row$events)),
      temporalValidation = list(period = "2013-2016", n = as.integer(validation_row$n), events = as.integer(validation_row$events))
    ),
    weighting = "Pooled NHANES Mobile Examination Center weights; fitting and reported validation metrics are population weighted.",
    adaptiveThreshold = list(
      algorithm = "XGBoost",
      basedOnModel = "xgb_model3",
      escalationModel = "xgb_model4",
      developmentGateProbability = round_number(adaptive_row$development_gate_threshold),
      targetSensitiveQuestionFraction = round_number(adaptive_row$target_sensitive_question_fraction),
      validationSensitiveQuestionFraction = round_number(adaptive_row$temporal_sensitive_question_fraction),
      validationQuestionnaireSparingFraction = round_number(adaptive_row$temporal_questionnaire_sparing_fraction),
      fractionOfFullAucGainRecovered = round_number(adaptive_row$fraction_of_full_auc_gain_recovered)
    ),
    intendedUse = "Research demonstration only; not a diagnosis, screening recommendation, treatment recommendation, or independently validated clinical device.",
    privacy = list(
      rowLevelDataIncluded = FALSE,
      participantIdentifiersIncluded = FALSE,
      modelFramesIncluded = FALSE,
      contents = "Aggregate coefficients, XGBoost tree structures, preprocessing constants, aggregate temporal-validation summaries, and synthetic parity fixtures only."
    ),
    parityCaseNotice = "Parity profiles are deterministic synthetic QA fixtures and are not NHANES participants or clinical vignettes."
  ),
  inputs = input_metadata,
  preprocessors = preprocessors,
  models = models
)

allowed_model_fields <- c(
  "id", "name", "shortName", "tier", "algorithm", "role", "description",
  "inputCount", "preprocessorId", "scoring", "training", "performance",
  "deciles", "parityCases"
)
for (model in artifact$models) {
  unexpected <- setdiff(names(model), allowed_model_fields)
  if (length(unexpected) > 0L) {
    stop("Privacy whitelist rejected unexpected model fields: ", paste(unexpected, collapse = ", "))
  }
}
allowed_preprocessor_fields <- c(
  "id", "tier", "featureNames", "numericInputs", "categoricalInputs"
)
for (preprocessor in artifact$preprocessors) {
  unexpected <- setdiff(names(preprocessor), allowed_preprocessor_fields)
  if (length(unexpected) > 0L) {
    stop("Privacy whitelist rejected unexpected preprocessor fields: ", paste(unexpected, collapse = ", "))
  }
}

json_path <- file.path(artifact_dir, "hsv2-models.json")
js_path <- file.path(website_dir, "model-data.js")
json_text <- jsonlite::toJSON(
  artifact,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA,
  na = "null",
  null = "null"
)

forbidden_json_patterns <- c(
  '"SEQN"\\s*:',
  '"predictions"\\s*:',
  '"pooled_mec_weight"\\s*:',
  '"psu_pool"\\s*:',
  '"strata_pool"\\s*:',
  '"model.frame"\\s*:'
)
for (pattern in forbidden_json_patterns) {
  if (grepl(pattern, json_text, perl = TRUE)) {
    stop("Privacy audit rejected serialized artifact pattern: ", pattern)
  }
}

writeLines(json_text, json_path, useBytes = TRUE)
writeLines(
  c(
    "/* Generated by website_hsv2/scripts/export_hsv2_web_artifact.R. Do not edit. */",
    paste0("window.HSV2_MODELS = ", json_text, ";")
  ),
  js_path,
  useBytes = TRUE
)

# Validate scoring again after a JSON round trip. The tolerance accounts only
# for deliberate 12-decimal serialization of coefficients and expectations.
round_trip <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
for (model_index in seq_along(round_trip$models)) {
  round_trip_model <- round_trip$models[[model_index]]
  tolerance <- if (identical(round_trip_model$scoring$kind, "xgboost")) 2e-6 else 5e-9
  for (parity_case in round_trip_model$parityCases) {
    score <- score_exported_model(
      round_trip_model,
      round_trip$preprocessors,
      parity_case$inputs
    )
    error <- max(abs(c(
      score$logit - as.numeric(parity_case$expectedLogit),
      score$probability - as.numeric(parity_case$expectedProbability)
    )))
    if (!is.finite(error) || error > tolerance) {
      stop("Serialized parity failure for ", round_trip_model$id, "/", parity_case$id)
    }
  }
}

message("Exported browser-safe HSV-2 model artifact:")
message("- ", normalizePath(json_path))
message("- ", normalizePath(js_path))
message("R-side parity: 20/20 deterministic synthetic cases passed.")
message("Privacy audit: no participant rows, identifiers, weights, model frames, or row predictions serialized.")
