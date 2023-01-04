#' @describeIn tidy_stats tidy_stats method for class 'rma.uni'
#' @export
tidy_stats.rma.uni <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  method = "Meta-Analysis via Linear Models"
  if (x$model == "rma.ls") {
    method = "Location-Scale Model"
  } else {
    if (is.element(x$method, c("FE", "EE", "CE"))) {
      if (x$int.only) {
        method = sapply(
          x$method,
          switch,
          "FE" = "Fixed-Effects Model",
          "EE" = "Equal-Effects Model",
          "CE" = "Common-Effects Model",
          USE.NAMES = FALSE
        )
      } else {
        method = "Fixed-Effects with Moderators Model"
      }
    } else {
      if (x$int.only) {
        method = "Random-Effects Model"
      } else {
        method = "Mixed-Effects Model"
      }
    }
  }
  
  if (inherits(x, "rma.uni.trimfill")) {
    analysis <- list(name = "rma_uni_trimfill",
      method = method)
  } else {
    analysis <- list(name = deparse(x$call[[2]]),
      method = method)
  }
  
  # Model fit
  if (x$model == "rma.uni" || x$model == "rma.uni.selmodel") {
    # Create a group and statistics list for the model fit statistics
    group <- list(name = "Model")
    statistics <- list()
    
    # Extract and add statistics to the statistics list
    statistics <- add_statistic(statistics, "I squared", x$I2, "I²")
    statistics <- add_statistic(statistics, "H squared", x$H2, "H²")
    statistics <- add_statistic(statistics, "R squared", x$R2, "R²")
    if (!is.element(x$method, c("FE", "EE", "CE"))) {
      statistics <-
        add_statistic(statistics, "Tau squared", x$tau2, "τ²")
      statistics <-
        add_statistic(statistics, "Τau", x$tau2 ** 0.5, "τ")
    }
    # Add statistics to the group
    group$statistics <- statistics
    
    # Add the model group (if any) to a groups element on the analysis
    if (length(group$statistics) > 1) {
      analysis$groups <- append(analysis$groups, list(group))
    }
  }
  
  # Heterogeneity
  # Create a group and statistics list for the Test for (Residual) Heterogeneity
  if (!is.na(x$QE)) {
    statistics <- list()
    if (x$int.only) {
      group <- list(name = "Heterogeneity")
      statistics <-
        add_statistic(statistics, "statistic", x$QE, "Q")
    } else {
      group <- list(name = "Residual Heterogeneity")
      statistics <-
        add_statistic(statistics, "statistic", x$QE, "QE")
    }
    statistics <- add_statistic(statistics, "df", x$k - x$p)
    statistics <- add_statistic(statistics, "p", x$QEp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if (x$model == "rma.uni.selmodel" && !is.na(x$LRT.tau2)) {
    statistics <- list()
    if (x$int.only) {
      group <- list(name = "Heterogeneity")
    } else {
      group <- list(name = "Residual Heterogeneity")
    }
    statistics <-
      add_statistic(statistics, "statistic", x$LRT.tau2, "LRT")
    statistics <- add_statistic(statistics, "df", 1)
    statistics <- add_statistic(statistics, "p", x$LRTp.tau2)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  # Moderators
  # Create a group and statistics list for the Test of Moderators (if any)
  if (x$p > 1L && !is.na(x$QM)) {
    statistics <- list()
    if (x$model == "rma.ls") {
      group <- list(name = "Test of Location Coefficients")
    } else {
      group <- list(name = "Test of Moderators")
    }
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      statistics <- add_statistic(statistics, "statistic", x$QM, "F")
      statistics <- add_statistic(statistics, "df numerator",
        x$QMdf[1], "df", "num.")
      statistics <- add_statistic(statistics, "df denominator",
        x$QMdf[2], "df", "den.")
    } else {
      statistics <- add_statistic(statistics, "statistic", x$QM, "QM")
      statistics <- add_statistic(statistics, "df", x$QMdf[1])
    }
    statistics <- add_statistic(statistics, "p", x$QMp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  # Create a groups list for the coefficients
  if (x$model == "rma.uni" || x$model == "rma.uni.selmodel") {
    groups <- list(name = "Coefficients")
  } else {
    groups <- list(name = "Coefficients (Location)")
  }
  # Extract statistics of the coefficients
  coefs_v <- stats::coef(x)
  if (is.list(coefs_v) && "beta" %in% names(coefs_v)) {
    coefs_v = coefs_v$beta
  }
  if (is.element(x$test, c("knha", "adhoc", "t"))) {
    stat_type = "t"
  } else{
    stat_type = "z"
  }
  # Loop over the coefficients and add statistics to a group list
  for (i in 1:length(coefs_v)) {
    # Create a new group list
    group <- list()
    
    # Add the name and type of the coefficient
    group$name <- names(coefs_v)[i]
    
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x$beta[i],
        "b",
        interval = "CI",
        level = .95,
        lower = x$ci.lb[i],
        upper = x$ci.ub[i]
      )
    statistics <-
      add_statistic(statistics, "SE", x$se[i])
    statistics <-
      add_statistic(statistics, "statistic", x$zval[i], stat_type)
    statistics <- add_statistic(statistics, "p", x$pval[i])
    
    # Add statistics to the group
    group$statistics <- statistics
    
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  # Create a groups list for the second group of coefficients (if any)
  if (x$model == "rma.ls") {
    if (x$q > 1L && !is.na(x$QS)) {
      statistics <- list()
      group <- list(name = "Test of Scale Coefficients")
      if (is.element(x$test, c("knha", "adhoc", "t"))) {
        statistics <- add_statistic(statistics, "statistic", x$QS, "F")
        statistics <- add_statistic(statistics, "df numerator",
          x$QSdf[[1]], "df", "num.")
        statistics <- add_statistic(statistics, "df denominator",
          x$QSdf[[2]], "df", "den.")
      } else {
        statistics <- add_statistic(statistics, "statistic", x$QS, "QS")
        statistics <- add_statistic(statistics, "df", x$QSdf[1])
      }
      
      statistics <- add_statistic(statistics, "p", x$QSp)
      # Add statistics to the group
      group$statistics <- statistics
      # Add the model group to a groups element on the analysis
      analysis$groups <- append(analysis$groups, list(group))
    }
    
    if (is.list(coefs_v) && "alpha" %in% names(coefs_v)) {
      coefs_a = coefs_v$alpha
    } else if (class(x$alpha)[1] == "matrix") {
      coefs_a = setNames(c(x$alpha), rownames(x$alpha))
    } else {
      coefs_a = coefs_v
    }
    groups <- list(name = "Coefficients (Scale)")
    # Loop over the coefficients and add statistics to a group list
    for (i in 1:length(coefs_a)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- names(coefs_a)[i]
      # Create a new statistics list
      statistics <- list()
      statistics <-
        add_statistic(
          statistics,
          "estimate",
          x$alpha[i],
          "b",
          interval = "CI",
          level = .95,
          lower = x$ci.lb.alpha[i],
          upper = x$ci.ub.alpha[i]
        )
      statistics <-
        add_statistic(statistics, "SE", x$se.alpha[i])
      statistics <-
        add_statistic(statistics, "statistic", x$zval.alpha[i], stat_type)
      statistics <-
        add_statistic(statistics, "df", x$ddf.alpha[i])
      statistics <-
        add_statistic(statistics, "p", x$pval.alpha[i])
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
  }
  
  if (x$model == "rma.uni.selmodel") {
    if (!is.na(x$LRT)) {
      statistics <- list()
      group <- list(name = "Test for Selection Model Parameters")
      statistics <-
        add_statistic(statistics, "statistic", x$LRT, "LRT")
      statistics <- add_statistic(statistics, "df", x$LRTdf[1])
      statistics <- add_statistic(statistics, "p", x$LRTp)
      # Add statistics to the group
      group$statistics <- statistics
      # Add the model group to a groups element on the analysis
      analysis$groups <- append(analysis$groups, list(group))
    }
    
    groups <- list(name = "Coefficients (Selection)")
    if (x$type == "stepfun") {
      rnames <- rownames(x$ptable)
    } else {
      rnames <- paste0("δ ", seq_along(x$delta))
    }
    # Loop over the coefficients and add statistics to a group list
    for (i in 1:length(x$delta)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- rnames[i]
      # Create a new statistics list
      statistics <- list()
      if (x$type == "stepfun") {
        statistics <-
          add_statistic(statistics, "count", x$ptable$k[i][[1]], 'k')
      }
      statistics <-
        add_statistic(
          statistics,
          "estimate",
          x$delta[i][[1]],
          "b",
          interval = "CI",
          level = .95,
          lower = ifelse(is.na(x$ci.lb.delta[i][[1]]), "-", x$ci.lb.delta[i][[1]]),
          upper = ifelse(is.na(x$ci.ub.delta[i][[1]]), "-", x$ci.ub.delta[i][[1]])
        )
      statistics <-
        add_statistic(statistics, "SE", 
          ifelse(is.na(x$se.delta[i][[1]]), "-", x$se.delta[i][[1]]))
      statistics <-
        add_statistic(statistics, "statistic", 
          ifelse(is.na(x$zval.delta[i][[1]]), "-", x$zval.delta[i][[1]]), "z")
      statistics <- add_statistic(statistics, "p", 
        ifelse(is.na(x$pval.delta[i][[1]]), "-", x$pval.delta[i][[1]]))
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
  }
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'rma.mh'
#' @export
tidy_stats.rma.mh <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(name = deparse(x$call[[2]]),
    method = "Equal-Effects Model")
  
  # Model fit
  # Create a group and statistics list for the model fit statistics
  group <- list(name = "Model")
  statistics <- list()
  # Extract and add statistics to the statistics list
  statistics <- add_statistic(statistics, "I squared", x$I2, "I²")
  statistics <- add_statistic(statistics, "H squared", x$H2, "H²")
  # Add statistics to the group
  group$statistics <- statistics
  # Add the model group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group))
  
  # Heterogeneity
  # Create a group and statistics list for the Test for (Residual) Heterogeneity
  if (!is.na(x$QE)) {
    statistics <- list()
    group <- list(name = "Heterogeneity")
    statistics <-
      add_statistic(statistics, "statistic", x$QE, "Q")
    statistics <- add_statistic(statistics, "df", x$k - x$p)
    statistics <- add_statistic(statistics, "p", x$QEp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if (is.element(x$measure, c("OR", "RR", "IRR"))) {
    # Loop over the coefficients and add statistics to a group list
    groups <- list(name = "Coefficients (log scale)")
    for (i in 1:length(x$beta)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- names(x$beta)[i]
      # Create a new statistics list
      statistics <- list()
      statistics <-
        add_statistic(
          statistics,
          "estimate",
          x$beta[i][[1]],
          "b",
          interval = "CI",
          level = .95,
          lower = x$ci.lb[i],
          upper = x$ci.ub[i]
        )
      statistics <-
        add_statistic(statistics, "SE", x$se[i])
      statistics <-
        add_statistic(statistics, "statistic", x$zval[i], "z")
      statistics <- add_statistic(statistics, "p", x$pval[i])
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
    
    groups <- list(name =paste0("Coefficients (", x$measure, " scale)"))
    for (i in 1:length(x$beta)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- names(x$beta)[i]
      # Create a new statistics list
      statistics <- list()
      statistics <-
        add_statistic(
          statistics,
          "estimate",
          exp(x$beta[i][[1]]),
          "b",
          interval = "CI",
          level = .95,
          lower = exp(x$ci.lb[i]),
          upper = exp(x$ci.ub[i])
        )
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
    
    if (x$measure == "OR") {
      if (!is.na(x$MH)) {
        statistics <- list()
        group <- list(name = "Cochran-Mantel-Haenszel Test")
        statistics <-
          add_statistic(statistics, "statistic", x$MH, "CMH")
        statistics <- add_statistic(statistics, "df", 1)
        statistics <- add_statistic(statistics, "p", x$MHp)
        # Add statistics to the group
        group$statistics <- statistics
        # Add the model group to a groups element on the analysis
        analysis$groups <- append(analysis$groups, list(group))
      }
      
      if (!is.na(x$TA)) {
        statistics <- list()
        group <- list(name = "Tarone's Test for Heterogeneity")
        statistics <-
          add_statistic(statistics, "statistic", x$TA, "χ²")
        statistics <- add_statistic(statistics, "df", x$k.pos - 1)
        statistics <- add_statistic(statistics, "p", x$TAp)
        # Add statistics to the group
        group$statistics <- statistics
        # Add the model group to a groups element on the analysis
        analysis$groups <- append(analysis$groups, list(group))
      }
    }
    if (x$measure == "IRR") {
      if (!is.na(x$MH)) {
        statistics <- list()
        group <- list(name = "Mantel-Haenszel Test")
        statistics <-
          add_statistic(statistics, "statistic", x$MH, "CMH")
        statistics <- add_statistic(statistics, "df", 1)
        statistics <- add_statistic(statistics, "p", x$MHp)
        # Add statistics to the group
        group$statistics <- statistics
        # Add the model group to a groups element on the analysis
        analysis$groups <- append(analysis$groups, list(group))
      }
    }
    
  } else {
    groups <- list(name = "Coefficients")
    for (i in 1:length(x$beta)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- names(x$beta)[i]
      # Create a new statistics list
      statistics <- list()
      statistics <-
        add_statistic(
          statistics,
          "estimate",
          x$beta[i][[1]],
          "b",
          interval = "CI",
          level = .95,
          lower = x$ci.lb[i],
          upper = x$ci.ub[i]
        )
      statistics <-
        add_statistic(statistics, "SE", x$se[i])
      statistics <-
        add_statistic(statistics, "statistic", x$zval[i], "z")
      statistics <- add_statistic(statistics, "p", x$pval[i])
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
  }
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'rma.peto'
#' @export
tidy_stats.rma.peto <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(name = deparse(x$call[[2]]),
    method = "Equal-Effects Model")
  
  # Model fit
  # Create a group and statistics list for the model fit statistics
  group <- list(name = "Model")
  statistics <- list()
  # Extract and add statistics to the statistics list
  statistics <- add_statistic(statistics, "I squared", x$I2, "I²")
  statistics <- add_statistic(statistics, "H squared", x$H2, "H²")
  # Add statistics to the group
  group$statistics <- statistics
  # Add the model group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group))
  
  # Heterogeneity
  # Create a group and statistics list for the Test for (Residual) Heterogeneity
  if (!is.na(x$QE)) {
    statistics <- list()
    group <- list(name = "Heterogeneity")
    statistics <-
      add_statistic(statistics, "statistic", x$QE, "Q")
    statistics <- add_statistic(statistics, "df", x$k - x$p)
    statistics <- add_statistic(statistics, "p", x$QEp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  # Loop over the coefficients and add statistics to a group list
  groups <- list(name = "Coefficients (log scale)")
  for (i in 1:length(x$beta)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- names(x$beta)[i]
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x$beta[i][[1]],
        "b",
        interval = "CI",
        level = .95,
        lower = x$ci.lb[i],
        upper = x$ci.ub[i]
      )
    statistics <-
      add_statistic(statistics, "SE", x$se[i])
    statistics <-
      add_statistic(statistics, "statistic", x$zval[i], "z")
    statistics <- add_statistic(statistics, "p", x$pval[i])
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  groups <- list(name = "Coefficients (OR scale)")
  for (i in 1:length(x$beta)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- names(x$beta)[i]
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        exp(x$beta[i][[1]]),
        "b",
        interval = "CI",
        level = .95,
        lower = exp(x$ci.lb[i]),
        upper = exp(x$ci.ub[i])
      )
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'rma.glmm'
#' @export
tidy_stats.rma.glmm <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  if (is.element(x$method, c("FE", "EE", "CE"))) {
    if (x$int.only) {
      method = sapply(
        x$method,
        switch,
        "FE" = "Fixed-Effects Model",
        "EE" = "Equal-Effects Model",
        "CE" = "Common-Effects Model",
        USE.NAMES = FALSE
      )
    } else {
      method = "Fixed-Effects with Moderators Model"
    }
  } else {
    if (x$int.only) {
      method = "Random-Effects Model"
    } else {
      method = "Mixed-Effects Model"
    }
  }
  if (is.element(x$measure, c("OR", "IRR"))) {
    
    if (x$model == "UM.FS")
      model_type = "Unconditional Model with Fixed Study Effects"
    if (x$model == "UM.RS")
      model_type = "Unconditional Model with Random Study Effects"
    if (x$model == "CM.AL")
      model_type = "Conditional Model with Approximate Likelihood"
    if (x$model == "CM.EL")
      model_type = "Conditional Model with Exact Likelihood"
  }
  analysis <- list(name = deparse(x$call[[2]]),
    method = paste0(method, " (", model_type, ")"))
  
  statistics <- list()
  if (!is.element(x$method, c("FE", "EE", "CE"))) {
    # Model fit
    # Create a group and statistics list for the model fit statistics
    # Extract and add statistics to the statistics list
    statistics <-
      add_statistic(statistics, "I squared", x$I2, "I²")
    statistics <-
      add_statistic(statistics, "H squared", x$H2, "H²")
    statistics <-
      add_statistic(statistics, "Tau squared", x$tau2, "τ²")
    statistics <-
      add_statistic(statistics, "Τau", x$tau2 ** 0.5, "τ")
  }
  
  if (!is.na(x$sigma2)) {
    statistics <-
      add_statistic(statistics, "sigma squared", x$sigma2, "σ²")
    statistics <-
      add_statistic(statistics, "sigma", sqrt(x$sigma2), "σ")
  }
  
  if (length(statistics) > 0) {
    group <- list(name = "Model")
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  if (x$int.only) {
    h_type = "Heterogeneity"
  } else {
    h_type = "Residual Heterogeneity"
  }
  
  # Heterogeneity
  # Create a group and statistics list for the Test for (Residual) Heterogeneity
  if (!is.na(x$QE.Wld)) {
    group <- list(name = paste(h_type, "(Wald-type test)"))
    statistics <- list()
    statistics <-
      add_statistic(statistics, "statistic", x$QE.Wld, "W")
    statistics <- add_statistic(statistics, "df", x$QE.df)
    statistics <- add_statistic(statistics, "p", x$QEp.Wld)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  if (!is.na(x$QE.LRT)) {
    group <- list(name = paste(h_type, "(likelihood ratio test)"))
    statistics <- list()
    statistics <-
      add_statistic(statistics, "statistic", x$QE.LRT, "LRT")
    statistics <- add_statistic(statistics, "df", x$QE.df)
    statistics <- add_statistic(statistics, "p", x$QEp.LRT)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if (x$p > 1L && !is.na(x$QM)) {
    statistics <- list()
    group <- list(name = "Test of Moderators")
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      statistics <-
        add_statistic(statistics, "statistic", x$QM, "F")
      statistics <- add_statistic(statistics, "df numerator",
        x$QSdf[[1]], "df", "num.")
      statistics <- add_statistic(statistics, "df denominator",
        x$QSdf[[2]], "df", "den.")
    } else {
      statistics <-
        add_statistic(statistics, "statistic", x$QM, "QM")
      statistics <- add_statistic(statistics, "df", x$QMdf[1])
    }
    statistics <- add_statistic(statistics, "p", x$QEp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  # Create a groups list for the coefficients
  groups <- list(name = "Coefficients")
  if (is.element(x$test, c("knha", "adhoc", "t"))) {
    stat_type = "t"
  } else{
    stat_type = "z"
  }
  # Loop over the coefficients and add statistics to a group list
  for (i in 1:length(x$beta)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- rownames(x$beta)[i]
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x$beta[i],
        "b",
        interval = "CI",
        level = .95,
        lower = x$ci.lb[i],
        upper = x$ci.ub[i]
      )
    statistics <-
      add_statistic(statistics, "SE", x$se[i])
    statistics <-
      add_statistic(statistics, "statistic", x$zval[i], stat_type)
    statistics <- add_statistic(statistics, "p", x$pval[i])
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'rma.mv'
#' @export
tidy_stats.rma.mv <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(name = deparse(x$call[[2]]),
    method = paste0("Multivariate Meta-Analysis Model (", x$method, ")"))
  
  # Model fit
  # Create a group and statistics list for the model fit statistics
  group <- list(name = "Model")
  
  if (x$withS || x$withG || x$withH) {
    tau2 <- x$tau2
    tau <- sqrt(x$tau2)
    if (x$withS) {
      vc <- cbind(
        estim = x$sigma2,
        sqrt = sqrt(x$sigma2),
        nlvls = x$s.nlevels,
        fixed = ifelse(x$vc.fix$sigma2, "yes", "no"),
        factor = x$s.names,
        R = ifelse(x$Rfix, "yes", "no")
      )
      colnames(vc) <-
        c("estimate", "square root", "nlvls", "fixed", "factor", "R")
      if (!x$withR)
        vc <- vc[, -6, drop = FALSE]
      if (length(x$sigma2) == 1L) {
        rownames(vc) <- "σ²"
      } else {
        rownames(vc) <- paste("σ²", seq_along(x$sigma2), sep = "")
      }
      analysis$groups <-
        append(analysis$groups, df_to_group("Sigma", vc))
    }
    
    if (x$withG) {
      if (is.element(
        x$struct[1],
        c(
          "CS",
          "AR",
          "CAR",
          "ID",
          "SPEXP",
          "SPGAU",
          "SPLIN",
          "SPRAT",
          "SPSPH",
          "PHYBM",
          "PHYPL",
          "PHYPD"
        )
      )) {
        vc <- cbind(tau2, tau, ifelse(x$vc.fix$tau2, "yes", "no"))
        vc <-
          rbind(vc, c(rho, "", ifelse(x$vc.fix$rho, "yes", "no")))
        colnames(vc) <- c("estimate", "square root", "fixed")
        rownames(vc) <- c("T²", "ρ")
        if (x$struct[1] == "ID")
          vc <- vc[1, , drop = FALSE]
        analysis$groups <-
          append(analysis$groups, df_to_group("Tau", vc))
      }
      
      if (is.element(x$struct[1], c("HCS", "HAR", "DIAG"))) {
        vc <-
          cbind(tau2,
            tau,
            x$g.levels.k,
            ifelse(x$vc.fix$tau2, "yes", "no"),
            x$g.levels.f[[1]])
        vc <-
          rbind(vc, c(rho, "", "", ifelse(x$vc.fix$rho, "yes", "no"), ""))
        colnames(vc) <-
          c("estimate", "square root", "k.lvl", "fixed", "level")
        if (length(x$tau2) == 1L) {
          rownames(vc) <- c("τ²", "ρ")
        } else {
          rownames(vc) <- c(paste("τ²", seq_along(x$tau2), " ", sep = ""), "ρ")
        }
        if (x$struct[1] == "DIAG")
          vc <- vc[seq_along(tau2), , drop = FALSE]
        analysis$groups <-
          append(analysis$groups, df_to_group("Tau", vc))
      }
      
      if (is.element(x$struct[1], c("UN", "UNR"))) {
        if (x$struct[1] == "UN") {
          vc <-
            cbind(tau2,
              tau,
              x$g.levels.k,
              ifelse(x$vc.fix$tau2, "yes", "no"),
              x$g.levels.f[[1]])
        } else {
          vc <-
            cbind(
              rep(tau2, length(x$g.levels.k)),
              rep(tau, length(x$g.levels.k)),
              x$g.levels.k,
              ifelse(rep(
                x$vc.fix$tau2, length(x$g.levels.k)
              ), "yes", "no"),
              x$g.levels.f[[1]]
            )
        }
        colnames(vc) <-
          c("estimate", "square root", "k.lvl", "fixed", "level")
        if (length(x$g.levels.k) == 1L) {
          rownames(vc) <- c("T²")
        } else {
          rownames(vc) <- paste("T²", seq_along(x$g.levels.k), " ", sep = "")
        }
        
        analysis$groups <-
          append(analysis$groups, df_to_group("Tau", vc))
        
        
        if (length(x$rho) == 1L) {
          G <- matrix(NA_real_, nrow = 2, ncol = 2)
        } else {
          G <- matrix(NA_real_,
            nrow = x$g.nlevels.f[1],
            ncol = x$g.nlevels.f[1])
        }
        G[lower.tri(G)] <- x$rho
        G[upper.tri(G)] <- t(G)[upper.tri(G)]
        diag(G) <- 1
        G[upper.tri(G)] <- ""
        if (length(x$rho) == 1L) {
          G.info <- matrix(NA_real_, nrow = 2, ncol = 2)
        } else {
          G.info <-
            matrix(NA_real_,
              nrow = x$g.nlevels.f[1],
              ncol = x$g.nlevels.f[1])
        }
        G.info[lower.tri(G.info)] <- x$g.levels.comb.k
        G.info[upper.tri(G.info)] <-
          t(G.info)[upper.tri(G.info)]
        G.info[lower.tri(G.info)] <-
          ifelse(x$vc.fix$rho, "yes", "no")
        diag(G.info) <- "-"
        
        vc <- cbind(G, "", G.info)
        colnames(vc) <-
          c(paste("ρ", abbreviate(x$g.levels.f[[1]]), sep = ""),
            "",
            abbreviate(x$g.levels.f[[1]]))
        rownames(vc) <- x$g.levels.f[[1]]
        analysis$groups <-
          append(analysis$groups, df_to_group("Rho", vc))
      }
      
      if (is.element(x$struct[1], c("GEN"))) {
        vc <- cbind(tau2, tau, ifelse(x$vc.fix$tau2, "yes", "no"), "")
        colnames(vc) <-
          c("estimate", "square root", "fixed", "rho")
        rownames(vc) <- x$g.names[-length(x$g.names)]
        
        G.info <- cov2cor(x$G)
        diag(G.info) <- "-"
        G.info[lower.tri(G.info)] <-
          ifelse(x$vc.fix$rho, "yes", "no")
        colnames(G.info) <-
          abbreviate(x$g.names[-length(x$g.names)])
        vc <- cbind(vc, G.info)
        analysis$groups <-
          append(analysis$groups, df_to_group("Tau", vc))
        
      }
      
      if (is.element(x$struct[1], c("GDIAG"))) {
        vc <- cbind(tau2, tau, ifelse(x$vc.fix$tau2, "yes", "no"))
        colnames(vc) <- c("estimate", "square root", "fixed")
        rownames(vc) <- x$g.names[-length(x$g.names)]
        analysis$groups <-
          append(analysis$groups, df_to_group("Tau", vc))
        
      }
    }
    
    if (x$withH) {
      gamma2 <- x$gamma2
      gamma  <- sqrt(x$gamma2)
      
      if (is.element(
        x$struct[2],
        c(
          "CS",
          "AR",
          "CAR",
          "ID",
          "SPEXP",
          "SPGAU",
          "SPLIN",
          "SPRAT",
          "SPSPH",
          "PHYBM",
          "PHYPL",
          "PHYPD"
        )
      )) {
        vc <- cbind(gamma2, gamma, ifelse(x$vc.fix$gamma2, "yes", "no"))
        vc <-
          rbind(vc, c(x$phi, "", ifelse(x$vc.fix$phi, "yes", "no")))
        colnames(vc) <- c("estimate", "square root", "fixed")
        rownames(vc) <- c("γ²", "φ")
        if (x$struct[2] == "ID")
          vc <- vc[1, , drop = FALSE]
        analysis$groups <-
          append(analysis$groups, df_to_group("Gamma", vc))
        
      }
      
      if (is.element(x$struct[2], c("HCS", "HAR", "DIAG"))) {
        vc <-
          cbind(
            gamma2,
            gamma,
            x$h.levels.k,
            ifelse(x$vc.fix$gamma2, "yes", "no"),
            x$h.levels.f[[1]]
          )
        vc <-
          rbind(vc, c(x$phi, "", "", ifelse(x$vc.fix$phi, "yes", "no"), ""))
        colnames(vc) <-
          c("estimate", "square root", "k.lvl", "fixed", "level")
        if (length(x$gamma2) == 1L) {
          rownames(vc) <- c("γ²", "φ")
        } else {
          rownames(vc) <-
            c(paste("γ²", seq_along(x$gamma2), "  ", sep = ""), "φ")
        }
        if (x$struct[2] == "DIAG")
          vc <- vc[seq_along(gamma2), , drop = FALSE]
        analysis$groups <-
          append(analysis$groups, df_to_group("Gamma", vc))
        
      }
      
      if (is.element(x$struct[2], c("UN", "UNR"))) {
        if (x$struct[2] == "UN") {
          vc <-
            cbind(
              gamma2,
              gamma,
              x$h.levels.k,
              ifelse(x$vc.fix$gamma2, "yes", "no"),
              x$h.levels.f[[1]]
            )
        } else {
          vc <-
            cbind(
              rep(gamma2, length(x$h.levels.k)),
              rep(gamma, length(x$h.levels.k)),
              x$h.levels.k,
              ifelse(rep(
                x$vc.fix$gamma2, length(x$h.levels.k)
              ), "yes", "no"),
              x$h.levels.f[[1]]
            )
        }
        colnames(vc) <-
          c("estimate", "square root", "k.lvl", "fixed", "level")
        if (length(x$h.levels.k) == 1L) {
          rownames(vc) <- c("γ²")
        } else {
          rownames(vc) <- paste("γ²", seq_along(x$h.levels.k), "  ", sep = "")
        }
        analysis$groups <-
          append(analysis$groups, df_to_group("Gamma", vc))
        
        
        if (length(x$phi) == 1L) {
          H <- matrix(NA_real_, nrow = 2, ncol = 2)
        } else {
          H <- matrix(NA_real_,
            nrow = x$h.nlevels.f[1],
            ncol = x$h.nlevels.f[1])
        }
        H[lower.tri(H)] <- x$phi
        H[upper.tri(H)] <- t(H)[upper.tri(H)]
        diag(H) <- 1
        #H[upper.tri(H)] <- ""
        
        if (length(x$phi) == 1L) {
          H.info <- matrix(NA_real_, nrow = 2, ncol = 2)
        } else {
          H.info <-
            matrix(NA_real_,
              nrow = x$h.nlevels.f[1],
              ncol = x$h.nlevels.f[1])
        }
        H.info[lower.tri(H.info)] <- x$h.levels.comb.k
        H.info[upper.tri(H.info)] <-
          t(H.info)[upper.tri(H.info)]
        H.info[lower.tri(H.info)] <-
          ifelse(x$vc.fix$phi, "yes", "no")
        diag(H.info) <- "-"
        
        vc <- cbind(H, "", H.info)
        colnames(vc) <-
          c(paste("φ", abbreviate(x$h.levels.f[[1]]), sep = ""),
            "",
            abbreviate(x$h.levels.f[[1]])) 
        ### FIXME: x$h.levels.f[[1]] may be numeric, in which case a wrapping 
        # 'header' is not recognized
        rownames(vc) <- x$h.levels.f[[1]]
        analysis$groups <-
          append(analysis$groups, df_to_group("Phi", vc))
      }
      
      if (is.element(x$struct[2], c("GEN"))) {
        vc <- cbind(gamma2, gamma, ifelse(x$vc.fix$gamma2, "yes", "no"), "")
        colnames(vc) <-
          c("estimate", "square root", "fixed", "φ")
        rownames(vc) <- x$h.names[-length(x$h.names)]
        
        H.info <- cov2cor(x$H)
        diag(H.info) <- "-"
        H.info[lower.tri(H.info)] <-
          ifelse(x$vc.fix$phi, "yes", "no")
        colnames(H.info) <-
          abbreviate(x$h.names[-length(x$h.names)])
        vc <- cbind(vc, H.info)
        analysis$groups <-
          append(analysis$groups, df_to_group("Gamma", vc))
        
      }
      
      if (is.element(x$struct[2], c("GDIAG"))) {
        vc <- cbind(gamma2, gamma, ifelse(x$vc.fix$gamma2, "yes", "no"))
        colnames(vc) <- c("estimate", "square root", "fixed")
        rownames(vc) <- x$h.names[-length(x$h.names)]
        analysis$groups <-
          append(analysis$groups, df_to_group("Gamma", vc))
        
      }
    }
  }
  
  # Heterogeneity
  # Create a group and statistics list for the Test for (Residual) Heterogeneity
  if (!is.na(x$QE)) {
    statistics <- list()
    if (x$int.only) {
      group <- list(name = "Heterogeneity")
      statistics <-
        add_statistic(statistics, "statistic", x$QE, "Q")
    } else{
      group <- list(name = "Residual Heterogeneity")
      statistics <-
        add_statistic(statistics, "statistic", x$QE, "QE")
    }
    statistics <- add_statistic(statistics, "df", x$k - x$p)
    statistics <- add_statistic(statistics, "p", x$QEp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if (x$p > 1L && !is.na(x$QM)) {
    statistics <- list()
    group <- list(name = "Test of Moderators")
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      statistics <-
        add_statistic(statistics, "statistic", x$QM, "F")
      statistics <- add_statistic(statistics, "df numerator",
        x$QMdf[[1]], "df", "num.")
      statistics <- add_statistic(statistics, "df denominator",
        x$QMdf[[2]], "df", "den.")
    } else {
      statistics <-
        add_statistic(statistics, "statistic", x$QM, "QM")
      statistics <- add_statistic(statistics, "df", x$QMdf[1])
    }
    statistics <- add_statistic(statistics, "p", x$QEp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  
  if (is.element(x$test, c("knha", "adhoc", "t"))) {
    stat_type = "t"
  } else{
    stat_type = "z"
  }
  # Loop over the coefficients and add statistics to a group list
  groups <- list(name = "Coefficients")
  for (i in 1:length(x$beta)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- rownames(x$beta)[i]
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x$beta[i][[1]],
        "b",
        interval = "CI",
        level = .95,
        lower = x$ci.lb[i],
        upper = x$ci.ub[i]
      )
    statistics <-
      add_statistic(statistics, "SE", x$se[i])
    statistics <-
      add_statistic(statistics, "statistic", x$zval[i], stat_type)
    statistics <- add_statistic(statistics, "p", x$pval[i])
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'confint.rma'
#' @export
tidy_stats.confint.rma <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()
  
  if (!(is.vector(args) && length(args) == 1)) {
    args <- NULL
  }
  
  if (is.matrix(x$random)) {
    analysis$groups <-
      append(analysis$groups,
        ci_df_to_group("RMA estimates with CIs (random)", x$random, args))
  }
  
  if (is.matrix(x$fixed)) {
    analysis$groups <-
      append(analysis$groups,
        ci_df_to_group("RMA estimates with CIs (fixed)", x$fixed, args))
  }
  
  # Add additional information
  analysis$level <- args
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'list.confint.rma'
#' @export
tidy_stats.list.confint.rma <- function(x, args = NULL) {
  
  # Create the analysis list
  analysis <- list()
  
  if (!(is.vector(args) && length(args) == 1)) {
    args <- NULL
  }
  
  for (x.elem in x[sapply(x, class) == "confint.rma"]) {
    if (is.matrix(x.elem$random)) {
      analysis$groups <-
        append(analysis$groups,
          ci_df_to_group("RMA estimates with CIs (random)", x.elem$random, args))
    }
    if (is.matrix(x.elem$fixed)) {
      analysis$groups <-
        append(analysis$groups,
          ci_df_to_group("RMA estimates with CIs (fixed)", x.elem$fixed, args))
    }
  }
  
  # Add additional information
  analysis$level <- args
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'anova.rma'
#' @export
tidy_stats.anova.rma <- function(x, args = NULL) {
  if (x$type == "LRT") {
    method = "Likelihood Ratio Tests for 'rma' Objects"
  } else {
    method = "Wald-Type Tests for 'rma' Objects"
  }
  
  analysis <- list(method = method)
  
  # Moderators
  # Create a group and statistics list for the Test of Moderators (if any)
  if (x$type == "Wald.btt") {
    statistics <- list()
    if (is.element("rma.ls", x$class)) {
      group <- list(name = "Test of Location Coefficients")
    } else {
      group <- list(name = "Test of Moderators")
    }
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      statistics <- add_statistic(statistics, "statistic", x$QM, "F")
      statistics <- add_statistic(statistics, "df numerator",
        x$QMdf[1], "df", "num.")
      statistics <- add_statistic(statistics, "df denominator",
        x$QMdf[2], "df", "den.")
    } else {
      statistics <- add_statistic(statistics, "statistic", x$QM, "QM")
      statistics <- add_statistic(statistics, "df", x$QMdf[1])
    }
    statistics <- add_statistic(statistics, "p", x$QMp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if (x$type == "Wald.att") {
    statistics <- list()
    group <- list(name = "Test of Scale Coefficients")
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      statistics <- add_statistic(statistics, "statistic", x$QS, "F")
      statistics <- add_statistic(statistics, "df numerator",
        x$QSdf[[1]], "df", "num.")
      statistics <- add_statistic(statistics, "df denominator",
        x$QSdf[[2]], "df", "den.")
    } else {
      statistics <- add_statistic(statistics, "statistic", x$QS, "QS")
      statistics <- add_statistic(statistics, "df", x$QSdf[1])
    }
    statistics <- add_statistic(statistics, "p", x$QSp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if (x$type == "Wald.Xb") {
    # Create a groups list for the coefficients
    groups <- list(name = "Table: Wald-type test of moderators")
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      stat_type = "t"
    } else{
      stat_type = "z"
    }
    # Loop over the coefficients and add statistics to a group list
    for (i in 1:length(x$Xb)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- toString(i)
      # Create a new statistics list
      statistics <- list()
      statistics <-
        add_statistic(statistics,
          "estimate",
          x$Xb[i][[1]],
          "b")
      if (is.element(x$test, c("knha", "adhoc", "t"))) {
        statistics <-
          add_statistic(statistics, "df", x$ddf[i])
      }
      statistics <-
        add_statistic(statistics, "SE", x$se[i][[1]])
      statistics <-
        add_statistic(statistics, "statistic", x$zval[i], stat_type)
      statistics <- add_statistic(statistics, "p", x$pval[i])
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
    
    if (!is.na(x$QM)) {
      statistics <- list()
      if (x$m == 1) {
        group <- list(name = "Test of Hypothesis")
      } else {
        group <- list(name = "Omnibus Test of Hypotheses")
      }
      if (is.element(x$test, c("knha", "adhoc", "t"))) {
        statistics <- add_statistic(statistics, "statistic", x$QM, "F")
        statistics <- add_statistic(statistics, "df numerator",
          x$QMdf[1], "df", "num.")
        statistics <- add_statistic(statistics, "df denominator",
          x$QMdf[2], "df", "den.")
      } else {
        statistics <- add_statistic(statistics, "statistic", x$QM, "QM")
        statistics <- add_statistic(statistics, "df", x$QMdf[1])
      }
      statistics <- add_statistic(statistics, "p", x$QMp)
      # Add statistics to the group
      group$statistics <- statistics
      # Add the model group to a groups element on the analysis
      analysis$groups <- append(analysis$groups, list(group))
    }
  }
  
  if (x$type == "Wald.Za") {
    # Create a groups list for the coefficients
    groups <- list(name = "Table: Wald-type test of moderators")
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      stat_type = "t"
    } else{
      stat_type = "z"
    }
    # Loop over the coefficients and add statistics to a group list
    for (i in 1:length(x$Za)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- rownames(x$Za)[i]
      # Create a new statistics list
      statistics <- list()
      statistics <-
        add_statistic(statistics,
          "estimate",
          x$Za[i][[1]],
          "b")
      if (is.element(x$test, c("knha", "adhoc", "t"))) {
        statistics <-
          add_statistic(statistics, "df", x$ddf[i])
      }
      statistics <-
        add_statistic(statistics, "SE", x$se[i][[1]])
      statistics <-
        add_statistic(statistics, "statistic", x$zval[i], stat_type)
      statistics <- add_statistic(statistics, "p", x$pval[i])
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
    
    if (!is.na(x$QS)) {
      statistics <- list()
      if (x$m == 1) {
        group <- list(name = "Test of Hypothesis")
      } else {
        group <- list(name = "Omnibus Test of Hypotheses")
      }
      if (is.element(x$test, c("knha", "adhoc", "t"))) {
        statistics <- add_statistic(statistics, "statistic", x$QS, "F")
        statistics <- add_statistic(statistics, "df numerator",
          x$QSdf[1], "df", "num.")
        statistics <- add_statistic(statistics, "df denominator",
          x$QSdf[2], "df", "den.")
      } else {
        statistics <- add_statistic(statistics, "statistic", x$QS, "QS")
        statistics <- add_statistic(statistics, "df", x$QSdf[1])
      }
      
      statistics <- add_statistic(statistics, "p", x$QSp)
      # Add statistics to the group
      group$statistics <- statistics
      # Add the model group to a groups element on the analysis
      analysis$groups <- append(analysis$groups, list(group))
    }
  }
  
  if (x$type == "LRT") {
    res.table <- data.frame(c(x$parms.f, x$parms.r),
      c(x$fit.stats.f["AIC"], x$fit.stats.r["AIC"]),
      c(x$fit.stats.f["BIC"], x$fit.stats.r["BIC"]),
      c(x$fit.stats.f["AICc"], x$fit.stats.r["AICc"]),
      c(x$fit.stats.f["ll"], x$fit.stats.r["ll"]),
      c(NA, x$LRT),
      c(NA, x$pval),
      c(x$QE.f,  x$QE.r),
      c(x$tau2.f, x$tau2.r),
      c(NA, NA), stringsAsFactors=FALSE)
    
    colnames(res.table) <- c("df", "AIC", "BIC", "AICc", "Log-Likelihood", "LRT", "p", "QE", "T²", "R²")
    rownames(res.table) <- c("Full", "Reduced")
    res.table["Full", c("LRT", "p")] <- NA
    res.table["Full", "R²"] <- NA
    res.table["Reduced","R²"] <- x$R2 / 100
    
    ### remove tau^2 column if full model is a FE/EE/CE model
    if (is.element(x$method, c("FE","EE","CE")))
      res.table <- res.table[-which(names(res.table) == "T²")]
    
    ### remove R^2 column if full model is a rma.mv or rma.ls model
    if (is.element("rma.mv", x$class.f) || is.element("rma.ls", x$class.f))
      res.table <- res.table[-which(names(res.table) == "R²")]
    
    analysis$groups <-
      append(analysis$groups, df_to_group("Likelihood ratio test of moderators", res.table))
  }
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'permutest.rma.uni'
#' @export
tidy_stats.permutest.rma.uni <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()
  
  if (!x$int.only) {
    statistics <- list()
    if (inherits(x, "permutest.rma.ls")) {
      group <- list(name = "Test of Location Coefficients")
    } else {
      group <- list(name = "Test of Moderators")
    }
    
    if (is.element(x$test, c("knha", "adhoc", "t"))) {
      statistics <- add_statistic(statistics, "statistic", x$QM, "F")
      statistics <- add_statistic(statistics, "df numerator",
        x$QMdf[1], "df", "num.")
      statistics <- add_statistic(statistics, "df denominator",
        x$QMdf[2], "df", "den.")
    } else {
      statistics <- add_statistic(statistics, "statistic", x$QM, "QM")
      statistics <- add_statistic(statistics, "df", x$QMdf[1])
    }
    statistics <- add_statistic(statistics, "p", x$QMp)
    # Add statistics to the group
    group$statistics <- statistics
    # Add the model group to a groups element on the analysis
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if (is.element(x$test, c("knha", "adhoc", "t"))) {
    stat_type = "t"
  } else{
    stat_type = "z"
  }
  # Loop over the coefficients and add statistics to a group list
  if (inherits(x, "permutest.rma.ls")) {
    groups <- list(name = "Coefficients (Location)")
  } else {
    groups <- list(name = "Coefficients")
  }
  for (i in 1:length(x$beta)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- rownames(x$beta)[i]
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x$beta[i][[1]],
        "b",
        interval = "CI",
        level = .95,
        lower = x$ci.lb[i],
        upper = x$ci.ub[i]
      )
    statistics <-
      add_statistic(statistics, "SE", x$se[i])
    statistics <-
      add_statistic(statistics, "statistic", x$zval[i], stat_type)
    statistics <- add_statistic(statistics, "p", x$pval[i])
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  if (inherits(x, "permutest.rma.ls")) {
    if (!x$Z.int.only) {
      statistics <- list()
      group <- list(name = "Test of Scale Coefficients")
      if (is.element(x$test, c("knha", "adhoc", "t"))) {
        statistics <- add_statistic(statistics, "statistic", x$QS, "F")
        statistics <- add_statistic(statistics, "df numerator",
          x$QSdf[[1]], "df", "num.")
        statistics <-
          add_statistic(statistics, "df denominator",
            x$QSdf[[2]], "df", "den.")
      } else {
        statistics <- add_statistic(statistics, "statistic", x$QS, "QS")
        statistics <- add_statistic(statistics, "df", x$QSdf[1])
      }
      statistics <- add_statistic(statistics, "p", x$QSp)
      # Add statistics to the group
      group$statistics <- statistics
      # Add the model group to a groups element on the analysis
      analysis$groups <- append(analysis$groups, list(group))
    }
    
    groups <- list(name = "Coefficients (Scale)")
    # Loop over the coefficients and add statistics to a group list
    for (i in 1:length(x$alpha)) {
      # Create a new group list
      group <- list()
      # Add the name and type of the coefficient
      group$name <- rownames(x$alpha)[i]
      # Create a new statistics list
      statistics <- list()
      statistics <-
        add_statistic(
          statistics,
          "estimate",
          x$alpha[i][[1]],
          "b",
          interval = "CI",
          level = .95,
          lower = x$ci.lb.alpha[i],
          upper = x$ci.ub.alpha[i]
        )
      statistics <-
        add_statistic(statistics, "SE", x$se.alpha[i][[1]])
      statistics <-
        add_statistic(statistics, "statistic", x$zval.alpha[i][[1]], stat_type)
      if (is.element(x$test, c("knha", "adhoc", "t"))) {
        statistics <-
          add_statistic(statistics, "df", x$ddf.alpha[i])
      }
      statistics <-
        add_statistic(statistics, "p", x$pval.alpha[i])
      # Add statistics to the group
      group$statistics <- statistics
      # Add the group to the groups of the coefficients groups list
      groups$groups <- append(groups$groups, list(group))
    }
    # Add the coefficient groups to the statistics list
    analysis$groups <- append(analysis$groups, list(groups))
  }
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'tes'
#' @export
tidy_stats.tes <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list(
    method = "Test of Excess Significance"
  )
  
  group <- list(name = "Descriptives")
  statistics <- list()
  statistics <-
    add_statistic(statistics, "count", x$k, 'n', 'total')
  statistics <-
    add_statistic(statistics, "count", x$O, 'n', 'significant')
  statistics <-
    add_statistic(statistics, "statistic", x$E, 'n', 'expected sig.')
  statistics <-
    add_statistic(statistics, "statistic", x$OEratio, 'ratio', 'sig./exp.')
  # Add statistics to the group
  group$statistics <- statistics
  # Add the model group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group))
  
  group_name = "Test of Excess Significance"
  if (x$test == "binom") {
    group <- list(name = "Test of Excess Significance ( test)")
    paste(group_name, "(binomial)")
  }
  if (x$test == "exact") {
    paste(group_name, "(exact)")
  }
  group <- list(name = group_name)
  statistics <- list()
  statistics <-
    add_statistic(statistics, "statistic", x$X2, "χ²")
  statistics <- add_statistic(statistics, "df", 1)
  statistics <- add_statistic(statistics, "p", x$pval)
  
  if (!is.null(x$theta.lim)) {
    if (!is.na(x$theta.lim[1])) {
      if (length(x$theta.lim) == 2L) {
        lim1 = "1 "
      } else {
        lim1 = ""
      }
      statistics <-
        add_statistic(statistics,
          "statistic",
          x$theta.lim[1],
          "θ",
          paste0(lim1, "limit"))
    }
    if (length(x$theta.lim) == 2L && !is.na(x$theta.lim[2])) {
      statistics <-
        add_statistic(statistics, "statistic", x$theta.lim[2], "θ", "2 limit")
    }
    if (any(!is.na(x$theta.lim))) {
      statistics <-
        add_statistic(
          statistics,
          "p",
          ifelse(x$tes.alternative == "two.sided", x$tes.alpha / 2, x$tes.alpha),
          "p",
          "limit"
        )
    }
  }
  
  # Add statistics to the group
  group$statistics <- statistics
  # Add the model group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group))
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'matreg'
#' @export
tidy_stats.matreg <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()
  
  groups <- list(name = "Table: Fit Regression Models")
  # Loop over the coefficients and add statistics to a group list
  for (i in 1:nrow(x$tab)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- rownames(x$tab)[i]
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x$tab$beta[i][[1]],
        "b",
        interval = "CI",
        level = .95,
        lower = x$tab$ci.lb[i],
        upper = x$tab$ci.ub[i]
      )
    statistics <-
      add_statistic(statistics, "SE", x$tab$se[i][[1]])
    if (x$test == "t") {
      statistics <-
        add_statistic(statistics, "statistic", x$tab$tval[i][[1]], "t")
      statistics <-
        add_statistic(statistics, "df", x$tab$df[i])
    } else {
      statistics <-
        add_statistic(statistics, "statistic", x$tab$zval[i][[1]], "z")
    }
    statistics <-
      add_statistic(statistics, "p", x$tab$pval[i])
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'ranktest'
#' @export
tidy_stats.ranktest <- function(x, args = NULL) {
  # Create the analysis list
  analysis <-
    list(method = "Rank Correlation Test for Funnel Plot Asymmetry")
  
  statistics <- list()
  statistics <-
    add_statistic(statistics, "statistic", x$tau[[1]], 'τ')
  statistics <-
    add_statistic(statistics, "p", x$pval)
  
  analysis$statistics <- statistics
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'regtest'
#' @export
tidy_stats.regtest <- function(x, args = NULL) {
  # Create the analysis list
  analysis <-
    list(method = "Regression Test for Funnel Plot Asymmetry")
  
  statistics <- list()
  if (!is.null(x$est)) {
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x$est[[1]],
        "b",
        interval = "CI",
        level = .95,
        lower = x$ci.lb,
        upper = x$ci.ub
      )
  }
  
  if (is.na(x$ddf)) {
    statistics <-
      add_statistic(statistics, "statistic", x$zval, 'z')
  } else {
    statistics <-
      add_statistic(statistics, "statistic", x$zval, 't')
    statistics <-
      add_statistic(statistics, "df", x$ddf)
  }
  
  statistics <-
    add_statistic(statistics, "p", x$pval)
  
  analysis$statistics <- statistics
  
  if (x$model == "lm") {
    method <- "weighted regression with multiplicative dispersion"
  } else {
    method <-
      paste(
        ifelse(
          is.element(
            x$method, c("FE", "EE", "CE")
          ), 
          "fixed-effects", 
          "mixed-effects"
        ), 
        "meta-regression model"
      )
  }
  if (x$predictor == "sei") {
    method <- paste(method, ("(predictor: standard error)"))
  } else if (x$predictor == "vi") {
    method <- paste(method, ("(predictor: sampling variance)"))
  } else if (x$predictor == "ni") {
    method <- paste(method, ("(predictor: sample size)"))
  } else if (x$predictor == "ninv") {
    method <-
      paste(method, ("(predictor: inverse of the sample size)"))
  } else if (x$predictor == "sqrtni") {
    method <-
      paste(method, ("(predictor: square root sample size)"))
  } else if (x$predictor == "sqrtninv") {
    method <-
      paste(method, ("(predictor: inverse of the square root sample size)"))
  }
  analysis$type <- method
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'fsn'
#' @export
tidy_stats.fsn <- function(x, args = NULL) {
  # Create the analysis list
  analysis <-
    list(method = paste0("Fail-Safe N Analysis (", x$type, " approach)"))
  
  statistics <- list()
  
  if (x$type == "Rosenthal") {
    statistics <-
      add_statistic(statistics, "p", x$pval, subscript = "observed")
    statistics <-
      add_statistic(statistics, "p", x$alpha, subscript = "target")
  }
  
  if (x$type == "Orwin" || x$type == "REM") {
    statistics <-
      add_statistic(statistics, "estimate", x$meanes, "b", subscript = "average")
    statistics <-
      add_statistic(statistics, "estimate", x$target, "b", subscript = "target")
  }
  
  if (x$type == "Rosenberg") {
    statistics <-
      add_statistic(statistics, "estimate", x$meanes, "b", subscript = "average")
    statistics <-
      add_statistic(statistics, "p", x$pval, subscript = "observed")
    statistics <-
      add_statistic(statistics, "p", x$alpha, subscript = "target")
  }
  
  statistics <-
    add_statistic(statistics, "count", x$fsnum, "n", subscript = "failsafe")
  
  analysis$statistics <- statistics
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}



#' @describeIn tidy_stats tidy_stats method for class 'hc.rma.uni'
#' @export
tidy_stats.hc.rma.uni <- function(x, args = NULL) {
  # Create the analysis list
  analysis <-
    list(method = "Henmi and Copas Meta-Analysis")
  
  # Create a groups list for the coefficients
  groups <- list(name = "Coefficients")
  # Loop over the coefficients and add statistics to a group list
  for (suffx in c(".rma", "")) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- x[[paste0("method", suffx)]]
    
    # Create a new statistics list
    statistics <- list()
    statistics <-
      add_statistic(
        statistics,
        "estimate",
        x[[paste0("beta", suffx)]][[1]],
        "b",
        interval = "CI",
        level = .95,
        lower = x[[paste0("ci.lb", suffx)]],
        upper = x[[paste0("ci.ub", suffx)]]
      )
    statistics <-
      add_statistic(statistics, "SE", x[[paste0("se", suffx)]])
    statistics <-
      add_statistic(statistics, "estimate", x[[paste0("tau2", suffx)]], "τ²")
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'list.rma'
#' @export
tidy_stats.list.rma <- function(x, args = NULL) {
  # Create the analysis list
  analysis <-
    list(name = "Cumulative Meta-Analysis")
  class(x) <- NULL
  x$cr.lb <- NULL
  x$cr.ub <- NULL
  slab.pos <- which(names(x) == "slab")
  out <- head(x, slab.pos - 1)
  out_df <- data.frame(out,
    row.names = x$slab,
    stringsAsFactors = FALSE)
  
  out_df[] <- sapply(out_df, function(x)
    suppressWarnings(as.numeric(as.character(x))))
  out_df = Filter(function(x)
    ! all(is.na(x)), out_df)
  if (ncol(out_df) == 0 || nrow(out_df) < 1) {
    return(NULL)
  }
  
  replacers2 = list(
    "tau" = "τ",
    "^2" = "²",
    "sigma" = "σ",
    "rho" = "ρ",
    "pvals" = "p",
    "zval" = "z",
    "tval" = "ρ",
    "se" = "SE",
    "2" = "²"
  )
  
  for (replacer in names(replacers2)) {
    colnames(out_df) = gsub(replacer, 
      replacers2[[replacer]], colnames(out_df), fixed = TRUE)
  }
  if (any(rownames(out_df) == "")) {
    rownames(out_df)[rownames(out_df) == ""] = 1:nrow(out_df)[rownames(out_df) == ""]
  }
  groups <- list(name = "Table: Cumulative Meta-Analysis")
  # Loop over the coefficients and add statistics to a group list
  for (i in 1:nrow(out_df)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- rownames(out_df)[i]
    # Create a new statistics list
    statistics <- list()
    for (j in 1:ncol(out_df)) {
      #print(colnames(out_df)[j])
      #print(out_df[i, j])
      if (colnames(out_df)[j] == "estimate") {
        statistics <-
          add_statistic(
            statistics,
            "estimate",
            out_df$estimate[i],
            interval = "CI",
            level = 1 - x$level,
            lower = out_df$ci.lb[i],
            upper = out_df$ci.ub[i]
          )
      } else if (!colnames(out_df)[j] %in% c("ci.lb", "ci.ub")) {
        statistics <-
          add_statistic(statistics,
            colnames(out_df)[j],
            ifelse(is.na(out_df[i, j]), "-", out_df[i, j]))
      }
    }
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))
  
  # Add package information
  analysis <- add_package_info(analysis, "metafor")
  
  return(analysis)
}