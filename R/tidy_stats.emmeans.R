#' @describeIn tidy_stats tidy_stats method for class 'emmGrid'
#' @export
tidy_stats.emmGrid <- function(x, args = NULL) {
  if (require("emmeans")) {
    return(tidy_stats(summary(x)))
  } else {
    stop(
      strwrap(
        "The 'emmeans' package needs to be loaded to add 'emmGrid' objects via 
        add_stats() in tidystats."
      )
    )
  }
}
  
group_statistics <- function(df) {
    statistics <- list()
    
    statistics <- add_statistic(
      statistics, "null", 
      df$null, 
      symbol = "ŷ",
      subscript = "null"
    )
    statistics <- add_statistic(
      statistics, 
      name = "estimate", 
      df$emmean, 
      symbol = "EMM", 
      interval = "CI", 
      level = parse_number(attr(df, "mesg")), 
      lower = df$lower.CL,
      upper = df$upper.CL
    )
    statistics <- add_statistic(
      statistics, "estimate", 
      df$estimate, 
      symbol = "b"
    )
    statistics <- add_statistic(
      statistics, "estimate", 
      df$response, 
      symbol = "ŷ",
      interval = "CI", 
      level = parse_number(attr(df, "mesg")[2]), 
      lower = df$lower.CL,
      upper = df$upper.CL
    )
    statistics <- add_statistic(statistics, "SE", df$SE)
    statistics <- add_statistic(
      statistics, 
      "statistic", 
      df$t.ratio, 
      symbol = "t"
    )
    statistics <- add_statistic(statistics, "statistic", df$F.ratio, "F")
    statistics <- add_statistic(statistics, "df", df$df)
    statistics <- add_statistic(
      statistics, 
      "df numerator", 
      df$df1[i], 
      "df", 
      "num."
    )
    statistics <- add_statistic(
      statistics, 
      "df denominator", 
      df$df2[i], 
      "df", 
      "den."
    )
    statistics <- add_statistic(statistics, "p", df$p.value)
}

group_pri_vars_statistics <- function(df, pri_vars) {
  # Create a group for the pri var
  group <- list(name = pri_vars)
  
  # Extract levels
  levels <- unique(df[, pri_vars])

  # Loop over the levels of the group
  for (i in 1:length(levels)) {
    # Create an empty terms group list
    group_level <- list(name = as.character(levels[i]))
    
    # Get statistics
    statistics <- group_statistics(df[i, ])
    
    # Add statistics to the term group
    group_level$statistics <- statistics
    
    # Add the level group to the group
    group$groups <- append(group$groups, list(group_level))
  }
  
  return(group)
}

group_vars <- function(vars, df) {
  group <- list(name = vars[1])
  levels <- levels(df[, vars[1]])
  
  for (i in 1:length(levels)) {
    group_level <- list(name = as.character(levels[i]))
    df_level <- df[df[, vars[1]] == levels[i], ]
    
    if (length(vars) > 1) {
      group_level_group <- group_vars(
        vars[-1], 
        df_level
      )
      group_level$groups <- append(group_level$groups, list(group_level_group))
    } else {
      group_level$statistics <- group_statistics(df_level)
    }
    
    group$groups <- append(group$groups, list(group_level))
  }
  
  return(group)
}


#' @describeIn tidy_stats tidy_stats method for class 'summary_emm'
#' @export
tidy_stats.summary_emm <- function(x, args = NULL) {
  analysis <- list()
  
  df <- as.data.frame(x)
  
  analysis$method <- dplyr::case_when(
    attr(x, "estName") == "emmean" ~ "Estimated marginal means",
    attr(x, "estName") == "estimate" ~ "Contrasts",
    attr(x, "estName") == "response" ~ "Predicted estimated marginal means",
    attr(x, "estName") == "F.ratio" & "F.ratio" %in% names(df) ~ "F test",
  )
  
  pri_vars <- attr(x, "pri.vars")
  by_vars <- attr(x, "by.vars")
  
  if (length(pri_vars) > 0) {
    group <- group_vars(pri_vars, df)
    
    analysis$groups <- append(analysis$groups, list(group))
  } else if (length(by_vars > 0)) {
    
    names <- ""
    
    for (i in 1:length(by_vars)) {
      names <- c(names, paste(by_vars[i], "=", df[by_vars[i]])
    }
    
    df[, by_vars]
  } else {
    statistics <- group_statistics(df)
      
    analysis$statistics <- statistics
  }
  
  if (length(by_vars) > 0) {
    group <- group_by_vars_statistics(df, by_vars, pri_vars)
    
    analysis$groups <- append(analysis$groups, list(group))
  } else if (length(pri_vars) > 0) {
    
    
  } else {
    
  }
  
  analysis <- add_package_info(analysis, "emmeans")
  
  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'emm_list'
#' @export
tidy_stats.emm_list <- function(x, args = NULL) {
  analysis <- list()
  
  for (i in i:length(x)) {
    group <- tidy_stats.emmGrid(x[[i]])
    
    # Remove package information because we'll set it at the analysis level
    group$package <- NULL
    
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  analysis <- add_package_info(analysis, "emmeans")
  
  return(analysis)
}