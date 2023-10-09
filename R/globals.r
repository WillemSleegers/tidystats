# Trick to suppress some check warnings
globalVariables(
  c(
    ":=", "identifier", "method", "variable", "value", "sd",
    "SD", "N", "median", "M", "pct", "DV", "group", "term", "terms", "n",
    "column", "name", "name1", "vars", "statistic", "statistic_name",
    "df_to_group", "ci_df_to_group", "fixed_cors", "ci_df_to_group", "head",
    "combn", "var", "name2", "corrs", "level", "lhs", "rhs", "op"
  )
)
