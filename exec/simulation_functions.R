bootstrap_mean = function(x, p = c(.025, .975)) {
  means = rep(NA, 10000)
  for (i in seq_along(means)) means[i] = mean(sample(x, length(x), replace = TRUE))
  quantile(means, p)
}

crps_latex_table = function(df) {
  stats = df %>%
    dplyr::mutate(bad_lower_boundary = lower_difference < 0 | is.na(lower_difference),
                  bad_upper_boundary = upper_difference < 0 | is.na(upper_difference)) %>%
    dplyr::group_by(d, n, iter) %>%
    dplyr::summarise(crps = expected_crps_fpld(estimate, truth),
                     se = (estimate - truth) ^ 2,
                     perfect_crps = expected_crps_fpld(truth, truth),
                     skill_score = 1 - perfect_crps / crps,
                     time = mean(time),
                     bad_lower_boundary = bad_lower_boundary[1],
                     bad_upper_boundary = bad_upper_boundary[1],
                     lower_exceedance = abs(mean(lower_difference[lower_difference < 0])),
                     upper_exceedance = abs(mean(upper_difference[upper_difference < 0]))) %>%
    dplyr::mutate(lower_exceedance = ifelse(is.nan(lower_exceedance), 0, lower_exceedance),
                  upper_exceedance = ifelse(is.nan(upper_exceedance), 0, upper_exceedance),
                  fail = is.na(crps)) %>%
    dplyr::group_by(d, n) %>%
    dplyr::summarise(crps = mean(crps, na.rm = TRUE),
                     mse = mean(se),
                     perfect_crps = mean(perfect_crps, na.rm = TRUE),
                     skill_score = mean(skill_score, na.rm = TRUE),
                     time = mean(time),
                     fail_percentage = mean(fail) * 100,
                     bad_lower_boundary_percentage = mean(bad_lower_boundary) * 100,
                     bad_upper_boundary_percentage = mean(bad_upper_boundary) * 100,
                     bad_boundary_percentage = mean(bad_lower_boundary | bad_upper_boundary) * 100,
                     mean_lower_exceedance = mean(lower_exceedance, na.rm = TRUE),
                     mean_upper_exceedance = mean(upper_exceedance, na.rm = TRUE),
                     mean_exceedance = mean(lower_exceedance + upper_exceedance, na.rm = TRUE))

  crps_stats = stats %>%
    dplyr::select(d, n, skill_score) %>%
    tidyr::pivot_wider(names_from = n, values_from = skill_score) %>%
    dplyr::ungroup() %>%
    dplyr::select(-d) %>%
    as.data.frame() %>%
    {. * 1000} %>%
    round(digits = 2) %>%
    format()
  mse_stats = stats %>%
    dplyr::select(d, n, mse) %>%
    tidyr::pivot_wider(names_from = n, values_from = mse) %>%
    dplyr::ungroup() %>%
    dplyr::select(-d) %>%
    as.data.frame() %>%
    round(digits = 2) %>%
    format()
  time_stats = stats %>%
    dplyr::select(d, n, time) %>%
    tidyr::pivot_wider(names_from = n, values_from = time) %>%
    dplyr::ungroup() %>%
    dplyr::select(-d) %>%
    round(digits = 1) %>%
    as.data.frame() %>%
    format()
  methods = stats %>%
    dplyr::select(d, n, skill_score) %>%
    tidyr::pivot_wider(names_from = n, values_from = skill_score) %>%
    dplyr::ungroup() %>%
    dplyr::pull(d)
  table = NULL
  table[1] = paste("& Method & ", paste0("\\(n = 2^{", log2(as.numeric(names(crps_stats))), "}\\)", collapse = " & "))
  table[2] = "\\midrule"
  for (i in seq_len(nrow(crps_stats))) {
    if (i == 1) {
      table[length(table) + 1] = paste("Skill score (\\(\\cdot 10^{3}\\)) &",
                                       methods[i], "&", paste0("\\(", crps_stats[i, ], "\\)", collapse = " & "))
    } else {
      table[length(table) + 1] = paste("&", methods[i], "&", paste0("\\(", crps_stats[i, ], "\\)", collapse = " & "))
    }
  }
  table[length(table) + 1] = "\\midrule"
  for (i in seq_len(nrow(mse_stats))) {
    if (i == 1) {
      table[length(table) + 1] = paste("MSE &", methods[i], "&", paste0("\\(", mse_stats[i, ], "\\)", collapse = " & "))
    } else {
      table[length(table) + 1] = paste("&", methods[i], "&", paste0("\\(", mse_stats[i, ], "\\)", collapse = " & "))
    }
  }
  table[length(table) + 1] = "\\midrule"
  for (i in seq_len(nrow(time_stats))) {
    if (i == 1) {
      table[length(table) + 1] = paste("Time [s] &", methods[i], "&", paste0("\\(", time_stats[i, ], "\\)", collapse = " & "))
    } else {
      table[length(table) + 1] = paste("&", methods[i], "&", paste0("\\(", time_stats[i, ], "\\)", collapse = " & "))
    }
  }
  table = paste(table, collapse = " \\\\\n")
  table = gsub("rule \\\\\\\\", "rule", table)
  table = gsub("_noconstr", "u", table)
  cat(table, "\n")
  stats
}
