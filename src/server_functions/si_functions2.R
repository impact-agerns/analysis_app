max_aggregate_sm <- function(data, severity_dictionary, admin_bounds = admin_boundaries) {
  # Identify select multiple prefixes
  prefixes <- severity_dictionary %>%
    filter(type %in% c("select_multiple", "select multiple")) %>%
    pull(question) %>%
    sub("\\..*|/.*", "", .) %>%
    unique()

  # Process data: adjust questions and calculate max severity_value
  data %>%
    mutate(
      question = ifelse(
        sub("\\..*|/.*", "", question) %in% prefixes,
        paste0(sub("\\..*|/.*", "", question)),
        question
      )
    ) %>%
    group_by(across(admin_bounds), sector, type, question) %>%
    summarize(
      severity_value = ifelse(all(is.na(severity_value)), NA, max(severity_value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    ungroup()
}


add_flag3 <- function(data) {
  data %>%
    mutate(flag3 = case_when(
      severity_value >= 3 ~ 1,
      severity_value < 3  ~ 0,
      TRUE ~ NA_real_
    ))
}

add_flag4 <- function(data) {
  data %>%
    mutate(flag4 = case_when(
      severity_value >= 4 ~ 1,
      severity_value < 4  ~ 0,
      TRUE ~ NA_real_
    ))
}

add_flag4_plus <- function(data) {
  data %>%
    mutate(flag4_plus = case_when(
      severity_value > 4  ~ 1,
      severity_value <= 4 ~ 0,
      TRUE ~ NA_real_
    ))
}

# NOTE on flag logic: flag4_plus (>4) is always <= flag4 (>=4) for any indicator,
# so proportion4+ will always be <= proportion4. This is enforced by the numeric
# comparisons above (severity_value must be numeric, not character).

# ---------------------------------------------------------------------------
# Per-settlement helpers
# ---------------------------------------------------------------------------

add_flag3_per_settlement <- function(data, admin_bounds) {
  data %>%
    group_by(across(admin_bounds)) %>%
    mutate(flag3_settlement = sum(flag3, na.rm = TRUE)) %>%
    ungroup()
}

add_flag4_per_settlement <- function(data, admin_bounds) {
  data %>%
    group_by(across(admin_bounds)) %>%
    mutate(flag4_settlement = sum(flag4, na.rm = TRUE)) %>%
    ungroup()
}

add_flag4_plus_per_settlement <- function(data, admin_bounds) {
  data %>%
    group_by(across(admin_bounds)) %>%
    mutate(flag4_plus_settlement = sum(flag4_plus, na.rm = TRUE)) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Per-area-sector helpers  (grouped by area admin bounds + sector)
# ---------------------------------------------------------------------------

add_flag3_per_sector <- function(data, admin_bounds) {
  data %>%
    group_by(across(admin_bounds), sector) %>%
    mutate(flag3_sector = sum(flag3, na.rm = TRUE)) %>%
    ungroup()
}

add_flag4_per_sector <- function(data, admin_bounds) {
  data %>%
    group_by(across(admin_bounds), sector) %>%
    mutate(flag4_sector = sum(flag4, na.rm = TRUE)) %>%
    ungroup()
}

add_flag4_plus_per_sector <- function(data, admin_bounds) {
  data %>%
    group_by(across(admin_bounds), sector) %>%
    mutate(flag4_plus_sector = sum(flag4_plus, na.rm = TRUE)) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Proportion per area-sector  (denominator = indicators available in that area-sector)
# ---------------------------------------------------------------------------

add_proportion3_per_sector <- function(data, official_admin_boundaries, admin_bounds_sector, num_sec_indicators) {
  admin_bounds <- admin_bounds_sector

  data <- data %>%
    add_flag3() %>%
    # flag counts at the SAME grouping level as the proportion (area-sector)
    add_flag3_per_sector(admin_bounds) %>%
    group_by(across(all_of(admin_bounds)), sector) %>%
    mutate(
      num_avail_ind_sector    = n_distinct(question[!is.na(severity_value)]),
      # denominator: all rows (available indicators) in this area-sector
      sum_indicators_admin_sector = n()
    ) %>%
    ungroup()

  if (!"total_ind_per_sector" %in% colnames(data)) {
    data <- data %>% left_join(num_sec_indicators, by = "sector")
  }

  data %>%
    group_by(across(admin_bounds)) %>%
    mutate(
      error_message_sector = ifelse(
        num_avail_ind_sector < (0.5 * total_ind_per_sector),
        "Number of available indicators in sector is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      ),
      proportion3_sector = case_when(
        !is.na(error_message_sector) ~ NA_real_,
        sum_indicators_admin_sector == 0 ~ NA_real_,
        TRUE ~ round(flag3_sector / sum_indicators_admin_sector * 100, 2)
      )
    ) %>%
    ungroup()
}

add_proportion4_per_sector <- function(data, official_admin_boundaries, admin_bounds_sector, num_sec_indicators) {
  admin_bounds <- admin_bounds_sector

  data <- data %>%
    add_flag4() %>%
    # BUG FIX: flag must be computed at the same admin level as the proportion (admin_bounds_sector),
    # not official_admin_boundaries (settlement level) as in the original code.
    add_flag4_per_sector(admin_bounds) %>%
    group_by(across(all_of(admin_bounds)), sector) %>%
    mutate(
      num_avail_ind_sector        = n_distinct(question[!is.na(severity_value)]),
      sum_indicators_admin_sector = n()
    ) %>%
    ungroup()

  if (!"total_ind_per_sector" %in% colnames(data)) {
    data <- data %>% left_join(num_sec_indicators, by = "sector")
  }

  data %>%
    group_by(across(admin_bounds)) %>%
    mutate(
      error_message_sector = ifelse(
        num_avail_ind_sector < (0.5 * total_ind_per_sector),
        "Number of available indicators in sector is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      ),
      proportion4_sector = case_when(
        !is.na(error_message_sector) ~ NA_real_,
        sum_indicators_admin_sector == 0 ~ NA_real_,
        TRUE ~ round(flag4_sector / sum_indicators_admin_sector * 100, 2)
      )
    ) %>%
    ungroup()
}

add_proportion4_plus_per_sector <- function(data, official_admin_boundaries, admin_bounds_sector, num_sec_indicators) {
  admin_bounds <- admin_bounds_sector

  data <- data %>%
    add_flag4_plus() %>%
    add_flag4_plus_per_sector(admin_bounds) %>%
    group_by(across(all_of(admin_bounds)), sector) %>%
    mutate(
      num_avail_ind_sector        = n_distinct(question[!is.na(severity_value)]),
      sum_indicators_admin_sector = n()
    ) %>%
    ungroup()

  if (!"total_ind_per_sector" %in% colnames(data)) {
    data <- data %>% left_join(num_sec_indicators, by = "sector")
  }

  data %>%
    group_by(across(admin_bounds)) %>%
    mutate(
      error_message_sector = ifelse(
        num_avail_ind_sector < (0.5 * total_ind_per_sector),
        "Number of available indicators in sector is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      ),
      proportion4_plus_sector = case_when(
        !is.na(error_message_sector) ~ NA_real_,
        sum_indicators_admin_sector == 0 ~ NA_real_,
        TRUE ~ round(flag4_plus_sector / sum_indicators_admin_sector * 100, 2)
      )
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Proportion per settlement  (denominator = non-NA indicators in that settlement)
# ---------------------------------------------------------------------------

add_proportion3_per_settlement <- function(data, admin_bounds, len_all_indicators) {
  cat('\n now adding proportion 3 settlement\n')
  data %>%
    add_flag3_per_settlement(admin_bounds) %>%
    filter(!is.na(severity_value)) %>%
    group_by(across(admin_bounds)) %>%
    mutate(
      num_ind = sum(!is.na(severity_value)),
      proportion_not_recommended = num_ind < (0.5 * len_all_indicators),
      error_message_settlement = ifelse(
        proportion_not_recommended,
        "Number of indicators is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      ),
      proportion3_settlement = ifelse(
        proportion_not_recommended | num_ind == 0,
        NA,
        round(flag3_settlement / num_ind * 100, 2)
      )
    ) %>%
    ungroup()
}

add_proportion4_per_settlement <- function(data, admin_bounds, len_all_indicators) {
  data %>%
    add_flag4_per_settlement(admin_bounds) %>%
    filter(!is.na(severity_value)) %>%
    group_by(across(admin_bounds)) %>%
    mutate(
      num_ind = sum(!is.na(severity_value)),
      proportion_not_recommended = num_ind < (0.5 * len_all_indicators),
      error_message_settlement = ifelse(
        proportion_not_recommended,
        "Number of indicators is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      ),
      proportion4_settlement = ifelse(
        proportion_not_recommended | num_ind == 0,
        NA,
        round(flag4_settlement / num_ind * 100, 2)
      )
    ) %>%
    ungroup()
}

add_proportion4_plus_per_settlement <- function(data, admin_bounds, len_all_indicators) {
  data %>%
    add_flag4_plus() %>%
    add_flag4_plus_per_settlement(admin_bounds) %>%
    filter(!is.na(severity_value)) %>%
    group_by(across(admin_bounds)) %>%
    mutate(
      num_ind = sum(!is.na(severity_value)),
      proportion_not_recommended = num_ind < (0.5 * len_all_indicators),
      error_message_settlement = ifelse(
        proportion_not_recommended,
        "Number of indicators is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      ),
      proportion4_plus_settlement = ifelse(
        proportion_not_recommended | num_ind == 0,
        NA,
        round(flag4_plus_settlement / num_ind * 100, 2)
      )
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Mean index helpers
# ---------------------------------------------------------------------------

add_mean_index_per_settlement <- function(data, admin_bounds) {
  data %>%
    filter(!is.na(severity_value)) %>%
    group_by(across(admin_bounds)) %>%
    mutate(mean_index_settlement = mean(severity_value, na.rm = TRUE)) %>%
    ungroup()
}

add_mean_anf_index_per_settlement <- function(data, admin_bounds) {
  mean_values <- data %>%
    filter(!grepl('edu|cm', sector, ignore.case = TRUE)) %>%
    group_by(across(admin_bounds)) %>%
    summarise(mean_anf_index_settlement = mean(severity_value, na.rm = TRUE), .groups = "drop")

  data %>%
    left_join(mean_values, by = admin_bounds)
}

# ---------------------------------------------------------------------------
# Score index (25th-percentile interpolation)
# ---------------------------------------------------------------------------

add_settlement_proportion_score <- function(data_index, official_admin_boundaries) {
  settlement_proportions <- data_index %>%
    filter(!is.na(severity_value)) %>%
    group_by(across(all_of(official_admin_boundaries)), severity_value) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(across(all_of(official_admin_boundaries))) %>%
    mutate(
      N    = sum(n),
      prop = round(n / N * 100, 2)
    ) %>%
    select(-n) %>%
    arrange(across(all_of(official_admin_boundaries)), desc(severity_value)) %>%
    group_by(across(all_of(official_admin_boundaries))) %>%
    mutate(
      cumulative_prop = cumsum(prop),
      score_index_settlement = ifelse(
        length(na.omit(cumulative_prop)) > 1,
        approx(cumulative_prop, severity_value, xout = 25, rule = 2)$y,
        severity_value
      ),
      score_index_settlement_rounded = round(score_index_settlement, 0)
    ) %>%
    ungroup() %>%
    select(all_of(official_admin_boundaries), score_index_settlement, score_index_settlement_rounded) %>%
    unique()

  data_index %>%
    left_join(settlement_proportions, by = c(all_of(official_admin_boundaries)))
}

add_sector_proportion_score <- function(data_index, official_admin_boundaries) {
  sector_proportions <- data_index %>%
    filter(!is.na(severity_value)) %>%
    group_by(across(all_of(official_admin_boundaries)), sector, severity_value) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(across(all_of(official_admin_boundaries)), sector) %>%
    mutate(
      N    = sum(n),
      prop = round(n / N * 100, 2)
    ) %>%
    select(-n) %>%
    arrange(across(all_of(official_admin_boundaries)), sector, desc(severity_value)) %>%
    group_by(across(all_of(official_admin_boundaries)), sector) %>%
    mutate(
      cumulative_prop = cumsum(prop),
      score_index_sector = ifelse(
        length(na.omit(cumulative_prop)) > 1,
        approx(cumulative_prop, severity_value, xout = 25, rule = 2)$y,
        severity_value
      ),
      score_index_sector_rounded = round(score_index_sector, 0)
    ) %>%
    ungroup() %>%
    select(all_of(official_admin_boundaries), sector, score_index_sector, score_index_sector_rounded) %>%
    unique()

  data_index %>%
    left_join(sector_proportions, by = c(all_of(official_admin_boundaries), "sector"))
}

# ---------------------------------------------------------------------------
# Area-level flag mean summaries
# ---------------------------------------------------------------------------

add_mean_flag3_area <- function(data, admin_bounds) {
  area_bounds <- admin_bounds

  out <- data %>%
    filter(!is.na(severity_value)) %>%
    select(-c(sector, type, question, severity_value, flag3, contains('sector'))) %>%
    unique() %>%
    group_by(across(all_of(area_bounds))) %>%
    summarise(mean_flag3_area = mean(flag3_settlement, na.rm = TRUE), .groups = "drop")

  data %>% left_join(out, by = c(all_of(area_bounds)))
}

add_mean_flag4_area <- function(data, admin_bounds) {
  area_bounds <- admin_bounds

  out <- data %>%
    filter(!is.na(severity_value)) %>%
    select(-c(sector, type, question, severity_value, flag4, contains('sector'))) %>%
    unique() %>%
    group_by(across(all_of(area_bounds))) %>%
    summarise(mean_flag4_area = mean(flag4_settlement, na.rm = TRUE), .groups = "drop")

  data %>% left_join(out, by = c(all_of(area_bounds)))
}

add_mean_flag4_plus_area <- function(data, admin_bounds) {
  area_bounds <- admin_bounds

  out <- data %>%
    filter(!is.na(severity_value)) %>%
    select(-c(sector, type, question, severity_value, flag4_plus, contains('sector'))) %>%
    unique() %>%
    group_by(across(all_of(area_bounds))) %>%
    summarise(mean_flag4_plus_area = mean(flag4_plus_settlement, na.rm = TRUE), .groups = "drop")

  data %>% left_join(out, by = c(all_of(area_bounds)))
}

add_mean_proportion3_area <- function(data, admin_bounds, len_all_indicators) {
  cat('\n now adding proportion 3 area\n')
  area_bounds <- admin_bounds

  # Denominator: total non-NA indicator rows across all settlements in the area
  # (num_ind is per-settlement; sum across unique settlements avoids double-counting)
  out <- data %>%
    filter(!is.na(severity_value)) %>%
    select(-c(sector, type, question, severity_value, flag3, contains('sector'))) %>%
    unique() %>%
    group_by(across(all_of(area_bounds))) %>%
    summarise(
      flag3_area = sum(flag3_settlement, na.rm = TRUE),
      n          = n(),
      num_ind    = sum(num_ind, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(
      mean_proportion3_area = case_when(
        num_ind < (0.5 * len_all_indicators) ~ NA_real_,
        num_ind == 0 ~ NA_real_,
        TRUE ~ round(flag3_area / num_ind * 100, 2)
      ),
      error_message_area = ifelse(
        num_ind < (0.5 * len_all_indicators),
        "Number of indicators is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      )
    ) %>%
    select(contains('admin'), contains('proportion'), error_message_area)

  data %>% left_join(out, by = c(all_of(area_bounds)))
}

add_mean_proportion4_area <- function(data, admin_bounds, len_all_indicators) {
  area_bounds <- admin_bounds

  out <- data %>%
    filter(!is.na(severity_value)) %>%
    select(-c(sector, type, question, severity_value, flag4, contains('sector'))) %>%
    unique() %>%
    group_by(across(all_of(area_bounds))) %>%
    summarise(
      flag4_area = sum(flag4_settlement, na.rm = TRUE),
      n          = n(),
      num_ind    = sum(num_ind, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(
      mean_proportion4_area = case_when(
        num_ind < (0.5 * len_all_indicators) ~ NA_real_,
        num_ind == 0 ~ NA_real_,
        TRUE ~ round(flag4_area / num_ind * 100, 2)
      ),
      error_message_area = ifelse(
        num_ind < (0.5 * len_all_indicators),
        "Number of indicators is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      )
    ) %>%
    select(contains('admin'), contains('proportion'), error_message_area)

  data %>% left_join(out, by = c(all_of(area_bounds)))
}

add_mean_proportion4_plus_area <- function(data, admin_bounds, len_all_indicators) {
  area_bounds <- admin_bounds

  out <- data %>%
    filter(!is.na(severity_value)) %>%
    select(-c(sector, type, question, severity_value, flag4_plus, contains('sector'))) %>%
    unique() %>%
    group_by(across(all_of(area_bounds))) %>%
    summarise(
      flag4_plus_area = sum(flag4_plus_settlement, na.rm = TRUE),
      n               = n(),
      num_ind         = sum(num_ind, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(
      mean_proportion4_plus_area = case_when(
        num_ind < (0.5 * len_all_indicators) ~ NA_real_,
        num_ind == 0 ~ NA_real_,
        TRUE ~ round(flag4_plus_area / num_ind * 100, 2)
      ),
      error_message_area = ifelse(
        num_ind < (0.5 * len_all_indicators),
        "Number of indicators is below 50% of the total. No proportion calculated; use absolute flag index.",
        NA_character_
      )
    ) %>%
    select(contains('admin'), contains('proportion'), error_message_area)

  data %>% left_join(out, by = c(all_of(area_bounds)))
}

add_area_score_index_proportion_25 <- function(data, admin_bounds) {
  area_bounds <- admin_bounds

  area_proportions <- data %>%
    group_by(across(all_of(area_bounds)), severity_value) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(across(all_of(area_bounds))) %>%
    mutate(
      N    = sum(n),
      prop = round(n / N * 100, 4)
    ) %>%
    arrange(across(all_of(area_bounds)), desc(severity_value)) %>%
    group_by(across(all_of(area_bounds))) %>%
    mutate(
      cumulative_prop = cumsum(prop),
      score_index_area = ifelse(
        length(na.omit(cumulative_prop)) > 1,
        approx(cumulative_prop, severity_value, xout = 25, rule = 2)$y,
        severity_value
      ),
      score_index_area_rounded = round(score_index_area, 0)
    ) %>%
    ungroup()

  area_proportions_short <- area_proportions %>%
    select(all_of(area_bounds), score_index_area, score_index_area_rounded) %>%
    unique()

  data %>%
    left_join(area_proportions_short, by = c(all_of(area_bounds)))
}

# ===========================================================================
# NEW: Aggregate sector summary (global/national level — across all areas)
# Denominator: total non-NA indicators for that sector across ALL settlements
# ===========================================================================

build_aggregate_sector_tab <- function(data_index_clean, selected_methods, num_sec_indicators, len_all_indicators) {

  # Base: one row per sector × question, ignoring admin splits
  base <- data_index_clean %>%
    filter(!is.na(severity_value)) %>%
    select(sector, question, severity_value,
           starts_with("flag3"), starts_with("flag4"))

  results <- data_index_clean %>%
    filter(!is.na(severity_value)) %>%
    group_by(sector) %>%
    summarise(
      n_indicators_available = n_distinct(question),
      .groups = "drop"
    ) %>%
    left_join(num_sec_indicators, by = "sector")

  # Helper: compute aggregate proportion for a flag column
  agg_proportion <- function(flag_col_name, prop_col_name) {
    data_index_clean %>%
      filter(!is.na(severity_value)) %>%
      group_by(sector) %>%
      summarise(
        flag_sum  = sum(.data[[flag_col_name]], na.rm = TRUE),
        denom     = n(),          # total non-NA indicator rows for that sector
        .groups   = "drop"
      ) %>%
      mutate(
        !!prop_col_name := ifelse(denom == 0, NA_real_, round(flag_sum / denom * 100, 2))
      ) %>%
      select(sector, all_of(prop_col_name))
  }

  if ("flag3" %in% selected_methods) {
    results <- results %>%
      left_join(agg_proportion("flag3", "proportion3_aggregate_sector"), by = "sector")
  }
  if ("flag4" %in% selected_methods) {
    results <- results %>%
      left_join(agg_proportion("flag4", "proportion4_aggregate_sector"), by = "sector")
  }
  if ("flag4+" %in% selected_methods) {
    results <- results %>%
      left_join(agg_proportion("flag4_plus", "proportion4_plus_aggregate_sector"), by = "sector")
  }

  # Logic check: proportion4+ should never exceed proportion4
  if (all(c("proportion4_aggregate_sector", "proportion4_plus_aggregate_sector") %in% names(results))) {
    check_fail <- results %>%
      filter(!is.na(proportion4_aggregate_sector) & !is.na(proportion4_plus_aggregate_sector)) %>%
      filter(proportion4_plus_aggregate_sector > proportion4_aggregate_sector)
    if (nrow(check_fail) > 0) {
      warning("Logic check FAILED: proportion4+ > proportion4 for sectors: ",
              paste(check_fail$sector, collapse = ", "),
              ". Check severity_value is numeric, not character.")
    } else {
      cat("Logic check PASSED: proportion4+ <= proportion4 for all sectors.\n")
    }
  }

  results
}
