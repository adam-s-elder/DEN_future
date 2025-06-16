# Calculate the data required to create the stacked bar charts
# that we present in the report.

deduplicate_and_remove_overlap <- function(long_time_data, split_info) {
  hd_data <- any(grepl("hd_", long_time_data$param_name))
  if (hd_data) {
    split_data <- split_info |> filter(hd_summary == "yes")
  } else {
    split_data <- split_info |> filter(hd_summary != "yes")
  }
  long_data <- long_time_data |>
    left_join(split_data, by = "param_name") |>
    select(param_name, periods, param_value,
           pm_start_date, metric, source, hd_summary) |>
    mutate(param_value = as.numeric(param_value)) |>
    filter(grepl("A|B|C|D", periods),
           !is.na(param_value))
  newlong <- long_data |> summarise(
    param_name = param_name[1],
    param_value = mean(param_value),
    pm_start_date = unique(pm_start_date),
    metric = unique(metric),
    source = unique(source),
    nrows = n(), .by = c("periods", "metric")
  )
  if (any(newlong$nrows > 1)) {
    collapsed_periods <- newlong |> filter(nrows > 1) |>
      pull(periods) |> unique()
    warning("Duplicate data collapsed for ",
            newlong$source[1], "\n", "Param Values were",
            long_data |> filter(periods %in% collapsed_periods) |>
              pull(param_value), "\nNew value: ",
            newlong |> filter(periods %in% collapsed_periods) |>
              pull(param_value), "\n")
    long_data <- newlong |> ungroup()
  }
  orig_data <- long_data
  metrics_recorded <- long_data |> pull(metric) |> unique()
  all_results <- list()
  for (metric_value in metrics_recorded) {
    long_data_m <- long_data |> filter(metric == metric_value)
    while (has_overlap(long_data_m)) {
      # cat("DATA IN: \n \n ", paste(long_data_m, "\n"))
      long_data_m <- remove_overlap(long_data_m, split_data)
    }
    no_ov_data <- long_data_m |> mutate(
      impt_type = "overlap_remove",
      df_type = "removed_overlap")
    all_results[[metric_value]] <- no_ov_data
  }
  return_df <- do.call(what = bind_rows, all_results)
  if (!("hd_summary" %in% colnames(return_df))) browser()
  if (any(is.na(return_df$periods))) {browser()}
  return(return_df)
}

has_overlap <- function(long_data) {
  p_list <- long_data$periods |> str_split("")
  period_counts <- p_list |> do.call(what = c) |> table()
  return(any(period_counts > 1))
}

remove_overlap <- function(long_metric_data, split_data) {
  p_list <- long_metric_data$periods |> str_split("")
  # cat("DATA WITHIN: \n \n ", long_metric_data |> head(), "\n")
  longest <- which.max(p_list |> map(length) |> do.call(what = c))
  remaining_periods <- p_list |> map(.f = function(x) {
    length(setdiff(x, p_list[[longest]]))
  })
  which_subset <- which(remaining_periods == 0)[-longest]
  if (length(which_subset) > 1) {
    subset_lens <-
      p_list[which_subset] |> map(.f = length) |> do.call(what = c)
    longest_subset <- which_subset[which.max(subset_lens)]
  } else {
    longest_subset <- which_subset
  }
  period_diffs <- setdiff(p_list[[longest]], p_list[[longest_subset]])
  # if (length(period_diffs) > 1)
  new_period <- period_diffs
  per_long <- p_list[[longest]] |> paste0(collapse = "")
  per_sub <- p_list[[longest_subset]] |> paste0(collapse = "")
  if (length(per_long) + length(per_sub) < 2) browser()
  wide_data <- long_metric_data |>
    select(periods, param_value) |> pivot_wider(
      names_from = periods,
      values_from = param_value
    )
  new_period_val <- wide_data[, per_long, drop = TRUE] -
    wide_data[, per_sub, drop = TRUE]
  if (new_period_val < 0) {
    warning("Negative value created: ", new_period_val)
    # browser()
  }
  wide_data[, new_period] <- new_period_val
  new_long <- wide_data |> pivot_longer(
    cols = everything(),
    names_to = "periods",
    values_to = "param_value"
  ) |> filter(periods != per_long) |> mutate(
    pm_start_date = unique(long_metric_data$pm_start_date),
    metric = unique(long_metric_data$metric),
    source = unique(long_metric_data$source)
  ) |> left_join(split_data, by = c("periods", "metric"))
  return(new_long)
}

impute_inner <- function(non_ol_data, prop_impt_df) {
  metrics_recorded <- non_ol_data |> pull(metric) |> unique()
  all_results <- list()
  for (metric_value in metrics_recorded) {
    inner_impute <- non_ol_data |> filter(metric %in% metric_value)
    p_list <- inner_impute$periods |> str_split("")
    multi_period_rows <- which(p_list |> map(length) |> do.call(what = c) > 1)
    if (length(multi_period_rows) == 0) {
      impt_df <- inner_impute
    } else {
      impt_df <- inner_impute[-multi_period_rows, ]
    }
    impt_df <- impt_df |> mutate(impt_type = "reported", df_type = "inner")
    for (row_num in multi_period_rows) {
      period_to_split <- p_list[[row_num]]
      props_for_split <- prop_impt_df |>
        filter(periods %in% period_to_split) |> mutate(
          sub_prop = prop_time / sum(prop_time)
        )
      expand_row <- inner_impute[row_num, ]
      expand_row$cnt <- expand_row$periods |> str_length()
      expand_row <- expand_row |> uncount(weights = cnt) |>
        bind_cols(props_for_split |>
                    select(new_period = periods, prop = sub_prop)) |>
        mutate(new_param_value = prop * param_value)
      check <- (abs(expand_row$param_value |> unique() -
                      sum(expand_row$new_param_value)) > 0.01)
      if (check) browser()
      expand_row <- expand_row |>
        mutate(periods = new_period,
               param_value = new_param_value,
               impt_type = "inner", df_type = "inner") |>
        select(-new_period, -new_param_value, -prop)
      impt_df <- impt_df |> bind_rows(expand_row)
    }
    all_results[[metric_value]] <- impt_df
  }
  return_df <- do.call(what = bind_rows, all_results)
  return(return_df)
}

impute_outer <- function(inner_impute, prop_impt_df) {
  metrics_recorded <- inner_impute |> pull(metric) |> unique()
  all_results <- list()
  for (metric_value in metrics_recorded) {
    inner_imput_metric <- inner_impute |> filter(metric %in% metric_value)
    needed_periods <- setdiff(prop_impt_df$periods, inner_imput_metric$periods)
    impt_df <- prop_impt_df
    inner_periods <- inner_imput_metric$periods
    obs_time <- inner_imput_metric$param_value |> sum()
    ratio_obs <- impt_df |>
      mutate(has_obs = as.numeric(periods %in% inner_periods)) |>
      summarise(inner_time = sum(has_obs * prop_time),
                total_time = sum(prop_time)) |>
      mutate(prop_obs = inner_time / total_time) |> pull(prop_obs)
    outer_impt <- impt_df |> select(-ave_time) |> left_join(
      inner_imput_metric |> select(periods, param_value, metric, impt_type,
                                   source, pm_start_date, param_name),
      by = "periods"
    ) |> mutate(
      new_param_value = prop_time * obs_time / ratio_obs
    )
    check <- outer_impt |> filter(periods %in% inner_periods) |>
      summarise(old_time = sum(param_value),
                new_time = sum(new_param_value)) |>
      mutate(check = abs(old_time - new_time) > 0.01) |>
      pull(check)
    if (check) browser()
    # outer_impt_perc <- outer_impt |> mutate(
    #   param_value = ifelse(periods %in% inner_periods,
    #                        param_value,
    #                        new_param_value),
    #   impt_type = ifelse(is.na(impt_type), "outer_perc", impt_type),
    #   df_type = "outer_perc") |>
    #   select(-prop_time, -new_param_value) |>
    #   fill(everything(), .direction = "updown")
    outer_impt_abs <- inner_imput_metric |>
      bind_rows(
        impt_df |> filter(periods %in% needed_periods) |>
          select(param_value = ave_time, periods) |>
          mutate(impt_type = "outer_abs")
      ) |> fill(everything(), .direction = "updown") |>
      mutate(df_type = "outer_abs")
    all_results[[metric_value]] <- outer_impt_abs
  }
  return(do.call(what = bind_rows, all_results))
}

guess_dates <- function(dates) {
  format_guess <- dates |>
    lubridate::guess_formats(orders = c("ymd", "mdy")) |> table()
  guess <- names(format_guess)[which.max(format_guess)]
  if (guess %in% c("%Om/%d/%y", "%m/%d/%y")) {
    date_parsed <- lubridate::mdy(dates) |> format("%Y-%m-%d") |>
      lubridate::ymd()
  } else {
    date_parsed <- lubridate::ymd(dates)
  }
  return(date_parsed)
}

add_plot_variables <- function(imputed_df, orig_data, count_data) {
  loc_df <- orig_data |> filter(grepl("_from_", param_name)) |>
    select(source, pm_location) |>
    group_by(source, pm_location) |>
    filter(row_number() == 1)
  comb_data <- imputed_df |>
    left_join(loc_df, by = "source") |> mutate(
      date = lubridate::mdy(pm_start_date)
    ) |> arrange(desc(date), pm_location)
  final_df <- comb_data |>
    mutate(pm_start_date = lubridate::mdy(pm_start_date)) |>
    left_join(count_data |>
                mutate(pm_start_date = lubridate::mdy(pm_start_date)),
              by = c("source", "pm_start_date"))
  labeled_data <- final_df |> filter(metric != "max") |> mutate(
    digital = ifelse(grepl("digital", source), "digital", "manual"),
    pm_location = gsub(pattern = "communities with higher than average COVID-19 incidence rates", "", pm_location),
    comb_loc_date = factor(
      x = paste0("(", digital, ") ", pm_location, ",", date),
      levels = paste0("(", digital, ") ", pm_location, ",", date),
      labels = paste0("(", digital, ") ", pm_location, ", ", date)),
    periods = forcats::fct_rev(as.factor(periods)),
    impt_type_alpha = case_when(
      impt_type == "reported" ~ 0,
      impt_type == "inner" ~ 0.1,
      impt_type == "outer_abs" ~ 0.7,
      impt_type == "outer_rel" ~ 0.7)
  ) |> mutate(count = ifelse(
    periods == "D", contacts_interviewed, cases_interviewed))
  period_median_count <- labeled_data |>
    filter(df_type == "outer_abs", impt_type != 0.7) |>
    group_by(periods, metric) |>
    summarise(impt_median = median(count, na.rm = TRUE))
  impt_count_data <- labeled_data |>
    left_join(period_median_count, by = c("periods", "metric")) |>
    group_by(periods, metric) |>
    mutate(
      impt_count = is.na(count),
      count = ifelse(impt_count, impt_median, count),
      metric_color = case_when(
        impt_count ~ "imputed",
        metric == "mean" ~ "mean",
        metric == "med" ~ "med"
      ))
  return(impt_count_data)
}


make_time_plot_data <- function(long_data, split_info, param_counts) {
  nested_data <- long_data |> filter(grepl("_from_", param_name)) |>
    group_by(nest_source, nest_start_date, param_type) |>
    tidyr::nest()
  initial_imputation <- nested_data |>
    # filter(grepl("Campus", nest_source)) |>
    mutate(no_ov_data = map(
      data, deduplicate_and_remove_overlap, split_info = split_info))

  impute_df <- calc_split_df(
    initial_imputation$no_ov_data |> do.call(what = bind_rows),
    param_count_df)

  imputed_df <- initial_imputation |>
    mutate(
      inner_impute_data = map(no_ov_data, impute_inner,
                              prop_impt_df = impute_df$impt_df),
      outer_impute_data = map(inner_impute_data, impute_outer,
                              prop_impt_df = impute_df$impt_df))
  period_split_df <- imputed_df$no_ov_data |> do.call(what = bind_rows)
  inner_impute_df <- imputed_df$inner_impute_data |> do.call(what = bind_rows)
  outer_impute_df <- imputed_df$outer_impute_data |> do.call(what = bind_rows)
  imputed_df <- bind_rows(period_split_df, inner_impute_df, outer_impute_df)
  impt_plot_data <- add_plot_variables(imputed_df, long_data, param_counts)
  sp_times <- impt_plot_data |>
    filter(!grepl("Using Automation", source)) |>
    filter(df_type == "outer_abs", impt_type_alpha != 0.7, metric != "max") |>
    mutate(
      month_year = lubridate::floor_date(pm_start_date, "month"),
      incl = 1 - as.numeric(impt_count),
    )
  return(list(
    imputed_df = impt_plot_data,
    single_period_times = sp_times
  ))
}