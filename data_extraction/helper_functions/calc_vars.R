# Calculating values that are functions of one another

fill_in_vars <- function(dataframe) {
  wide_data <- dataframe |> pivot_wider(
      id_cols = c("pm_start_date", "source", "param_type", "pm_location"),
                  names_from = "param_name",
                  values_from = "param_value")
  wide_data_impt <- wide_data |> mutate(
    cases_interviewed_perc =
          mutate_for_missing(cases_interviewed_count, cases_assigned_count,
                       cases_interviewed_perc, function(x, y) {100 * x / y}),
    cases_reached_perc =
      mutate_for_missing(cases_reached_count, cases_assigned_count,
                         cases_reached_perc, function(x, y) {100 * x / y}),
    percent_naming_contacts =
      mutate_for_missing(cases_named_contacts_count, cases_interviewed_count,
                         percent_naming_contacts, function(x, y) {100 * x / y}),
    contacts_infected_perc =
      mutate_for_missing(contacts_infected_count, contacts_named_count,
                         contacts_infected_perc, function(x, y) {100 * x / y}),
    contacts_interviewed_perc =
      mutate_for_missing(contacts_interviewed_count, contacts_named_count,
                         contacts_interviewed_perc,
                         function(x, y) {100 * x / y}),
    contacts_reached_perc =
      mutate_for_missing(contacts_reached_count, contacts_named_count,
                         contacts_reached_perc, function(x, y) {100 * x / y}),
    contacts_named_perc_household =
      mutate_for_missing(contacts_named_count_household, contacts_named_count,
                         contacts_named_perc_household,
                         function(x, y) {100 * x / y})
  )
  wide_data_impt <- wide_data_impt |> mutate(
    cases_assigned_perc = 100 *
      (cases_assigned_count /
         (cases_not_assigned_count + cases_assigned_count)),
    contacts_reached_per_cases_assigned = contacts_reached_count /
      cases_assigned_count)
  long_data_new_vals <- wide_data_impt |> pivot_longer(
    cols = -c(pm_start_date, source, param_type, pm_location),
    names_to = "param_name", values_to = "param_value"
  ) |> filter(!is.na(param_value))
  additional_non_name_data <- dataframe |>
    select(pm_start_date, pm_end_date, pm_location, source, param_type) |>
    group_by(pm_start_date, source, param_type, pm_location) |>
    filter(row_number() == 1) |> ungroup()
  param_name_data <- dataframe |>
    select(param_name) |>
    group_by(param_name) |> filter(row_number() == 1) |> ungroup()
  source_text_data <- dataframe |> filter(!is.na(source_text)) |>
    group_by(pm_start_date, source, param_type,
             pm_location, param_value, param_name) |>
    summarise(source_text = unique(source_text))
  return_df <- left_join(
    long_data_new_vals, additional_non_name_data,
    by = c("pm_start_date", "source", "param_type", "pm_location")) |>
    left_join(
      param_name_data, by = "param_name"
    ) |>
    left_join(
      source_text_data,
      by = c("pm_start_date", "source", "param_type",
             "pm_location", "param_value", "param_name"))
  return(return_df)
}

mutate_for_missing <- function(variable_x, variable_y,
                                 var_to_calc, calcfun) {
  return_val <-
    ifelse(is.na(var_to_calc), calcfun(variable_x, variable_y), var_to_calc)
  plot_cmpr <- bind_cols(post = return_val, pre = var_to_calc) |>
    mutate(impt = is.na(pre)) |>
    pivot_longer(cols = c("pre", "post"),
                 names_to = "type", values_to = "val") |>
    ggplot(aes(x = val, fill = impt)) +
    geom_histogram(alpha = 0.4, position = position_identity(), bins = 30)
  # suppressWarnings(plot_cmpr |> print())
  return(return_val)
}
