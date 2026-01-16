# Filter to variables of interest and remove both "hd_" prefix and
# "_mean" but only at the end of the variable name
clean_elic_data <- function(line_level_data) {
  vars_of_interest <- c(
    "contacts_reached_per_cases_assigned",
    "hd_percent_naming_contacts_mean",
    "percent_naming_contacts",
    "hd_contacts_reached_perc_mean",
    "contacts_reached_perc",
    "hd_contacts_named_ratio_mean_cases_naming_mean",
    "contacts_named_ratio_mean_cases_naming",
    "contacts_named_ratio_mean_household",
    "contacts_named_ratio_mean",
    "hd_contacts_named_ratio_mean_mean",
    "contacts_named_perc_household",
    "contacts_named_ratio_med",
    "cases_reached_perc",
    "hd_cases_interviewed_perc_mean",
    "cases_interviewed_perc",
    "percent_naming_contacts",
    "contacts_interviewed_perc"
  )
  cleaned_line_level_data <- line_level_data |>
    filter(param_name %in% vars_of_interest) |>
    mutate(
      simp_pname = param_name,
      stargel_param = ifelse(
        grepl("64 HD accross US and collonies", source),
        "stargel",
        "non-stargel"
      ),
      simp_pname = gsub("hd_", "", simp_pname),
      simp_pname = gsub("_med", "_mean", simp_pname),
      simp_pname = gsub("_mean$|_mean_mean", "", simp_pname)
      # param_value = ifelse((simp_pname %in% "percent_naming_contacts") &
      #                        (stargel_param %in% "stargel"),
      #                      param_value * 100, param_value)
    )

  return(cleaned_line_level_data)
  # wide_data <- cleaned_line_level_data |> pivot_wider(
  #   id_cols = c("pm_start_date", "source", "pm_location", "param_type"),
  #   values_from = param_value, names_from = param_name
  #   )
}
