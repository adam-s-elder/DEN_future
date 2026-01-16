source_count_plot <- function(file_location, pub_file_location) {
  pub_dates <- read_csv(pub_file_location) |>
    mutate(
      publish_date = lubridate::mdy(date_published)
    ) |>
    select(-date_published)
  split_month_data <- read_csv(
    file_location
  ) |>
    left_join(pub_dates, by = "source") |>
    mutate(
      pm_start_date = lubridate::mdy(pm_start_date),
      param_type = ifelse(is.na(param_type), "manual", param_type),
      source = paste0(source, "%%%", pm_location, "%%%", param_type),
      nest_source = source,
      nest_start_date = pm_start_date
    )

  # Simple source
  trunc_source <-
    split_month_data$source |>
    str_split("%%%") |>
    map(.f = function(x) x[1]) |>
    do.call(what = c)
  split_month_data$source <- trunc_source
  start_and_end_dates <- split_month_data |>
    group_by(pm_start_date, pm_end_date, source, publish_date) |>
    count()

  start_vs_end <- start_and_end_dates |>
    ungroup() |>
    select(-n) |>
    mutate(
      start_date = lubridate::round_date(pm_start_date, unit = "month"),
      end_date = lubridate::round_date(
        lubridate::mdy(pm_end_date),
        unit = "month"
      )
    )

  start_vs_end <- start_vs_end |>
    mutate(
      simple_source = case_when(
        grepl(pattern = "ENPA and ENCV", source) ~ "ENPA and ENCV",
        grepl(pattern = "NPR JHU colaborative survey", source) ~ "NPR JHU",
        TRUE ~ "Academic Sources"
      )
    )
  counts_by_study_month <- start_vs_end |>
    group_by(simple_source, start_date, end_date) |>
    count()
  counts_by_publish_month <- start_vs_end |>
    mutate(
      publish_month = lubridate::round_date(publish_date, unit = "month")
    ) |>
    group_by(publish_month) |>
    summarise(n = length(unique(source)))
  both_counts <- bind_rows(
    counts_by_study_month |> mutate(type = "Study Month"),
    counts_by_publish_month |>
      mutate(type = "Publish Date", n = -1 * n, start_date = publish_month)
  )

  study_period_hist <- counts_by_study_month |>
    ggplot(aes(x = start_date, y = n, fill = simple_source)) +
    geom_col() +
    ggthemes::scale_fill_tableau() +
    guides(fill = guide_legend(title = "")) +
    theme_altair() +
    theme(legend.position = "bottom", axis.title = element_blank()) +
    ggtitle("Data Sources Count by Month")

  study_vs_publish_hist <- both_counts |>
    ggplot(aes(x = start_date, y = n, fill = type)) +
    geom_col() +
    ggthemes::scale_fill_tableau() +
    geom_abline(slope = 0, intercept = 0) +
    guides(fill = guide_legend(title = "")) +
    theme_altair() +
    theme(legend.position = "bottom", axis.title = element_blank()) +
    ggtitle("Data Sources Count by Month") +
    scale_y_continuous(breaks = c(-5, 0, 5, 10), labels = c(5, 0, 5, 10)) +
    coord_flip()

  study_vs_publish_scatter <- start_vs_end |>
    ggplot(aes(x = publish_date, y = pm_start_date)) +
    geom_line(
      aes(group = source),
      alpha = 0.5,
      linewidth = 0.1
    ) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "lightgray") +
    theme_altair() +
    labs(
      title = "Source Study months against Publish Date",
      caption = "ENCV and NPR data are ommited.These data were either not published (ENCV) \n or were published shortly after they were collected (NPR)."
    ) +
    xlab("Date Published") +
    ylab("Date of Study")
  return(list(
    study_period_hist = study_period_hist,
    study_vs_publish_hist = study_vs_publish_hist,
    study_vs_publish_scatter = study_vs_publish_scatter
  ))
}
