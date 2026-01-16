demand_plot <- function(plt_data) {
  plt_data |>
    mutate(
      param_name = gsub(pattern = "_", replacement = " ", param_name) |>
        gsub(pattern = "count", replacement = "") |>
        stringr::str_to_title() |>
        gsub(pattern = "Us Cases", replacement = "Cases Reported to CDC")
    ) |>
    ggplot(aes(
      x = pm_start_date,
      y = param_value,
      color = param_name,
      group = param_name
    )) +
    geom_line() +
    geom_point() +
    ggthemes::scale_color_tableau() +
    scale_y_log10(labels = scales::label_comma()) +
    theme_altair() +
    theme(
      axis.title = element_blank(),
      legend.position = "bottom"
    ) +
    ggtitle("Overview of Case Investigation and Contact Tracing Demand") +
    guides(color = guide_legend(nrow = 2, title = ""))
}
