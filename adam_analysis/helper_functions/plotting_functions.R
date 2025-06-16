# Invert the order of an axis for a date object.
library(scales)

calc_monthly_averages <- function(data_frame) {
  monthly_averages <- data_frame |> group_by(month_year, metric, periods) |>
    summarise(
      num_sources = n(),
      overall_count = sum(count),
      inclusive_ave = sum(count * param_value) / overall_count,
      exclusive_ave = sum(incl * count * param_value) / sum(incl * count)
    ) |> mutate(count = overall_count)
  return(monthly_averages)
}

fill_missing_values <- function(monthly_averages, imputation_df) {
  monthly_wide <- monthly_averages |> select(
    month_year, metric, inclusive_ave, periods
  ) |> pivot_wider(
    id_cols = c("month_year", "metric"),
    values_from = inclusive_ave, names_from = periods)

  date_metric_missing <- monthly_wide |> pivot_longer(
    cols = setdiff(colnames(monthly_wide), c("month_year", "metric")),
    names_to = "periods", values_to = "average")
  if (!("Symptoms to Test" %in% colnames(monthly_wide))) {
    monthly_wide$`Symptoms to Test` <- NA_real_
  }

  monthly_wide_filled <- monthly_wide |> mutate(
    `Contact Named to Reached` = ifelse(
      is.na(`Contact Named to Reached`),
      imputation_df |> filter(periods == "D") |> pull(ave_time),
      `Contact Named to Reached`),
    `Result to Case Notified` = ifelse(
      is.na(`Result to Case Notified`),
      imputation_df |> filter(periods == "C") |> pull(ave_time),
      `Result to Case Notified`),
    `Test to Result` = ifelse(
      is.na(`Test to Result`),
      imputation_df |> filter(periods == "B") |> pull(ave_time),
      `Test to Result`),
    `Symptoms to Test` = ifelse(
      is.na(`Symptoms to Test`),
      imputation_df |> filter(periods == "A") |> pull(ave_time),
      `Symptoms to Test`)
  )

  long_monthly_with_impt <- monthly_wide_filled |>
    pivot_longer(
      cols = setdiff(colnames(monthly_wide), c("month_year", "metric")),
      names_to = "periods", values_to = "average") |>
    left_join(
      date_metric_missing |> mutate(imputed = is.na(average)) |>
        select(-average), by = c("month_year", "metric", "periods")) |>
    mutate(periods = factor(periods,
                            levels = names(periods_to_label),
                            labels = names(periods_to_label)))
  return(long_monthly_with_impt)
}

c_trans <- function(a, b, breaks = b$breaks, format = b$format, domain = b$domain) {
  a <- as.trans(a)
  b <- as.trans(b)
  name <- paste(a$name, b$name, sep = "-")
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  trans_new(name, trans, inv, breaks = breaks, format = format, domain = domain)
}

rev_date <- c_trans("reverse", "time")

theme_altair <- function() {
  theme_minimal(base_family = "Arial", base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      legend.position = "right",
      plot.title = element_text(size = 16, hjust = 0, face = "plain"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    )
}
# This also needs to be added for the altair theme:
# scale_y_continuous(expand = c(0, 0))
#
# periods_to_label <- c("A" = "Symptoms to Test",
#                       "B" = "Test to Result",
#                       "C" = "Result to Case Notified",
#                       "D" = "Contact Named to Reached")

faceted_period_article_split_plot <- function(dat, metr, title) {
  dat |>
    filter(metric == metr) |>
    filter(impt_type_alpha != 0.7) |>
    ggplot(aes(x = comb_loc_date,
               y = param_value / 24,
               fill = periods,
               linewidth = impt_type_alpha)) +
    scale_linewidth_continuous(range = c(0, 0.4)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_col(position = "stack", color = "black") + coord_flip() +
    scale_fill_manual(
      "", values = ggthemes::tableau_color_pal("Tableau 10")(4) |> rev()) +
    guides(linewidth = "none") +
    ylab("Days") +
    facet_wrap(~periods, nrow = 1, scales = "free_x") +
    theme_altair() +
    ggtitle(title) +
    theme(axis.title.y = element_blank())
}

single_period_plot <- function(monthly_data, sp_val) {
  sub_df <- monthly_data |> filter(periods == sp_val) |>
    mutate(
      has_sample_size = !is.na(exclusive_ave),
      average = ifelse(has_sample_size, inclusive_ave, exclusive_ave),
      sample_size = ifelse(has_sample_size, "SS available", "SS unavailable"),
      count = ifelse(has_sample_size, overall_count, 1))
  overall_ave <- sub_df |> group_by(metric) |>
    summarise(exclusive_ave =
                sum(exclusive_ave * overall_count, na.rm = TRUE) /
                sum(overall_count * !is.na(exclusive_ave)))
  sub_df |>
    ggplot(aes(y = inclusive_ave / 24,
               x = month_year, shape = has_sample_size,
               color = metric)) +
    scale_size_continuous(guide = NULL, range = c(2, 8)) +
    scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
    geom_point(aes(size = count)) +
    scale_shape_manual("", values = c(1, 20),
                       breaks = c(FALSE, TRUE),
                       labels = c("Sample Size Unavailable",
                                  "Sample Size Available")) +
    geom_hline(data = overall_ave,
               aes(yintercept = exclusive_ave / 24,
                   color = metric)) +
    scale_x_date(date_breaks = "2 months",
                 date_minor_breaks = "month",
                 labels = scales::label_date("%b-%Y"),
                 expand = c(0.025, 0.1)) +
    scale_y_continuous(expand = c(0, 0.1), limits = c(0, NA)) +
    theme_altair() +
    theme(axis.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0), # turned off for alignment
          legend.justification.top = "left",
          legend.justification.left = "bottom",
          legend.justification.bottom = "right",
          legend.justification.inside = c(0.9, 0.1),
          legend.location = "plot",
          legend.box = "horizontal",
          legend.direction = "horizontal",
          panel.grid.minor.x = element_line(color = "lightgray",
                                            linewidth = 0.25)) +
    scale_color_manual("", values =
                         ggthemes::tableau_color_pal("Tableau 10")(2),
                       breaks = c("med", "mean"),
                       labels = c("Median", "Mean")) +
    guides(color = guide_legend(nrow = 1, position = "inside"),
           shape = guide_legend(nrow = 1, position = "inside")) +
    ggtitle(paste0("Time from ", sp_val, " (in days)"))
}

periods_to_label <- c("Symptoms to Test" = "A",
                      "Test to Result" = "B",
                      "Result to Case Notified" = "C",
                      "Contact Named to Reached" = "D")


