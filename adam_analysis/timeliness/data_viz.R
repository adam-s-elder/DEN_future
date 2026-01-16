library(tidyverse)
plot_data_folder <- paste0("../intermediate_data/", "split_by_month")
source("../helper_functions/read_in_functions.R", chdir = TRUE)

sp_times <- read_csv(paste0(
  plot_data_folder,
  "/plot_single_period_times.csv"
))

filled_monthly <- read_csv(paste0(
  plot_data_folder,
  "/filled_single_period_times.csv"
))

impt_df <- read_csv(paste0(
  plot_data_folder,
  "/time_impute_df.csv"
))

filled_monthly_sympt_to_notified <- read_csv(paste0(
  plot_data_folder,
  "/filled_time_to_notify.csv"
))

# Main text single period-plots

monthly_aves <- sp_times |>
  filter(en_type == "Manual") |>
  calc_monthly_averages()

sympt_to_test <- single_period_plot(monthly_aves, "Symptoms to Test")

ggsave("out/sympt_to_test.png", sympt_to_test, height = 8, width = 14)

test_to_result <-
  single_period_plot(monthly_aves, "Test to Result") +
  ggtitle(
    "Time from Test Submission to Result Reported to Health Department"
  )

ggsave("out/test_to_result.png", test_to_result, height = 8, width = 14)

result_to_notified <-
  single_period_plot(monthly_aves, "Result to Case Notified") + ylim(0, 3)

ggsave(
  "out/result_to_notified.png",
  result_to_notified,
  height = 8,
  width = 14
)

named_to_reached <-
  single_period_plot(monthly_aves, "Contact Named to Reached") +
  theme(
    legend.position.inside = c(0.9, 0.9)
  ) +
  ggtitle("Time from Contact Named to Contact Reached (in days)")

ggsave("out/named_to_reached.png", named_to_reached, height = 8, width = 14)

# Appendix Plot

timeliness_plots <- make_time_appendix_plots(sp_times, monthly_aves)

ggsave(
  "out/all_times_all_types.png",
  timeliness_plots$timeliness1,
  height = 11,
  width = 14
)
ggsave(
  "out/stargle_vs_count_no_count.png",
  timeliness_plots$timeliness2,
  height = 11,
  width = 14
)

stargel_compare <- sp_times |>
  mutate(
    stargel = as.numeric(grepl("64 HD accross US and collonies", source)),
    stargel = ifelse(stargel == 1, "Stargel", "Non-Stargel")
  )

monthly_starg_aves <- stargel_compare |>
  group_by(
    stargel,
    periods,
    metric,
    month_year
  ) |>
  summarise(
    param_value = mean(param_value, na.rm = TRUE)
  ) |>
  ungroup()

starg_time_cmpr <- stargel_compare |>
  ggplot(aes(y = param_value / 24, x = date, color = metric)) +
  geom_point(aes(shape = stargel, size = sqrt(count)), alpha = 0.5) +
  scale_size_continuous(
    "Sample Size",
    breaks = c(10, 100, sqrt(1000000)),
    labels = c(10, 100, sqrt(1000000))**2 |>
      scales::label_comma()()
  ) +
  geom_line(
    data = monthly_starg_aves,
    aes(
      x = month_year,
      y = param_value / 24,
      color = metric,
      linetype = stargel
    )
  ) +
  scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
  scale_x_date(
    date_breaks = "month",
    labels = scales::label_date("%b-%Y"),
    expand = c(0, 0)
  ) +
  scale_shape_manual(
    "",
    values = c(1, 20),
    breaks = c("Stargel", "Non-Stargel"),
    labels = c("Stargel Only", "No Stargel")
  ) +
  scale_y_continuous(expand = c(0, 0.1), limits = c(0, 4)) +
  theme_altair() +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~periods, ncol = 1) +
  ggtitle("Timiliness of Contact Tracing Process (in days)") +
  ggthemes::scale_color_tableau()

ggsave("out/stargel_compare.svg", starg_time_cmpr, height = 10, width = 14)

# The following should mostly be done using the single period times
# and should not have the split month times since the benefits here are
# mostly from looking at article level estimates (per time period reported
# in the article).
faceted_period_article_split_plot(sp_times, "med", "Median")

## Stacked Plot

stacked_time_plot <- filled_monthly |>
  mutate(
    imputed_alpha = 1 - as.numeric(imputed),
    metric = ifelse(metric == "mean", "Mean", "Median")
  ) |>
  ggplot(aes(
    y = average / 24,
    x = month_year,
    fill = forcats::fct_rev(periods)
  )) +
  geom_col(position = position_stack(), aes(alpha = imputed_alpha)) +
  scale_alpha_continuous(
    breaks = c(0, 1),
    labels = c("Imputed", "Calculated"),
    range = c(0.6, 1)
  ) +
  theme_altair() +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "2 month",
    labels = scales::label_date("%b\n%Y")
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(
    fill = guide_legend(title = "", reverse = TRUE),
    alpha = guide_legend(title = "", reverse = TRUE)
  ) +
  theme(legend.position = "bottom", axis.title = element_blank()) +
  facet_wrap(~metric) +
  ggthemes::scale_fill_tableau() +
  ggtitle("Estimated time between case symptom onset and contact notification")
ggsave(
  "out/stacked_time_plot.png",
  stacked_time_plot,
  height = 10,
  width = 18,
  dpi = 300
)

filled_monthly_sympt_to_notified |>
  mutate(metric = ifelse(metric == "mean", "Mean", "Median")) |>
  ggplot(aes(y = symptom_to_notified / 24, x = month_year, color = metric)) +
  geom_point(size = 3) +
  geom_line(alpha = 0.5) +
  theme_altair() +
  scale_x_date(date_breaks = "2 month", labels = scales::label_date("%b\n%Y")) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom", axis.title = element_blank()) +
  ggthemes::scale_fill_tableau() +
  ggtitle("Estimated time between case symptom onset and contact notification")

## Manual vs digital
wa_verify_times <- read_csv(
  "../../data_extraction/datasets/wa_cases_averted/etoen_wa_notify.csv"
)

digital_monthly_filled <-
  calc_monthly_averages(
    sp_times |>
      filter(en_type == "Digital", grepl("California", source))
  ) |>
  fill_missing_values(imputation_df = impt_df) |>
  group_by(month_year, metric) |>
  summarise(symptom_to_notified = sum(average))

waver <- wa_verify_times |>
  mutate(
    value = 24 * (mean_etoen),
    param_name = "test_to_notified"
  )


manual_vs_digital <- waver |>
  select(month_year = monthyear, param_name, symptom_to_notified = value) |>
  mutate(type = "WA Notify") |>
  bind_rows(
    filled_monthly_sympt_to_notified |> mutate(type = "Manual (All Sources)"),
    digital_monthly_filled |> mutate(type = "CA Notiy")
  )

manual_vs_digital_cn_times <- manual_vs_digital |>
  filter(!(metric %in% "med")) |>
  ggplot(aes(x = month_year, y = symptom_to_notified / 24, fill = type)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 25) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_altair() +
  scale_fill_manual(
    "",
    values = ggthemes::tableau_color_pal("Tableau 10")(3)
    # breaks = c("digital", "manual", "other"),
    # labels = c("Digital", "Manual", "Digital Other")
  ) +
  ylab("Days") +
  scale_x_date(date_breaks = "2 month", labels = scales::label_date("%b\n%Y")) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.justification = "right",
    axis.title.x = element_blank()
  ) +
  ggtitle(
    "Time from case symptom onset or contact exposure to contact notification"
  )


ggsave(
  "out/manual_vs_digital_timeliness.png",
  plot = manual_vs_digital_cn_times,
  width = 14,
  height = 10,
  dpi = 300
)


## Appendix / Supplemental plots

publish_and_covered <- source_count_plot(
  file_location = "../../data_extraction/output/combined_data.csv",
  pub_file_location = "../../data_extraction/datasets/manual_data/publish_dates.csv"
)

ggsave(
  "out/data_source_counts_by_month.png",
  plot = publish_and_covered$study_period_hist,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "out/published_vs_study_period_hist.png",
  plot = publish_and_covered$study_vs_publish_hist,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "out/published_vs_study_period_scatter.png",
  plot = publish_and_covered$study_vs_publish_scatter,
  width = 12,
  height = 8,
  dpi = 300
)
