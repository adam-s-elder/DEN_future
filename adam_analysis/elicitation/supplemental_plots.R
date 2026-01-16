# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(patchwork)
source("../helper_functions/read_in_functions.R", chdir = TRUE)

article_level_elicit_data <- read_csv(
  file = "../intermediate_data/article_level_elicit.csv"
)
combined_data <- read_csv(
  file = "../../data_extraction/output/combined_data.csv"
)
month_level_elicit_data <- read_csv(
  file = "../intermediate_data/month_level_elicit.csv"
)

cont_named_per_case_mean_v_median <- combined_data |>
  filter(
    param_name %in%
      c("contacts_named_ratio_med", "contacts_named_ratio_mean")
  ) |>
  ggplot() +
  geom_dotplot(aes(x = param_value, fill = param_name)) +
  theme_altair() +
  theme(legend.position = "bottom", axis.title = element_blank()) +
  guides(fill = guide_legend(title = "")) +
  scale_fill_manual(
    breaks = c("contacts_named_ratio_med", "contacts_named_ratio_mean"),
    values = ggthemes::tableau_color_pal()(2),
    labels = c("Median", "Mean")
  ) +
  ggtitle(
    "Comparing Between Mean and Median Values of Contacts Named Per Case"
  )

ggsave(
  "out/cont_named_per_case_mean_v_median.png",
  cont_named_per_case_mean_v_median,
  width = 8,
  height = 4
)

three_param_details <-
  make_three_param_stargel_cmpr_plot(month_level_elicit_data)

ggsave(
  "out/three_param_with_details.png",
  three_param_details,
  height = 10,
  width = 14
)

contacts_per_case_assigned <-
  make_article_level_plot(
    month_level_elicit_data |> filter(param_type == "manual"),
    "contacts_reached_per_cases_assigned",
    "Contacts Reached Per Case Assigned (Manual)"
  )

ggsave(
  "out/contacts_per_case_assigned.png",
  contacts_per_case_assigned,
  height = 10,
  width = 14
)

contacts_per_case_naming <-
  make_article_level_plot(
    month_level_elicit_data |> filter(param_type == "manual"),
    c(
      "contacts_named_ratio_mean_cases_naming",
      "hd_contacts_named_ratio_mean_cases_naming_mean"
    ),
    "Contacts Named Per Case Naming (Manual)"
  ) +
  ylim(c(0, 5.5))

ggsave(
  "out/contacts_per_case_naming.png",
  contacts_per_case_naming,
  height = 10,
  width = 14
)

# diagnostic plots

## How much month to month variability is there in reported metrics

combined_long_data <- read_csv(
  "../../data_extraction/output/split_month_data.csv"
)

compare_wide_df <- combined_long_data |>
  pivot_wider(
    id_cols = c("pm_start_date", "source", "param_type", "pm_location"),
    names_from = "param_name",
    values_from = "param_value"
  ) |>
  mutate(
    cases_per_ci = cases_assigned_count / staff_hired_ci_count,
    contacts_per_ct = contacts_named_count / staff_hired_ct_count,
    prop_interviewed = cases_interviewed_count / cases_assigned_count,
    prop_cont_reached = contacts_reached_count / contacts_named_count,
  )
compare_wide_df |>
  filter(!is.na(percent_naming_contacts)) |>
  ggplot(
    aes(
      x = pm_start_date,
      y = percent_naming_contacts,
      color = source,
      group = interaction(source, pm_location)
    )
  ) +
  geom_line() +
  geom_point() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.key.size = unit(2, "mm")
  ) +
  guides(color = guide_legend(title = "", ncol = 2))
