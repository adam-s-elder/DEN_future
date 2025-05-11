library(tidyverse)

generation_times <- read.csv("../data_extraction/additional_data/wa_cases_averted/generation_times.csv")

generation_times |> group_by(variant) |>
  arrange(days) |> mutate(
    prop_lost = cumsum(prob_gt),
    prop_left = 1 - prop_lost,
    prop_pcr = lead(prop_left, 2),
    min_left =  min(prop_left[-which.min(prop_left)]),
    prop_pcr = ifelse(is.na(prop_pcr) | prop_pcr < min_left,
                      min_left,
                      prop_pcr)
    ) |> filter(days < 11) |>
  mutate(frac_left = prop_pcr / prop_left) |>
  select(days, variant, prop_left, prob_gt,
         prop_left, prop_pcr, frac_left) |>
  pivot_longer(cols = c("prop_left", "prob_gt",
                        "prop_left", "prop_pcr", "frac_left"),
               names_to = "type", values_to = "value") |>
  ggplot(aes(x = days, y = value)) + geom_line(aes(color = type)) +
  facet_wrap(~ variant)
age standarized meanin
