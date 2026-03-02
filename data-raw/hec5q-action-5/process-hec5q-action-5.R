library(tidyverse)

hec_raw <- read_csv("data-raw/hec5q-action-5/hec5q-action5.csv")

hec_daily <- hec_raw |>
  group_by(dataset, datetime) |>
  summarize(values = mean(values), .groups = "drop")

juv_tmp <- hec_daily |>
  mutate(value_c = (values - 32) * 5 / 9,
         month = month(datetime, label = TRUE),
         year = year(datetime)) |>
  filter(year %in% 1980:2000) |>
  group_by(dataset, month, year) |>
  summarize(mean_temp = mean(value_c), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = mean_temp)

degday <- hec_daily |>
  mutate(value_c = (values - 32) * 5 / 9,
         month = month(datetime, label = TRUE),
         year = year(datetime)) |>
  filter(year %in% 1979:2000) |>
  group_by(dataset, month, year) |>
  summarize(degday = sum(value_c), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = degday)
