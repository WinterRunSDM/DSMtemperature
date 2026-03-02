library(tidyverse)

hec_raw <- read_csv("data-raw/hec5q-action-5/hec5q-action5.csv")

hec_daily <- hec_raw |>
  group_by(dataset, datetime) |>
  summarize(values = mean(values), .groups = "drop")

# STREAM TEMPERATURE -----------------------------------------------------------

## hec5q ----
# v2 (BLW CLEAR CREEK) selected over v1 (BLW KESWICK + RED BLUFF DAM) for Upper Sacramento River
# because its residual trends are most consistent with Clear Creek and Cottonwood Creek.
# See: https://github.com/BDO-Science/SIT-DSM-LTO/blob/40f2369910765b0fad8841423748360bf51806bd/cvpiaTemperature/Scripts/DSS%20workflow_cvpiaTemperature_CalSim%203_action5.R
watershed_name_mapping <- c(
  upper_sacramento_river_v2 = "Upper Sacramento River",
  clear_creek               = "Clear Creek",
  cottonwood_creek          = "Cottonwood Creek"
)

hec5q_monthly <- hec_daily |>
  mutate(value_c = (values - 32) * 5 / 9,
         year = year(datetime),
         month = month(datetime),
         watershed = watershed_name_mapping[dataset]) |>
  filter(year %in% 1979:2000) |>
  group_by(watershed, year, month) |>
  summarize(monthly_mean_temp_c = mean(value_c), .groups = "drop") |>
  mutate(date = ymd(paste(year, month, 1))) |>
  select(date, watershed, monthly_mean_temp_c)

## regression ----
regression_monthly <- bind_rows(
  read_rds("data-raw/battle_creek/battle_creek_water_temp_c.rds"),
  read_rds("data-raw/big_chico_creek/big_chico_creek_water_temp_c.rds"),
  read_rds("data-raw/butte_creek/butte_creek_water_temp_c.rds"),
  read_rds("data-raw/cosumnes_river/cosumnes_water_temp_c.rds"),
  read_rds("data-raw/deer_creek/deer_creek_water_temp_c.rds"),
  read_rds("data-raw/lower_mid_sacramento_river/lower_mid_sacramento_river_water_temp_c.rds"),
  read_rds("data-raw/merced_river/merced_river_water_temp_c.rds"),
  read_rds("data-raw/mill_creek/mill_creek_water_temp_c.rds"),
  read_rds("data-raw/mokelumne_river/mokelumne_river_water_temp_c.rds"),
  read_rds("data-raw/san_joaquin/san_joaquin_creek_water_temp_c.rds"),
  read_rds("data-raw/stanislaus_river/stanislaus_river_water_temp_c.rds"),
  read_rds("data-raw/tuolumne_river/tuolumne_river_water_temp_c.rds"),
  read_rds("data-raw/upper_mid_sacramento_river/upper_mid_sacramento_river_water_temp_c.rds"),
  read_rds("data-raw/yuba_river/yuba_river_water_temp_c.rds"),
  read_rds("data-raw/mike_wright_temperature_regression/juv_temp_regression.rds")
)

## empirical ----
empirical_monthly <- bind_rows(
  read_rds("data-raw/lower_sacramento/lower_sac_water_temp_c.rds"),
  read_rds("data-raw/sutter/sutter_bypass_water_temp_c.rds"),
  read_rds("data-raw/yolo/yolo_bypass_water_temp_c.rds")
)

# DEGREE DAYS ------------------------------------------------------------------
temperatures_action_5 <- hec_daily |>
  mutate(date = as_date(datetime),
         watershed = watershed_name_mapping[dataset],
         mean_daily_temp_F = values,
         mean_daily_temp_C = (values - 32) * 5 / 9) |>
  filter(year(date) %in% 1979:2000) |>
  group_by(date, watershed) |>
  summarize(mean_daily_temp_F = mean(mean_daily_temp_F),
            mean_daily_temp_C = mean(mean_daily_temp_C),
            .groups = "drop")

# DEGREE DAYS SR ---------------------------------------------------------------
# TODO: add action 5 updates if spring run updates are needed

# DEGREE DAYS SR ABV DAM -------------------------------------------------------
# TODO: add action 5 updates if spring run updates are needed
