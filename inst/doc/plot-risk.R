## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(preventr)

## ----plot-risk-helper---------------------------------------------------------
plot_risk_no_add_no_prog <- function(..., add_to_dat = FALSE, progress = FALSE) {
  plot_risk(..., add_to_dat = add_to_dat, progress = progress)
}

## ----example-data-------------------------------------------------------------
risk_10_year <- est_risk(
  age = 55,
  sex = "female",
  sbp = 140,
  bp_tx = TRUE,
  total_c = 210,
  hdl_c = 50,
  statin = FALSE,
  dm = TRUE,
  smoking = FALSE,
  egfr = 90,
  bmi = 31,
  time = "10yr"
)

risk_30_year <- est_risk(
  age = 55,
  sex = "female",
  sbp = 140,
  bp_tx = TRUE,
  total_c = 210,
  hdl_c = 50,
  statin = FALSE,
  dm = TRUE,
  smoking = FALSE,
  egfr = 90,
  bmi = 31,
  time = "30yr"
)

risk_both <- rbind(risk_10_year, risk_30_year)
# Identical to a call to `est_risk()` with the arguments used for either
# `risk_10_year` or `risk_30_year`, other than setting `time = "both"` and
# `collapse = TRUE`.

fake_dat <- data.frame(
    age = c(45L, 55L),
    sex = c("female", "male"),
    sbp = c(140, 144),
    bp_tx = c(TRUE, FALSE),
    total_c = c(210, 240),
    hdl_c = c(50, 40),
    statin = c(FALSE, TRUE),
    dm = c(TRUE, FALSE),
    smoking = c(FALSE, TRUE),
    egfr = c(90, 60),
    bmi = c(31, 28)
)

risk_multi <- est_risk(use_dat = fake_dat, progress = FALSE)
# Setting `progress = FALSE` here to avoid showing the progress bar in the
# vignette, as it does not print well in a knitted document.

fake_dat_warning <- fake_dat
fake_dat_warning$age[[2]] <- 65

risk_warning <- est_risk(use_dat = fake_dat_warning, time = 30, progress = FALSE)

manual_single <- data.frame(
  total_cvd = 0.152,
  ascvd = 0.101,
  heart_failure = 0.051,
  chd = 0.062,
  stroke = 0.039,
  model = "base",
  over_years = 10,
  input_problems = NA_character_
)

manual_multi <- data.frame(
  preventr_id = c(1L, 2L),
  total_cvd = c(0.152, 0.280),
  ascvd = c(0.101, 0.210),
  heart_failure = c(0.051, 0.070),
  chd = c(0.062, 0.135),
  stroke = c(0.039, 0.075),
  model = c("base", "base"),
  over_years = c(10L, 10L),
  input_problems = c(NA_character_, NA_character_)
)

manual_multi_with_pce <- data.frame(
  preventr_id = c(1L, rep(2L, 3)),
  total_cvd = c(0.152, 0.175, NA_real_, 0.280),
  ascvd = c(0.101, 0.105, 0.2, 0.210),
  heart_failure = c(0.051, 0.07, NA_real_, 0.070),
  chd = c(0.062, 0.075, NA_real_, 0.135),
  stroke = c(0.039, 0.03, NA_real_, 0.075),
  model = c("base", "sdi", "pce_orig", "sdi"),
  over_years = c(rep(10L, 3), 30L),
  input_problems = rep(NA_character_, 4)
)

manual_list <- list(
  risk_est_10yr = data.frame(
    total_cvd = 0.152,
    ascvd = 0.101,
    heart_failure = 0.051,
    chd = 0.062,
    stroke = 0.039,
    model = "base",
    over_years = 10L,
    input_problems = NA_character_
  ),
  risk_est_30yr = data.frame(
    total_cvd = 0.430,
    ascvd = 0.280,
    heart_failure = 0.150,
    chd = 0.160,
    stroke = 0.120,
    model = "base",
    over_years = 30L,
    input_problems = NA_character_
  )
)

## ----default-return-----------------------------------------------------------
# Note this first example uses the real `plot_risk()` with the default behavior of
# `add_to_dat = TRUE` to show the data frame with the plot attached as a list-column.
# It still uses `progress = FALSE` to avoid showing the progress bar in the vignette,
# as it does not print well in a knitted document.
default_plot_df <- plot_risk(risk_multi, progress = FALSE)

names(default_plot_df)

str(default_plot_df, max.level = 1)

all(vapply(default_plot_df$plot, ggplot2::is_ggplot, logical(1)))

## ----default-return-plot------------------------------------------------------
default_plot_df$plot[[1]]

## ----default-return-plot-list-------------------------------------------------
default_plot_df$plot

## ----direct-single-plot-------------------------------------------------------
# Again, this example uses the real `plot_risk()` with `add_to_dat = FALSE`
# to show the plot object directly. It still uses `progress = FALSE` to
# avoid showing the progress bar in the vignette, as it does not print well
# in a knitted document.
p_direct <- plot_risk(risk_10_year, add_to_dat = FALSE, progress = FALSE)
class(p_direct)
p_direct

## ----manual-single-plot-------------------------------------------------------
plot_risk_no_add_no_prog(manual_single)

## ----manual-single-str--------------------------------------------------------
str(manual_single)

## ----subset-outcomes----------------------------------------------------------
plot_risk_no_add_no_prog(risk_10_year, outcomes = c("stroke", "chd", "ascvd"))

## ----annotation-none----------------------------------------------------------
plot_risk_no_add_no_prog(risk_10_year, annotation = "none")

## ----annotation-selected------------------------------------------------------
plot_risk_no_add_no_prog(risk_10_year, annotation = c("title", "caption"))

## ----annotation-warning-subtitle----------------------------------------------
# Reminder of ages and time horizons for the `risk_warning` data frame,
# remembering that the 30-year age warning applies to people older than
# 59 years when estimating over a 30-year time horizon.
risk_warning[, c("age", "over_years")]

# We thus expect a warning subtitle for the second row of `risk_warning`
# but not the first row.
plot_risk_no_add_no_prog(risk_warning)

## ----color-single-------------------------------------------------------------
plot_risk_no_add_no_prog(
  risk_10_year,
  color_scheme = "single",
  color_dat = "#1b9e77"
)

## ----color-single-named-------------------------------------------------------
plot_risk_no_add_no_prog(
  risk_10_year,
  color_scheme = "single",
  color_dat = "mediumorchid4"
)

plot_risk_no_add_no_prog(
  risk_10_year,
  color_scheme = "single",
  color_dat = rgb(0.8, 0.6, 0.7)
)

## ----color-dat----------------------------------------------------------------
color_dat <- data.frame(
  threshold = c(0.20, 0.30, 0.40),
  color = c("#1db8b8", "#d70b9a", "#799dfa")
)

## ----color-categories---------------------------------------------------------
plot_risk_no_add_no_prog(
  risk_30_year,
  color_scheme = "categories",
  color_dat = color_dat
)

## ----color-last-group---------------------------------------------------------
plot_risk_no_add_no_prog(
  risk_30_year,
  color_scheme = "categories",
  color_dat = color_dat,
  color_for_last_group = rgb(25, 25, 112, maxColorValue = 255)
)

## ----color-categories-cleaning------------------------------------------------
# Note: The "messy" aspect here pertains to the thresholds being
# out of order. The colors are fine, because any valid color value
# is accepted, including a mixture of named colors, hex codes, and
# calls to `rgb()`.
color_dat_messy <- data.frame(
  threshold = c(0.375, 0.175, 0.275),
  color = c(rgb(0.5, 0.3, 0.9), "#1c1c69", "brown4")
)

plot_risk_no_add_no_prog(
  risk_30_year,
  color_scheme = "categories",
  color_dat = color_dat_messy
)

## ----categories-no-legend-----------------------------------------------------
plot_risk_no_add_no_prog(
  risk_30_year,
  color_scheme = "categories",
  color_dat = color_dat,
  legend = FALSE
)

## ----categories-no-lines------------------------------------------------------
plot_risk_no_add_no_prog(
  risk_30_year,
  color_scheme = "categories",
  color_dat = color_dat,
  lines = FALSE
)

## ----categories-no-line-text--------------------------------------------------
plot_risk_no_add_no_prog(
  risk_30_year,
  color_scheme = "categories",
  color_dat = color_dat,
  line_text = FALSE
)

## ----base-size----------------------------------------------------------------
plot_risk_no_add_no_prog(risk_10_year, base_size = 14)

## ----multiple-horizons--------------------------------------------------------
plots_by_horizon <- plot_risk_no_add_no_prog(risk_both)

length(plots_by_horizon)

## ----multiple-horizons-plot-10------------------------------------------------
plots_by_horizon[[1]]

## ----multiple-horizons-plot-30------------------------------------------------
plots_by_horizon[[2]]

## ----multiple-people----------------------------------------------------------
plots_by_person <- plot_risk_no_add_no_prog(manual_multi)
length(plots_by_person)

## ----multiple-people-plot-1---------------------------------------------------
plots_by_person[[1]]

## ----multiple-people-plot-2---------------------------------------------------
plots_by_person[[2]]

## ----manual-multi-with-pce-table----------------------------------------------
knitr::kable(manual_multi_with_pce)

## ----multiple-people-multiple-horizons-plot-----------------------------------
plots_by_person_and_horizon <- plot_risk(
  manual_multi_with_pce,
  progress = FALSE
)

# Should be `TRUE` because the 10-year plot for the second person is 
# repeated across their two rows for the 10-year time horizon.
identical(
  plots_by_person_and_horizon$plot[[2]],
  plots_by_person_and_horizon$plot[[3]]
)

# Expect identicality between 2 and 3; expect differences otherwise
plots_by_person_and_horizon$plot

## ----list-input-uncollapsed-plot----------------------------------------------
list_with_plots <- plot_risk_no_add_no_prog(manual_list)
length(list_with_plots)

list_with_plots

## ----list-input-collapsed-----------------------------------------------------
collapsed_list_with_plots <- plot_risk(
  manual_list,
  collapse = TRUE,
  progress = FALSE
)

collapsed_list_with_plots[, c("model", "over_years")]

## ----list-input-collapsed-plot------------------------------------------------
collapsed_list_with_plots$plot[[1]]

## ----list-input-plots-direct--------------------------------------------------
direct_list_plots <- plot_risk(
  manual_list,
  add_to_dat = FALSE,
  progress = FALSE
)

length(direct_list_plots)

## ----list-input-plots-direct-plot---------------------------------------------
direct_list_plots[[2]]

## ----malformed-list-names, error = TRUE---------------------------------------
try({
# When `risk_dat` is a list of data frames, the names of the list
# elements must be "risk_est_10yr" and "risk_est_30yr". This input
# violates that requirement.
malformed_list_names <- manual_list

names(malformed_list_names) <- c("ten_year", "thirty_year")

plot_risk(malformed_list_names)
})

## ----malformed-list-too-many-rows, error = TRUE-------------------------------
try({
# When `risk_dat` is a list of data frames, there must be no more than 3
# rows for the 10-year estimates and no more than 1 row for the 30-year
# estimates. This input violates that requirement.
malformed_list_more_than_one_person <- manual_list

malformed_list_more_than_one_person$risk_est_10yr <- rbind(
  malformed_list_more_than_one_person$risk_est_10yr,
  manual_multi |> dplyr::select(-preventr_id),
  manual_multi |> dplyr::select(-preventr_id)
)

plot_risk(malformed_list_more_than_one_person)
})

## ----malformed-list-preventr-id, error = TRUE---------------------------------
try({
# When `risk_dat` is a list of data frames, the column `preventr_id` must
# not be present. This input violates that requirement.
malformed_list_preventr_id_preset <- manual_list
malformed_list_preventr_id_preset$risk_est_10yr$preventr_id <- 1L
malformed_list_preventr_id_preset$risk_est_30yr$preventr_id <- 1L

plot_risk(malformed_list_preventr_id_preset)
})

