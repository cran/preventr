## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(preventr)

## -----------------------------------------------------------------------------
make_vignette_dat <- function(n = 10, add_time_and_model = FALSE) {
  dat <- dplyr::tibble(
    # I am specifying `age`, `sex`, `egfr`, and `bmi` manually while letting
    # other parameters vary via `sample()` to facilitate later aspects of this
    # vignette (to show identical results from approaches I show below).
    age = c(40, 55, 45, 51, 52, 58, 57, 36, 49, 47),
    sex = rep(c("female", "male"), 5),
    sbp = sample(90:180, n, replace = TRUE),
    bp_tx = sample(c(TRUE, FALSE), n, replace = TRUE),
    total_c = sample(130:320, n, replace = TRUE),
    hdl_c = sample(20:100, n, replace = TRUE),
    statin = sample(c(TRUE, FALSE), n, replace = TRUE),
    dm = sample(c(TRUE, FALSE), n, replace = TRUE),
    smoking = sample(c(TRUE, FALSE), n, replace = TRUE),
    egfr = c(73, 71, 80, 73, 77, 70, 86, 89, 78, 68),
    bmi = c(37.4, 32.9, 37.5, 28.6, 37.5, 36.0, 36.7, 28.6, 18.7, 38.6),
    hba1c = sample(
      # I want to ensure NAs are equally represented in the sample space,
      # hence the composition shown below.
      c(
        seq(4.5, 15, 0.1), 
        rep(NA_real_, length(seq(4.5, 15, 0.1)))
      ), 
      n, 
      replace = TRUE
    ),
    uacr = sample(
      c(
        seq(0.1, 25000, 0.1), 
        rep(NA_real_, length(seq(0.1, 25000, 0.1)))
      ), 
      n, 
      replace = TRUE
    ),
    zip = sample(
      # (random sample of valid zips)
      c(
        "01518", "33321", "85206", "98591", "29138", 
        "98101", "44124", "48708", "48206", "77642", 
        rep(NA_character_, n)
      ), 
      n, 
      replace = TRUE
    )
  )
  
  if(add_time_and_model) {
    dat <- dat |> 
      dplyr::mutate(
        # I use `rep("both", 2)` for `time` because I want that option to have a
        # higher chance of being selected for this example.
        time = sample(c("10yr", "30yr", rep("both", 2)), n, replace = TRUE),
        model = sample(c("base", "hba1c", "uacr", "sdi", "full"), n, replace = TRUE)
      )
  }
  
  dat
}

dat <- make_vignette_dat()

knitr::kable(dat)

## -----------------------------------------------------------------------------
res <- est_risk(use_dat = dat, progress = FALSE)

knitr::kable(res)

## ----eval = FALSE-------------------------------------------------------------
# # The default for `progress` when `use_dat` is a data frame is `TRUE`, so this
# # call would yield a progress bar during computation.
# res_for_prog_bar <- est_risk(use_dat = dat)

## -----------------------------------------------------------------------------
dat_age_rename <- dat |> dplyr::rename(years_old = age)

res_age_rename_sym <- est_risk(
  use_dat = dat_age_rename, 
  age = years_old, 
  progress = FALSE
)

res_age_rename_chr <- est_risk(
  use_dat = dat_age_rename, 
  age = "years_old", 
  progress = FALSE
)

## -----------------------------------------------------------------------------
identical(res, res_age_rename_sym) 

identical(res, res_age_rename_chr)

## -----------------------------------------------------------------------------
identical(
  res |> dplyr::select(-age),
  res_age_rename_sym |> dplyr::select(-years_old)
)

identical(
  res |> dplyr::select(-age),
  res_age_rename_chr |> dplyr::select(-years_old)
)

identical(res_age_rename_sym, res_age_rename_chr)

## -----------------------------------------------------------------------------
res_age_rename_sym <- res_age_rename_sym |> dplyr::rename(age = years_old)

res_age_rename_chr <- res_age_rename_chr |> dplyr::rename(age = years_old)

## -----------------------------------------------------------------------------
identical(res, res_age_rename_sym) 

identical(res, res_age_rename_chr)

## -----------------------------------------------------------------------------
dat_time_model <- make_vignette_dat(add_time_and_model = TRUE)

res_time_model_in_dat <- est_risk(use_dat = dat_time_model, progress = FALSE)

knitr::kable(res_time_model_in_dat)

## -----------------------------------------------------------------------------
dat_time_model[["time"]] 

dat_time_model[["model"]]

## -----------------------------------------------------------------------------
res_time_and_model_in_call <- est_risk(
  use_dat = dat_time_model, 
  time = 10, 
  model = "base",
  progress = FALSE
) 

all.equal(unique(res_time_and_model_in_call[["over_years"]]), 10) 

all.equal(unique(res_time_and_model_in_call[["model"]]), "base")  

## -----------------------------------------------------------------------------
res_time_and_model_in_call <- est_risk(
  use_dat = dat_time_model |> dplyr::mutate(model = "base"), 
  model = NULL,
  progress = FALSE
) 

all.equal(unique(res_time_and_model_in_call[["model_input"]]), "base") 
res_time_and_model_in_call[["model"]]

## -----------------------------------------------------------------------------
show_random_row <- function(dat, res, n = 5) {
  
  rows <- seq_len(nrow(dat))
  already_seen <- vector("double", n)
  
  for(i in seq_len(n)) {
    
    random_row <- sample(rows, 1)
    while(random_row %in% already_seen) random_row <- sample(rows, 1)
    already_seen[[i]] <- random_row
    
    cat(paste0("\n", "--- `preventr_id` ", random_row, " ---", "\n\n"))
    
    print(
      list(
        # `model_input` has `unlist(..., recursive = FALSE)` because sometimes
        # column `model` will be a list column, so each item therein will be
        # enclosed in a list, and unlisting one level improves the appearance of
        # printing a bit in this case.
        model_input = unlist(dat[random_row, ][["model"]], recursive = FALSE),
        time_input = dat[random_row, ][["time"]],
        nrow_res = dplyr::filter(res, preventr_id == random_row) |> nrow()
      )
    )
  }
  
}

show_random_row(dat_time_model, res_time_model_in_dat)

## -----------------------------------------------------------------------------
res_without_dat <- est_risk(
  use_dat = dat_time_model, 
  add_to_dat = FALSE, 
  progress = FALSE
)

knitr::kable(res_without_dat)

## -----------------------------------------------------------------------------
res_with_dat <- est_risk(use_dat = dat_time_model, progress = FALSE)

# Now, let's check identicality of `res_with_dat` with a version we
# recreate using `dat_for_join` and `res_without_dat`.
dat_for_join <- dat_time_model |> 
  # First, add the `preventer_id` column ...
  dplyr::mutate(preventr_id = seq_len(nrow(dat_time_model))) |> 
  # ... and then move it to be the first column in the data frame.
  dplyr::relocate(preventr_id) 

# Now, do the left join.
res_with_dat_manual_join <- dat_for_join |> 
  dplyr::left_join(
    res_without_dat, 
    by = "preventr_id",
    # Because both data frames will have a column named `model`, I'll provide
    # suffixes to distinguish them. The suffixes below will result in the column 
    # `model` in `dat_for_join` being renamed to `model_input` and column
    # `model` in the data frame `res_without_dat` retaining the same name.
    suffix = c("_input", "")  
    )

# (You could also do all the above without a pipe sequence, of course.)

identical(res_with_dat, res_with_dat_manual_join)   

## -----------------------------------------------------------------------------
dat_tbl <- dat |> dplyr::mutate(quiet = TRUE)       
dat_dt <- data.table::as.data.table(dat_tbl)
dat_df <- as.data.frame(dat_tbl)

class(dat_tbl)
class(dat_dt)
class(dat_df)

res_tbl <- est_risk(use_dat = dat_tbl, progress = FALSE)  # Return: tibble
res_dt <- est_risk(use_dat = dat_dt, progress = FALSE)    # Return: data.table
res_df <- est_risk(use_dat = dat_df, progress = FALSE)    # Return: data frame

identical(class(dat_tbl), class(res_tbl))
identical(class(dat_dt), class(res_dt))
identical(class(dat_df), class(res_df))

# Other than the attributes, these are all equal (of course).
all.equal(res_tbl, res_dt, check.attributes = FALSE)
all.equal(res_tbl, res_df, check.attributes = FALSE)

## -----------------------------------------------------------------------------
dat_with_pce_requests <- dat_time_model |> 
  # We'll start with the data in `dat_time_model` and then overwrite the `model`
  # column for this example.
  dplyr::mutate(
    # Base R `lapply()` is a convenient choice here, as it will return a list; 
    # however, this is not the only way to create list columns.
    model = lapply(
      seq_len(nrow(dat_time_model)),
      function(x) {
        # Let's make some rows just have `NA` (leading to automatic PREVENT
        # model selection and no risk estimation from the PCEs) and other rows
        # specify both the PREVENT and PCE models. This is just to demonstrate
        # flexibility. You could also just generate a basic list column, and
        # that would be less involved than what I do here.
        if(x %% 2 == 0) {
          NA
        } else { 
          list(
            # (We could also omit `main_model`, in which case the PREVENT model
            # will be selected automatically.)
            main_model = sample(c("base", "hba1c", "uacr", "sdi", "full"), 1),
            other_models = sample(c("pce_both", "pce_rev", "pce_orig"), 1),
            race_eth = sample(c("Black", "White", "Other"), 1)
          )
        }
      }
    )
  )

res_with_pce_requests <- est_risk(
  use_dat = dat_with_pce_requests, 
  progress = FALSE
)

knitr::kable(res_with_pce_requests)

## -----------------------------------------------------------------------------
identical_cols <- vapply(
  seq_len(nrow(dat_with_pce_requests)),
  
  function(x) {
    n_row <- res_with_pce_requests |> dplyr::filter(preventr_id == x) |> nrow()
    
    identical(
      rep(dat_with_pce_requests[["model"]][x], n_row),
      
      res_with_pce_requests |> 
        dplyr::filter(preventr_id == x) |>
        dplyr::pull(model_input)
    )
  },
  
  logical(1)
)

all(identical_cols)

## -----------------------------------------------------------------------------
show_random_row(dat_with_pce_requests, res_with_pce_requests)

## -----------------------------------------------------------------------------
dat_with_calls_basic <- dat_with_pce_requests |> 
  dplyr::mutate(
    egfr = lapply(
      seq_len(nrow(dat)),
      function(x) {
        # We can make some rows have calls to `calc_egfr` and some just have
        # values. This is just for demonstration, and one could instead have a
        # simple list column composed entirely of calls.
        if(x %% 2 == 0) {
          call("calc_egfr", cr = sample(seq(0.5, 1.5, 0.1), 1))
        } else {
          sample(45:90, 1)
        }
      }
    ),
    bmi = lapply(
      seq_len(nrow(dat)),
      function(x) {
        # The comment above for `egfr` applies here as well.
        if(x %% 2 == 0) {
          call(
            "calc_bmi", 
            height = sample(60:78, 1),
            weight = sample(110:200, 1)
          )
        } else {
          sample(20:38, 1)
        }
      }
    )
  )

res_with_calls_basic <- est_risk(
  use_dat = dat_with_calls_basic, 
  progress = FALSE
)

knitr::kable(res_with_calls_basic)

## -----------------------------------------------------------------------------
dat_with_cr_cm_kg <- dat_with_pce_requests |> 
  dplyr::mutate(
    # Let's use values for `cr` in mg/dL, `cm`, and `kg` that would yield the
    # values originally entered directly for `egfr` and `bmi` in
    # `make_vignette_dat()` to demonstrate identical results when using the
    # direct values for eGFR and BMI vs. using calls to the convenience
    # functions. This is why the function `make_vignette_dat()` specifies values
    # for `age`, `sex`, `egfr`, and `bmi` directly while letting others vary
    # randomly.
    cr = c(1, 1.2, 0.9, 1.2, 0.9, 1.2, 0.8, 1.1, 0.9, 1.3),
    cm = c(199, 182, 184, 197, 189, 187, 191, 163, 199, 171),
    kg = c(148, 109, 127, 111, 134, 126, 134, 76, 74, 113),
    # Now, we'll create new list columns containing calls to calculate eGFR and
    # BMI (and remember, `dat_with_pce_requests` will already have columns for
    # `egfr` and `bmi`).
    egfr_call = lapply(
      seq_len(nrow(dat_with_pce_requests)),
      function(x) {
        call("calc_egfr", cr = cr[[x]])
      }
    ),
    bmi_call = lapply(
      seq_len(nrow(dat_with_pce_requests)),
      function(x) {
        call("calc_bmi", height = cm[[x]], weight = kg[[x]], units = "metric")
      }
    )
  )

res_with_calls <- est_risk(
  use_dat = dat_with_cr_cm_kg, 
  # Instruct `est_risk()` to use the call columns, else it will default to
  # grabbing values from `egfr` and `bmi`, which have direct values in them.
  egfr = "egfr_call", # Again, can pass column names as a character string ...
  bmi = bmi_call,     # ... or as a symbol
  progress = FALSE
)

res_without_calls <- est_risk(
  use_dat = dat_with_cr_cm_kg,
  # If you don't specify the call columns, `est_risk()` will default to using
  # the columns `egfr` and `bmi`, which have the original, direct values for
  # eGFR and BMI
  progress = FALSE
)

knitr::kable(res_with_calls)

identical(res_with_calls, res_without_calls)  

## -----------------------------------------------------------------------------
knitr::kable(head(dat_with_cr_cm_kg))

## -----------------------------------------------------------------------------
# First, add `preventr_id` to data frame for joining later, then move it to the
# first position.
dat_with_cr_cm_kg <- dat_with_cr_cm_kg |> 
  dplyr::mutate(preventr_id = seq_len(nrow(dat))) |> 
  dplyr::relocate(preventr_id)

res_basic_lapply <- lapply(
  # Using the row numbers of `dat_with_cr_cm_kg` as `x` in `function(x)`...
  seq_len(nrow(dat_with_cr_cm_kg)),
  function(x) {
    # ... run `est_risk()` on each row of `dat_with_cr_cm_kg`
    est_risk(
      age = dat_with_cr_cm_kg[["age"]][[x]],
      sex = dat_with_cr_cm_kg[["sex"]][[x]],
      sbp = dat_with_cr_cm_kg[["sbp"]][[x]],
      bp_tx = dat_with_cr_cm_kg[["bp_tx"]][[x]],
      total_c = dat_with_cr_cm_kg[["total_c"]][[x]],
      hdl_c = dat_with_cr_cm_kg[["hdl_c"]][[x]],
      statin = dat_with_cr_cm_kg[["statin"]][[x]],
      dm = dat_with_cr_cm_kg[["dm"]][[x]],
      smoking = dat_with_cr_cm_kg[["smoking"]][[x]],
      egfr = dat_with_cr_cm_kg[["egfr"]][[x]],
      bmi = dat_with_cr_cm_kg[["bmi"]][[x]],
      hba1c = dat_with_cr_cm_kg[["hba1c"]][[x]],
      uacr = dat_with_cr_cm_kg[["uacr"]][[x]],
      zip = dat_with_cr_cm_kg[["zip"]][[x]],
      model = dat_with_cr_cm_kg[["model"]][[x]],
      time = dat_with_cr_cm_kg[["time"]][[x]],
      quiet = TRUE
    )  |>
      # Bind the rows of the return from `est_risk()` together.
      # (Side note: You can skip this step if you call `est_risk()` with
      # `collapse = TRUE`.)
      dplyr::bind_rows() |>
      # Add column `preventr_id` to facilitate reassociation with the input
      # data frame.
      dplyr::mutate(preventr_id = x)
  }
) |>
  # Bind all the results from the `lapply()` call together to make a
  # single data frame.
  dplyr::bind_rows() |> 
  # Finally, do a quick left join to match the results with their
  # corresponding input row in `dat_with_cr_cm_kg`.
  dplyr::left_join(
    x = dat_with_cr_cm_kg, 
    y = _, 
    by = "preventr_id",
    # Because both data frames will have a column named `model`, we'll provide
    # suffixes to distinguish them. The suffixes below will cause the column 
    # `model` in `dat_with_cr_cm_kg` to be renamed to `model_input` and column
    # `model` in the data frame from the pipe sequence (represented via `_`) 
    # retaining the same name.
    suffix = c("_input", "")  
  )

# If all has proceeded as it should've, `res_basic_lapply` should be identical
# to `res_without_calls` (and thus also to `res_with_calls`) from the above 
# example (spoiler, it will be).
identical(res_basic_lapply, res_without_calls)

## ----eval = FALSE-------------------------------------------------------------
# with(
#   dat_with_cr_cm_kg[x, ],
#   est_risk(
#     age = age,
#     sex = sex,
#     ...
#   )
# )

## -----------------------------------------------------------------------------
do_lapply_and_join <- function(dat, with_arg, ..., eval = TRUE) {
  
  dat <- substitute(dat)
  with_arg <- substitute(with_arg)
  dots <- eval(substitute(alist(...)))
  
  mini_cl <- bquote(
    {
      lapply(
        # Using the row numbers of `.(dat)` as `x` in `function(x)`...
        seq_len(nrow(.(dat))),
        function(x) {
          with(
            # With the data mask contained in `with_arg` ...
            .(with_arg),
              # ... run `est_risk()` with the arguments contained within `dots`.
              est_risk(..(dots)) 
          ) |>
            # The vast majority of the following is nearly identical to the
            # basic `lapply()` example; it does not make any further use of 
            # metaprogramming unless otherwise noted.
            dplyr::bind_rows() |>
            dplyr::mutate(preventr_id = x)
        }
      ) |>
        dplyr::bind_rows() |> 
        dplyr::left_join(
          x = .(dat),       # Note the use of `.(dat)` 
          y = _, 
          by = "preventr_id",
          suffix = c("_input", "")  
        )
    },
    splice = TRUE           # This tells `bquote()` to splice anything in `..()`
  )
  
  if(eval) eval(mini_cl, parent.frame()) else mini_cl
}

## -----------------------------------------------------------------------------
# Let's start by showing results identical to `res_basic_lapply`.
res_aug_lapply <- do_lapply_and_join(
  dat = dat_with_cr_cm_kg,
  with_arg = dat_with_cr_cm_kg[x, ],
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  egfr = egfr,
  bmi = bmi,
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  # Because of the data mask passed via argument `with_arg`, the evaluation
  # environment will be row x of the data frame (where x is defined within the
  # `lapply()` call). Thus, `model` will still be a list column, so I need to
  # get that list item out of the list column before passing it to
  # `est_risk()`.
  #
  # For `model`, I could instead do `unlist()`, but given this vignette also
  # demonstrates list columns containing calls (where `unlist()` will not do),
  # I will use `[[1]]` here for consistency. Note I can be confident the list
  # item I need from the list column `model` is indeed the first (and only)
  # list item, and the list item I extract via `[[1]]` will then either be
  # `NA` or a list with list items `main_model`, `other_models`, and
  # `race_eth` given how I created `dat_with_cr_cm_kg`.
  model = model[[1]],
  time = time,
  quiet = TRUE
)

## ----eval = FALSE-------------------------------------------------------------
# lapply(
#   seq_len(nrow(dat_with_cr_cm_kg)),  # `dat_with_cr_cm_kg` replaces `.(dat)`
#   function(x) {
#     with(
#       dat_with_cr_cm_kg[x, ],        # `dat_with_cr_cm_kg[x, ]` replaces
#       est_risk(                      # `.(with_arg)`
#         age = age,
#         sex = sex,                   # The arguments appearing in `est_risk()`
#         sbp = sbp,                   # were spliced into the call from `..(dots)`
#         bp_tx = bp_tx,
#         total_c = total_c,
#         hdl_c = hdl_c,
#         statin = statin,
#         dm = dm,
#         smoking = smoking,
#         egfr = egfr,
#         bmi = bmi,
#         hba1c = hba1c,
#         uacr = uacr,
#         zip = zip,
#         model = model[[1]],
#         time = time,
#         quiet = TRUE
#       )
#     ) |>
#       dplyr::bind_rows() |>
#       dplyr::mutate(preventr_id = x)
#   }
# ) |>
#   dplyr::bind_rows() |>
#   dplyr::left_join(
#     x = dat_with_cr_cm_kg,            # `dat_with_cr_cm_kg` replaces `.(dat)`
#     y = _,
#     by = "preventr_id",
#     suffix = c("_input", "")
#   )

## ----include = FALSE----------------------------------------------------------
# Run this to get the return, but then put it in the code block that follows so
# it doesn't look quite as bad
do_lapply_and_join(
  dat = dat_with_cr_cm_kg,
  with_arg = dat_with_cr_cm_kg[x, ],
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  egfr = egfr,
  bmi = bmi,
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  model = model[[1]],
  time = time,
  quiet = TRUE,
  eval = FALSE
)

## ----eval = FALSE-------------------------------------------------------------
# {
#     dplyr::left_join(x = dat_with_cr_cm_kg, y =
#         dplyr::bind_rows(lapply(seq_len(nrow(dat_with_cr_cm_kg)),
#         function(x) {
#             dplyr::mutate(dplyr::bind_rows(with(dat_with_cr_cm_kg[x,
#                 ], est_risk(age = age, sex = sex, sbp = sbp,
#                 bp_tx = bp_tx, total_c = total_c, hdl_c = hdl_c,
#                 statin = statin, dm = dm, smoking = smoking,
#                 egfr = egfr, bmi = bmi, hba1c = hba1c, uacr = uacr,
#                 zip = zip, model = model[[1]], time = time, quiet = TRUE))),
#                 preventr_id = x)
#         })), by = "preventr_id", suffix = c("_input",
#         ""))
# }

## -----------------------------------------------------------------------------
identical(res_aug_lapply, res_basic_lapply)

## -----------------------------------------------------------------------------
res_aug_lapply_variant <- do_lapply_and_join(
  dat = dat_with_cr_cm_kg,
  with_arg = dat_with_cr_cm_kg,
  age = age[[x]],
  sex = sex[[x]],
  sbp = sbp[[x]],
  bp_tx = bp_tx[[x]],
  total_c = total_c[[x]],
  hdl_c = hdl_c[[x]],
  statin = statin[[x]],
  dm = dm[[x]],
  smoking = smoking[[x]],
  egfr = egfr[[x]],
  bmi = bmi[[x]],
  hba1c = hba1c[[x]],
  uacr = uacr[[x]],
  zip = zip[[x]],
  model = model[[x]],
  time = time[[x]],
  quiet = TRUE
)

identical(res_aug_lapply_variant, res_basic_lapply)

## -----------------------------------------------------------------------------
res_aug_lapply_with_calls <- do_lapply_and_join(
  dat = dat_with_cr_cm_kg,
  with_arg = dat_with_cr_cm_kg[x, ],
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  # If needed, review the comment associated with `res_aug_lapply` to understand
  # why arguments `egfr`, `bmi`, and `model` are specified like this.
  egfr = egfr_call[[1]],
  bmi = bmi_call[[1]],
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  model = model[[1]],
  time = time,
  quiet = TRUE
)

identical(res_aug_lapply_with_calls, res_basic_lapply)

## -----------------------------------------------------------------------------
res_aug_lapply_with_calls_in_flight <- do_lapply_and_join(
  dat = dat_with_cr_cm_kg,
  with_arg = dat_with_cr_cm_kg[x, ],
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  egfr = call("calc_egfr", cr = cr),
  bmi = call("calc_bmi", height = cm, weight = kg, units = "metric"),
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  model = model[[1]],
  time = time,
  quiet = TRUE
)

identical(res_aug_lapply_with_calls_in_flight, res_basic_lapply)

## -----------------------------------------------------------------------------
res_auto_opts_in_call <- est_risk(
  use_dat = dat_with_cr_cm_kg,
  model = "base",
  time = "10yr",
  progress = FALSE
)

res_aug_lapply_opts_in_call <- do_lapply_and_join(
  dat = dat_with_cr_cm_kg,
  with_arg = dat_with_cr_cm_kg[x, ],
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  egfr = egfr,
  bmi = bmi,
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  model = "base",
  time = "10yr",
  quiet = TRUE
)

identical(res_auto_opts_in_call, res_aug_lapply_opts_in_call)

## -----------------------------------------------------------------------------
do_map_and_join <- function(dat, ...) {

  dat <- dat |> dplyr::mutate(preventr_id = seq_len(nrow(dat)))
  dots <- eval(substitute(alist(...)))
  
  res <- eval(
    bquote(
      # With the data mask introduced by `dat`, evaluate `Map()` with the
      # function `est_risk()` and the arguments contained in `dots`.
      # (In other words, call `est_risk()` with the arguments in `dots` for
      # each row of `dat`.) 
      with(dat, Map(est_risk, ..(dots))),
      splice = TRUE
    )
  )
  
  # `res` from the above call to `Map()` will be a list, and the items in
  # the list may also be a list (e.g., a list of data frames), as such, we'll
  # need to iterate through `res` and bind the data frames together. We'll also
  # need to add the `preventr_id` column.
  for(i in seq_along(res)) {
    res[[i]] <- res[[i]] |> 
      dplyr::bind_rows() |>
      dplyr::mutate(preventr_id = i) |> 
      dplyr::relocate(preventr_id)
  }
  
  # Now do the left join, detailed previously in this vignette.
  dplyr::left_join(
      x = dat, 
      y = dplyr::bind_rows(res), 
      by = "preventr_id",
      suffix = c("_input", "")  
    )
}

res_map <- do_map_and_join(
  dat_with_cr_cm_kg,
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  egfr = egfr,
  bmi = bmi,
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  model = "base",
  time = "10yr",
  quiet = TRUE
)

identical(res_auto_opts_in_call, res_map)

## -----------------------------------------------------------------------------
res_map_all_cols <- do_map_and_join(
  dat_with_cr_cm_kg,
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  # Note I'm passing the call columns here, showing you can still use the
  # convenience functions (stored as calls in list columns) with `Map()`.
  egfr = egfr_call,
  bmi = bmi_call,
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  model = model,
  time = time,
  quiet = TRUE
)

identical(res_map_all_cols, res_basic_lapply)

# You can also pass applicable optional behavior variables.
res_map_only_10yr_hba1c_not_quiet <- do_map_and_join(
  dat_with_cr_cm_kg,
  age = age,
  sex = sex,
  sbp = sbp,
  bp_tx = bp_tx,
  total_c = total_c,
  hdl_c = hdl_c,
  statin = statin,
  dm = dm,
  smoking = smoking,
  egfr = egfr_call,
  bmi = bmi_call,
  hba1c = hba1c,
  uacr = uacr,
  zip = zip,
  model = "hba1c",
  time = "10yr",
  quiet = FALSE
)

# Despite `dat_with_cr_cm_kg` having columns `time` and `model`, the `time` and
# `model` arguments in the call to `est_risk()` (via `Map()`) get priority.
dat_with_cr_cm_kg[["model"]]
dat_with_cr_cm_kg[["time"]]

all.equal(unique(res_map_only_10yr_hba1c_not_quiet[["over_years"]]), 10)
all.equal(unique(res_map_only_10yr_hba1c_not_quiet[["model"]]), "hba1c")

## -----------------------------------------------------------------------------
pmap_data_frame_approach <- 
  dat_with_cr_cm_kg |>
  # Remove columns not corresponding to an argument in `est_risk()`.
  dplyr::select(-c(preventr_id, cr, cm, kg, egfr_call, bmi_call)) |> 
  purrr::pmap(est_risk)

# Very similar to the `Map()` examples above, we'll need to bind the results
# from `purrr::pmap()` together and do some other minor actions, so I've 
# converted that into a mini-function to avoid repetition in these examples.
combine_pmap_res_and_join <- function(pmap_res, dat) {
  for(i in seq_along(pmap_res)) {
    pmap_res[[i]] <- pmap_res[[i]] |> 
      dplyr::bind_rows() |>
      dplyr::mutate(preventr_id = i) |> 
      dplyr::relocate(preventr_id)
  }
  
  dplyr::left_join(
    x = dat, 
    y = dplyr::bind_rows(pmap_res), 
    by = "preventr_id",
    suffix = c("_input", "")  
  )
}

pmap_data_frame_approach <- 
  combine_pmap_res_and_join(pmap_data_frame_approach, dat_with_cr_cm_kg)

identical(pmap_data_frame_approach, res_basic_lapply)

## -----------------------------------------------------------------------------
pmap_list_approach <- purrr::pmap(
  with(
    dat_with_cr_cm_kg,
    list(
      age = age,
      sex = sex,
      sbp = sbp,
      bp_tx = bp_tx,
      total_c = total_c,
      hdl_c = hdl_c,
      statin = statin,
      dm = dm,
      smoking = smoking,
      egfr = egfr,
      bmi = bmi,
      hba1c = hba1c,
      uacr = uacr,
      zip = zip,
      model = model,
      time = time,
      # Note passing an explicitly-delineated list for argument `.l` allows us
      # to easily specify the `quiet` argument here
      quiet = TRUE
    )
  ),
  est_risk
)

pmap_list_approach <- 
  combine_pmap_res_and_join(pmap_list_approach, dat_with_cr_cm_kg)

identical(pmap_list_approach, res_basic_lapply)

## -----------------------------------------------------------------------------
pmap_list_approach_with_call <- purrr::pmap(
  with(
    dat_with_cr_cm_kg,
    list(
      age = age,
      sex = sex,
      sbp = sbp,
      bp_tx = bp_tx,
      total_c = total_c,
      hdl_c = hdl_c,
      statin = statin,
      dm = dm,
      smoking = smoking,
      egfr = egfr_call,
      bmi = bmi_call,
      hba1c = hba1c,
      uacr = uacr,
      zip = zip,
      model = model,
      time = time,
      quiet = TRUE
    )
  ),
  est_risk
)

pmap_list_approach_with_call <- 
  combine_pmap_res_and_join(pmap_list_approach_with_call, dat_with_cr_cm_kg)

identical(pmap_list_approach_with_call, res_basic_lapply)

