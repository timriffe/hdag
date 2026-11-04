# -----------------------------------------------------------------------------
# Shared functions and package setup for the SHARE H/DAG workflow
#
# Manifest: function, purpose, and calling script
#   gather_wave_data()           Recode one wave's SHARE interview health data; 02_prepare_SHARE.R
#   append_death_observations()  Append observed terminal deaths; 02_prepare_SHARE.R
#   fit_msm_health()             Fit sex-specific MSMs for one health measure; 03_estimate_hazards.R
#   init_constant()              Set initial H/U composition; 08_hdag.R, 09_manhattan.R
#   calc_dxh()                   Build the 2D healthy/unhealthy death mass; 09_manhattan.R
#   find_share_file()            Find a SHARE module in files or ZIPs; 01_extract_SHARE_files.R
#   read_hmd_product()           Read a Spanish HMD product; 05_smooth_HMD.R
#   prepare_hmd_product()        Tidy HMD deaths or exposures; 05_smooth_HMD.R
#   hazards_to_probabilities()   Convert one hazard stratum to probabilities; 07_derive_probabilities.R
#   calculate_group_hdag()       Calculate life table and HDAG; 08_hdag.R
#   calculate_group_dxh()        Calculate one stratum's 2D death distribution; 09_manhattan.R
#
#   Legacy helpers not used by steps 01-09 remain commented out at the end.
#
# Package setup: install missing CRAN packages and timriffe/mscalc from GitHub,
# then attach the packages used across the numbered scripts. This requires an
# internet connection on a fresh machine; pak is installed from CRAN if needed.
# -----------------------------------------------------------------------------

cran_packages <- c(
  "tidyverse",
  "haven",
  "msm",
  "mgcv",
  "expm",
  "janitor",
  "HMDHFDplus"
)

github_packages <- c(
  mscalc = "github::timriffe/mscalc"
)

if (!requireNamespace("pak", quietly = TRUE)) {
  utils::install.packages("pak", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("pak", quietly = TRUE)) {
  stop("Could not install 'pak'. Install it from CRAN, then source this file again.")
}

missing_cran_packages <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]
missing_github_packages <- github_packages[
  !vapply(names(github_packages), requireNamespace, logical(1), quietly = TRUE)
]
packages_to_install <- c(missing_cran_packages, unname(missing_github_packages))

if (length(packages_to_install) > 0) {
  pak::pkg_install(packages_to_install)
}

packages_to_load <- c(cran_packages, names(github_packages))
for (package_name in packages_to_load) {
  suppressPackageStartupMessages(
    library(package_name, character.only = TRUE)
  )
}

# some globals:
ax_assumption_global <- 0.05

#' Gather and recode one SHARE wave's interview health records.
#' Used by: R/02_prepare_SHARE.R.
gather_wave_data <- function(data = health_data[[6]],
                             wave = "w7") {
  
  disease <- str_c("chronic",       wave)
  age_int <- str_c("age_int_",      wave)
  int_m   <- str_c("int_month_",    wave)
  int_y   <- str_c("int_year_",     wave)
  wave    <- str_c("deadoralive_" , wave)
  
  sample <- gen_files %>%
    dplyr::select(mergeid,
                  gender,
                  all_of(c(int_m, int_y, age_int)),
                  contains(wave),
                  yrbirth,
                  mobirth) %>%
    filter(!!sym(int_y) > 0) %>%
    set_names("mergeid", 
              "gender", 
              "int_month",
              "int_year",
              "age",
              "deadoralive",
              "yrbirth",
              "mobirth") %>%
    mutate(int_month = ifelse(int_month < 0 | is.na(int_month), 1, int_month)) %>%
    filter(deadoralive %in% c(1, 2)) %>% 
    dplyr::select(-deadoralive) %>%
    unite(time_int, c("int_year", "int_month"), sep = ".") %>% 
    mutate(time_int = ym(time_int))
  
  generated <- sample %>%
    filter(age > 0) %>%
    mutate(
      age         = as.numeric(age),
      gender      = ifelse(gender == 1, "male", "female"),
      year        = year(time_int),
    ) %>%
    unite(time_born, c("yrbirth", "mobirth"), sep = ".") %>%
    mutate(time_born = ym(time_born)) %>%
    mutate(time_born_dec = decimal_date(time_born),
           time_int_dec  = decimal_date(time_int),
           age_dec       = time_int_dec - time_born_dec) %>%
    select(-c(time_int, time_born))
  
  # age_years = interval(time_born, time_int) / years(1)
  
  health <- data %>%
    rename("chronic" := !!sym(disease))
  
  health %>%
    left_join(generated, by = join_by(mergeid)) %>% 
    # prepare 3 health definitions
    mutate(gali = case_when(
      gali == 0 ~ "H",
      gali == 1 ~ "U",
      TRUE      ~ NA_character_
    )) %>%
    mutate(sphus = case_when(
      sphus %in% c(1, 2, 3) ~ "H",
      sphus %in% c(4, 5)    ~ "U",
      TRUE                  ~ NA_character_
    )) %>%
    mutate(chronic = case_when(
      chronic <  2  ~ "H",
      chronic >= 2  ~ "U",
      TRUE  ~ NA_character_
    )) %>%
    mutate(adl = case_when(
      adl == 0 ~ "H",
      adl  > 0 ~ "U",
      TRUE ~ NA_character_
    )) %>%
    mutate(iadl = case_when(
      iadl == 0 ~ "H",
      iadl  > 0 ~ "U",
      TRUE ~ NA_character_
    )) %>%
    # remove cases where none of the health states is known
    # Maybe fill??
    filter(!if_all(c(chronic, gali, sphus, adl, iadl), is.na))
  
}

#' Append valid death records after each person's last interview.
#' Used by: R/02_prepare_SHARE.R.
append_death_observations <- function(data, gen_files) {
  
  last_interview <- data %>%
    filter(!is.na(time_int_dec)) %>%
    group_by(mergeid) %>%
    summarise(last_interview_dec = max(time_int_dec), .groups = "drop")
  
  death_observations <- gen_files %>%
    transmute(
      mergeid,
      gender_code = as.numeric(gender),
      birth_year = as.numeric(yrbirth),
      birth_month = as.numeric(mobirth),
      death_year = as.numeric(deceased_year),
      death_month = as.numeric(deceased_month),
      death_age = as.numeric(deceased_age),
      death_status = as.numeric(deadoralive_w9)
    ) %>%
    filter(
      death_status == 2,
      death_year > 0,
      death_age >= 0
    ) %>%
    mutate(
      death_month = ifelse(
        is.na(death_month) | death_month < 1 | death_month > 12,
        1,
        death_month
      ),
      gender = ifelse(gender_code == 1, "male", "female"),
      time_dead = ym(sprintf("%04d-%02d", death_year, death_month)),
      time_born = ym(if_else(
        !is.na(birth_year) & birth_year > 0 & birth_month %in% 1:12,
        sprintf("%04d-%02d", birth_year, birth_month),
        NA_character_
      )),
      time_int_dec = decimal_date(time_dead),
      time_born_dec = decimal_date(time_born),
      age_dec = time_int_dec - time_born_dec,
      year = year(time_dead)
    ) %>%
    inner_join(last_interview, by = "mergeid") %>%
    filter(time_int_dec > last_interview_dec) %>%
    transmute(
      wave = "death",
      mergeid,
      gender,
      age = death_age,
      year,
      time_born_dec,
      time_int_dec,
      age_dec,
      chronic = "D",
      gali = "D",
      sphus = "D",
      adl = "D",
      iadl = "D"
    )
  
  bind_rows(data, death_observations) %>%
    arrange(mergeid, time_int_dec)
}

#' Fit sex-specific continuous-time MSMs for one health measure.
#' Used by: R/03_estimate_hazards.R.
fit_msm_health <- function(data = share, 
                           health_var = "sphus", Q,
                           process_time = "age_dec",
                           pci = NULL) {
  
  model_data <- data %>%
    transmute(
      mergeid,
      gender,
      age_dec,
      time_int_dec,
      health = .data[[health_var]]
    ) %>%
    filter(
      !is.na(health),
      !is.na(time_int_dec),
      health %in% c("H", "U", "D")
    ) %>%
    arrange(mergeid, time_int_dec) %>%
    group_by(mergeid) %>%
    mutate(first_death = match("D", health)) %>%
    # Keep the first death row and discard anything after death.
    filter(is.na(first_death) | row_number() <= first_death) %>%
    ungroup() %>%
    select(-first_death) %>%
    # msm needs at least two usable observations per respondent.
    filter(n() > 1, .by = mergeid) %>%
    arrange(mergeid, time_int_dec) %>%
    ungroup()
  
  missing_age_ids <- model_data %>%
    filter(any(is.na(age_dec)), .by = mergeid) %>%
    distinct(mergeid)
  
  if (nrow(missing_age_ids) > 0) {
    warning(
      health_var,
      ": excluding ",
      nrow(missing_age_ids),
      " respondent(s) with multiple observations but missing age at one or more observations.",
      call. = FALSE
    )
  }
  
  model_data <- model_data %>%
    # Keep complete respondent histories; otherwise a missing-age death row
    # could silently turn a known death into apparent censoring.
    filter(all(!is.na(age_dec)), .by = mergeid) %>%
    arrange(mergeid, time_int_dec) %>%
    mutate(
      health = case_when(
        health == "H" ~ 1L,
        health == "U" ~ 2L,
        health == "D" ~ 3L
      ),
      # Interviews are panel snapshots; appended deaths use the reported
      # death month (the preparation helper imputes January when month is missing).
      obstype = if_else(health == 3L, 3L, 1L)
    )
  
  sexes <- sort(unique(model_data$gender))
  model_rows <- vector("list", length(sexes))
  names(model_rows) <- sexes
  model_formula <- reformulate(process_time, response = "health")
  
  for (sex in sexes) {
    sex_data <- model_data %>%
      filter(gender == sex)
    
    model_fit <- msm(
      formula    = model_formula,
      subject    = mergeid,
      data       = sex_data,
      qmatrix    = Q,
      obstype    = obstype,
      covariates = ~ age_dec + time_int_dec,
      pci        = pci,
      control    = list(
        fnscale = 5000,
        maxit = 25000
      ),
      method = "BFGS",
      gen.inits = TRUE
    )
    
    model_rows[[sex]] <- tibble(
      gender = sex,
      data = list(sex_data),
      model = list(model_fit),
      health_var = health_var
    )
  }
  
  bind_rows(model_rows)
}

#' Calculate the starting H/U mix from first-age transition probabilities.
#' Used by: calculate_group_hdag() in R/08_hdag.R and
#' calculate_group_dxh() in R/09_manhattan.R.
init_constant <- function(x) {
  transition_values <- unlist(x[c("HH", "UH", "HU", "UU")])
  transient_matrix <- matrix(transition_values, nrow = 2, byrow = TRUE)
  eigen_result <- eigen(transient_matrix)
  dominant_index <- which.max(Re(eigen_result$values))
  initial <- Re(eigen_result$vectors[, dominant_index])

  if (sum(initial) < 0) {
    initial <- -initial
  }
  if (any(initial < -1e-8) || sum(initial) <= 0) {
    stop("Could not calculate a valid initial H/U composition from the first-age probabilities.")
  }

  initial[initial < 0] <- 0
  initial <- initial / sum(initial)
  setNames(as.numeric(initial), c("H", "U"))
}

#' Calculate the 2D distribution of healthy and unhealthy years at death.
#' Used by: calculate_group_dxh() in R/09_manhattan.R.
# Standalone 2D healthy/unhealthy-years death-distribution function.
# Source this file to define calc_dxh(). It requires dplyr and tibble.

#' Calculate the 2D distribution of healthy and unhealthy years at death,
#' allowing transitions at fraction ax of each interval. ax = 0 assigns the
#' full interval to the destination state; ax = 1 assigns it to the origin.
#' Death occurs at the transition time, so a death contributes ax of the
#' interval in its origin state.
#' Used by: calculate_group_dxh() in R/09_manhattan.R.
calc_dxh <- function(p_tibble,
                     age_int = 1,
                     init = c(H = 1, U = 0),
                     ax = 0,
                     keep_zero = FALSE) {
  required_transitions <- c("HH", "HU", "HD", "UH", "UU", "UD")
  missing_columns <- setdiff(c("age", required_transitions), names(p_tibble))
  if (length(missing_columns) > 0) {
    stop("Missing column(s): ", paste(missing_columns, collapse = ", "))
  }
  if (length(ax) != 1L || is.na(ax) || !is.finite(ax) || ax < 0 || ax > 1) {
    stop("ax must be one finite number between 0 and 1.")
  }
  if (length(age_int) != 1L || is.na(age_int) || !is.finite(age_int) || age_int <= 0) {
    stop("age_int must be one positive finite number.")
  }
  if (!all(c("H", "U") %in% names(init)) ||
      anyNA(init[c("H", "U")]) ||
      any(!is.finite(init[c("H", "U")])) ||
      any(init[c("H", "U")] < 0) ||
      abs(sum(init[c("H", "U")]) - 1) > 1e-8) {
    stop("init must be a non-negative H/U vector that sums to one.")
  }
  
  p_tibble <- p_tibble |>
    dplyr::arrange(age)
  ages <- p_tibble$age
  if (length(ages) < 1L || anyNA(ages) || any(!is.finite(ages))) {
    stop("age must contain at least one finite value.")
  }
  if (length(ages) > 1L && any(abs(diff(ages) - age_int) > 1e-8)) {
    stop("Ages must be consecutive and separated by age_int.")
  }
  
  probability_matrix <- as.matrix(p_tibble[required_transitions])
  if (anyNA(probability_matrix) || any(!is.finite(probability_matrix)) ||
      any(probability_matrix < 0 | probability_matrix > 1)) {
    stop("Transition probabilities must be finite values between zero and one.")
  }
  row_sums <- c(
    p_tibble$HH + p_tibble$HU + p_tibble$HD,
    p_tibble$UH + p_tibble$UU + p_tibble$UD
  )
  if (any(abs(row_sums - 1) > 1e-8)) {
    stop("Each H/U transition row must sum to one.")
  }
  
  # Each stock is indexed by accumulated healthy years. Total elapsed years
  # determine unhealthy years, so u = x - h.
  mass_by_state <- list(
    H = tibble::tibble(h = 0, mass = unname(init["H"])),
    U = tibble::tibble(h = 0, mass = unname(init["U"]))
  )
  death_parts <- vector("list", length(ages))
  
  for (age_index in seq_along(ages)) {
    age_now <- ages[age_index]
    elapsed_now <- (age_now - ages[1])
    terminal <- age_index == length(ages)
    
    if (terminal) {
      terminal_stock <- dplyr::bind_rows(
        mass_by_state$H |> dplyr::mutate(current_state = "H"),
        mass_by_state$U |> dplyr::mutate(current_state = "U")
      )
      death_parts[[age_index]] <- terminal_stock |>
        dplyr::transmute(
          current_state,
          age = age_now,
          x = elapsed_now,
          h,
          u = elapsed_now - h,
          dxsc = mass
        )
      next
    }
    
    next_mass_by_state <- list(H = list(), U = list())
    deaths_this_age <- list()
    death_part_number <- 0L
    
    for (origin in c("H", "U")) {
      origin_stock <- mass_by_state[[origin]]
      origin_h_increment <- if (origin == "H") ax * age_int else 0
      
      for (destination in c("H", "U", "D")) {
        transition <- paste0(origin, destination)
        transition_probability <- p_tibble[[transition]][age_index]
        branch <- origin_stock |>
          dplyr::mutate(mass = mass * transition_probability)
        
        if (destination == "D") {
          death_part_number <- death_part_number + 1L
          death_time <- elapsed_now + ax * age_int
          deaths_this_age[[death_part_number]] <- branch |>
            dplyr::mutate(
              h_death = round(h + origin_h_increment, 10),
              u_death = death_time - h_death
            ) |>
            dplyr::transmute(
              current_state = origin,
              age = ages[1] + death_time,
              x = death_time,
              h = h_death,
              u = u_death,
              dxsc = mass
            )
        } else {
          destination_h_increment <- if (destination == "H") {
            (1 - ax) * age_int
          } else {
            0
          }
          next_mass_by_state[[destination]][[length(next_mass_by_state[[destination]]) + 1L]] <-
            branch |>
            dplyr::transmute(
              h = round(h + origin_h_increment + destination_h_increment, 10),
              mass
            )
        }
      }
    }
    
    death_parts[[age_index]] <- dplyr::bind_rows(deaths_this_age)
    mass_by_state <- lapply(next_mass_by_state, function(state_parts) {
      if (length(state_parts) == 0L) {
        return(tibble::tibble(h = numeric(), mass = numeric()))
      }
      dplyr::bind_rows(state_parts) |>
        dplyr::group_by(h) |>
        dplyr::summarise(mass = sum(mass), .groups = "drop")
    })
  }
  
  answer <- dplyr::bind_rows(death_parts)
  if (!keep_zero) {
    answer <- dplyr::filter(answer, dxsc > 0)
  }
  answer
}


calculate_group_dxh <- function(.x, .y,
                                expected_ages, required_transitions,
                                radix_age, last_age,
                                ax = 1) {
  probability_wide <- .x %>%
    mutate(transition = paste0(as.character(from), as.character(to))) %>%
    select(age, transition, prob) %>%
    pivot_wider(names_from = transition, values_from = prob) %>%
    arrange(age)
  
  missing_transitions <- setdiff(required_transitions, names(probability_wide))
  if (length(missing_transitions) > 0) {
    stop(
      "A model group is missing transition(s): ",
      paste(missing_transitions, collapse = ", ")
    )
  }
  
  if (!identical(as.integer(probability_wide$age), expected_ages)) {
    stop(
      "Each model group must have one complete annual probability grid from age ",
      radix_age, " through ", last_age, "."
    )
  }
  
  if (anyNA(probability_wide[required_transitions])) {
    stop("A model group has an incomplete age-by-transition probability grid.")
  }
  
  initial_composition <- init_constant(probability_wide[1, ])
  
  # calc_dxh_fast() treats its final input row as the terminal age and forces
  # qH = qU = 1 there. This row closes the life table after the age-110 step.
  probability_wide <- probability_wide %>%
    add_row(
      age = last_age + 1L,
      HH = 0,
      HU = 0,
      HD = 1,
      UH = 0,
      UU = 0,
      UD = 1
    )
  
  calc_dxh(
    p_tibble = probability_wide %>% select(age, all_of(required_transitions)),
    age_int = 1,
    init = initial_composition,
    keep_zero = FALSE,
    ax = ax
  )
}


#' Find a requested SHARE data file loose or inside a ZIP archive.
#' Used by: R/01_extract_SHARE_files.R.
find_share_file <- function(target, download_dir, zip_files) {
  loose_files <- list.files(
    download_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  loose_match <- loose_files[basename(loose_files) == target]
  
  if (length(loose_match) > 0) {
    return(list(type = "file", path = loose_match[1]))
  }
  
  for (zip_path in zip_files) {
    archive_names <- utils::unzip(zip_path, list = TRUE)$Name
    archive_match <- archive_names[basename(archive_names) == target]
    
    if (length(archive_match) > 0) {
      return(list(
        type = "zip",
        path = zip_path,
        member = archive_match[1]
      ))
    }
  }
  
  NULL
}

#' Read one Spanish HMD data product, using environment credentials when set.
#' Used by: R/05_smooth_HMD.R.
read_hmd_product <- function(item,
                             hmd_user = Sys.getenv("HMD_USER"),
                             hmd_password = Sys.getenv("HMD_PASSWORD")) {
  if (nzchar(hmd_user) && nzchar(hmd_password)) {
    HMDHFDplus::readHMDweb(
      CNTRY = "ESP",
      item = item,
      username = hmd_user,
      password = hmd_password
    )
  } else {
    HMDHFDplus::readHMDweb(CNTRY = "ESP", item = item)
  }
}

#' Convert one HMD product to tidy year-age-sex-value records.
#' Used by: R/05_smooth_HMD.R.
prepare_hmd_product <- function(data, year_range, age_range) {
  data %>%
    janitor::clean_names() %>%
    transmute(
      year = as.integer(readr::parse_number(as.character(year))),
      age = readr::parse_number(as.character(age)),
      male = readr::parse_number(as.character(male)),
      female = readr::parse_number(as.character(female))
    ) %>%
    filter(
      between(year, year_range[1], year_range[2]),
      between(age, age_range[1], age_range[2])
    ) %>%
    pivot_longer(
      cols = c(male, female),
      names_to = "sex",
      values_to = "value"
    )
}

#' Convert one grouped set of transition hazards to probabilities.
#' Used by: R/07_derive_probabilities.R.
hazards_to_probabilities <- function(.x, .y, age_interval) {
  states <- c("H", "U", "D")
  
  Q <- matrix(
    0,
    nrow = length(states),
    ncol = length(states),
    dimnames = list(from = states, to = states)
  )
  
  for (transition_row in seq_len(nrow(.x))) {
    from_state <- as.character(.x$from[transition_row])
    to_state <- as.character(.x$to[transition_row])
    Q[from_state, to_state] <- .x$hazard[transition_row]
  }
  
  # Death has no outgoing hazards, so its row remains zero and P_DD = 1.
  diag(Q) <- -rowSums(Q)
  
  P <- expm::expm(Q * age_interval)
  
  as.data.frame(as.table(P)) %>%
    rename(from = Var1, to = Var2, prob = Freq) %>%
    filter(from != "D") %>%
    mutate(prob = as.numeric(prob))
}

# Timing convention:
#   ax is the fraction of each age interval lived before a transition occurs.
#   At ax = 0 transitions occur at the interval start; at ax = 1 they occur at
#   its end. Expected time in a state during an interval is interpolated between
#   the state distribution at its two birthdays using this same ax.
#
# The age-expanded fundamental matrix calculates conditional survivorship from
# every possible starting age and state in one solve. Its interval rewards give
# HLEh, ULEh, HLEu, and ULEu without repeating a forward recursion for every
# starting age. Set return_details = TRUE to inspect the tidy conditional lxs
# and Lxs records alongside the usual age-specific result. Source
# R/00_functions.R first so init_constant() and the project packages are loaded.
# -----------------------------------------------------------------------------

#' Calculate age-specific life-table and HDAG quantities for one group.
#' Used by: R/08_hdag.R.
calculate_group_hdag <- function(.x, .y,
                                 required_transitions,
                                 age_interval = 1,
                                 ax = 0,
                                 return_details = FALSE) {
  if (length(age_interval) != 1L || is.na(age_interval) ||
      !is.finite(age_interval) || age_interval <= 0) {
    stop("age_interval must be one finite positive number.")
  }
  if (length(ax) != 1L || is.na(ax) || !is.finite(ax) || ax < 0 || ax > 1) {
    stop("ax must be one finite number between 0 and 1.")
  }
  
  probability_wide <- .x %>%
    dplyr::mutate(
      # Normalize labels/factors to plain numeric before adding the terminal
      # age; this avoids class-specific arithmetic changing the final grid.
      age = as.numeric(as.character(age)),
      from = as.character(from),
      to = as.character(to),
      prob = as.numeric(prob),
      from_to = paste0(from, to)
    ) %>%
    dplyr::select(age, from_to, prob) %>%
    tidyr::pivot_wider(names_from = from_to, values_from = prob) %>%
    dplyr::arrange(age)
  
  missing_transitions <- setdiff(required_transitions, names(probability_wide))
  if (length(missing_transitions) > 0L) {
    stop("A model group is missing transition(s): ",
         paste(missing_transitions, collapse = ", "))
  }
  if (anyNA(probability_wide[required_transitions])) {
    stop("A model group has an incomplete age-by-transition probability grid.")
  }
  
  ages <- probability_wide$age
  n_ages <- length(ages)
  if (n_ages < 1L) stop("A model group has no age rows.")
  if (anyNA(ages) || any(!is.finite(ages))) {
    stop("A model group has a missing or non-numeric age value.")
  }
  if (n_ages > 1L && any(abs(diff(ages) - age_interval) > 1e-8)) {
    stop("A model group must have consecutive ages separated by age_interval.")
  }
  
  states <- c("H", "U")
  terminal_age <- max(ages) + age_interval
  node_ages <- c(ages, terminal_age)
  node_names <- paste0(
    rep(states, times = length(node_ages)), "__",
    rep(format(node_ages, scientific = FALSE, trim = TRUE), each = length(states))
  )
  node_count <- length(node_names)
  
  # Build each age-specific transient transition matrix. Death has no outgoing
  # transitions and is therefore omitted from these H/U blocks.
  transient_by_age <- vector("list", n_ages)
  for (age_index in seq_len(n_ages)) {
    transient_by_age[[age_index]] <- matrix(
      c(
        probability_wide$HH[age_index], probability_wide$HU[age_index],
        probability_wide$UH[age_index], probability_wide$UU[age_index]
      ),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(states, states)
    )
  }
  
  # U advances the age-state node by one interval. N = (I - U)^-1 gives the
  # expected number of birthday visits to every later age-state, conditional
  # on starting at each age-state.
  age_expanded_U <- matrix(
    0,
    nrow = node_count,
    ncol = node_count,
    dimnames = list(node_names, node_names)
  )
  for (age_index in seq_len(n_ages)) {
    current_rows <- (age_index - 1L) * 2L + seq_len(2L)
    next_columns <- age_index * 2L + seq_len(2L)
    age_expanded_U[current_rows, next_columns] <- transient_by_age[[age_index]]
  }
  fundamental_N <- solve(diag(node_count) - age_expanded_U)
  
  conditional_lxs <- as.data.frame(fundamental_N, check.names = FALSE) %>%
    tibble::rownames_to_column("state_age_from") %>%
    tidyr::pivot_longer(
      cols = -state_age_from,
      names_to = "state_age_to",
      values_to = "lxs"
    ) %>%
    tidyr::separate_wider_delim(
      state_age_from,
      delim = "__",
      names = c("state_from", "age_from")
    ) %>%
    tidyr::separate_wider_delim(
      state_age_to,
      delim = "__",
      names = c("state_to", "age_to")
    ) %>%
    dplyr::mutate(
      age_from = as.numeric(age_from),
      age_to = as.numeric(age_to)
    ) %>%
    dplyr::filter(age_to >= age_from - 1e-8) %>%
    dplyr::arrange(state_from, age_from, state_to, age_to)
  
  # For a starting age-state and a destination state, Lxs(x) is the interval
  # exposure implied by conditional birthday survivorship at both endpoints:
  # age_interval * {ax*lxs(x) + (1-ax)*lxs(x+age_interval)}. Summing Lxs over
  # attained ages gives each of the four conditional state expectancies.
  conditional_Lxs <- conditional_lxs %>%
    dplyr::group_by(state_from, age_from, state_to) %>%
    dplyr::arrange(age_to, .by_group = TRUE) %>%
    dplyr::mutate(
      lxs_next = dplyr::lead(lxs),
      Lxs = age_interval * (ax * lxs + (1 - ax) * lxs_next)
    ) %>%
    dplyr::filter(age_to < terminal_age - 1e-8) %>%
    dplyr::ungroup()
  
  conditional_expectancies <- conditional_Lxs %>%
    dplyr::group_by(state_from, age_from, state_to) %>%
    dplyr::summarise(expectancy = sum(Lxs, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      expectancy_name = dplyr::case_when(
        state_from == "H" & state_to == "H" ~ "HLEh",
        state_from == "U" & state_to == "H" ~ "HLEu",
        state_from == "H" & state_to == "U" ~ "ULEh",
        state_from == "U" & state_to == "U" ~ "ULEu"
      )
    ) %>%
    tidyr::pivot_wider(
      # The four labels already encode both starting and destination state.
      # Keeping state_from as an id column here would leave two rows per age
      # and duplicate the life-table rows at the join below.
      id_cols = age_from,
      names_from = expectancy_name,
      values_from = expectancy
    ) %>%
    dplyr::select(age = age_from, HLEh, ULEh, HLEu, ULEu)
  
  if (anyDuplicated(conditional_expectancies$age)) {
    stop("Conditional expectancies must have exactly one row per starting age.")
  }
  
  # Cohort survivorship at birthdays, beginning with the stationary H/U mix
  # implied by the first age's transition matrix (the project's init_constant).
  # Keep the existing mscalc survivorship calculation unchanged; only extend it
  # one birthday to close the final age interval for Lx and conditional Lxs.
  initial_probabilities <- probability_wide[1L, , drop = FALSE]
  initial <- init_constant(initial_probabilities)
  transition_probabilities <- .x %>%
    dplyr::mutate(
      from_to = paste0(as.character(from), as.character(to))
    ) %>%
    dplyr::select(age, from_to, prob)
  cohort_lxs <- mscalc:::calc_survivorship(
    transitions = transition_probabilities,
    init = initial,
    delim = "",
    age_col = "age",
    trans_col = "from_to",
    p_col = "prob"
  ) %>%
    dplyr::select(age, H, U) %>%
    dplyr::rename(lh = H, lu = U) %>%
    dplyr::arrange(age)
  if (anyDuplicated(cohort_lxs$age)) {
    stop("Cohort survivorship must have exactly one row per age.")
  }
  terminal_survivorship <- as.numeric(
    c(cohort_lxs$lh[nrow(cohort_lxs)], cohort_lxs$lu[nrow(cohort_lxs)]) %*%
      transient_by_age[[n_ages]]
  )
  cohort_lxs <- dplyr::bind_rows(
    cohort_lxs,
    tibble::tibble(
      age = terminal_age,
      lh = terminal_survivorship[1L],
      lu = terminal_survivorship[2L]
    )
  )
  
  # Join the birth-age conditional expectancies to the cohort life table.
  result <- probability_wide %>%
    dplyr::left_join(cohort_lxs, by = "age") %>%
    dplyr::left_join(conditional_expectancies, by = "age")
  
  # Add the terminal birthday row. It closes the life table at the end of the
  # supplied probability schedule; there is no post-terminal interval exposure.
  terminal_row <- tibble::tibble(
    age = terminal_age,
    HH = NA_real_, HU = NA_real_, HD = NA_real_,
    UH = NA_real_, UU = NA_real_, UD = NA_real_,
    lh = cohort_lxs$lh[nrow(cohort_lxs)],
    lu = cohort_lxs$lu[nrow(cohort_lxs)],
    HLEh = 0, ULEh = 0, HLEu = 0, ULEu = 0
  )
  result <- dplyr::bind_rows(result, terminal_row) %>%
    dplyr::arrange(age)
  
  # Cohort transfer counts at each age interval.
  result <- result %>%
    dplyr::mutate(
      HDt = HD * lh,
      HUt = HU * lh,
      UHt = UH * lu,
      UDt = UD * lu,
      future_H_from_H = dplyr::lead(HLEh, default = 0),
      future_H_from_U = dplyr::lead(HLEu, default = 0),
      future_U_from_H = dplyr::lead(ULEh, default = 0),
      future_U_from_U = dplyr::lead(ULEu, default = 0),
      HLE_at_death_from_H = HLEh + ax * (future_H_from_H - HLEh),
      HLE_at_death_from_U = HLEu + ax * (future_H_from_U - HLEu),
      ULE_at_death_from_H = ULEh + ax * (future_U_from_H - ULEh),
      ULE_at_death_from_U = ULEu + ax * (future_U_from_U - ULEu),
      # Transition components compare the origin path with the destination
      # path after the transition. Only the remaining (1 - ax) interval is
      # credited to the post-transition state.
      HLEdag_hu = HUt * ((1 - ax) * age_interval + future_H_from_H - future_H_from_U),
      HLEdag_uh = UHt * (-(1 - ax) * age_interval + future_H_from_U - future_H_from_H),
      ULEdag_hu = HUt * (-(1 - ax) * age_interval + future_U_from_H - future_U_from_U),
      ULEdag_uh = UHt * ((1 - ax) * age_interval + future_U_from_U - future_U_from_H),
      # Interpolate each origin-state expectancy to the assumed death age, in
      # the same way as the life-table e-dagger helper below.
      HLEdag_hd = HDt * HLE_at_death_from_H,
      ULEdag_hd = HDt * ULE_at_death_from_H,
      HLEdag_ud = UDt * HLE_at_death_from_U,
      ULEdag_ud = UDt * ULE_at_death_from_U,
      LEdag_hu = HLEdag_hu + ULEdag_hu,
      LEdag_hd = HLEdag_hd + ULEdag_hd,
      LEdag_uh = HLEdag_uh + ULEdag_uh,
      LEdag_ud = HLEdag_ud + ULEdag_ud,
      age_interval = age_interval,
      ax = ax
    ) %>%
    dplyr::select(
      -dplyr::starts_with("future_"),
      -dplyr::starts_with("HLE_at_death_"),
      -dplyr::starts_with("ULE_at_death_")
    )
  
  result <- result %>% dplyr::filter(age %in% ages)
  if (!return_details) return(result)
  
  list(
    age_specific = result,
    conditional_lxs = conditional_lxs,
    conditional_Lxs = conditional_Lxs,
    conditional_expectancies = conditional_expectancies,
    cohort_lxs = cohort_lxs,
    age_expanded_U = age_expanded_U,
    fundamental_N = fundamental_N
  )
}

#' Calculate age-specific e-dagger from a closed period life table.
#' Used by: R/08_hdag.R after cohort survivorship and life-table columns exist.
life_table_edag <- function(age, dx, lx, ex, ax, age_interval = 1) {
  if (length(ax) != 1L || is.na(ax) || !is.finite(ax) || ax < 0 || ax > 1) {
    stop("ax must be one finite number between 0 and 1.")
  }
  if (length(age_interval) != 1L || is.na(age_interval) ||
      !is.finite(age_interval) || age_interval <= 0) {
    stop("age_interval must be one finite positive number.")
  }
  if (!all(length(age) == length(dx), length(dx) == length(lx), length(lx) == length(ex))) {
    stop("age, dx, lx, and ex must have the same length.")
  }
  if (length(age) > 1L) {
    bad_steps <- which(abs(diff(age) - age_interval) > 1e-8)
    if (length(bad_steps) > 0L) {
      bad_intervals <- paste0(
        age[bad_steps], " to ", age[bad_steps + 1L],
        " (width ", diff(age)[bad_steps], ")"
      )
      stop(
        "age values must be consecutive and separated by age_interval. ",
        "Unexpected interval(s): ", paste(bad_intervals, collapse = "; ")
      )
    }
  }
  
  ex_next <- c(ex[-1L], 0)
  ex_at_death <- ex + ax * (ex_next - ex)
  edag <- rev(cumsum(rev(dx * ex_at_death))) / lx
  edag[!is.finite(edag)] <- 0
  edag
}

