# ----------------------------------------------------------------- #
Sys.setenv(LANG = "en")
# packages
library(compositions)
library(data.table)
library(lubridate)
library(tidyverse)
library(tidyfast)
library(collapse)
library(ggthemes)
library(magrittr)
library(splines)
library(janitor)
library(ggpubr)
library(scales)
library(glue)
library(VGAM)

# read csv files select columns, slightly improve names
# ----------------------------------------------------------------- #
read_data <- function(x, y) {
  
  files <- list.files(pattern = x)
  
  map(files,
      read_csv,
      col_select = any_of(y)) %>%
    set_names(str_sub(files, start = 3, end = 5))
  
}

# prepare data for future calculations
# ----------------------------------------------------------------- #
create_data_from_the_chosen_cohort <- function(a) {
  
  # ID
  # ----------------------------------------------------------------- #
  # so technically 2006 to 2017 but we can start with 2004
  id <- pt2 %>%
    map(~ .x %>%
          filter(PB010 == min(PB010)) %>% # we can identify people from the first year of observation
          count(PB010, PB030, PB140, PB130, PB150) %>% # keeping identification columns
          dplyr::select(-n)) %>%
    keep(~ min(.x$PB010) == a) %>% # keep just one file with the lowest year == a
    bind_rows() %>%
    dplyr::select(-1)
  
  # Health
  # ----------------------------------------------------------------- #
  health <- pt2 %>%
    map(~ .x %>%
          dplyr::select(PB010, PB100, PB030, PB150, PB050, PH010, PB140, PB130) %>%
          inner_join(id)) %>%
    bind_rows() %>%
    mutate(PB050 = round(PB050)) %>% ### this one was important. floating error
    distinct() %>%
    # just in case.
    filter(PB010 > a) %>%
    # month imputation part. basically uniform
    # for survey month data
    mutate(PB100 = ifelse(is.na(PB100) & !is.na(PB130), PB130, PB100)) %>%
    # for birth month data
    mutate(PB130 = ifelse(is.na(PB130) & !is.na(PB100), PB100, PB130)) %>%
    # is birth are missing
    mutate(across(c(PB100, PB130), ~ ifelse(is.na(.), 1, .))) %>%
    # give columns a name that makes sense 
    set_names(c("y", "m", "id", "sex", "w", "health", "b_y", "b_m")) %>%
    # recode the health outcome
    mutate(health = case_when(
      health %in% c(1:3) ~ "H",
      health %in% c(4:5) ~ "NH",
      TRUE               ~ NA_character_)) %>%
    # combine month and year into date column for survey
    unite("m_y_survey",  c(m, y), sep = "-", remove = FALSE) %>%
    # same for birth date
    unite("m_y_birth",   c(b_m, b_y), sep = "-") %>%
    # transform to age format
    mutate(across(c(m_y_survey, m_y_birth), ~ my(.))) %>%
    # calculate age in years
    mutate(age = interval(m_y_birth, m_y_survey) / years(1)) %>%
    # remove unnecessary columns
    dplyr::select(-c(m_y_survey, m)) %>%
    # make age integer
    mutate(age = floor(age))
  
  # Deaths
  # ----------------------------------------------------------------- #
  dead <- pt1 %>%
    map(~ .x %>%
          filter(RB110 == 6) %>% # keep only deaths
          dplyr::select(RB010, RB030, RB070, RB080, RB060, RB090, RB110, RB140,  RB150) %>%
          inner_join(
            set_names(id, c("RB030", "RB080", "RB070", "RB090")))) %>%
    bind_rows() %>%
    distinct() %>%
    set_names(c("y", "id", "b_m", "b_y", "w",
                "sex", "health", "m_d", "y_d")) %>%
    # choose columns
    # create date of birth
    unite("m_y_birth",   c(b_m, b_y), sep = "-") %>%
    # create date of death
    unite("m_y_d_or_tr", c(m_d, y_d), sep = "-") %>%
    # transform to date format
    mutate(across(c(m_y_birth, m_y_d_or_tr), ~ my(.))) %>%
    # calculate age
    mutate(age = interval(m_y_birth, m_y_d_or_tr) / years(1)) %>%
    dplyr::select(-m_y_d_or_tr, health) %>%
    # create the death state = Death column
    mutate(health = "D") %>%
    # make integer ages
    mutate(age = floor(age))
  
  # Combine databases
  # ----------------------------------------------------------------- #
  dt <- health %>%
    # join two databases
    full_join(dead) %>%
    # dplyr::select(-m_y_birth) %>% ############
  group_by(id) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>% # remove people who are only shown once in the data
    group_by(sex, id) %>%
    # arrange by age
    arrange(age) %>%
    mutate(tst = ifelse(last(health == "D") & any(is.na(health)), 1, 0)) %>%
    mutate(health = ifelse(tst == 1 & is.na(health), "NH", health)) %>%
    # input time t + 1 as time t if missing
    fill(health, .direction = "down") %>%
    # do vice versa
    fill(health, .direction = "up") %>%
    # now create to and from states
    # for each ID
    group_by(id) %>%
    # arrange by are
    arrange(age) %>%
    # create from variable
    rename(from = health) %>%
    # create to variable with lead
    mutate(to   = lead(from)) %>%
    ungroup() %>%
    # final imputation
    mutate(to = ifelse(from == "D", "D", to)) %>%
    # remove the people for which we know nothing
    filter(!is.na(from)) %>%
    filter(!is.na(w)) %>% # remove transitions from D to D
    mutate(to = ifelse(is.na(to) & !is.na(from), from, to)) %>%
    filter(w > 0) %>% # also from D to D
    # create period variable
    mutate(period = glue("{y}-{y+1}")) %>%
    # filter only inner 2 transitions
    filter(str_detect(period, as.character(a + 2))) %>% # this is to keep only inner 2 transitions
    dplyr::select(-c(n, period, tst))
  
  return(dt)
  
}

# calculate lh and lu
# ----------------------------------------------------------------- #
lets <- function(.data) {
  
  for(i in 1:length(.data$lu[-1])) {
    
    .data$lh[i + 1] <-
      .data$lh[i] * .data$`H-H`[i] + .data$lu[i] * .data$`NH-H`[i]
    
    .data$lu[i + 1] <-
      .data$lu[i] * .data$`NH-NH`[i] + .data$lh[i] * .data$`H-NH`[i]
    
  }
  
  return(.data)
  
}

# lu / lx is the prevalence of disability

# calculate lifetable
# ----------------------------------------------------------------- #
calculate_lt <- function(.data) {
  .data %>%
    mutate(lx = lu + lh,
           du = lu * `NH-D`,
           dh = lh * `H-D`,
           dx = du + dh,
           qx = dx / lx,
           px = 1 - qx
    ) %>%
    mutate(Lh = (lh + lead(lh)) / 2,
           Lh = ifelse(is.na(Lh), (lh + (lh * `H-H` + lu * `NH-H`)) / 2, Lh),
           Lu = (lu + lead(lu)) / 2,
           Lu = ifelse(is.na(Lu), (lu + (lu * `NH-NH` + lh * `H-NH`)) / 2, Lu),
           Lx = Lh + Lu
    ) %>%
    mutate(mh = dh / Lh,
           mu = du / Lu,
           mx = dx / Lx) %>%
    mutate(Ra = mu / mh)
  
}

# recalculate probabilities to sum to one
# ----------------------------------------------------------------- #
recalculate <- function(x) {
  
  x[1:2] <- x[1:2] / sum(x[1:2]) * (1 - x[3])
  
  return(x)
  
}

#' @title init_constant
#' @description derive initial conditions as function of probabilities in the first time step. This assumes that probabilities in ages prior to the first time step were constant.
#' @param x numeric vector with named elements `HH`, `UH`, `HU`, `UU`
#' @return numeric vector of length two with initial composition. `H` gives the fraction healthy at start, and `U` gives the fraction unhealthy at start.

init_constant <- function(x) {
  x    <- unlist(x[c("HH", "UH", "HU", "UU")]) 
  u    <- matrix(x, nrow = 2, byrow = TRUE)
  v    <- eigen(u)$vectors[, 1]
  init <- v / sum(v)
  setNames(init, c("H", "U"))
}

#' @title pi_block
#' @description Produce a submatrix for a given transition type, to be composed together with other submatrices to form the full transient matrix.
#' @param p vector of transition probabilities, ordered by age
#' @param state_from character, the origin state, used for margin labeling.
#' @param state_to character, the destination state, used for margin labeling.
#' @param age vector giving the age classes (lower bounds), used for margin labeling.
#' @return a matrix of dimension `length(p)+1` by `length(p)+1`.

pi_block <- function(p, state_from, state_to, age) {
  
  state_fromi  <- state_from[1]
  state_toi    <- state_to[1]
  age          <- c(age, max(age) + 1)
  P            <- diag(p)
  P            <- cbind(rbind(0, P), 0)
  from_names   <- paste(state_fromi, age, sep = "_")
  to_names     <- paste(state_toi,   age, sep = "_")
  dimnames(P)  <- list(to_names, from_names)
  return(P)
}
#' @title pi_block_outer
#' @description Produce a submatrix for a given transition type, to be composed together with other submatrices to form the full transient matrix. For use in tidy framework. This function wraps `pi_block()`.
#' @param chunk data.frame with columns `p`, `from`, `to`, and `age`
#' @return a tibble of dimension `length(p)+1` by `length(p)+1`.

pi_block_outer <- function(chunk) {
  
  pi_block(chunk[["p"]]|> as.double(),
           chunk[["from"]],
           chunk[["to"]],
           chunk[["age"]]) |>
    as_tibble()
}

#' @title p_tibble2U
#' @description Produce the transient matrix `U` based on a tidy `data.frame` of transition probabilities.
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state.
#' @return matrix `U` composed of submatrices for each transition.

p_tibble2U <- function(p_tibble) {
  
  age <- p_tibble[["age"]] |>
    unique() |>
    sort()
  
  age <- c(age, age[length(age)] + 1)
  pre <- p_tibble |>
    dt_pivot_longer(-age, 
                    names_to  = "fromto", 
                    values_to = "p") |>
    fmutate(from = substr(fromto, 0, 1),
            to   = substr(fromto, 2, 2)) |>
    fselect(-fromto) |>
    fsubset(to != "D")
  
  states <- pre$from |>
    unique()
  
  pre |>
    group_by(from, to) |>
    nest() |>
    fmutate(data = map(data, ~ .x |>
                         pi_block_outer())) |> 
    dt_pivot_wider(names_from  = from, 
                   values_from = data) |> 
    unnest(cols = all_of(states),
           names_sep = "") |> 
    ungroup() |> 
    fmutate(to = paste(rep(states, each = length(age)), 
                       rep(age,    length(states)), # each
                       sep = "_")) |> 
    column_to_rownames("to") |> 
    as.matrix()
}

#' @title p_tibble2N
#' @description Produce the fundamental matrix `N` based on a tidy `data.frame` of transition probabilities.
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state.
#' @return the fundamental matrix, `N`, containing age-state conditional survivorship values

p_tibble2N <- function(p_tibble, discount = FALSE) {
  
  U <- p_tibble2U(p_tibble)
  I <- diag(rep(1, nrow(U)))
  N <- solve(I - U) 
  if (discount) {
    N < N - I / 2
  }
  return(N)
}

#' @title p_tibble2lxs
#' @description produce a tidy data.frame of age-state survivorships for a given `state`, duly weighted by some declared initial conditions, `init`.
#' @param p_tibble a `data.frame` with columns `age`, and columns containing transitions, where column names are two concatenated letters where the first letter gives the origin state and the second letter gives the destination state.
#' @param state character for which state we want `lxs`. Presumably but not necessarily `"H"` or `"U"`
#' @param init numeric vector giving initial conditions. Elements should be labelled with the state shorthand used, presumably but not necessarily `"H"` and `"U"`.
#' @return data.frame giving the age-specific values of survivors for the given state

p_tibble2lxs <- function(p_tibble, state = "H", init = c(H = 0.8, U = 0.2)) {
  
  age                 <- p_tibble[["age"]] |> min()
  N                   <- p_tibble2N(p_tibble)
  cols                <- grepl(colnames(N),
                               pattern =
                                 paste0("^._", age, "$"))
  rows                <- grepl(rownames(N), pattern = state)
  to_weight           <- N[rows, cols] 
  colnames(to_weight) <- substr(colnames(to_weight), 0, 1)  
  
  init <- as.matrix(init) |>
    as.data.frame() |>
    rownames_to_column("init_state") |>
    rename(init = V1)
  
  to_weight |>
    as.data.frame() |>
    rownames_to_column("age") |>
    as_tibble() |>
    fmutate(age = parse_number(age)) |>
    dt_pivot_longer(-age, 
                    names_to  = "init_state", 
                    values_to = "lxs") |>
    left_join(init, by = "init_state") |> 
    fgroup_by(age) |>
    fsummarize(lxs = sum(lxs * init)) |> 
    fungroup()
}

# extrapolate alr transformed transition probabilities
# ----------------------------------------------------------------- #
extrap_lm <- function(y) {
  x    <- 17:86
  xnew <- data.frame(x = 87:110)
  y2   <-  predict(lm(y ~ x), newdata = xnew)
  c(y, y2)
}

# ----------------------------------------------------------------- #
# extrapolate alr transformed transition probabilities down
extrap_lm_down <- function(y) {
  x    <- 17:110
  xnew <- data.frame(x = 10:16)
  y2   <-  predict(lm(y ~ x), newdata = xnew)
  c(y2, y)
}
