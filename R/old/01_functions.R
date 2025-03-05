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

p_tibble2lxs <- function(p_tibble = adl, 
                         state = "H", 
                         init = init) {
  
  age                 <- p_tibble[["age"]] |> min()
  N                   <- p_tibble2N(p_tibble)
  cols                <- grepl(colnames(N), pattern = age)
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
