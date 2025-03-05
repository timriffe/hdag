source("R/0_functions.R")
load("Results/adjusted_and_raw_trans_prob.RData")

pred_data_raw <- full_trns %>% 
  filter(type == "Adjusted") %>%
  unite(state, c("from", "to"), sep = "-") %>%
  group_nest(sex, time, age) %>% 
  mutate(data = map(data, ~ .x %>% 
                      pivot_wider(
                        names_from  = state,
                        values_from = val
                      ))) %>% 
  unnest(data) %>% 
  group_nest(sex, time) %>%
  mutate(data = map(
    data,
    ~ .x %>%
      dplyr::select(
        age,
        HH = `H-H`,
        HU = `H-NH`,
        HD = `H-D`,
        UH = `NH-H`,
        UU = `NH-NH`,
        UD = `NH-D`
      ) %>%
      mutate(age = as.numeric(age))
  )) %>%
  mutate(initials = map(data, ~ .x %>%
                          slice(1) %>%
                          init_constant))  %>%
  mutate(lh = map2(
    .x = data,
    .y = initials,
    ~ .x %>%
      p_tibble2lxs(init = .y, state = "H") %>%
      rename(lh = lxs) %>% 
      filter(age < 111)
  )) %>%
  mutate(lu = map2(
    .x = data,
    .y = initials,
    ~ .x %>%
      p_tibble2lxs(init = .y, state = "U") %>%
      rename(lu = lxs)%>% 
      filter(age < 111)
  )) %>%
  mutate(
    LEi = map(
      data,
      ~ .x %>%
        p_tibble2N() |>
        as.data.frame() |>
        rownames_to_column("state_age_to") |>
        pivot_longer(-state_age_to, names_to  = "state_age_from", values_to = "lxs") |>
        separate_wider_delim(
          cols  = state_age_from,
          delim = "_",
          names = c("state_from", "age_from")
        ) |>
        separate_wider_delim(
          cols  = state_age_to,
          delim = "_",
          names = c("state_to", "age_to")
        ) |>
        mutate(age_from = as.integer(age_from), 
               age_to   = as.integer(age_to)) |>
        filter(age_from < 111,
               age_to   < 111) %>% 
        group_by(state_from, age_from, state_to) |>
        summarize(LEi = sum(lxs), .groups = "drop") |>
        mutate(expectancy = paste0(state_to, "LE", tolower(state_from))) |>
        select(!starts_with("state")) |>
        pivot_wider(names_from  = expectancy, values_from = LEi) |>
        rename(age = age_from)
    )) %>%
  mutate(dags = map2(
    .x = data,
    .y = lh,
    ~ .x %>%
      left_join(.y, by = join_by(age))
  )) %>%
  mutate(dags = map2(
    .x = dags,
    .y = lu,
    ~ .x %>%
      left_join(.y, by = join_by(age))
  )) %>%
  mutate(
    dags = map2(
      .x = dags,
      .y = LEi,
      ~ .x %>%
        left_join(.y, by = join_by(age)) %>%
        mutate(
          HLEdag_hu = HU * lh * (HLEh - HLEu),
          HLEdag_hd = HD * lh * HLEh,
          HLEdag_uh = UH * lu * (HLEu - HLEh),
          # negative
          HLEdag_ud = UD * lu * HLEu,
          
          ULEdag_hu = HU * lh * (ULEh - ULEu),
          # negative
          ULEdag_hd = HD * lh * ULEh,
          ULEdag_uh = UH * lu * (ULEu - ULEh),
          ULEdag_ud = UD * lu * ULEu,
          
          LEdag_hu = ULEdag_hu + HLEdag_hu,
          LEdag_hd = ULEdag_hd + HLEdag_hd,
          LEdag_uh = ULEdag_uh + HLEdag_uh,
          LEdag_ud = ULEdag_ud + HLEdag_ud
        ) |>
        select(age, contains("dag")) |>
        pivot_longer(-age, names_to  = "dag_component", values_to = "dag") |>
        separate_wider_delim(
          dag_component,
          delim = "_",
          names = c("state_dag", "transition")
        )
    )) %>%
  mutate(edag = map2(
    .x = lh,
    .y = lu,
    ~ .x %>%
      left_join(.y, by = join_by(age)) %>%
      mutate(lx = lh + lu)
  )) %>%
  mutate(
    edag = map2(
      .x = edag,
      .y = data,
      ~ .x %>%
        left_join(.y, by = join_by(age)) %>%
        mutate(across(c(HD, UD), ~ ifelse(is.na(
          .
        ), 1, .))) %>%
        mutate(dx = lh * HD + lu * UD) %>%
        mutate(ex = rev(cumsum(rev(
          lx
        ))) / lx) %>%
        summarise(ed = sum(dx * ex))
    )
  )


save(pred_data_raw, file = "Results/h_dag.RData")


