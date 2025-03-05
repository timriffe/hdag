source("0_setup.R")
source("01_functions.R")
# data
load("adjusted_and_raw_trans_prob.RData")

full_trns %>%
  filter(type == "Adjusted") %>%
  dplyr::select(-type) %>% 
  filter(time == 2014) %>% 
  mutate(sex = str_to_title(sex),
         to = str_to_title(to)) %>% 
  mutate(to = case_when(
    to == "D"  ~ "Dead",
    to == "H"  ~ "Healthy",
    to == "Nh" ~ "Unhealthy"
  )) %>% 
  mutate(from = ifelse(from == "H", "Healthy",
                        "Unhealthy")) %>%
  mutate(to = factor(to,
                         levels = c("Healthy",
                                "Unhealthy",
                                "Dead"))) %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female"))) %>% 
  rename(To = to) %>% 
  ggplot(aes(x = age, y = val, col = To)) + 
  geom_line(linewidth = 1) + 
  facet_grid(from ~ sex, switch = "y") + 
  theme_minimal() + 
  xlab("Age")+
  scale_x_continuous(breaks = pretty_breaks(n = 8))+
  theme(legend.position = "bottom",
        strip.placement = "outside",
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))

transitions <- full_trns %>%
  filter(type == "Adjusted") %>%
  dplyr::select(-type) %>%
  group_nest(sex, time) %>%
  mutate(data = map(data, ~ .x %>%
                      unite("fromto", c(from, to), sep = "-") %>%
                      pivot_wider(names_from  = fromto,
                                  values_from = val) %>%
                      dplyr::select(age,
                                    HH = `H-H`,
                                    HU = `H-NH`,
                                    HD = `H-D`,
                                    UH = `NH-H`,
                                    UU = `NH-NH`,
                                    UD = `NH-D`))) %>% 
  mutate(init = map(data, ~ .x %>% 
                      slice(1) %>% 
                      init_constant)) %>% 
  mutate(lh = map2(.x = data, .y = init, ~ .x %>% 
                     p_tibble2lxs(init = .y, state = "H") |> 
                     rename(lh = lxs))) %>% 
  mutate(lu = map2(.x = data, .y = init, ~ .x %>% 
                     p_tibble2lxs(init = .y, state = "U") |> 
                     rename(lu = lxs))) %>% 
  mutate(LEi = map(data, ~ .x %>% 
                     p_tibble2N() |> 
                     as.data.frame() |> 
                     rownames_to_column("state_age_to") |> 
                     pivot_longer(-state_age_to, 
                                  names_to = "state_age_from", 
                                  values_to = "lxs") |>   
                     separate_wider_delim(cols = state_age_from, 
                                          delim = "_",
                                          names = c("state_from","age_from")) |> 
                     separate_wider_delim(cols = state_age_to,
                                          delim = "_",
                                          names = c("state_to", "age_to")) |> 
                     mutate(age_from = as.integer(age_from),
                            age_to = as.integer(age_to)) |> 
                     group_by(state_from, age_from, state_to) |> 
                     summarize(LEi = sum(lxs),.groups = "drop") |> 
                     mutate(expectancy = paste0(state_to,"LE",tolower(state_from))) |> 
                     select(!starts_with("state")) |> 
                     pivot_wider(names_from = expectancy, values_from = LEi) |> 
                     rename(age = age_from))) %>% 
  mutate(dags = map2(.x = data, 
                     .y = lh, ~ .x %>% 
                       left_join(.y, by = join_by(age)))) %>% 
  mutate(dags = map2(.x = dags, 
                     .y = lu, ~ .x %>% 
                       left_join(.y, by = join_by(age)))) %>% 
  mutate(dags = map2(.x = dags, 
                     .y = LEi, ~ .x %>% 
                       left_join(.y, by = join_by(age)) %>% 
                       mutate(HLEdag_hu = HU * lh * (HLEh - HLEu),
                              HLEdag_hd = HD * lh * HLEh,
                              HLEdag_uh = UH * lu * (HLEu - HLEh), # negative
                              HLEdag_ud = UD * lu * HLEu,
                              
                              ULEdag_hu = HU * lh * (ULEh - ULEu), # negative
                              ULEdag_hd = HD * lh * ULEh,
                              ULEdag_uh = UH * lu * (ULEu - ULEh),
                              ULEdag_ud = UD * lu * ULEu,
                              
                              LEdag_hu = ULEdag_hu + HLEdag_hu,
                              LEdag_hd = ULEdag_hd + HLEdag_hd,
                              LEdag_uh = ULEdag_uh + HLEdag_uh,
                              LEdag_ud = ULEdag_ud + HLEdag_ud) |> 
                       select(age, contains("dag")) |> 
                       pivot_longer(-age, 
                                    names_to  = "dag_component", 
                                    values_to = "dag") |> 
                       separate_wider_delim(dag_component, 
                                            delim = "_", 
                                            names = c("state_dag", "transition"))
  )) %>% 
  mutate(edag = map2(.x = lh,
                     .y = lu, ~ .x %>% 
                       left_join(.y, by = join_by(age)) %>% 
                       mutate(lx = lh + lu))) %>% 
  mutate(e0 = map2(.x = edag,
                     .y = data, ~ .x %>%
                       left_join(.y, by = join_by(age)) %>%
                       mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                       mutate(dx = lh * HD + lu * UD) %>%
                       mutate(ex = rev(cumsum(rev(lx))) / lx))
  ) %>%
  mutate(edag = map2(.x = edag,
                     .y = data, ~ .x %>% 
                       left_join(.y, by = join_by(age)) %>% 
                       mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                       mutate(dx = lh * HD + lu * UD) %>% 
                       mutate(ex = rev(cumsum(rev(lx))) / lx) %>% 
                       reframe(ed = dx * ex)
  ))


# 71 to 70
transitions %>% 
  filter(time == 2014) %>%
  dplyr::select(sex, dags) %>% 
  unnest(dags) %>%
  mutate(sex = str_to_title(sex),
         sex = factor(sex, levels = c("Male", "Female"))) %>%
  mutate(state_dag = factor(state_dag, 
                            levels = c("HLEdag", "ULEdag", "LEdag"))) |>
  rename(Transitions = transition) %>% 
  mutate(Transitions = case_when(
    Transitions == "hd" ~ "HD",
    Transitions == "hu" ~ "HU",
    Transitions == "ud" ~ "UD",
    Transitions == "uh" ~ "UH",
  )) %>% 
  ggplot(aes(x = age, y = dag, color = Transitions)) +
  geom_line(linewidth = 1) +
  facet_grid(sex ~ state_dag, switch = "y") + 
  theme_minimal() + 
  xlab("Age")+
  geom_hline(yintercept = 0)+
  ylab("Contribution")+
  scale_x_continuous(breaks = pretty_breaks(n = 8))+
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))

transitions %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    transition %in% c("hd", "ud"),
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, state_dag) |> 
  summarize(dag = sum(dag), .groups = "drop") %>% 
  mutate(type = "mstate")

transitions %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>% 
  filter(time == 2014) %>% 
  mutate(sex = str_to_title(sex),
         sex = factor(sex, levels = c("Male",
                                      "Female"))) %>% 
  group_by(state_dag, time, sex, transition) |> 
  rename(Transitions = transition) %>% 
  summarise(Years = sum(dag)) |> 
  mutate(Transitions = case_when(
    Transitions == "hd" ~ "HD",
    Transitions == "hu" ~ "HU",
    Transitions == "ud" ~ "UD",
    Transitions == "uh" ~ "UH",
  )) %>% 
  mutate(state_dag = factor(state_dag, 
                            levels = 
                              c("HLEdag",
                                "ULEdag",
                                "LEdag"))) |>
  ggplot(aes(x    = state_dag, 
             fill = Transitions, 
             y    = Years)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  facet_grid(~ sex) + 
  xlab("Measure") +
  scale_y_continuous(breaks = pretty_breaks())+
  theme(legend.position = "bottom",
        strip.placement = "outside",
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))


# maybe weight this? by corresponding transition?
mstate <- transitions %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    # transition %in% c("hd", "ud"),
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, state_dag) |>
  summarize(dag = sum(dag), .groups = "drop") %>% 
  mutate(type = "mstate")


orig <- transitions %>%
  dplyr::select(sex, time, edag) %>% 
  unnest(edag) %>%
  group_by(sex, time) %>%
  mutate(age = 17:87) %>% 
  filter(age != 87) %>%
  mutate(type = "edag") %>% 
  rename(dag = ed) %>% 
  group_by(sex, time, type) %>% 
  summarise(dag = sum(dag))

mstate %>% 
  full_join(orig) %>% 
  mutate(sex  = str_to_title(sex),
         type = str_to_title(type)) %>%
  mutate(dag  = ifelse(type == "Mstate", dag, dag)) %>%
  mutate(sex = factor(sex, levels = c("Male", "Female"))) %>% 
  mutate(type = ifelse(type == "Mstate", "Mstate", "Classic")) %>% 
  ggplot(aes(x = time, y = dag, color = type)) + 
  geom_line() +
  geom_point()+
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ sex) + 
  theme_minimal() + 
  xlab("Year") + 
  ylab(bquote(e^"\u2020")) + 
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))

# classic e-dagger coming from total population lifetable 
# will be different from total 
# stationary population in terms of prevalence structure.

mstate <- transitions %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    transition %in% c("hd", "ud"),
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, age, state_dag) |>
  summarize(dag     = sum(dag), 
            .groups = "drop") %>% 
  filter(time == 2014) %>% 
  mutate(type = "mstate")


orig <- transitions %>%
  dplyr::select(sex, time, edag) %>% 
  unnest(edag) %>%
  group_by(sex, time) %>%
  mutate(age = 17:87) %>% 
  filter(age != 87) %>%
  mutate(type = "edag") %>% 
  rename(dag = ed) %>% 
  group_by(sex, time, type, age) %>% 
  summarise(dag = sum(dag)) %>% 
  ungroup() %>% 
  filter(time == 2014)



mstate %>% 
  full_join(orig) %>% 
  mutate(sex  = str_to_title(sex),
         type = str_to_title(type)) %>%
  mutate(dag  = ifelse(type == "Mstate", dag, dag)) %>%
  mutate(sex = factor(sex, levels = c("Male", "Female"))) %>% 
  mutate(type = ifelse(type == "Mstate", "Mstate", "Classic")) %>% 
  ggplot(aes(x = age, y = dag, color = type)) + 
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ sex) + 
  theme_minimal() + 
  xlab("Age") + 
  ylab(bquote(e^"\u2020")) + 
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))




e0 <- transitions %>%
  dplyr::select(sex, time, e0) %>% 
  unnest(e0) %>%
  filter(age == min(age)) %>% 
  dplyr::select(sex, time, ex)



orig <- transitions %>%
  dplyr::select(sex, time, edag) %>% 
  unnest(edag) %>%
  group_by(sex, time) %>%
  mutate(age = 17:87) %>% 
  filter(age != 87) %>%
  rename(dag = ed) %>% 
  group_by(sex, time) %>% 
  summarise(dag_orig = sum(dag))

transitions %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    transition %in% c("hd", "ud"),
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, state_dag) |>
  summarize(dag = sum(dag), .groups = "drop") %>%
  dplyr::select(-state_dag) %>% 
  full_join(e0) %>% 
  full_join(orig) %>% 
  mutate(H_mstate = dag       / ex,
         H_classic = dag_orig / ex) %>% 
  dplyr::select(-c(ex, dag, dag_orig)) %>% 
  pivot_longer(-c(sex, time),
               names_to = "type",
               values_to = "val") %>% 
  mutate(sex = str_to_title(sex)) %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female"))) %>%
  ggplot(aes(x = time, y = val, color = type)) + 
  geom_line() +
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ sex) + 
  theme_minimal() + 
  xlab("Year") + 
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title.y = element_blank(),
        legend.text = element_text(color = "black"))






e0 <- transitions %>%
  dplyr::select(sex, time, e0) %>% 
  unnest(e0) %>%
  dplyr::select(sex, time, age, ex) %>% 
  filter(age < 87)



orig <- transitions %>%
  dplyr::select(sex, time, edag) %>% 
  unnest(edag) %>%
  group_by(sex, time) %>%
  mutate(age = 17:87) %>% 
  filter(age != 87) %>%
  rename(dag = ed) %>% 
  group_by(sex, time, age) %>% 
  summarise(dag_orig = sum(dag))



one <- e0 %>% 
  filter(age == min(age)) %>% 
  dplyr::select(-age)


H <- orig %>% 
  group_by(sex, time) %>%
  summarise(dag = sum(dag_orig)) %>% 
  full_join(one) %>% 
  mutate(H = dag / ex) %>%
  ungroup() %>% 
  dplyr::select(sex, time, H)

orig %>%
  full_join(e0) %>% 
  mutate(
    e_dagger_x = dag_orig,
    entropy_x  = e_dagger_x / ex
  ) %>% 
  full_join(H) %>% 
  group_by(sex, time) %>% 
  filter(abs(entropy_x - H) == 
           min(abs(entropy_x - H))) %>%
  ungroup() %>% 
  dplyr::select(sex, time, age)

