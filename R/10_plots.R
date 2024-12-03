source("R/0_functions.R")
load("Results/adjusted_and_raw_trans_prob.RData")
load("Results/h_dag.RData")

trans <- full_trns %>%
  filter(time == 2014) %>% 
  mutate(type = ifelse(type == "Raw", "Unadjusted", type)) %>% 
  rename(Type = type,
         Age = age,
         To = to) %>%
  mutate(sex = str_to_title(sex),
         sex = factor(sex, levels = c("Male", "Female"))) %>% 
  mutate(from = ifelse(from == "H", "Healthy", "Unhealthy"),
         To = case_when(
           To == "H" ~ "Healthy",
           To == "NH" ~ "Unhealthy",
           To == "D" ~ "Dead"
         )) %>%
  mutate(To = factor(To, levels = c("Healthy", "Unhealthy",
                                    "Dead"))) %>% 
  ggplot() + 
  geom_line(aes(x = Age, 
                y = val, 
                color = To, 
                lty = Type), 
            linewidth = 0.8) +
  facet_grid(sex ~ from, switch = "y") + 
  theme_bw() + 
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text = element_text(color = "black"),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))

ggsave("Plots/Plot1.pdf", 
       plot = trans, 
       device = "pdf",
       scale = 2)

# ----------------------------------------------------- #

# pred_data_raw$LEi[[1]] %>% 
#   names()
# 
# 
# pred_data_raw$data[[1]] %>%
#   p_tibble2N() |>
#   as.data.frame() |>
#   rownames_to_column("state_age_to") |>
#   pivot_longer(-state_age_to, names_to  = "state_age_from", values_to = "lxs") |>
#   separate_wider_delim(
#     cols  = state_age_from,
#     delim = "_",
#     names = c("state_from", "age_from")
#   ) |>
#   separate_wider_delim(
#     cols  = state_age_to,
#     delim = "_",
#     names = c("state_to", "age_to")
#   ) |>
#   mutate(age_from = as.integer(age_from),
#          age_to   = as.integer(age_to)) |>
#   filter(age_from < 111,
#          age_to   < 111) %>% 
#   group_by(state_from, age_from, state_to) |>
#   summarize(LEi = sum(lxs), .groups = "drop") |>
#   mutate(expectancy = paste0(state_to, "LE", tolower(state_from))) |>
#   select(!starts_with("state")) |>
#   pivot_wider(names_from  = expectancy, values_from = LEi) |>
#   rename(age = age_from)
# )



dags_by_age_and_trans <- pred_data_raw %>%
  dplyr::select(sex, time, dags) %>%
  unnest(dags) %>%
  filter(time == 2014) %>%
  rename(Age = age, Transition = transition) %>%
  mutate(sex = str_to_title(sex), sex = factor(sex, levels = c("Male", "Female"))) %>%
  mutate(
    state_dag = str_remove(state_dag, "dag$"),
    state_dag = factor(state_dag, levels = c("HLE", "ULE", "LE"))
  ) |>
  mutate(
    Transition = case_when(
      Transition == "hd" ~ "HD",
      Transition == "hu" ~ "HU",
      Transition == "ud" ~ "UD",
      Transition == "uh" ~ "UH"
    ),
    Transition = factor(Transition, levels = c("HU", "HD", "UH", "UD"))
  ) %>%
  ggplot(aes(x = Age, y = dag, color = Transition), 
         linewidth = 0.8) +
  geom_line() +
  facet_grid(sex ~ state_dag, switch = "y") +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(color = "black", face = "bold"),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text = element_text(color = "black"),
    legend.title = element_text(color = "black", face = "bold"),
    legend.text = element_text(color = "black", face = "bold")
  )

ggsave("Plots/Plot2.pdf", 
       plot = dags_by_age_and_trans, 
       device = "pdf",
       scale = 2)

# ----------------------------------------------------- #
overall <- pred_data_raw %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>% 
  filter(time == 2014) %>%
  rename(Age = age, Transition = transition) %>%
  mutate(sex = str_to_title(sex), sex = factor(sex, levels = c("Male", "Female"))) %>%
  mutate(
    state_dag = str_remove(state_dag, "dag$"),
    state_dag = factor(state_dag, levels = c("HLE", "ULE", "LE"))
  ) |>
  mutate(
    Transition = case_when(
      Transition == "hd" ~ "HD",
      Transition == "hu" ~ "HU",
      Transition == "ud" ~ "UD",
      Transition == "uh" ~ "UH"
    ),
    Transition = factor(Transition, levels = c("HU", "HD", "UH", "UD"))
  ) %>% 
  group_by(sex, time, Age, state_dag) %>% 
  summarise(dag = sum(dag), .groups = "drop") %>% 
  mutate(state_dag = factor(state_dag, 
                            levels = c("HLE", "ULE", "LE"))) |>
  ggplot(aes(x = Age, y = dag, color = state_dag),
         linewidth = 0.8) +
  geom_line() +
  facet_grid(~ sex, switch = "y") +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(color = "black", face = "bold"),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text = element_text(color = "black"),
    legend.title = element_text(color = "black", face = "bold"),
    legend.text = element_text(color = "black", face = "bold")
  )

ggsave("Plots/Plot3_1.pdf", 
       plot = overall, 
       device = "pdf",
       scale = 2)

# ----------------------------------------------------- #

mstate <- pred_data_raw %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    transition %in% c("hd", "ud"),
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, state_dag) |> 
  summarize(dag = sum(dag), .groups = "drop") %>% 
  mutate(type = "mstate")

mstate_all <- pred_data_raw %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, state_dag) |> 
  summarize(dag = sum(dag), .groups = "drop") %>% 
  mutate(type = "mstate_all")


orig <- pred_data_raw %>%
  dplyr::select(sex, time, edag) %>% 
  unnest(edag) %>%
  mutate(type = "edag") %>% 
  rename(dag = ed)

x <- mstate %>% 
  full_join(orig) %>% 
  full_join(mstate_all) %>% 
  mutate(sex = str_to_title(sex),
         type = str_to_title(type)) %>%
  mutate(dag = ifelse(type == "Mstate", dag, dag)) %>%
  ggplot(aes(x = time, y = dag, color = type)) + 
  geom_line() +
  geom_point()+
  facet_wrap(~ sex) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.title = element_blank())


x1 <- mstate %>% 
  full_join(orig) %>% 
  mutate(sex = str_to_title(sex),
         type = str_to_title(type)) %>%
  mutate(dag = ifelse(type == "Mstate", dag, dag)) %>%
  ggplot(aes(x = time, y = dag, color = type)) + 
  geom_line() +
  geom_point()+
  facet_wrap(~ sex) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.title = element_blank())

e_dag <- mstate %>% 
  full_join(orig) %>% 
  full_join(mstate_all) %>% 
  mutate(sex = str_to_title(sex),
         type = str_to_title(type)) %>%
  mutate(sex = factor(sex, levels = c("Male", "Female"))) %>%
  mutate(type = case_when(
    type == "Edag"         ~ "Classic",
    type == "Mstate"       ~ "Mstate d",
    type == "Mstate_all"   ~ "Mstate all"
  )) %>% 
  ggplot(aes(x = time, y = dag, color = type)) + 
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ sex) + 
  theme_bw() + 
  xlab("Age") + 
  ylab(bquote("e-dagger")) + 
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))

ggsave("Plots/Plot4.pdf", 
       plot = e_dag, 
       device = "pdf",
       scale = 2)

# ----------------------------------------------------- #
mstate <- pred_data_raw %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    transition %in% c("hd", "ud"),
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, state_dag, age) |> 
  summarize(dag = sum(dag), .groups = "drop") %>% 
  mutate(type = "mstate") %>% 
  filter(time == 2014)

mstate_all <- pred_data_raw %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>%
  filter(
    state_dag == "LEdag"
  ) %>%
  group_by(sex, time, state_dag, age) |> 
  summarize(dag = sum(dag), .groups = "drop") %>% 
  mutate(type = "mstate_all")%>% 
  filter(time == 2014)

orig <- pred_data_raw %>%
  mutate(edag1 = map2(.x = lh,
                     .y = lu, ~ .x %>% 
                       left_join(.y, by = join_by(age)) %>% 
                       mutate(lx = lh + lu))) %>% 
  mutate(e0 = map2(.x = edag1,
                   .y = data, ~ .x %>%
                     left_join(.y, by = join_by(age)) %>%
                     mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                     mutate(dx = lh * HD + lu * UD) %>%
                     mutate(ex = rev(cumsum(rev(lx))) / lx))
  ) %>%
  mutate(edag1 = map2(.x = edag1,
                     .y = data, ~ .x %>% 
                       left_join(.y, by = join_by(age)) %>% 
                       mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                       mutate(dx = lh * HD + lu * UD) %>% 
                       mutate(ex = rev(cumsum(rev(lx))) / lx) %>% 
                       reframe(ed = dx * ex)
  )) %>% 
  dplyr::select(sex, time, edag1) %>% 
  unnest(edag1) %>%
  mutate(type = "edag") %>% 
  rename(dag = ed) %>%
  filter(time == 2014) %>% 
  group_by(sex) %>%
  mutate(age = 10:110)



edagage <- 
mstate %>% 
  full_join(orig) %>% 
  full_join(mstate_all) %>% 
  mutate(sex = str_to_title(sex),
         type = str_to_title(type)) %>%
  mutate(sex = factor(sex, levels = c("Male", "Female"))) %>%
  mutate(type = case_when(
    type == "Edag"         ~ "Classic",
    type == "Mstate"       ~ "Mstate d",
    type == "Mstate_all"   ~ "Mstate all"
  )) %>% 
  ggplot(aes(x = age, y = dag, color = type)) + 
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ sex) + 
  theme_bw() + 
  xlab("Age") + 
  ylab(bquote("e-dagger")) + 
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))


ggsave("Plots/Plot5.pdf", 
       plot = edagage, 
       device = "pdf",
       scale = 2)
# ----------------------------------------------------- #
e0 <- pred_data_raw %>%
  mutate(edag1 = map2(.x = lh,
                      .y = lu, ~ .x %>% 
                        left_join(.y, by = join_by(age)) %>% 
                        mutate(lx = lh + lu))) %>% 
  mutate(e0 = map2(.x = edag1,
                   .y = data, ~ .x %>%
                     left_join(.y, by = join_by(age)) %>%
                     mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                     mutate(dx = lh * HD + lu * UD) %>%
                     mutate(ex = rev(cumsum(rev(lx))) / lx))
  ) %>%
  mutate(edag1 = map2(.x = edag1,
                      .y = data, ~ .x %>% 
                        left_join(.y, by = join_by(age)) %>% 
                        mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                        mutate(dx = lh * HD + lu * UD) %>% 
                        mutate(ex = rev(cumsum(rev(lx))) / lx) %>% 
                        reframe(ed = dx * ex)
  )) %>%
  dplyr::select(sex, time, e0) %>% 
  unnest(e0) %>%
  filter(age == min(age)) %>% 
  dplyr::select(sex, time, ex)


orig <- pred_data_raw %>%
  mutate(edag1 = map2(.x = lh,
                      .y = lu, ~ .x %>% 
                        left_join(.y, by = join_by(age)) %>% 
                        mutate(lx = lh + lu))) %>% 
  mutate(e0 = map2(.x = edag1,
                   .y = data, ~ .x %>%
                     left_join(.y, by = join_by(age)) %>%
                     mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                     mutate(dx = lh * HD + lu * UD) %>%
                     mutate(ex = rev(cumsum(rev(lx))) / lx))
  ) %>%
  mutate(edag1 = map2(.x = edag1,
                      .y = data, ~ .x %>% 
                        left_join(.y, by = join_by(age)) %>% 
                        mutate(across(c(HD, UD), ~ ifelse(is.na(.), 1, .))) %>% ##
                        mutate(dx = lh * HD + lu * UD) %>% 
                        mutate(ex = rev(cumsum(rev(lx))) / lx) %>% 
                        reframe(ed = dx * ex)
  )) %>%
  dplyr::select(sex, time, edag1) %>% 
  unnest(edag1) %>%
  group_by(sex, time) %>%
  mutate(age = 10:110) %>%
  rename(dag = ed) %>% 
  group_by(sex, time) %>% 
  summarise(dag_orig = sum(dag), .groups = "drop")


H <- pred_data_raw %>% 
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

ggsave("Plots/Plot6.pdf", 
       plot = H, 
       device = "pdf",
       scale = 2)
# ----------------------------------------------------- #



library(ggpubr)
ggarrange(x, x1, labels = c("all transitions", "only hd, ud"),
          common.legend = T) + 
  theme(legend.position = "bottom")



bars <- pred_data_raw %>% 
  dplyr::select(sex, time, dags) %>% 
  unnest(dags) %>% 
  filter(time == 2014) |> 
  group_by(state_dag, transition, sex) |> 
  summarise(dag = sum(dag)) |>
  rename(Transition = transition) %>%
  mutate(sex = str_to_title(sex), sex = factor(sex, levels = c("Male", "Female"))) %>%
  mutate(
    state_dag = str_remove(state_dag, "dag$"),
    state_dag = factor(state_dag, levels = c("HLE", "ULE", "LE"))
  ) %>% 
  mutate(state_dag = factor(state_dag, 
                            levels = c("HLE", "ULE", "LE"))) |>
  mutate(
    Transition = case_when(
      Transition == "hd" ~ "HD",
      Transition == "hu" ~ "HU",
      Transition == "ud" ~ "UD",
      Transition == "uh" ~ "UH"
    ),
    Transition = factor(Transition, levels = c("HU", "HD", "UH", "UD"))
  ) %>% 
  ggplot(aes(x = state_dag, 
             fill = Transition, y = dag)) +
  geom_col() +
  facet_grid( ~ sex, switch = "y") +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(color = "black", face = "bold"),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text.y = element_text(color = "black", face = "bold"),
    axis.text = element_text(color = "black"),
    legend.title = element_text(color = "black", face = "bold"),
    legend.text = element_text(color = "black", face = "bold")
  )+
  coord_flip() + 
  ylab("Contribution in years")

ggsave("Plots/Plot3_2.pdf", 
       plot = bars, 
       device = "pdf",
       scale = 2)

# ----------------------------------------------------- #



# Here's a cheap lifetable edagger, low quality
n  = nrow(adl)
lx = lh$lh + lu$lu
wh = lh$lh / (lh$lh + lu$lu)
wh = wh[-length(wh)]
wu = lu$lu / (lh$lh + lu$lu)
wu = wu[-length(wu)]
dx = lh$lh * c(adl$HD, 1) + lu$lu * c(adl$UD, 1)
ex = rev(cumsum(rev(lx))) / lx
sum(ex[-length(ex)] * dx[-length(dx)])


test <- dags |> 
  group_by(state_dag, age) |> 
  summarize(dag = sum(dag)) %>% 
  filter(state_dag != "LEdag") %>%
  pivot_wider(names_from  = state_dag,
              values_from = dag) %>% 
  mutate(wh = wh,
         wu = wu) %>% 
  mutate(LEdag = wh * HLEdag + wu * ULEdag)


a <- dags |> 
  filter(transition %in% c("hd","ud"),
         state_dag == "LEdag") |> 
  group_by(age) %>% 
  summarize(a = sum(dag)) %>% 
  pull(a)


plot((dx * ex))
lines(a, col = "red")

sum((dx * ex)[-length(dx)])
sum(a)

lt <- tibble(age = 10:111,
             lx,
             dx,
             ex)

e_dagger <- sum(dx * ex / lx)
H        <- e_dagger / ex[1]

lt <- lt %>%
  mutate(
    e_dagger_x = cumsum(dx * ex) / lx,
    entropy_x  = e_dagger_x / ex
  )

# ax - age 81
lt %>%
  filter(abs(entropy_x - H) == 
           min(abs(entropy_x - H)))
