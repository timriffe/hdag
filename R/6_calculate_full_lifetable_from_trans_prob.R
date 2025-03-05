# ----------------------------------------------------------------- #
# source function
source("R/0_functions.R")
load("Results/pred_alr_from_raw_trns.RData")

pred_data_raw <- pred_data_raw %>%
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
      rename(lh = lxs)
  )) %>%
  mutate(lu = map2(
    .x = data,
    .y = initials,
    ~ .x %>%
      p_tibble2lxs(init = .y, state = "U") %>%
      rename(lu = lxs)
  ))

z <- pred_data_raw %>% 
  dplyr::select(sex, time, data, lh, lu) %>%
  mutate(lh = map(lh, ~ .x %>% 
                 dplyr::select(-age) %>% 
                   slice(-nrow(.)))) %>% 
  mutate(lu = map(lu, ~ .x %>% 
                    dplyr::select(-age)%>% 
                    slice(-nrow(.)))) %>% 
  unnest(c(data, lh, lu)) %>% 
  group_by(sex, time) %>% 
  mutate(lx = lu + lh,
         du = lu * `UD`,
         dh = lh * `HD`,
         dx = du + dh,
         qx = dx / lx,
         px = 1 - qx
  ) %>%
  mutate(Lh = (lh + lead(lh)) / 2,
         Lh = ifelse(is.na(Lh), (lh + (lh * `HH` + lu * `UH`)) / 2, Lh),
         Lu = (lu + lead(lu)) / 2,
         Lu = ifelse(is.na(Lu), (lu + (lu * `UU` + lh * `HH`)) / 2, Lu),
         Lx = Lh + Lu
  ) %>%
  mutate(mh = dh / Lh,
         mu = du / Lu,
         mx = dx / Lx) %>%
  mutate(Ra = mu / mh) %>% 
  mutate(hle = rev(cumsum(rev(lh))),
         ule = rev(cumsum(rev(lu))),
         ex = hle + ule) %>% 
  ungroup()
  
save(z, file = "Results/full_lifetable_from_pred_trans_prob.RData")         
# diagnostic plot
# what really different is the mux across years
# ----------------------------------------------------------------- #
z %>%
  group_by(sex, time) %>% 
  mutate(log_mx = log(mx),
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx / lx,
         log_mux = log(mu),
         log_mhx = log(mh)) %>% 
  ungroup() %>% 
  dplyr::select(sex, time, age, lx, dx, qx, px, Lx, 
                log_mx, Tx, ex, log_mux, log_mhx) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "measure",
               values_to = "val") %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  facet_wrap(sex ~ measure, scales = "free_y") + 
  theme_light() + 
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(strip.placement = "outside",
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))

# diagnostic plot of ex hle and uhle
# ----------------------------------------------------------------- #
z %>%
  dplyr::select(sex, time, age, hle, ule, ex) %>% 
  pivot_longer(-c(sex, time, age),
               names_to = "measure",
               values_to = "val") %>%
  mutate(time = as.factor(time)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = val, color = time)) +
  theme_light() +
  facet_wrap(sex ~ measure)+
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold")
)

# ----------------------------------------------------------------- #
# save Ra
Ra <- z %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

save(Ra, file = "Results/Ra.RData")
# ----------------------------------------------------------------- #
# diagnostic plot looks ok
Ra %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line()+
  facet_wrap( ~ sex) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))
