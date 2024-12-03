# ----------------------------------------------------------------- #
# source function
source("R/0_functions.R")

# load database
load("Results/transition_probs_model_raw.RData")

# extrapolate age with alr for raw transitions
# ----------------------------------------------------------------- #
zz <- tst %>%
  unnest(qxdata) %>% 
  group_nest(sex, time) %>% # test time, test age 
  mutate(hh_alr = map(data, ~ .x %>% 
                        dplyr::select(age, starts_with("H-")) %>% 
                        dplyr::select("H-NH", "H-D", "H-H") %>% 
                        as.matrix() %>% 
                        alr() %>% 
                        apply(2, extrap_lm)%>% 
                        apply(2, extrap_lm_down)
  )) %>% 
  mutate(nh_alr = map(data, ~ .x %>% 
                        dplyr::select(age, starts_with("NH-")) %>%
                        dplyr::select("NH-H", "NH-D", "NH-NH") %>% 
                        as.matrix() %>% 
                        alr() %>% 
                        apply(2, extrap_lm)%>% 
                        apply(2, extrap_lm_down)
  )) %>% 
  mutate(hh_inverse = map(hh_alr, ~ .x %>% 
                            alrInv()  %>%  
                            as.data.frame() %>% 
                            rename(`H-H` = V3))) %>% 
  mutate(nh_inverse = map(nh_alr, ~ .x %>% 
                            alrInv()  %>%  
                            as.data.frame() %>% 
                            rename(`NH-NH` = V3)))


# plot alr transformed data for NH diagnostic
# ----------------------------------------------------------------- #
zz %>% 
  dplyr::select(sex, time, nh_alr) %>%
  mutate(nh_alr = map(nh_alr, ~ .x %>% 
                        as.data.frame())) %>% 
  unnest(c(nh_alr)) %>%
  group_by(sex, time) %>% 
  mutate(age = 10:110, .after = 2) %>%
  ungroup() %>% 
  pivot_longer(-c(sex:age), 
               names_to = "from_to", 
               values_to = "p") %>%
  ggplot(aes(x = age, y = p, color = from_to)) +
  geom_line() +
  facet_wrap(sex ~ time, labeller = label_wrap_gen(multi_line = TRUE), ncol = 7) + 
  theme_bw()  + 
  geom_vline(aes(xintercept = 87)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold"),
        strip.text.x.top = element_text(face = "bold"))


# plot the inverse data for unhealthy
# ----------------------------------------------------------------- #
zz %>% 
  dplyr::select(sex, time, nh_inverse) %>% 
  unnest(c(nh_inverse)) %>%
  group_by(sex, time) %>% 
  mutate(age = 10:110, .after = 2) %>%
  ungroup() %>% 
  pivot_longer(-c(sex:age), 
               names_to = "from_to", 
               values_to = "p") %>%
  ggplot(aes(x = age, y = p, color = from_to)) +
  geom_line() +
  theme_bw() + 
  facet_wrap(sex ~ time, labeller = label_wrap_gen(multi_line = TRUE), ncol = 7) + 
  theme_bw()  +
  scale_x_continuous(breaks =pretty_breaks())+
  geom_vline(aes(xintercept = 87))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold"),
        strip.text.x.top = element_text(face = "bold"))


# plot inverse data for healty
# ----------------------------------------------------------------- #
zz %>% 
  dplyr::select(sex, time, hh_inverse) %>% 
  unnest(c(hh_inverse)) %>%
  group_by(sex, time) %>% 
  mutate(age = 10:110, .after = 2) %>%
  ungroup() %>% 
  pivot_longer(-c(sex:age), 
               names_to = "from_to", 
               values_to = "p") %>%
  # filter(from_to == "NH-H") %>% 
  ggplot(aes(x = age, y = p, color = from_to)) +
  geom_line() +
  theme_bw() + 
  facet_wrap(sex ~ time, labeller = label_wrap_gen(multi_line = TRUE), ncol = 7) + 
  theme_bw()  +
  scale_x_continuous(breaks =pretty_breaks())+
  geom_vline(aes(xintercept = 87))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold"),
        strip.text.x.top = element_text(face = "bold"))

# save
# ----------------------------------------------------------------- #
pred_data_raw <- zz %>% 
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>% 
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>% 
  mutate(age = 10:110) %>%
  ungroup()

save(pred_data_raw, 
     file = "Results/pred_alr_from_raw_trns.RData")
