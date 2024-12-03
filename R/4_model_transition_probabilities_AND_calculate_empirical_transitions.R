# ----------------------------------------------------------------- #
# source function
source("R/0_functions.R")

# load database
load("Results/all_years_data.RData")

# data for prediction fitting
# ----------------------------------------------------------------- #
new_data <- expand_grid(age  = 17:86,
                        from = c("H", "NH"),
                        time = 2005:2019,   # time measure
                        sex  = c("male", "female"))

# Calculate transition probabilities
# ----------------------------------------------------------------- #
  # the model itself


tst <- x %>%
  bind_rows() %>%
  dplyr::select(-w) %>% 
  distinct() %>% # remove duplicates from adjacent years // all are from H to H.
  dplyr::select(-c(m_y_birth), time = y) %>%
  # new weight
  count(time, sex, age, from, to) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%
  group_nest(sex, from) %>%
  # make sure that the denominator is always H or NH accordingly
  # NOTE: This is not necessary. It will not make any difference
  # But lets keep it, why not.
  mutate(data = ifelse(from == "H", map(data, ~ .x %>%
                                          mutate(to = factor(to, 
                                                             levels = c("H", "NH", "D"))
                                          )),
                       map(data, ~ .x %>%
                             mutate(to = factor(to, levels = c("NH", "H", "D"))))
  )) %>% 
  # the model itself
  mutate(model =  map(data, ~ vgam(
    to ~ s(age,  df = 1) + 
         s(time, df = 1),
    weights = n,
    data = .x,
    family = multinomial
  ))) %>%
  # predicted data. all this is to simply fit the new_data for transiton probabilities
  nest_join(new_data, by = c("sex", "from")) %>%
  mutate(predicted_data = map2(.x = model, .y = new_data, ~ predict(.x, .y, type = "respons"))) %>%
  mutate(finale = map2(.x = new_data, .y = predicted_data, ~ .x %>%
                         bind_cols(.y))) %>%
  dplyr::select(sex, from, finale) %>% 
  unnest(finale) %>% 
  group_nest(sex) %>%
  mutate(qxdata = map(
    data,
    ~ .x %>%
      pivot_longer(c(H, NH, D),
                   names_to  = "var",
                   values_to = "val") %>%
      unite("trans", c(from, var), sep = "-") %>%
      pivot_wider(names_from  = trans,
                  values_from = val))) %>% 
  dplyr::select(sex, qxdata)

# save transition probabiilities
save(tst, file = "Results/transition_probs_model_raw.RData")

# calculate empirical transition probabilities
# ----------------------------------------------------------------- #
empiric <- x %>%
  bind_rows() %>%
  dplyr::select(-w) %>% 
  distinct() %>% # remove duplicates from adjacent years // all are from H to H.
  dplyr::select(-c(m_y_birth), time = y) %>%
  # new weight
  count(time, sex, age, from, to) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%
  group_by(sex, time, age, from) %>% 
  reframe(to = to,
          prob_emp = n / sum(n)) %>% 
  ungroup()

save(empiric, file = "Results/transition_probs_emp.RData")
# check empirical vs fitted graph
# ----------------------------------------------------------------- #
tst %>% 
  unnest(qxdata) %>% 
  pivot_longer(-c(sex:time),
               names_to = "trans",
               values_to = "prob") %>%
  separate(trans, into = c("from", "to"), sep = "-") %>%
  full_join(empiric) %>%
  # remove empirical values that are == 1, for better visualization
  # I think it is arefact of the data rather than really important values
  mutate(prob_emp = ifelse(prob_emp == 1, NA, prob_emp)) %>%
  filter(time == 2018) %>% # change years here. 
  ggplot() +
  geom_line(aes(x = age, y = prob, group = to, color = to), linewidth = 1) + 
  geom_line(aes(x = age, y = prob_emp, color = to)) +
  facet_grid(from ~ sex, switch = "y") +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = seq(15, 85, 5)) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))
