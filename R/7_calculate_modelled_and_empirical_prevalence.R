# ----------------------------------------------------------------- #
# source function
source("R/0_functions.R")

# load database
load("Results/all_years_data.RData")
load("Results/full_lifetable_from_pred_trans_prob.RData")
# predict data
# ----------------------------------------------------------------- #
new_data <- expand_grid(age   = 10:110,
                        time  = 2005:2019,
                        sex   = c("male", "female"))

# modeled prevalence data
md_prev <- z %>% 
  dplyr::select(sex, time, age, lu, lx) %>%
  mutate(md_prv = lu / lx)

# looks good
md_prev %>% 
ggplot(aes(x = age, y = md_prv, color = as.factor(time))) +
  geom_line() + 
  facet_wrap(~ sex) + 
  theme_bw()

# model prevalence
# ----------------------------------------------------------------- #
prev <- x %>%
  bind_rows() %>% 
  dplyr::select(-w) %>% 
  distinct() %>% 
  dplyr::select(-c(m_y_birth), time = y) %>%
  # new weight
  count(sex, time, age, from) %>%
  group_by(sex, time, age) %>% #from 
  summarise(N = sum(n[from == "NH"]), # empirical prevalence
            n = sum(n), .groups = "drop") %>%
  mutate(prev = N / n) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%
  group_nest(sex) %>%
  # model the prevalence rate with binomial logit
  mutate(model =  map(data, ~ glm(
    prev ~ time + age,
    weights = n,
    family  = binomial(link = "logit"),
    data    = .x
  ))) %>% 
  ungroup() %>%
  # predict
  nest_join(new_data, by = "sex") %>% 
  mutate(predicted_data = map2(.x = model, .y = new_data, ~ predict(.x, .y, type = "response"))) %>%
  mutate(finale = map2(.x = new_data, .y = predicted_data, ~ .x %>%
                         bind_cols(.y) %>% 
                         set_names(c(names(.)[c(1:2)], "case"))))

# model prevalence
# ----------------------------------------------------------------- #
mod_prev <- prev %>%
  dplyr::select(sex, finale) %>% 
  unnest(finale) %>%
  rename(mod_prev = case)

# diagnostic plot
# ----------------------------------------------------------------- #
mod_prev %>%
  mutate(time = factor(time)) %>% 
  ggplot(aes(x = age, y = mod_prev, color = time)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~ sex) + 
  theme(strip.background = element_blank(),
        legend.position = "bottom")

emp_prev <- x %>% 
  bind_rows() %>% 
  dplyr::select(-w) %>% 
  distinct() %>% 
  dplyr::select(-c(m_y_birth), time = y) %>%
  # new weight
  count(sex, time, age, from) %>%
  group_by(sex, time, age) %>% #from 
  summarise(N = sum(n[from == "NH"]), # empirical prevalence
            n = sum(n)) %>%
  ungroup() %>% 
  mutate(prev = N / n) %>%
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%
  dplyr::select(sex, time, age, emp_prev = prev)

# empirical vs fitted diagnostic
# slightly off in some years, but all in all good fit
# ----------------------------------------------------------------- #
prevalence <- mod_prev %>% 
  full_join(emp_prev) %>%
  full_join(md_prev) %>% 
  dplyr::select(-c(lu, lx))

prevalence %>%
  mutate(time = as.factor(time)) %>%
  mutate(emp_prev = ifelse(emp_prev == 0, NA, emp_prev)) %>% 
  ggplot() +
  # this one is lu / lx after alr
  geom_line(aes(x = age,  y = md_prv, color = sex)) +
  # this one is fit predict glm after alr
  geom_line(aes(x = age,  y = mod_prev, color = sex), lty = 2) +
  # this one is simple ratio
  geom_point(aes(x = age, y = emp_prev, color = sex), size = 0.2) +
  facet_wrap(~ time, ncol = 3) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"),
        strip.background = element_blank())

# save prevalence
# ----------------------------------------------------------------- #
save(prevalence, file = "Results/prevalence.RData")
