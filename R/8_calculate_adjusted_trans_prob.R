# ----------------------------------------------------------------- #
# source function
source("R/0_functions.R")

# load database
load("Results/Ra.RData")
load("Results/prevalence.RData")
load("Results/pred_alr_from_raw_trns.RData")

prev_test <- prevalence %>%
  dplyr::select(-c(mod_prev, emp_prev)) %>% 
  rename(case = md_prv)

# read and filter hmd female data for males and females for Spain
# ----------------------------------------------------------------- #
hmd_f <- read_table("Data/fltper_1x1.txt", skip = 1) %>%
  mutate(mx_hmd = mx) %>% 
  dplyr::select(time = Year, age = Age, mx_hmd) %>%
  mutate(age = parse_number(age)) %>%
  mutate(sex = "female") %>% 
  filter(time %in% unique(prev_test$time),
         age %in% unique(prev_test$age)) 

hmd_m <- read_table("Data/mltper_1x1.txt", skip = 1) %>%
  mutate(mx_hmd = mx) %>% 
  dplyr::select(time = Year, age = Age, mx_hmd) %>%
  mutate(age = parse_number(age)) %>%
  mutate(sex = "male") %>% 
  filter(time %in% unique(prev_test$time),
         age %in% unique(prev_test$age)) 

# overall hmd
hmd <- hmd_f %>%
  full_join(hmd_m)

# recalculate the mortality rates using Tim PAA abstract formula 5 and 4
# ----------------------------------------------------------------- #
new_mx <- Ra %>%
  full_join(prev_test) %>%
  full_join(hmd) %>%
  mutate(mh_new = mx_hmd / (1 - case + case * Ra), # formula 5
         mu_new = mh_new * Ra) %>% # formula 4
  dplyr::select(-c(`HD`, `UD`)) %>%
  # calculate mortality from probabilities
  # qx from mx q(x) = 1 - exp(-mx)
  mutate(HD = 1 - exp(-mh_new),
         UD = 1 - exp(-mu_new)) %>%
  dplyr::select(sex, time, age, `HD`, `UD`)

# diagnostic plot
# new mortality is always lower than old mortality
# unhealty mortality is very high always
# ----------------------------------------------------------------- #
Ra %>% 
  full_join(prev_test) %>% 
  full_join(hmd) %>%
  mutate(mh_new = mx_hmd / (1 - case + case * Ra), # formula 5
         mu_new = mh_new * Ra) %>% # formula 4
  dplyr::select(sex, time, age, 
                mh_old = mh, 
                mu_old = mu, 
                mh_new, 
                mu_new) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "variable",
               values_to = "val") %>%
  separate(variable, c("indicator", "old_new")) %>%
  mutate(time = as.factor(time)) %>% 
  filter(sex == "male") %>% 
  ggplot(aes(x = age, y = val, group = interaction(old_new, indicator), color = indicator, linetype = old_new)) + 
  geom_line() + 
  scale_y_log10() +
  facet_wrap( ~ time, strip.position = "left", ncol = 3) + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside")

# recalculate the transition probabilities with new mx values
# ----------------------------------------------------------------- #
old_trns <- pred_data_raw %>% 
  dplyr::select(-c(`H-D`, `NH-D`))

new_trans <- old_trns %>% 
  full_join(new_mx) %>% 
  rename(`H-D`  = HD,
         `NH-D` = UD) %>% 
  pivot_longer(-c(sex, age, time),
               names_to  = "var",
               values_to = "val") %>% 
  separate(var, c("from", "to"), sep = "-") %>% 
  group_by(sex, time, age, from) %>% 
  mutate(val = recalculate(val)) %>% 
  mutate(type = "Adjusted") %>% 
  ungroup()

# old transition for plot
old_plot <-  pred_data_raw %>% 
  pivot_longer(-c(sex, age, time),
               names_to  = "var",
               values_to = "val") %>% 
  separate(var, c("from", "to"), sep = "-") %>%
  mutate(type = "Raw") %>% 
  ungroup()

full_trns <- new_trans %>% 
  full_join(old_plot)

# diagnostic plot of old and new transitions
# transitions to death are higher now
# we have this jump in the last year. 
# ----------------------------------------------------------------- #
full_trns %>%
  filter(time == 2018) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = val, color = to, lty = type), linewidth = 1) +
  facet_wrap(from ~ sex) + 
  theme_light() + 
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))

# save
save(full_trns, file = "Results/adjusted_and_raw_trans_prob.RData")
