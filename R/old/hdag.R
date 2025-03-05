library(tidyverse)
source("https://raw.githubusercontent.com/timriffe/ms_dist/master/code/00_setup.R")
source("https://raw.githubusercontent.com/timriffe/ms_dist/master/code/01_functions.R")
adl <- read_csv("Data/hrs_adl_iadl_all.csv") |>
  filter(measure == "ADL",
         sex == "f") %>%
  select(-measure, -sex)

init <- init_constant(adl[1,])

lh <- adl |>  
  select(-measure, -sex) |>
  p_tibble2lxs(init  = init, 
               state = "H") |> 
  rename(lh = lxs)

lu <- adl |>  
  select(-measure, -sex) |> 
  p_tibble2lxs(init = init, state = "U") |> 
  rename(lu = lxs)

LEi <- 
  adl |> 
  select(-measure, - sex) |> 
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
  rename(age = age_from)

dags <- adl |>   
  select(-HH, -UU) |>
  left_join(lh,by=join_by(age)) |> 
  left_join(lu,by=join_by(age)) |> 
  left_join(LEi, by = join_by(age)) |> 
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
               names_to = "dag_component", 
               values_to = "dag") |> 
  separate_wider_delim(dag_component, 
                       delim = "_", 
                       names = c("state_dag", "transition"))

                                                                         
dags |> 
  mutate(state_dag = factor(state_dag, 
                            levels = c("HLEdag","ULEdag","LEdag"))) |>
  ggplot(aes(x = age, y = dag, color = transition)) +
  geom_line() +
  facet_wrap(~state_dag) +
  theme_minimal()

dags |> 
  group_by(state_dag, transition) |> 
  summarise(dag = sum(dag)) |> 
  mutate(state_dag = factor(state_dag, 
         levels = c("HLEdag","ULEdag","LEdag"))) |>
  ggplot(aes(x=state_dag, fill = transition, y = dag)) +
  geom_col() +
  theme_minimal()





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

sum(test$LEdag)


a <- dags |> 
  filter(transition %in% c("hd","ud"),
         state_dag == "LEdag") |> 
  group_by(age) %>% 
  summarize(a = sum(dag)) %>% 
  pull(a)


# RT: NOTE
# Here is what we have for e-dagger
# look at the plot. see?
# there is one additional point on the right
# this is die to the p_tibble2lxs where we introduce an additional age
# in some examples it might be actually very big
# like in my with SILC data with OAGE
plot((dx * ex))
lines(a, col = "red")

# now lets remove it
# in this case this value is very small.
# so still big difference.
# maybe there is something else?
# I will check the code carefully
sum((dx * ex)[-62])
sum(a)


# classic ax
lt <- tibble(age = 50:111,
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
