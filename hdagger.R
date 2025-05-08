# packages
library(tidyverse)
library(collapse)
library(tidyfast)

# functions from Tim
source("functions_e_dagger.R")

# here are the transition probabilities 
# self reported health for 2013, single year, HMD adjustment
load("test_data_for_edagger.RData")


# filter only one year and one sex
test <- final %>% 
  filter(time == 2013, sex == "female") %>% 
  unite(state, c("from", "to"), sep = "") %>%
  pivot_wider(names_from  = state,
              values_from = prob) %>% 
  dplyr::select(-c(1, 2))

# all good with sum constraints
test %>% 
  dplyr::select(-1) %>% 
  rowSums()

# calculate initial constant
initial <- test %>% 
  slice(1) %>%
  init_constant()

# calculate lh and lu
lh <- test %>%
  p_tibble2lxs(init = initial, state = "H") %>%
  rename(lh = lxs)

lu <- test %>% 
  p_tibble2lxs(init = initial, state = "U") %>%
  rename(lu = lxs)

# calculate the LE conditional on from state
LEi <- test %>% 
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
    filter(age_to > age_from) |> 
    group_by(state_from, age_from, state_to) |>
  # !!!!!
    summarize(LEi = sum(lxs), .groups = "drop") |>
    mutate(expectancy = paste0(state_to, "LE", tolower(state_from))) |>
    select(!starts_with("state")) |>
    pivot_wider(names_from  = expectancy, values_from = LEi) |>
    rename(age = age_from)
lu %>% 
  full_join(lh) %>%
  full_join(LEi) %>%
  as_tibble()
# join and calculate daggers and lifetable for classic dagger
daggers <- lu %>% 
  full_join(lh) %>%
  full_join(LEi) %>%
  as_tibble() %>% 
  full_join(test) %>% 
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
         ) %>% 
  mutate(lx = lh + lu,
         dx = lx - lead(lx),
         ex = rev(cumsum(rev(lx))) / lx,
         ex2 = (HLEh + ULEh) * lh/lx + (HLEu + ULEu) * lu/lx,
         # these two should be euqal but are not
         edex  = dx * ex,
         edex2 = (lu * UD + lh * HD) * ex2,
         other = lu * UD * (HLEu + ULEu) + lh * HD * (HLEh + ULEh))
View(daggers)
plot(daggers$ex)
lines(daggers$ex2)

# very similar but are NOT equal
daggers |> select(age,ex,ex2) |> head()
  
# these two are the same and should be the same
# HLE
(daggers$HLEh  * (daggers$lh / daggers$lx)) + 
(daggers$HLEu  * (daggers$lu / daggers$lx)) + 

# PLUS + ULE
(daggers$ULEh  * (daggers$lh / daggers$lx)) + 
(daggers$ULEu  * (daggers$lu / daggers$lx))

# is same as
daggers$ex


# this is classic edagger

sum(daggers$edex, na.rm = T)

# this is edagger calculated from your yesterday formula
sum(daggers$other, na.rm = T)

# this is the h_dagger and it is just a smidge larger than classic edagger
# BUT is exactly equal to the e-dagger from you yesterday formula
sum(daggers$HLEdag_hd, na.rm = T) +
sum(daggers$HLEdag_ud, na.rm = T) +
sum(daggers$ULEdag_hd, na.rm = T) +
sum(daggers$ULEdag_ud, na.rm = T)


