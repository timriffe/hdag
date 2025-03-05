# ----------------------------------------------------------------- #
# source function
source("R/0_functions.R")

# csv files
setwd("Data/unzipped/DATA_RAW/CSV")

# columns to select from 2 file types R and P
name_vec <- c("RB010", "RB060", "RB062", "RB063", "RB064", "RB030", "RB070",
              "RB080", "RB090", "RB110", "RB140", "RB150",
              "PB010", "PB030", "PB050", "PB110", "PB100", "PB130", "PB140",
              "PB150", "PH010", "PH020", "PH030")

# read data
# ----------------------------------------------------------------- #
# the last 2 years do not have the month of death or transition for some reason.
# This age cannot be calculated properly. Can make rough approximations.
# I excluded them for now
pt1 <- read_data(x = "^es.*r.csv$", y = name_vec)
pt2 <- read_data(x = "^es.*p.csv$", y = name_vec)

# no info on death dates for last two archives.
# moreover the data looks incomplete
# I would disregard them for now
pt1 <- pt1[rev(rev(names(pt1))[-c(1:2)])]
pt2 <- pt2[rev(rev(names(pt2))[-c(1:2)])]

# here should be the minimal year in the final file. currently 2017
# ----------------------------------------------------------------- #
years <- 2004:2017
x     <- vector(mode = "list", length = length(years)) %>%
  set_names(str_c('y',  2004:2017))

for(i in 1:length(years)) {
  
  x[[i]] <- create_data_from_the_chosen_cohort(a = years[i])
  
 }

# save data in Results folder
# ----------------------------------------------------------------- #
if(!dir.exists("../../../../Results")) { 
  
  dir.create("../../../../Results")
  
}

save(x, file = "../../../../Results/all_years_data.RData")