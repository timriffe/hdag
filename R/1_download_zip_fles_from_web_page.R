# create folders to store the data

if(!dir.exists("Data")) { 
  
  dir.create("Data")
  
  }

if(!dir.exists("Data/RAW")) { 
  
dir.create("Data/RAW")
  
 }

# base 2013 files
year_from    <- 2008:2019 # subject to change when new years are added
year_to      <- year_from + 3
year_from_to <- paste0(year_from, "-", year_to)
request_urls <-
  paste0("https://www.ine.es/ftp/microdatos/ecv/ecv_b2013/periodo_",
         year_from_to,
         ".zip")

for(i in 1:length(request_urls)) {
  
  save_file <-
    here::here("Data",
               "Raw",
               paste0("EUSILC_base2013_", year_from_to[i], ".zip"))
  download.file(request_urls[i], destfile = save_file)

  }

# base 2004 files
year_from    <- 2004:2009
year_to      <- year_from + 3
year_from_to <- paste0(year_from, "-", year_to)
request_urls <-
  paste0("https://www.ine.es/ftp/microdatos/ecv/ecv_b2004/periodo_",
         year_from_to,
         ".zip")

for(i in 1:length(request_urls)) {

  save_file <-
    here::here("Data",
               "Raw",
               paste0("EUSILC_base2004_", year_from_to[i], ".zip"))
  download.file(request_urls[i], destfile = save_file)

  }