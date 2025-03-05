# unzip fles and put them in dedicated directories
setwd("Data/RAW")

if(!dir.exists("../unzipped")) { 
dir.create("../unzipped")
  
}

# unzip all top level** folders into new folder called unzipped
# ** folders primarily contain data zipped into folder within the top folder and some more files i.e. "usuario"

for(i in 1:length(list.files())) {
  
  unzip(zipfile = list.files()[i], 
        exdir   = "../unzipped")
  
  }

setwd("../unzipped")


if(!dir.exists("guides")) { 
  
  dir.create("guides")
  
}

# copy all usuario files to dedicated folder
file.copy(from = list.files()[grepl("usuario", list.files())],
          to   = "guides")

# remove them from folder unzipped.
file.remove(list.files()[grepl("usuario", list.files())])


if(!dir.exists("DATA_RAW")) { 
  
  dir.create("DATA_RAW")
  
}

# unzip all remaining folder that presumably should contain only data**
# unfortunately they contain more then the data. There are R files txt and csv files and more rubish
for(i in 1:length(list.files()[grepl(".zip", list.files())])) {
  
  unzip(zipfile = list.files()[grepl(".zip", list.files())][i], 
        exdir = "DATA_RAW")
  
  }

file.remove(list.files()[grepl(".zip", list.files())])


# now move all .csv files to the folder DATA_RAV and remove them from this folder
file.copy(from = list.files()[grepl(".csv", list.files())],
          to   = "DATA_RAW")

# remove them from folder unzipped.
file.remove(list.files()[grepl(".csv", list.files())])

# My plan now is to separate the files by their extension (.csv, .txt, .R etc.) into dedicated folders
# CSV directory already created. Apparently existed in one of the files MESS :-(
setwd("DATA_RAW")

if(!dir.exists("XLSX")) { 
  
  dir.create("XLSX")
  
}

if(!dir.exists("TXT")) { 
  
  dir.create("TXT")
  
}

# csv
file.copy(from = list.files()[grepl(".csv", list.files())],
          to   = "CSV")

# remove them from folder unzipped.
file.remove(list.files()[grepl(".csv", list.files())])

# txt
file.copy(from = list.files()[grepl(".txt", list.files())],
          to   = "TXT")

# remove them from folder unzipped.
file.remove(list.files()[grepl(".txt", list.files())])

# xlsx
file.copy(from = list.files()[grepl(".xlsx", list.files())],
          to   = "XLSX")

# remove them from folder unzipped.
file.remove(list.files()[grepl(".xlsx", list.files())])

# THERE IS ONE FOLDER of .7z format that maybe failed to unzip due to strange format