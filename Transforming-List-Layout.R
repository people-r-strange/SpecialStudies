## read in txt files automatically 
fname <- list.files("OPUS", full.names = T)

## creates list of txt files 
filelist <- lapply(fname, read.delim, header = F)

##Check structure of filelist 
str(filelist, give.attr = FALSE)

##Checking Names (We need to rename...)
lapply(filelist, names)

##Adding names of data frames to refer to each lake core 
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

## Viewing what the list file looks like 
View(filelist)

glimpse(filelist)

##Transform dataframe
my_data <- do.call(rbind.data.frame, filelist)