##Trying a shortcut 
#filenames <- list.files("OPUS", pattern="*.txt", full.names=TRUE)
#ldf <- lapply(filenames, read.table)
#res <- lapply(ldf, summary)
#names(res) <- substr(filenames, 6, 30)

##Viewing the df created in shortcut
#View(ldf)
#View(res)
##Trying to extract individual summary tables 
#for (i in 1:length(res))
#  assign(paste(paste("df", i, sep=""), "summary", sep="."), res[[i]])

#View(df1.summary)


##Reading in txt file 
#lines <- readLines("FISK-10.0.txt")
#lines <- sapply(lines, gsub, pattern="[-]{2,}|[|]", replacement="")
#lines <- c(lines[2], lines[lines!="" & lines!=lines[2]])

#read.table(text=lines, header=T)

-----------------------------------------------------------------------------------------------------
#**This Works** 

##Reading in Multiple files
library(foreign)

## create and view an object with file names and full paths
(fname <- file.path("/Users/viviennemaxwell/Documents/Special Studies/SpecialStudies/OPUS", 
                c("FISK-10.0.txt", 
                  "FISK-110.0.txt",
                  "FISK-270.0.txt",
                  "LSA1-30.0.txt",
                  "LSA2-35.0.txt",
                  "LSA2-50.0.txt",
                  "LSA2-55.0.txt",
                  "LSA3-55.0.txt",
                  "LSA3-95.0.txt",
                  "NAN-DB-2.0.txt",
                  "NAN-DB-4.0.txt",
                  "NAN-DB-10.0.txt",
                  "NAN-DB-31.0.txt",
                  "NANA1A-109cm.0.txt",
                  "NANA1A-116cm.0.txt",
                  "NANA2-50.0.txt",
                  "NANA2-55.0.txt",
                  "NANA2-60.0.txt",
                  "NANA2-65.0.txt",
                  "NANA2-132.0.txt",
                  "NANB3A1-131.5.0.txt",
                  "NANB3A2-6.0.txt",
                  "NANB3A2-10.5.0.txt",
                  "NANDB-13.5.txt",
                  "NANDB-17.0.txt",
                  "NANDB-20.0.txt",
                  "SS.0.txt",
                  "WQ.0.txt")))

##List with all text files
filelist <- lapply(fname, read.table)

##Check structure of filelist 
str(filelist, give.attr = FALSE)

##Checking Names (We need to rename...)
lapply(filelist, names)

##Renaming 
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

##Check structure 
str(filelist, give.attr=FALSE)