##Fix Fix gsub() using OR 
##Fix Problem with non UTF-8 Character strings

library(data.table)
library(dplyr)
library(stringr)
##Data
data <- data.table(read.csv("data/realdata.csv"))
add_wine <- data[, .(id, Full.Name, Vintage, Producer, Variety, Designation, Vineyard, Country, Region, SubRegion, Appelation)]
add_wine<- as.data.frame(lapply(add_wine,function(x){gsub(",","",x)}))
add_wine<- as.data.frame(lapply(add_wine,function(x){gsub('"',"",x)}))
add_wine[] <- lapply(add_wine, as.character)

##new_wines is the character vector of wines to add 
add_wine <- tbl_df(add_wine)
new_wines<- add_wine %>% filter(is.na(id)) %>% select(Full.Name)
new_wines <- as.character(new_wines$Full.Name)

##function to iterate through matches in "newine" dataframe
input <- function(newine){
  z<-lapply(add_wine[,3:11], function(x) x[str_detect(newine, x)])
  z <- lapply(z, unique)
  z <- lapply(z,function(x)x[!is.na(x)])
  z$Full.Name <- newine
  z
}

##Apply matching function to new_wines character vector
z <- data.frame(do.call(rbind, lapply(new_wines, input)))
z <- as.data.frame(z)
z[] <- lapply(z, as.character)

##Add on id column as a continuation of database
max_id <- max(as.numeric(add_wine$id), na.rm = TRUE) + 1
z <- as.data.table(cbind(id =id <- max_id : (max_id+nrow(z) -1),z ))
z[,compare := paste(Vintage, Producer, Variety, Designation, Vineyard, Country, Region, SubRegion, Appelation)]

z$new <- apply(z, 1, function(x) {
  x1 <- unlist(strsplit(x[11], split = " "))
  x2 <- unlist(strsplit(x[12], split = " "))
  setdiff(x1,x2)
})

z$id <- as.numeric(z$id)
z[z=="character(0)"] <- NA
z1<- as.data.frame(lapply(z, function(x) gsub("c(","",x,fixed = TRUE, ignore.case = TRUE)))
z2<- as.data.frame(lapply(z1, function(x) gsub(")","",x,fixed = TRUE, ignore.case = TRUE)))
z3<- as.data.frame(lapply(z2, function(x) gsub('"',"",x,fixed = TRUE, ignore.case = TRUE)))
write.table(z3,file="z3.csv")

