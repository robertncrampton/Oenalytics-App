data <- data.table(read.csv("data/realdata.csv"))
new <- data[Producer !=""]
add <- new[, concatenate := paste(Vintage, Variety, Producer)]
check <- duplicated(add$concatenate) | duplicated(add$concatenate, fromLast = TRUE)
matches <- new[check]
matches <- matches[order(concatenate),]
write.csv(matches, file ="matches.csv")