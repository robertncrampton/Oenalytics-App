##Define Data in Country Pricing for Dashboard####
a <- uapc[Country %in% mapc$Country,]
b <- mapc
c <- merge(a,b, by = "Country")
c[,V3.x:= NULL]
c[,V3.y:= NULL]
c[,diff := Price.y - Price.x]
##How much more or less
z <- c[which.max((abs(c$diff))),]
posneg <- if(z$diff > 0 ) {"more"} else if(z$diff < 0) {"fewer"}
val <- round(z$diff)

d <- c[which.max((abs(c$diff))), .(Country)]
e <- as.character(d[[1]])
##Define Data in Variety Pricing for Dashboard####
aa <- uapv[Variety %in% mapv$Variety,]
bb <- mapv
cc <- merge(aa, bb, by = "Variety")
cc[,V3.x:= NULL]
cc[,V3.y:= NULL]
cc[,diff := Price.y - Price.x]
##How much more or less
zz <- cc[which.max((abs(cc$diff))),]
posneg1 <- if(zz$diff > 0 ) {"more"} else if(zz$diff < 0) {"fewer"}
val1 <- round(zz$diff)

dd <- cc[which.max((abs(cc$diff))), .(Variety)]
ee <- as.character(dd[[1]])

##Define Data in Variety Numbers for dashboard####
aaa <- unv[Variety %in% mnv$Variety,]
bbb <- mnv
ccc <- merge(aaa,bbb, by = "Variety")
ccc[,V3.x:= NULL]
ccc[,V3.y:= NULL]
ccc[,diff := Number.y - Number.x]
##How much more or less
zzz <- ccc[which.max((abs(ccc$diff))),]
posneg2 <- if(zzz$diff > 0 ) {"more"} else if(zzz$diff < 0) {"fewer"}
val2 <- round(abs(zzz$diff))

ddd <- ccc[which.max((abs(ccc$diff))), .(Variety)]
eee <- as.character(ddd[[1]])

##Inventory Info Boxes

more <- as.data.table(more)
low <- more[,.N]
less <- as.data.table(less)
high <- less[,.N]

numberofbottles <- numb
