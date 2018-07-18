##Code to Fix

##Focus variable beginning of import.R


##Week1
jan1 <- salestable[,.(Bottle.Number,jan1)]
colnames(jan1) <- c("Bottle.Number","Sales")
jan2 <- salestable[,.(Bottle.Number,jan2)]
colnames(jan2) <- c("Bottle.Number","Sales")
jan3 <- salestable[,.(Bottle.Number,jan3)]
colnames(jan3) <- c("Bottle.Number","Sales")
jan4 <- salestable[,.(Bottle.Number,jan4)]
colnames(jan4) <- c("Bottle.Number","Sales")
jan5 <- salestable[,.(Bottle.Number,jan5)]
colnames(jan5) <- c("Bottle.Number","Sales")
jan6 <- salestable[,.(Bottle.Number,jan6)]
colnames(jan6) <- c("Bottle.Number","Sales")
jan7 <- salestable[,.(Bottle.Number,jan7)]
colnames(jan7) <- c("Bottle.Number","Sales")
##Week2
jan8 <- salestable[,.(Bottle.Number,jan8)]
colnames(jan8) <- c("Bottle.Number","Sales")
jan9 <- salestable[,.(Bottle.Number,jan9)]
colnames(jan9) <- c("Bottle.Number","Sales")
jan10 <- salestable[,.(Bottle.Number,jan10)]
colnames(jan10) <- c("Bottle.Number","Sales")
jan11 <- salestable[,.(Bottle.Number,jan11)]
colnames(jan11) <- c("Bottle.Number","Sales")
jan12 <- salestable[,.(Bottle.Number,jan12)]
colnames(jan12) <- c("Bottle.Number","Sales")
jan13 <- salestable[,.(Bottle.Number,jan13)]
colnames(jan13) <- c("Bottle.Number","Sales")
jan14 <- salestable[,.(Bottle.Number,jan14)]
colnames(jan14) <- c("Bottle.Number","Sales")
##Add Markup, Sales, Retail, profit to my_restaurant






retail <- my_restaurant[,.(Retail, Bottle.Number, Bottle.Price, Variety, Region, SubRegion)]
week1 <- as.data.table(rbind(jan1,jan2,jan3, jan4, jan5, jan6, jan7))
week2 <- as.data.table(rbind(jan8, jan9, jan10, jan11, jan12, jan13, jan14))
week1 <- merge(week1, retail, by = "Bottle.Number")
week2 <- merge(week2, retail, by = "Bottle.Number")
week1[,profit := (Bottle.Price - Retail)*Sales]
week2[,profit := (Bottle.Price - Retail)*Sales]


sales.jan <- week1[,sum(Sales), by = Bottle.Number]
colnames(sales.jan) <- c("Bottle.Number","sales.jan")
sales.feb <- week2[,sum(Sales), by = Bottle.Number]
colnames(sales.feb) <- c("Bottle.Number","sales.feb")
my_restaurant <- merge(my_restaurant, sales.jan, by = "Bottle.Number")
my_restaurant <- merge(my_restaurant, sales.feb, by = "Bottle.Number")