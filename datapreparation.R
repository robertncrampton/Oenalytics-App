##Tabledata####
other <- data_all[!(r %in% restaurant),]
other$Bottle.Price <- as.numeric(as.character(other$Bottle.Price))
##Most/Least Expensive Wines
top <- mydata %>% arrange(desc(as.numeric(as.character(Bottle.Price)))) %>% select(Bottle.Price, Full.Name)
colnames(top) <- c("Bottle Price","Bottle Name")
##Add random column for amount of Inventory
Inventory <- mydata %>% arrange(desc(Inventory.Days)) %>% select(Full.Name, Inventory.Days, Inventory,sales.feb, invratio)
colnames(Inventory) <- c("Bottle Name", "Days in Inventory (Oldest Bottle)","Bottles in Inventory","Sales Last Month","Inventory Ratio")
##Profit Data for Table
January<- mydata %>% arrange(desc(as.numeric(as.character(sales.jan)))) %>% select(Bottle.Price, Full.Name, sales.jan, profit.jan)
February<- mydata %>% arrange(desc(as.numeric(as.character(sales.feb)))) %>% select(Bottle.Price, Full.Name, sales.feb, profit.feb)
ytd <- mydata%>% arrange(desc(as.numeric(as.character(ytd.profit)))) %>% select(Full.Name, ytd.profit)
colnames(ytd) <- c("Bottle Name","Profit Year-to-Date")
##Glasses Data
glasses <- data_all[Glass.Price !="",] %>% select(Full.Name, Glass.Price, glass.sales )%>% arrange(desc(glass.sales))
colnames(glasses) <- c("Bottle Name","Glass Price","Glass Sales")
##Top Bottles you dont have
numb <- 20
otherbottles <- other[sample(1:200,numb)] %>% select (Full.Name)
rank <- rep(1:numb)
otherbottles <- cbind(otherbottles, rank)
colnames(otherbottles) <- c("Bottle Name","Bottle Ranking")

##Assign quantiles for Profit, Inventory and Sales Data####   

data_all$profit.feb <- as.numeric(as.character(data_all$profit.feb))
one <- mydata[,quantile(profit.feb, c(.2), na.rm = TRUE)]
two <- mydata[,quantile(profit.feb, c(.4), na.rm = TRUE)]
three <- mydata[,quantile(profit.feb, c(.6), na.rm = TRUE)]
four <- mydata[,quantile(profit.feb, c(.8), na.rm = TRUE)]

sone <- mydata[,quantile(sales.feb, c(.2), na.rm = TRUE)]
stwo <- mydata[,quantile(sales.feb, c(.4), na.rm = TRUE)]
sthree <- mydata[,quantile(sales.feb, c(.6), na.rm = TRUE)]
sfour <- mydata[,quantile(sales.feb, c(.8), na.rm = TRUE)]

data_all$Inventory <- as.numeric(as.character(data_all$Inventory))
inv1 <- mydata[,quantile(Inventory, c(.2), na.rm = TRUE)]
inv2 <- mydata[,quantile(Inventory, c(.4), na.rm = TRUE)]
inv3 <- mydata[,quantile(Inventory, c(.6), na.rm = TRUE)]
inv4 <- mydata[,quantile(Inventory, c(.8), na.rm = TRUE)]

profit <- function(profit) {
  if(profit <= one) {
    y <-1
  } else if (profit > one && profit <=two) {
    y <-2
  } else if (profit > two && profit <= three) {
    y <- 3
  } else if (profit >three && profit <= four) {
    y <- 4
  } else if (profit > four) {
    y <- 5
  }
  print(y)
}

sales <- function(sales) {
  if(sales <= sone) {
    z <-1
  } else if (sales > sone && sales <=stwo) {
    z <-2
  } else if (sales > stwo && sales <= sthree) {
    z <- 3
  } else if (sales >sthree && sales<= sfour) {
    z <- 4
  } else if (sales> sfour) {
    z <- 5
  }
  print(z)
}

invassessment <- function(z) {
  if(z <= inv1) {
    x <-1
  } else if (z > inv1 && z <=inv2) {
    x <-2
  } else if (z > inv2 && z <= inv3) {
    x <- 3
  } else if (z >inv3 && z <= inv4) {
    x <- 4
  } else if (z > inv4) {
    x <- 5
  }
  print(x)
}

##Add Profit,Sales, Inventory Categorization to Data ####
mydata[, success := sapply(profit.feb, profit)]
mydata[, inv:= sapply(Inventory, invassessment)]
mydata[, salesdeets := sapply(sales.feb, sales)]


more <- mydata[invratio < 3,] %>% select(Full.Name, Inventory,sales.feb, invratio) %>% arrange(invratio)
colnames(more) <- c("Bottle Name","Bottles in Inventory","Sales Last Month","Inventory Ratio")
less <- mydata[invratio >30,] %>% select(Full.Name, Inventory, sales.feb, invratio) %>% arrange(desc(invratio))
colnames(more) <- c("Bottle Name","Bottles in Inventory","Sales Last Month","Inventory Ratio")

##Graphing Sales Details####
v <- week1[,sum(profit), by = Variety][Variety !=""] %>% arrange(desc(V1)) %>% head(numbvar)
r <- week1[,sum(profit), by = Region][Region !=""] %>% arrange(desc(V1)) %>% head(numbvar)
s <- week1[,sum(profit), by = SubRegion] [SubRegion !=""] %>% arrange(desc(V1)) %>% head(numbvar)
week1[Variety %in% v$Variety, newv := Variety]
week1[!(Variety %in% v$Variety), newv := c("Other")]
week1[Region %in% r$Region,newr := Region]
week1[!(Region %in% r$Region), newr := c("Other")]
week1[SubRegion %in% s$SubRegion, news := SubRegion]
week1[!(SubRegion %in% s$SubRegion), news := c("Other")]
week1[,Day.of.Week := rep(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))]
pbdv <- week1[, sum(profit), by = list(Day.of.Week, newv)]
pbdr <- week1[, sum(profit), by = list(Day.of.Week, newr)]
pbds <- week1[, sum(profit), by = list(Day.of.Week, news)]

##Profit by 3 classifications
pbr <- week1[,sum(profit), by = newr]
pbv <- week1[,sum(profit), by = newv]
pbs <- week1[,sum(profit), by = news]

##Dynamic Sales Data for Optimization####
##Assign elasticity values for each of the shifts
sd <- mydata
e1.05 <- rnorm(1, .98, .1)
e1.1 <- rnorm(1, .95, .1)
e1.15 <- rnorm(1, .93, .1)
e1.2 <- rnorm(1, .9, .1)
e.95 <- rnorm(1, 1.02, .1)
e.9 <- rnorm(1, 1.05, .1)
e.85 <- rnorm(1, 1.1, .1)
e.8 <- rnorm(1, 1.2, .1)
sd[,up5 := sales.jan*e1.05]
sd[,up10 := sales.jan*e1.1]
sd[,up15 := sales.jan*e1.15]
sd[,up20 := sales.jan*e1.2]
sd[,down5 := sales.jan*e.95]
sd[,down10 := sales.jan*e.9]
sd[,down15 := sales.jan*e.85]
sd[,down20 := sales.jan*e.8]
sd[,profbase := round(sales.jan*(Bottle.Price - retail))]
sd[,profup5 := round(up5 *(Bottle.Price*1.05-retail))]
sd[,profup10 := round(up10 *(Bottle.Price*1.1-retail))]
sd[,profup15 := round(up15 *(Bottle.Price*1.15-retail))]
sd[,profup20 := round(up20 *(Bottle.Price*1.2-retail))]
sd[,profdown5 := round(down5 *(Bottle.Price*.95-retail))]
sd[,profdown10 := round(down5 *(Bottle.Price*.9-retail))]
sd[,profdown15 := round(down15 *(Bottle.Price*.85-retail))]
sd[,profdown20 := round(down20 *(Bottle.Price*.8-retail))]

