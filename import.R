##my_restaurant & restaurant assignment
data <- read_csv("data/realdata.csv")
restaurant <- c("sonoma")
my_restaurant <- data %>% filter(r %in% restaurant)

data$Bottle.Price <- as.numeric(as.character(data$Bottle.Price))

##Generate fake sales data for each bottle
price1 <- quantile(my_restaurant$Bottle.Price, c(.2), na.rm = TRUE)
price2 <- quantile(my_restaurant$Bottle.Price, c(.4), na.rm = TRUE)
price3 <- quantile(my_restaurant$Bottle.Price, c(.6), na.rm = TRUE)
price4 <- quantile(my_restaurant$Bottle.Price, c(.8), na.rm = TRUE)

salesgen <- function(x) { 
  
  if(x <= price1) {
    sample(1:20,1)
  }else if (x > price1 & x <= price2){ 
    sample(1:15,1)
  }else if (x > price2 & x <= price3){ 
    sample(0:10,1)
  }else if (x > price3 & x <= price4){ 
    sample(0:5,1)
  }else if (x > price4){ 
    sample(1:2,1)
  }
}

##Markup Curve Function####
amu <- function(p) { 
  markup <- 2 +.005*p + rnorm(1, mean = 1, sd = .5)
  markup
}
## Universe Pricing curve
bmu <- function(p) { 
  markup <- 3 -.0005*p + rnorm(1, mean = 1, sd = .5)
  markup
}

##Generate Sales Data####
salestable <- my_restaurant %>% select(`Bottle Number`, Bottle.Price) %>% 
  mutate(jan1 = sapply(Bottle.Price, salesgen),
                                    jan2 = sapply(Bottle.Price, salesgen), 
                                    jan3 = sapply(Bottle.Price, salesgen),
                                    jan4 = sapply(Bottle.Price, salesgen),
                                    jan5 = sapply(Bottle.Price, salesgen),
                                    jan6 = sapply(Bottle.Price, salesgen),
                                    jan7 = sapply(Bottle.Price, salesgen),
                                    jan8 = sapply(Bottle.Price, salesgen),
                                    jan9 = sapply(Bottle.Price, salesgen),
                                    jan10 = sapply(Bottle.Price, salesgen),
                                    jan11 = sapply(Bottle.Price, salesgen),
                                    jan12 = sapply(Bottle.Price, salesgen),
                                    jan13 = sapply(Bottle.Price, salesgen),
                                    jan14 = sapply(Bottle.Price, salesgen)
                                    )
salestable <- salestable %>% select(-Bottle.Price) 

my_restaurant <- inner_join(my_restaurant, salestable, by = "Bottle Number")
my_restaurant %>% select(jan1:jan7) %>% rowSums(na.rm=TRUE) -> my_restaurant$sales.jan
my_restaurant %>% select(jan8:jan14) %>% rowSums(na.rm=TRUE) -> my_restaurant$sales.feb

my_restaurant <- my_restaurant %>% mutate (markup = sapply(Bottle.Price, amu), 
                                                       retail=Bottle.Price/markup,
                                                       profit.jan = (Bottle.Price - retail)*(sales.jan),
                                                       profit.feb = (Bottle.Price - retail)*(sales.feb),
                                                       ytd.profit = (profit.jan + profit.feb),
                                                       invratio = round(Inventory/sales.feb,1))


##Glass Data ####
glass <- function(x) { x/4}

data <- data %>% filter(Bottle.Price <300) %>% mutate(Glass.Price = sapply(Bottle.Price, glass))
my_restaurant <- my_restaurant %>% filter(Bottle.Price <300) %>% mutate(Glass.Price = sapply(Bottle.Price, glass))

Glass.Price <- 5
sales <- function(x) { 
  if(Glass.Price < 10) { 
    round(10 + rnorm(1, mean = 0, sd =3))
  } else if(Glass.Price >=10 & Glass.Price <= 20) { 
    round(8 + rnorm(1,mean = 0, sd =3))  
  } else if(Glass.Price >20) { 
    round(5 + rnorm(1,mean = 0, sd=3))
  }
}

data <- data %>% mutate(glass.sales = sapply(Glass.Price, sales))
