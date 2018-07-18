##See two below examples for how variable names are identified
##mapv..My Average Price by Variety
##uapvp..Universe Average Price by Variety filtered by Price
##Define variables

data_all <- as.data.table(data)
mydata <- as.data.table(my_restaurant)

numbvar <- 10 ##number of variables to display

varieties <- mydata[,.N, by = Variety] %>% arrange(desc(N)) %>% head(numbvar)
regions <- mydata[,.N, by = Region] %>% arrange(desc(N)) %>% head(numbvar)
subregions <- mydata[,.N, by = SubRegion][SubRegion !=""] %>% arrange(desc(N)) %>% head(numbvar)
countries <- mydata[,.N, by = Country][Country !=""] %>% arrange(desc(N)) %>% head(numbvar)
divide <- 5

##Country Data####
##Pricing - Bottles
mapc <- mydata[Country %in% countries$Country,.(Price = mean(as.numeric(Bottle.Price))), by = .(Country = Country)][Country !=""][,Group:=rep("My Wines")]
uapc <- data_all[Country %in% countries$Country,.(Price = mean(as.numeric(Bottle.Price))), by = .(Country = Country)][Country !=""][,Group:= rep("Selected Peer Group")]
upapc <- data_all[Price ==4 & Country %in% countries$Country,.(Price = mean(as.numeric(Bottle.Price))), by = .(Country = Country)][Country !=""][,Group:= rep("Selected Peer Group")]
ulapc <- data_all[Location =="DC" & Country %in% countries$Country,.(Price = mean(as.numeric(Bottle.Price))), by = .(Country = Country)][Country !=""][,Group:= rep("Selected Peer Group")]
##Pricing - Glasses
gmapc <- mydata[Country %in% countries$Country,.(Price = mean(as.numeric(Glass.Price))), by = .(Country = Country)][Country !=""][,Group:=rep("My Wines")]
guapc <- data_all[Country %in% countries$Country,.(Price = mean(as.numeric(Glass.Price))), by = .(Country = Country)][Country !=""][,Group:= rep("Selected Peer Group")]
gupapc <- data_all[Price ==4 & Country %in% countries$Country,.(Price = mean(as.numeric(Glass.Price))), by = .(Country = Country)][Country !=""][,Group:= rep("Selected Peer Group")]
gulapc <- data_all[Location =="DC" & Country %in% countries$Country,.(Price = mean(as.numeric(Glass.Price))), by = .(Country = Country)][Country !=""][,Group:= rep("Selected Peer Group")]
##Numbers
mnc <- mydata[Country %in% countries$Country,.(Number = .N), by = .(Country = Country)][Country !=""][,Group:=rep("My Wines")]
unc <- data_all[Country %in% countries$Country,.(n = .N), by = .(Country = Country)][Country !=""][,Number:= n/divide][,n := NULL][,Group:=rep("Selected Peer Group")]
ulnc <- data_all[Location =="DC" & Country %in% countries$Country, .(n = .N), by = .(Country = Country)][Country !=""][,Number:= n/divide][,n := NULL][,Group:=rep("Selected Peer Group")]
upnc <- data_all[Price ==4 & Country %in% countries$Country, .(n = .N), by = .(Country = Country)][Country !=""][,Number:= n/divide][,n := NULL][,Group:=rep("Selected Peer Group")]
##Join
apc <- rbind(mapc, uapc)
apcp <- rbind(mapc, upapc)
apcl <- rbind(mapc, ulapc)
gapc <- rbind(gmapc, guapc)
gapcp <- rbind(gmapc, gupapc)
gapcl <- rbind(gmapc, gulapc)
nc <- rbind(mnc, unc)
ncp <- rbind(mnc, upnc)
ncl <- rbind(mnc, ulnc)

##Varieties#### 
##Pricing - Bottles
mapv <- mydata[Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(Variety = Variety)][,Group:=rep("My Wines")]
uapv <- data_all[Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
uapvp <- data_all[Price ==4 & Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
uapvl <- data_all[Location =="DC" & Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
##Pricing - Glasses
gmapv <- mydata[Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(Variety = Variety)][,Group:=rep("My Wines")]
guapv <- data_all[Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
guapvp <- data_all[Price ==4 & Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
guapvl <- data_all[Location =="DC" & Variety %in% varieties$Variety,][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]

##Numbers
mnv <- mydata[Variety %in% varieties$Variety,][,.(Number = .N), by = .(Variety = Variety)][,Group:=rep("My Wines")]
unv <- data_all[Variety %in% varieties$Variety,][,.(Number = .N/divide), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
unvp <- data_all[Price ==4 & Variety %in% varieties$Variety,][,.(Number = .N/divide), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
unvl <- data_all[Location =="DC" & Variety %in% varieties$Variety,][,.(Number = .N/divide), by = .(Variety = Variety)][,Group:=rep("Selected Peer Group")]
##Join
apv <- rbind(mapv, uapv)
apvp <- rbind(mapv, uapvp)
apvl <- rbind(mapv, uapvl)
gapv <- rbind(gmapv, guapv)
gapvp <- rbind(gmapv, guapvp)
gapvl <- rbind(gmapv, guapvl)
nv <- rbind(mnv, unv)
nvp <- rbind(mnv, unvp)
nvl <- rbind(mnv, unvl)

##Region####
##Pricing - Bottles
mapr <- mydata[Region %in% regions$Region,][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(Region = Region)][,Group:= rep("My Wines")]
uapr <- data_all[Region %in% regions$Region,][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(Region = Region)][,Group:= rep("Selected Peer Group")]
uaprp <- data_all[Price ==4 & Region %in% regions$Region,.(Price = mean(as.numeric(Bottle.Price))), by = .(Region = Region)][Region !=""][,Group:= rep("Selected Peer Group")]
uaprl <- data_all[Location =="DC" & Region %in% regions$Region,.(Price = mean(as.numeric(Bottle.Price))), by = .(Region = Region)][Region !=""][,Group:= rep("Selected Peer Group")]
##Pricing - Glasses
gmapr <- mydata[Region %in% regions$Region,][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(Region = Region)][,Group:= rep("My Wines")]
guapr <- data_all[Region %in% regions$Region,][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(Region = Region)][,Group:= rep("Selected Peer Group")]
guaprp <- data_all[Price ==4 & Region %in% regions$Region,.(Price = mean(as.numeric(Glass.Price))), by = .(Region = Region)][Region !=""][,Group:= rep("Selected Peer Group")]
guaprl <- data_all[Location =="DC" & Region %in% regions$Region,.(Price = mean(as.numeric(Glass.Price))), by = .(Region = Region)][Region !=""][,Group:= rep("Selected Peer Group")]

##Numbers
mnr <- mydata[Region %in% regions$Region,][,.(Number = .N), by = .(Region = Region)][,Group:=rep("My Wines")]
unr <- data_all[Region %in% regions$Region,][,.(Number = .N/divide), by = .(Region = Region)][,Group:=rep("Selected Peer Group")]
unrp <- data_all[Price ==4 & Region %in% regions$Region,][,.(Number = .N/divide), by = .(Region = Region)][,Group:=rep("Selected Peer Group")]
unrl <- data_all[Location =="DC" & Region %in% regions$Region,][,.(Number = .N/divide), by = .(Region = Region)][,Group:=rep("Selected Peer Group")]
##Join
ar <- rbind(mapr, uapr)
arp <- rbind(mapr, uaprp)
arl <- rbind(mapr, uaprl)
gar <- rbind(gmapr, guapr)
garp <- rbind(gmapr, guaprp)
garl <- rbind(gmapr, guaprl)
nr <- rbind(mnr, unr)
nrp <- rbind(mnr, unrp)
nrl <- rbind(mnr, unrl)

##SubRegion####
##Pricing - Bottles
maps <- mydata[SubRegion %in% subregions$SubRegion,][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(SubRegion = SubRegion)][,Group:= rep("My Wines")]
uaps <- data_all[SubRegion %in% subregions$SubRegion][,.(Price = mean(as.numeric(as.character(Bottle.Price)))), by = .(SubRegion = SubRegion)][,Group:= rep("Selected Peer Group")]
uapsp <- data_all[Price ==4 & SubRegion %in% subregions$SubRegion,.(Price = mean(as.numeric(Bottle.Price))), by = .(SubRegion = SubRegion)][SubRegion !=""][,Group:= rep("Selected Peer Group")]
uapsl <- data_all[Location =="DC" & SubRegion %in% subregions$SubRegion,.(Price = mean(as.numeric(Bottle.Price))), by = .(SubRegion = SubRegion)][SubRegion !=""][,Group:= rep("Selected Peer Group")]
##Pricing - Glasses
gmaps <- mydata[SubRegion %in% subregions$SubRegion,][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(SubRegion = SubRegion)][,Group:= rep("My Wines")]
guaps <- data_all[SubRegion %in% subregions$SubRegion][,.(Price = mean(as.numeric(as.character(Glass.Price)))), by = .(SubRegion = SubRegion)][,Group:= rep("Selected Peer Group")]
guapsp <- data_all[Price ==4 & SubRegion %in% subregions$SubRegion,.(Price = mean(as.numeric(Glass.Price))), by = .(SubRegion = SubRegion)][SubRegion !=""][,Group:= rep("Selected Peer Group")]
guapsl <- data_all[Location =="DC" & SubRegion %in% subregions$SubRegion,.(Price = mean(as.numeric(Glass.Price))), by = .(SubRegion = SubRegion)][SubRegion !=""][,Group:= rep("Selected Peer Group")]

##Numbers
mns <- mydata[SubRegion %in% subregions$SubRegion,][,.(Number = .N), by = .(SubRegion = SubRegion)][,Group:=rep("My Wines")]
uns <- data_all[SubRegion %in% subregions$SubRegion,][,.(Number = .N), by = .(SubRegion = SubRegion)][,Group:=rep("Selected Peer Group")]
unsp <- data_all[Price ==4 & SubRegion %in% subregions$SubRegion,][,.(Number = .N), by = .(SubRegion = SubRegion)][,Group:=rep("Selected Peer Group")]
unsl <- data_all[Location == "DC" & SubRegion %in% subregions$SubRegion,][,.(Number = .N), by = .(SubRegion = SubRegion)][,Group:=rep("Selected Peer Group")]
##Join
as <- rbind(maps, uaps)
asp <- rbind(maps, uapsp)
asl <- rbind(maps, uapsl)
gas <- rbind(gmaps, guaps)
gasp <- rbind(gmaps, guapsp)
gasl <- rbind(gmaps, guapsl)
ns <- rbind(mns, uns)
nsp <- rbind(mns, unsp)
nsl <- rbind(mns, unsl)

##Sales vs Space####
##Variety
vsncnumbers<- mydata[Variety %in% varieties$Variety,.(Number = .N), by = .(Variety = Variety)][,Group:=rep("Numbers")]
vsncnumbers[, proportion := Number/sum(Number)]
vsncnumbers[, Number := NULL]
colnames(vsncnumbers) <- c("Variety","Group","Proportion")

vsncsales <- mydata[Variety %in%varieties$Variety,]
vsncsales[,proportion := ytd.profit/sum(ytd.profit)]
vsncsales <- vsncsales [,sum(proportion), by = Variety]
vsncsales[,Group := rep("Profit Proportion")]
colnames(vsncsales) <- c("Variety","Proportion","Group")

vsalesnumbercomparison <- rbind(vsncnumbers, vsncsales)

##Region
rsncnumbers<- mydata[Region %in% regions$Region,.(Number = .N), by = .(Region = Region)][,Group:=rep("Numbers")]
rsncnumbers[, proportion := Number/sum(Number)]
rsncnumbers[, Number := NULL]
colnames(rsncnumbers) <- c("Region","Group","Proportion")

rsncsales <- mydata[Region %in% regions$Region,]
rsncsales[,proportion := ytd.profit/sum(ytd.profit)]
rsncsales <- rsncsales [,sum(proportion), by = Region]
rsncsales[,Group := rep("Profit Proportion")]
colnames(rsncsales) <- c("Region","Proportion","Group")

rsalesnumbercomparison <- rbind(rsncnumbers, rsncsales)

##SubRegion
ssncnumbers<- mydata[SubRegion %in% subregions$SubRegion,.(Number = .N), by = .(SubRegion = SubRegion)][,Group:=rep("Numbers")]
ssncnumbers[, proportion := Number/sum(Number)]
ssncnumbers[, Number := NULL]
colnames(ssncnumbers) <- c("SubRegion","Group","Proportion")

ssncsales <- mydata[SubRegion %in% subregions$SubRegion,]
ssncsales[,proportion := ytd.profit/sum(ytd.profit)]
ssncsales <- ssncsales [,sum(proportion), by = SubRegion]
ssncsales[,Group := rep("Profit Proportion")]
colnames(ssncsales) <- c("SubRegion","Proportion","Group")

ssalesnumbercomparison <- rbind(ssncnumbers, ssncsales)
