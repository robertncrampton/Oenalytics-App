#Server.R
library("shiny")
library("ggvis")
library("data.table")
library("ggplot2")
library("dplyr")
library("shinydashboard")
source("import.R")
source("datapreparation.R")
source("comparative.R")
source("dashboard.R")
data_all <- as.data.table(data)
mydata <- as.data.table(my_restaurant)

shinyServer(
  function(input, output) {
    
    ##Dashboard Value Boxes#### 
    output$nwines <- renderValueBox({
      valueBox(
        data_all[r %in% restaurant, .N], "Bottles on Your Menu", icon = icon("credit-card"),
        color = "red"
      )
    })
    
    
    output$wines <- renderValueBox({
      valueBox(
        data_all[r %in% restaurant, sum(sales.jan, na.rm = TRUE)], "Bottles Sold in January", icon = icon("credit-card"),
        color = "red"
      )
    })
    
    output$profit <- renderValueBox({
      valueBox(
        paste0("$",mydata[, sum(profit.jan, na.rm = TRUE)]), "Profit in January", icon = icon("credit-card"),
        color = "red"
      )
    })
    
    
    ##Dashboard Icon Cards####
    output$other <- renderText({
      paste("There are",numb,"top selling bottles other restaurants have on their menu that you do not.")
      
    })
    
    output$spaceprofit <- renderText({
      paste("Your Pinot Noirs take up a lot of space on the menu but aren't very profitable. Have you thought about repricing these?")
      
    })
    output$cp <- renderText({
      paste("You price wines in",print(e),"$",print(val), print(posneg), "than competitors on average.")
      
    })
    
    output$vp <- renderText ({ 
      paste("You price your", print(ee),"$",print(val1),print(posneg1), "than competitors on average.")
    })
    
    output$nv <- renderText ({ 
      paste("Your restaurant has", print(val2), print(posneg2),"bottles of", print(eee),"on your menu","than competitors on average.")
    })
    
    output$low <- renderInfoBox({
      infoBox(
        "Inventory", paste(paste(low),"More bottles needed"),
        icon = icon("list"), color = "aqua")
    })
    
    output$high <- renderInfoBox({
      infoBox(
        "Inventory", paste(paste(high),"Bottles you need to sell"),
        icon = icon("list"), color = "yellow"
      )
    })
    
    ##Numbers Graph####
    output$plot1 <- renderPlot({
      if(input$var1 == "Number by Country" & input$comps =="All") {
        selection <- nc
      } else if (input$var1 == "Number by Country" & input$comps =="Price") {
        selection <- ncp
      } else if (input$var1 == "Number by Country" & input$comps =="Location") {
        selection <- ncl
      } else if (input$var1 == "Number by Variety" & input$comps =="All") {
        selection <- nv
      } else if (input$var1 == "Number by Variety" & input$comps =="Price") {
        selection <- nvp
      } else if (input$var1 == "Number by Variety" & input$comps =="Location") {
        selection <- nvl
      } else if (input$var1 == "Number by Region" & input$comps =="All") {
        selection <- nr
      } else if (input$var1 == "Number by Region" & input$comps =="Price") {
        selection <- nrp
      } else if (input$var1 == "Number by Region" & input$comps =="Location") {
        selection <- nrl
      } else if (input$var1 == "Number by SubRegion" & input$comps =="All") {
        selection <- ns
      }else if (input$var1 == "Number by SubRegion" & input$comps =="Price") {
        selection <- nsp
      }else if (input$var1 == "Number by SubRegion" & input$comps =="Location") {
        selection <- nsl
      }
      
      if(identical(selection, nc)) { 
        selection %>%
          ggplot(aes(Country, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by Country vs. All Restaurants") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, ncl)) {
        selection %>%
          ggplot(aes(Country, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") + 
          labs(title = "Number by Country") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, ncp)) {
        selection %>%
          ggplot(aes(Country, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by Country") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, nv)) {
        selection %>%
          ggplot(aes(Variety, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by Variety") +
          theme(plot.title = element_text(size = rel(2)))  
      }else if (identical(selection, nvp)) {
        selection %>%
          ggplot(aes(Variety, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by Variety") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, nvl)) {
        selection %>%
          ggplot(aes(Variety, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by Variety") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, nr)) {
        selection %>%
          ggplot(aes(Region, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by Region") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, nrp)) {
        selection %>%
          ggplot(aes(Region, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by Region") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, nrl)) {
        selection %>%
          ggplot(aes(Region, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")  +
          labs(title = "Number by Region") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, ns)) {
        selection %>%
          ggplot(aes(SubRegion, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")  +
          labs(title = "Number by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))  
      }else if (identical(selection, nsp)) {
        selection %>%
          ggplot(aes(SubRegion, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Number by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selection, nsl)) {
        selection %>%
          ggplot(aes(SubRegion, Number, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")  +
          labs(title = "Number by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))  
      }
      
    })    
    ##Table####
    output$table <- renderDataTable({
      if(input$sum == "Pricing"){
        top
      } else if(input$sum == "Inventory"){
        Inventory
      } else if(input$sum =="January") {
        January
      } else if (input$sum =="February") {
        February
      } else if(input$sum =="Year-to-date Profit") { 
        ytd
      } else if(input$sum =="Low Inventory, Top Selling Wine") { 
        more
      } else if(input$sum =="High Inventory, Scarcely Sold Wine") { 
        less  
      } else if(input$sum =="Top Selling Glasses") { 
        glasses  
      } else if(input$sum =="Top Selling Bottles Not On Your Menu") { 
        otherbottles  
      }
    })
    ##Histogram####
    output$hist <- renderPlot({
      ggplot(data, aes(Bottle.Price)) + 
        geom_histogram(data = other, binwidth = input$bins, fill = "red", alpha = 0.2, aes(y=..count../sum(..count..), colour="red")) + 
        geom_histogram(data = mydata, binwidth = input$bins, fill = "blue", alpha = 0.2,aes(y=..count../sum(..count..), colour="blue")) +
        coord_cartesian(xlim =c(input$range[1], input$range[2]))+ 
        scale_colour_manual(name="Legend", values=c("red" = "red", "blue"="blue"), labels=c("blue"="My Wines", "red"="Peer Group Wines")) + 
        labs(title = "Pricing Distribution Comparison") + 
        theme(plot.title = element_text(size = rel(2))) + 
        xlab("Bottle Price")+
        ylab("Price Frequency")
      
    })
    
    ##Pricing Graph####
    ##re-add success column - not being recognized 
    mydata[, success := sapply(profit.feb, profit)]    
    output$curve <- renderPlot({
      ggplot(data = mydata, aes(x=Bottle.Price,y=markup)) +
        geom_point(aes(colour = factor(mydata$success))) +
        coord_cartesian(xlim =c(input$a[1], input$a[2])) +
        scale_color_manual(name = "Legend",values = c("1"= "red", "2"= "orange", "3"= "yellow", "4"= "lawngreen","5"= "springgreen4"), 
                           breaks = c("1","2","3","4","5"),
                           labels =c("Least Profitable", "Second Least Profitable", "Average Profitability","Second Most Profitable","Most Profitable")) +  
        labs(title = "Bottle Profitability Comparison") + 
        theme(plot.title = element_text(size = rel(2))) + 
        xlab("Bottle Price")+
        ylab("Markup")
      
    })
    
   
    output$hover_info <- renderPrint({
      if(!is.null(input$plot_hover)){
        hover=input$plot_hover
        dist=sqrt((hover$x-mydata$Bottle.Price)^2+(hover$y-mydata$markup)^2)
        if(min(dist) < 4)
          as.character(mydata$Full.Name)[which.min(dist)]
      }
    })
    ##Sales Details####
    output$sales <- renderPlot({
      if(input$salesinfo == "Group by Day" & input$grouping =="Region") { 
        selected <- pbdr
      } else if(input$salesinfo == "Group by Day" & input$grouping =="SubRegion") { 
        selected <- pbds
      } else if(input$salesinfo == "Group by Day" & input$grouping =="Variety") { 
        selected <- pbdv
      } else if(input$salesinfo =="Group by Selection" & input$ grouping =="Region") { 
        selected <- pbr  
      } else if(input$salesinfo =="Group by Selection" & input$ grouping =="SubRegion") { 
        selected <- pbs  
      } else if(input$salesinfo =="Group by Selection" & input$ grouping =="Variety") { 
        selected <- pbv
      }else if(input$salesinfo =="Profit to Space Comparison" & input$ grouping =="Variety") { 
        selected <- vsalesnumbercomparison
      }else if(input$salesinfo =="Profit to Space Comparison" & input$ grouping =="Region") { 
        selected <- rsalesnumbercomparison
      }else if(input$salesinfo =="Profit to Space Comparison" & input$ grouping =="SubRegion") { 
        selected <- ssalesnumbercomparison
      }
      
      if(identical(selected, pbdr)) { 
        selected %>% 
          ggplot(aes(x = Day.of.Week, y = V1, fill = newr)) + geom_bar(stat = "identity") + 
          scale_fill_discrete(name = "Region") +
          scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
          labs(title = "Profit by Day") + 
          theme(plot.title = element_text(size = rel(2))) + 
          xlab("Day of the Week")+
          ylab("Total Profit")
      } else if (identical(selected, pbds)) { 
        selected %>%
          ggplot(aes(x = Day.of.Week, y = V1, fill = news)) + geom_bar(stat="identity")+
          scale_fill_discrete(name = "SubRegion") +
          scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
          labs(title = "Profit by Day") + 
          theme(plot.title = element_text(size = rel(2))) + 
          xlab("Day of the Week")+
          ylab("Total Profit")
      } else if (identical(selected, pbdv)) { 
        selected %>%
          ggplot(aes(x = Day.of.Week, y = V1, fill = newv)) + geom_bar(stat="identity")+
          scale_fill_discrete(name = "Variety") +
          scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
          labs(title = "Profit by Day") + 
          theme(plot.title = element_text(size = rel(2))) + 
          xlab("Day of the Week")+
          ylab("Total Profit")
      } else if (identical(selected, pbr)) { 
        selected %>%
          ggplot(aes(x = newr, y = V1)) + geom_bar(stat="identity", color = "black", fill= "blue") + 
          scale_fill_discrete(name = "Region") +
          labs(title = "Profit by Region") + 
          theme(plot.title = element_text(size = rel(2))) + 
          xlab("Region")+
          ylab("Total Profit")
      } else if (identical(selected, pbv)) { 
        selected %>%
          ggplot(aes(x = newv, y = V1)) + geom_bar(stat="identity", color = "black", fill= "blue") + 
          scale_fill_discrete(name = "Variety") +
          labs(title = "Profit by Variety") + 
          theme(plot.title = element_text(size = rel(2))) + 
          xlab("Variety")+
          ylab("Total Profit")
      } else if (identical(selected, pbs)) { 
        selected %>%
          ggplot(aes(x = news, y = V1)) + geom_bar(stat="identity", color = "black", fill= "blue")+ 
          scale_fill_discrete(name = "SubRegion") +
          labs(title = "Profit by SubRegion") + 
          theme(plot.title = element_text(size = rel(2))) + 
          xlab("SubRegion")+
          ylab("Total Profit")
      }else if (identical(selected, vsalesnumbercomparison)) { 
        selected %>%
          ggplot(aes(x = Group, y = Proportion, fill = Variety))+ geom_bar(stat = "identity") + 
          labs(title = "Menu Space to Profit Comparison") + 
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selected, rsalesnumbercomparison)) { 
        selected %>%
          ggplot(aes(x = Group, y = Proportion, fill = Region))+ geom_bar(stat = "identity") + 
          labs(title = "Menu Space to Profit Comparison") + 
          theme(plot.title = element_text(size = rel(2)))
      }else if (identical(selected, ssalesnumbercomparison)) { 
        selected %>%
          ggplot(aes(x = Group, y = Proportion, fill = SubRegion))+ geom_bar(stat = "identity") + 
          labs(title = "Menu Space to Profit Comparison") + 
          theme(plot.title = element_text(size = rel(2)))
      }
    })
    
    
    ##Sample Optimization####
    
Day.of.Week<- rep(c("Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),length(sd))
    sd <- cbind(sd, Day.of.Week)
    
    output$optimization <- renderPlot({
      if(input$updown == -20) {
        yess <- sd[,sum(profdown20, na.rm = TRUE), by = Day.of.Week]
      } else if (input$updown ==-15) {
        yess <- sd[,sum(profdown15, na.rm = TRUE), by = Day.of.Week]
      }else if (input$updown ==-10) {
        yess <- sd[,sum(profdown10, na.rm = TRUE), by = Day.of.Week]
      }else if (input$updown ==-5) {
        yess <- sd[,sum(profdown5, na.rm = TRUE), by = Day.of.Week]
      } else if (input$updown ==0) {
        yess <- sd[,sum(profbase, na.rm = TRUE), by = Day.of.Week]
      }else if (input$updown ==5) {
        yess <- sd[,sum(profup5, na.rm = TRUE), by = Day.of.Week]
      }else if (input$updown ==10) {
        yess <- sd[,sum(profup10, na.rm = TRUE), by = Day.of.Week]
      }else if (input$updown ==15) {
        yess <- sd[,sum(profup15, na.rm = TRUE), by = Day.of.Week]
      } else if (input$updown ==20) {
        yess <- sd[,sum(profup20, na.rm = TRUE), by = Day.of.Week]
      }
      ggplot(data=yess, aes(x=Day.of.Week, y=V1)) +
        geom_bar(stat="identity", fill = "red") + 
        scale_y_continuous(minor_breaks = seq(0 , 75000, 10000), breaks = seq(0, 75000, 10000))+ 
        coord_cartesian(ylim = c(0,75000))+
        labs(title = "Sample Optimization Scenario") + 
        theme(plot.title = element_text(size = rel(2))) + 
        xlab("Section of Menu")+
        ylab("Total Pofit") + 
        scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
    })
    output$money <- renderText({
      if(input$updown == -20) {
        sd[,sum(profdown20, na.rm = TRUE)]
      } else if (input$updown ==-15) {
        sd[,sum(profdown15, na.rm = TRUE)]
      }else if (input$updown ==-10) {
        sd[,sum(profdown10, na.rm = TRUE)]
      }else if (input$updown ==-5) {
        sd[,sum(profdown5, na.rm = TRUE)]
      }else if (input$updown ==0) {
        sd[,sum(profbase, na.rm = TRUE)]
      }else if (input$updown ==5) {
        sd[,sum(profup5, na.rm = TRUE)]
      }else if (input$updown ==10) {
        sd[,sum(profup10, na.rm = TRUE)]
      }else if (input$updown ==15) {
        sd[,sum(profup15, na.rm = TRUE)]
      } else if (input$updown ==20) {
        sd[,sum(profup20, na.rm = TRUE)]
      }
    })
    ##New Pricing####
    output$newplot <- renderPlot({
      if(input$newp == "Avg Price by Region" & input$peers == "All" &input$bg =="Bottle") {
        yours <- ar
      } else if (input$newp == "Avg Price by Region" & input$peers == "Price" &input$bg =="Bottle") {
        yours <- arp
      } else if (input$newp == "Avg Price by Region" & input$peers == "Location" &input$bg =="Bottle") {
        yours <- arl
      } else if (input$newp == "Avg Price by SubRegion" & input$peers == "All" &input$bg =="Bottle") {
        yours <- as
      } else if (input$newp == "Avg Price by SubRegion" & input$peers == "Price" &input$bg =="Bottle") {
        yours <- asp
      } else if (input$newp == "Avg Price by SubRegion" & input$peers == "Location" &input$bg =="Bottle") {
        yours <- asl
      } else if (input$newp == "Avg Price by Country" & input$peers == "All" &input$bg =="Bottle") {
        yours <- apc
      }else if (input$newp == "Avg Price by Country" & input$peers == "Price" &input$bg =="Bottle") {
        yours <- apcp
      }else if (input$newp == "Avg Price by Country" & input$peers == "Location" &input$bg =="Bottle") {
        yours <- apcl
      } else if (input$newp == "Avg Price by Variety" & input$peers == "All" &input$bg =="Bottle") {
        yours <- apv
      }else if (input$newp == "Avg Price by Variety" & input$peers == "Price" &input$bg =="Bottle") {
        yours <- apvp
      }else if (input$newp == "Avg Price by Variety" & input$peers == "Location" &input$bg =="Bottle") {
        yours <- apvl
      }else if(input$newp == "Avg Price by Region" & input$peers == "All" &input$bg =="Glass") {
        yours <- gar
      } else if (input$newp == "Avg Price by Region" & input$peers == "Price" &input$bg =="Glass") {
        yours <- garp
      } else if (input$newp == "Avg Price by Region" & input$peers == "Location" &input$bg =="Glass") {
        yours <- garl
      } else if (input$newp == "Avg Price by SubRegion" & input$peers == "All" &input$bg =="Glass") {
        yours <- gas
      } else if (input$newp == "Avg Price by SubRegion" & input$peers == "Price" &input$bg =="Glass") {
        yours <- gasp
      } else if (input$newp == "Avg Price by SubRegion" & input$peers == "Location" &input$bg =="Glass") {
        yours <- gasl
      } else if (input$newp == "Avg Price by Country" & input$peers == "All" &input$bg =="Glass") {
        yours <- gapc
      }else if (input$newp == "Avg Price by Country" & input$peers == "Price" &input$bg =="Glass") {
        yours <- gapcp
      }else if (input$newp == "Avg Price by Country" & input$peers == "Location" &input$bg =="Glass") {
        yours <- gapcl
      } else if (input$newp == "Avg Price by Variety" & input$peers == "All" &input$bg =="Glass") {
        yours <- gapv
      }else if (input$newp == "Avg Price by Variety" & input$peers == "Price" &input$bg =="Glass") {
        yours <- gapvp
      }else if (input$newp == "Avg Price by Variety" & input$peers == "Location" &input$bg =="Glass") {
        yours <- gapvl
      }
      
      if(identical(yours, ar)) { 
        yours %>%
          ggplot(aes(Region, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Region") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, arp)) {
        yours %>%
          ggplot(aes(Region, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Region") +
          theme(plot.title = element_text(size = rel(2)))     
      }else if (identical(yours, arl)) {
        yours %>%
          ggplot(aes(Region, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Region") +
          theme(plot.title = element_text(size = rel(2)))     
      }else if (identical(yours, as)) {
        yours %>%
          ggplot(aes(SubRegion, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))    
      }else if (identical(yours, asp)) {
        yours %>%
          ggplot(aes(SubRegion, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, asl)) {
        yours %>%
          ggplot(aes(SubRegion, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, apc)) {
        yours %>%
          ggplot(aes(Country, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Country") +
          theme(plot.title = element_text(size = rel(2)))    
      }else if (identical(yours, apcp)) {
        yours %>%
          ggplot(aes(Country, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Country") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, apcl)) {
        yours %>%
          ggplot(aes(Country, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Country") +
          theme(plot.title = element_text(size = rel(2)))   
      } else if (identical(yours, apv)) {
        yours %>%
          ggplot(aes(Variety, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Variety") +
          theme(plot.title = element_text(size = rel(2)))    
      }else if (identical(yours, apvp)) {
        yours %>%
          ggplot(aes(Variety, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Variety") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, apvl)) {
        yours %>%
          ggplot(aes(Variety, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Variety") +
          theme(plot.title = element_text(size = rel(2)))    
      }
      else if(identical(yours, gar)) { 
        yours %>%
          ggplot(aes(Region, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Region") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, garp)) {
        yours %>%
          ggplot(aes(Region, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Region") +
          theme(plot.title = element_text(size = rel(2)))     
      }else if (identical(yours, garl)) {
        yours %>%
          ggplot(aes(Region, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Region") +
          theme(plot.title = element_text(size = rel(2)))     
      }else if (identical(yours, gas)) {
        yours %>%
          ggplot(aes(SubRegion, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))    
      }else if (identical(yours, gasp)) {
        yours %>%
          ggplot(aes(SubRegion, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, gasl)) {
        yours %>%
          ggplot(aes(SubRegion, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by SubRegion") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, gapc)) {
        yours %>%
          ggplot(aes(Country, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Country") +
          theme(plot.title = element_text(size = rel(2)))    
      }else if (identical(yours, gapcp)) {
        yours %>%
          ggplot(aes(Country, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Country") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, gapcl)) {
        yours %>%
          ggplot(aes(Country, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Country") +
          theme(plot.title = element_text(size = rel(2)))   
      } else if (identical(yours, gapv)) {
        yours %>%
          ggplot(aes(Variety, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Variety") +
          theme(plot.title = element_text(size = rel(2)))    
      }else if (identical(yours, gapvp)) {
        yours %>%
          ggplot(aes(Variety, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge") +
          labs(title = "Average Price by Variety") +
          theme(plot.title = element_text(size = rel(2)))   
      }else if (identical(yours, gapvl)) {
        yours %>%
          ggplot(aes(Variety, Price, fill = Group)) +
          geom_bar(stat = "identity", position="dodge")+
          labs(title = "Average Price by Variety") +
          theme(plot.title = element_text(size = rel(2)))    
      }
      
      
    }) 
    
  })