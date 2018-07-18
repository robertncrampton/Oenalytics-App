##Dependencies

require(shiny)
require(ggvis)
require(data.table)
require(ggplot2)
require(tidyverse)
require(shinydashboard)
require(openssl)

##Dashboard Page, Header & Menus ####   
ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "There are hot bottles not on your menu"
                               )),
                  dropdownMenu(type = "notifications",
                               badgeStatus = "warning",
                               notificationItem(
                                 text = "Order More Cab Franc '09",
                                 icon("warning")
                                 
                               ),
                               notificationItem(
                                 text = "Discount your Syrah '10",
                                 icon("users")
                               )
                  )
                  
  ),
  ##Dashboard Sidebar####                           
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Comparative Selection", tabName= "comparatives", icon = icon("th")),
      menuItem("Compartive Pricing", tabName = "comparativepnew", icon = icon("th")),
      menuItem("Pricing & Inventory", tabName = "table", icon = icon("th")),
      menuItem("Comparative Pricing Histogram", tabName = "hista", icon = icon("th")),
      menuItem("Pricing & Profitability", tabName = "pricing", icon = icon("th")),
      menuItem("Sales Breakdown", tabName = "sales", icon = icon("th")),
      menuItem("Sample Optimization", tabName = "opti", icon = icon("th")) 
      
      
    )
  ),
  ##Dashboard Body####
  dashboardBody(
    tabItems(
      ##Dashboard Tab####    
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("nwines"),
                valueBoxOutput("wines"),
                valueBoxOutput("profit"),
                box(
                  title = "Top Selling Bottles", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("other")
                ),
                box(
                  title = "Space to Profit", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("spaceprofit")
                ),
                box(
                  title = "Comparative Pricing", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("cp")
                ),
                box(
                  title = "Comparative Pricing", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("vp")
                ),
                box(
                  title = "Comparative Selections", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("nv")
                ),
                infoBoxOutput("low", width = 6),
                infoBoxOutput("high", width = 6)
              )
      ),
      ##Comparative Selection Tab####
      tabItem(tabName = "comparatives",
              fluidRow(
                box(
                  selectInput("var1", 
                              label = "Categorization",
                              choices = c("Number by Country",
                                          "Number by Variety",
                                          "Number by Region",
                                          "Number by SubRegion"
                              ),
                              selected = "Number by Region"), 
                  width = 6
                ),
                box(
                  selectInput("comps", 
                              label = "Peer Group Comparison",
                              choices = c("Location",
                                          "Price",
                                          "All"
                              ),
                              selected = "All"),
                  width =6
                ),
                box(
                  plotOutput('plot1'), width = 12
                )
              )      
      ),
      ##Table Data Tab####
      tabItem(tabName = "table",
              fluidRow(
                box(
                  helpText("Display information about the selected variable"),
                  selectInput("sum", 
                              label = "Choose a variable to show",
                              choices = c("Pricing",
                                          "Inventory",
                                          "Year-to-date Profit",
                                          "Low Inventory, Top Selling Wine", 
                                          "High Inventory, Scarcely Sold Wine",
                                          "Top Selling Glasses",
                                          "Top Selling Bottles Not On Your Menu"
                              ),
                              selected = "Pricing"), 
                  width = 12
                ),
                box(
                  dataTableOutput('table'), width = 12
                )
              )
      ),
      ##Histogram Data####
      tabItem(tabName = "hista",
              fluidRow(
                box(
                  sliderInput("range", 
                              label = "Range of interest:",
                              min = 0, max = 1000, value = c(0, 200)), 
                  width = 6
                ),
                box(
                  sliderInput("bins", 
                              label = "Box Size:",
                              min = 1, max = 20, value = c(5)), 
                  width = 6
                ),        
                box(
                  plotOutput('hist'), width = 12
                )
              )
      ),
      ##Pricing Tab####
      tabItem(tabName = "pricing",
              fluidRow(
                box(
                  sliderInput("a", 
                              label = "Range of interest:",
                              min = 0, max = 1000, value = c(0, 250)),
                  width = 12
                ),
                box(
                  plotOutput('curve',height = 350,hover = hoverOpts(id ="plot_hover")),
                  verbatimTextOutput("hover_info"),
                  width = 12
                )
              )
      ),
      ##Sales Details####
      tabItem(tabName = "sales",
              fluidRow(
                box(
                  selectInput("salesinfo", 
                              label = "Choose a variable to display",
                              choices = c("Group by Day",
                                          "Group by Selection",
                                          "Profit to Space Comparison"
                              ),
                              selected = "Group by Day"), 
                  width = 6
                ),
                box(
                  selectInput("grouping", 
                              label = "Choose a profit grouping",
                              choices = c("Region",
                                          "SubRegion",
                                          "Variety"
                              ),
                              selected = "Variety"), 
                  width = 6
                ),
                box(
                  plotOutput('sales'), width = 12
                )
              )
      ),
      ##Optimization Example####
      tabItem(tabName = "opti",
              fluidRow(
                box(
                  sliderInput("updown", 
                              label = "Even Price Shift",
                              min = -20, max = 20, value = c(0), step = 5)
                  
                ),
                box(
                  "Total Profits:",
                  textOutput('money')
                ),
                box(
                  plotOutput('optimization'), width =12
                )
              )
      ), 
      ##New Comparative Pricing Tab####
      tabItem(tabName = "comparativepnew",
              fluidRow(
                box(
                  selectInput("newp", 
                              label = "Choose a categorization",
                              choices = c("Avg Price by Region",
                                          "Avg Price by SubRegion", 
                                          "Avg Price by Country",
                                          "Avg Price by Variety"
                              ),
                              selected = "Avg Price by Region"),
                  width =4
                  
                ),
                box(
                  selectInput("peers", 
                              label = "Peer Group of Restaurants",
                              choices = c("Location",
                                          "Price",
                                          "All"
                              ),
                              selected = "All"),
                  width =4
                ),
                box(
                  selectInput("bg", 
                              label = "Bottle or Glass",
                              choices = c("Bottle",
                                          "Glass"
                              ),
                              selected = "Bottles"),
                  width =4
                ),
                box(plotOutput('newplot'), width = 12
                )
                
              )
      )
      
      
      
      
      
    )##Tab Items
  )##Dashboard Page
)           


