# Script to make real estate investing calculator to analyze deals
# Author: Kevin Zolea
# Date: 4/24/20

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(rmarkdown)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)
library(DT)
options(scipen = 999)
# Define UI for application 
shinyUI(navbarPage(theme = shinytheme('superhero'),
                   tags$b("Rental Property Calculator"),
                   windowTitle = "Rental Calc",
###### Here : insert shinydashboard dependencies ######
                   header = tagList(
                     useShinydashboard()
                   ),
 #######################################################
  tabPanel("Property Calculations",
           tags$head(
             includeCSS("www/styles.css")),
           fluidRow(
             column(width= 3,
              div(style= "text-align:center",h3("Property Info"))),
             column(width = 3,
             textInput("address","Address:",placeholder = "35 Wilson Ave Parlin NJ 08859")),
             column(width = 3,
            textInput("descp","Description:",placeholder = "This duplex style colonial has all the 
                      amenities you can ask for and is within walking distance to the 
                      downtown shops and restaurants.")),
            column(width =3,
            fileInput("upload", "Upload image:", accept = c("image/png",
                                                            "image/jpeg",
                                                            "image/pdf")))),hr(),
  column(width = 3,
         h3("Purchase Info"),
         numericInputIcon("purchase",
                   "Purchase Price:",170000,width = 200,
                   icon = icon("dollar")),
       numericInputIcon("closing","Purchase closing costs:",6000,width = 200,
                        icon = icon("dollar")),
       numericInputIcon("rehab_costs","Estimated Rehab Costs:",10000,width = 200,
                        icon = icon("dollar")),
       numericInputIcon("arv","Estimated ARV:",175000,width = 200,
                        icon = icon("dollar")),
       numericInputIcon("appreciation","Estimated Annual Appreciation:",3,width = 200,
                        icon= list(NULL, icon("percent"))),
       numericInputIcon("rent_increase","Annual Rent Increase:",2,width = 200,
                        icon= list(NULL, icon("percent"))),
    h3("Mortgage Details"),
    numericInputIcon(
      inputId = "down",
      label = "Down Payment:",
      value = 5,
      width = 200,
      icon= list(NULL, icon("percent"))),
    numericInputIcon("loan","Loan Amount:",NULL,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("rate","Interest Rate:",3.625,width = 200,
                     icon= list(NULL, icon("percent"))),
    numericInputIcon("pmi","PMI Rate:",1,width = 200,
                     icon= list(NULL, icon("percent"))),
    numericInputIcon("loan_term","Loan Term in Years:",30,width = 200),
    numericInputIcon("payments_per","Payments Made Per Year:",12,width = 200)),
  column(width = 3,
    h3("Rental Income"),
    numericInputIcon("unit1","Unit 1:",1200,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("unit2","Unit 2:",1000,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("unit3","Unit 3:",0,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("unit4","Unit 4:",0,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("unit5","Unit 5:",0,width = 200,
                     icon = icon("dollar")),
    "Subtotal Estimated Rent:",
    textOutput("output"),br(),br(),br(),
    infoBoxOutput("dp",width = 12),
    infoBoxOutput("coc",width = 12)),
  column(width = 3,
    h3("Other Income"),
    numericInputIcon("pet","Pet Rent:",0,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("garage","Garage Parking:",0,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("storage","Extra Storage:",0,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("utility_fee","Utility Fee:",0,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("other","Other:",0,width = 200,
                     icon = icon("dollar")),
    "Subtotal Other Income:",
    textOutput("output2"),
    "Total Monthly Income:",
    textOutput("output3"),br(),
    infoBoxOutput("all_in",width = 12),
    infoBoxOutput("cashflow",width = 12)),
   # downloadBttn(
   #   'downloadReport',
   #   label = "Download Report",
   #   style = "unite",
   #   color = "default",
   #   size = "lg",
   #   block = FALSE,
   #   no_outline = TRUE)),
  column(width=3,
    h3("Property Expenses"),
    numericInputIcon("princ","Principal:",NULL,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("inter","Interest:",450,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("taxes","Estimated Monthly Taxes:",450,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("insurance","Estimated Monthly Insurance:",80,width = 200,
                     icon = icon("dollar")),
    numericInputIcon("pmi_dollar","PMI:",NULL,width = 200,
                     icon= icon("dollar")),
    actionBttn("extra_ex",label = "Add Expense:"),
    #uiOutput("ui"),
    br(),
    "Subtotal Monthly Payment:",
    textOutput("debt_payment"),br(),
    em("*Repairs & Maintenance, vacancy, capital expenditures, 
    and management fees are expressed as percentages 
    of total monthly income."),br(),br(),
    numericInputIcon("repairs_mait","Repairs & Maintenance:",7,width = 200,
                     icon= list(NULL, icon("percent"))),
    numericInputIcon("vacancy","Vacancy:",7,width = 200,
                     icon= list(NULL, icon("percent"))),
    numericInputIcon("capex","Capital Expenditures:",7,width = 200,
                     icon= list(NULL, icon("percent"))),
    numericInputIcon("management","Management Fees:",10,width = 200,
                     icon= list(NULL, icon("percent"))),
    "Subtotal Reserves:",
    textOutput("output_reserves"),br(),
    "Total Monthly Expenses:",
  textOutput("total_expenses"))),
tabPanel("Mortgage Calculations",
         # Sidebar layout with input and output definitions ----
         sidebarLayout(
           
           # Sidebar to demonstrate various slider options ----
           sidebarPanel(
             
             # Input: Simple integer interval ----
             numericInput("principal", "Principal (loan amount)", 200000, min = 0, step = 1000),
             hr(),
             numericInput("interest", "Interest rate (in %)", 2, min = 0, max = 100, step = 0.01),
             hr(),
             sliderInput("length", "Duration of the loan (in years)",
                         min = 0,
                         max = 30,
                         value = 25,
                         step = 1
             ),
             hr(),
             checkboxInput("plot", "Display plot?", TRUE)
           ),
           
           # Main panel for displaying outputs ----
           mainPanel(
             
             # Output: Table summarizing the values entered ----
             uiOutput("text"),
             br(),
             plotOutput("distPlot"),
             br(),
             DT::dataTableOutput("tbl")
            
           )
         )
#  tabPanel("Plots",
#    fluidRow(column(6,
#    plotOutput("plot1",6),
#    plotOutput("plot2")%>%withSpinner(type = 5, color = "red"))
#    
#  ))
)))



