# Load Packages
library(shiny)
library(rmarkdown)
library(ggplot2)

# Define server logic 
shinyServer(function(input, output, session) {
  
  # Updates loan amount based on purchase price input
  observeEvent(c(input$purchase, input$down), {
    loan_amount <- input$purchase * (1 - input$down / 100)
    updateNumericInput(session, "loan", value = loan_amount)
  })
  
  # Calculates interest amount
  observeEvent(c(input$loan, input$loan_term, input$rate, input$payments_per), {
    interest_dollar <- round((input$rate / 100) / input$payments_per * input$loan)
    updateNumericInput(session, "inter", value = interest_dollar)
  })
})
# Principal amount
observeEvent(c(input$loan, input$loan_term, input$rate, input$payments_per), {
  # Get input values and calculate estimated monthly payment
  p <- input$loan
  j <- input$rate / (12 * 100)
  L <- input$loan_term
  N <- L * 12
  estimated_pay <- round(p * (j / (1 - (1 + j)^-N)), digits = 2)
  
  # Calculate interest and principal amounts
  interest_dollar <- round((input$rate / 100) / input$payments_per * input$loan)
  principal_dollar <- round(estimated_pay - interest_dollar)
  
  # Update principal input field
  updateNumericInput(session, "princ", value = principal_dollar)
})

# Gets PMI amount based on % of PMI rate  
observeEvent(c(input$pmi, input$loan), {
  # Calculate PMI amount and update input field
  pmi_show <- round(input$loan * (input$pmi / 100 / 12), digits = 2)
  updateNumericInput(session, "pmi_dollar", value = pmi_show)
})
# Gets rent total 
  output$output <- renderText({
    paste("$",input$unit1+input$unit2+input$unit3+input$unit4+input$unit5)
  })
# Gets other income total 
  output$output2 <- renderText({
    paste("$",input$pet+input$garage+input$storage+input$utility_fee+input$other)
  })
# Gets total gross income   
  output$output3 <- renderText({
    req(input$unit1)
    req(input$unit2)
    req(input$unit3)
    req(input$unit4)
    req(input$unit5)
    req(input$pet)
    req(input$garage)
    req(input$storage)
    req(input$utility_fee)
    req(input$other)

   paste("$",total_rent<-input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
           input$pet+input$garage+input$storage+input$utility_fee+input$other)
    
  })
# Estimates monthly mortgage payment  
  output$debt_payment<-renderText({
    req(input$loan)
    req(input$rate)
    req(input$loan_term)
    req(input$taxes)
    req(input$insurance)
    req(input$pmi_dollar)
    
    p= input$loan
    j=input$rate/(12*100)
    L=input$loan_term
    N= L*12
    
    paste("$",round(round(p*(j/(1-(1+j)^-N)),digits = 2)+input$taxes+input$insurance+input$pmi_dollar))

    })
# Gets total reserves
  output$output_reserves <- renderText({
    paste("$",(input$repairs_mait/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$vacancy/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$capex/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$management/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other))
    
  })
  
# Reactive UI based on action button 
  observeEvent(input$extra_ex, {
    insertUI(
      selector = "#extra_ex",
      where = "beforeBegin",
      ui = numericInput("txt", "Other",input$extra_ex,
                     0)
    )
  })
# Gets total expenses  
  output$total_expenses<-renderText({
    req(input$loan)
    req(input$rate)
    req(input$loan_term)
    req(input$taxes)
    req(input$insurance)
    req(input$pmi_dollar)
    
    p= input$loan
    j=input$rate/(12*100)
    L=input$loan_term
    N= L*12
    
   paste("$",round(round(p*(j/(1-(1+j)^-N)),digits = 2)+input$taxes+input$insurance+input$pmi_dollar+
      (input$repairs_mait/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$vacancy/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$capex/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$management/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)))
  })
  
# Creates box showing cashflow
  output$cashflow <- renderInfoBox({
    req(input$loan)
    req(input$rate)
    req(input$loan_term)
    req(input$taxes)
    req(input$insurance)
    req(input$pmi_dollar)
     
    p= input$loan
    j=input$rate/(12*100)
    L=input$loan_term
    N= L*12
    
    te<-round(p*(j/(1-(1+j)^-N)),digits = 2)+input$taxes+input$insurance+input$pmi_dollar+
      (input$repairs_mait/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$vacancy/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+                                                                                                                  input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$capex/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
         input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$management/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
         input$pet+input$garage+input$storage+input$utility_fee+input$other)
  
    ti<- input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other
    
    cf<-round((ti-te))
    if(cf>0){
     
    infoBox("Cash Flow:",value = paste0("$",cf),color = "green",
            fill = T,icon =shiny::icon("hand-holding-usd"))}
    
    else{
    
      infoBox("Cash Flow:",value = paste0("$",cf),color = "red",
              fill = T,icon =shiny::icon("hand-holding-usd"))
    }
    
  })
# Box to show down payment 
  output$dp<-renderInfoBox({
    
    dp<-round(input$purchase*(input$down/100))
    
    infoBox("Down Payment:",value = paste0("$",dp),color = "blue",
            fill = T,icon =shiny::icon("coins"))
  })
# Box to show initial investment 
  output$all_in<-renderInfoBox({
    
    dp<-round(input$purchase*(input$down/100))
    
    inital_investment<-round(dp+input$closing+input$rehab_costs)
    
    infoBox("Initial Investment:",value = paste0("$",inital_investment),
            color = "blue",icon =shiny::icon("coins"),
            fill = T)
  })
# Box to show cash on cash return on investment
  output$coc <- renderInfoBox({
    p= input$loan
    j=input$rate/(12*100)
    L=input$loan_term
    N= L*12
    
    te<-round(p*(j/(1-(1+j)^-N)),digits = 2)+input$taxes+input$insurance+input$pmi_dollar+
      (input$repairs_mait/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
                                  input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$vacancy/100)*(input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+                                                                                                                  input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$capex/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
         input$pet+input$garage+input$storage+input$utility_fee+input$other)+(input$management/100)*
      (input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
         input$pet+input$garage+input$storage+input$utility_fee+input$other)
    
    ti<-input$unit1+input$unit2+input$unit3+input$unit4+input$unit5+
      input$pet+input$garage+input$storage+input$utility_fee+input$other
    
    dp<-input$purchase*(input$down/100)
    
    total_cash<-round(input$closing+input$rehab_costs+dp)
    
    annual_cashflow<-round((ti-te)*12)
    
    cashoncash<-round((annual_cashflow/total_cash)*100)
    if(cashoncash>0){
    infoBox("CoC RoI:",value = paste0(cashoncash,"%"),color = "green",
            fill = T)}
    
    else{
      infoBox("CoC RoI:",value = paste0(cashoncash,"%"),color = "red",
              fill = T)
    }
  })
# Creates download button to download report from calculations,etc.
  #file1 <- reactive({gsub("\\\\", "/", input$upload$datapath)})
  
 # output$downloadReport <- downloadHandler(
 #   filename = 
 #     paste("report_file","file",".html",sep=""),
 #   content = function(file) {
 #     tempReport<-file.path(tempdir(),"report_file.Rmd")
 #     file.copy('report_file.Rmd', tempReport,overwrite = TRUE)
 #     ##Parameters to pass
 #     params <- list(text1=input$descp,pic1=file1(),text2=input$address,
 #                    Unit1=input$unit1,Unit2=input$unit2,Unit3=input$unit3,
 #                    Unit4=input$unit4,Unit5=input$unit5,Pet=input$pet,Garage=input$garage,
 #                    Storage=input$storage,Utility=input$utility_fee,Other=input$other,
 #                    Principal = input$princ,Interest = input$inter,Taxes=input$taxes,
 #                    Insurance = input$insurance,PMI=input$pmi_dollar,Repairs = input$repairs_mait,
 #                    Vacancy = input$vacancy,Capex=input$capex, Mait=input$management,
 #                    Loan = input$loan, Rate = input$rate,Loan_term=input$loan_term,
 #                    Purchase = input$purchase,Down = input$down,Closing=input$closing,
 #                    Rehab = input$rehab_costs)
 #     rmarkdown::render(tempReport,output_file=file, params=params,
 #                       envir = new.env(parent = globalenv()))
 #   }  
 # )
  
  mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- P * J / (1 - (1 + J)^(-N))
    monthPay <<- M
    # Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
      
      ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "Payment") +
        scale_y_continuous(labels = percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top")
    }
  }
  
  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<h3>", "Summary", "</h3>",
      "Principal (loan amount): ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "Annual interest rate: ", input$interest, "%",
      "<br>",
      "Term: ", input$length, " years (", input$length * 12, " months)",
      "<br>",
      "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  
  output$distPlot <- renderPlot({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
  })
  
  # Data output
  output$tbl <- DT::renderDataTable({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
                              extensions = "Buttons",
                              options = list(
                                lengthChange = TRUE,
                                dom = "Blrtip",
                                buttons = c("copy", "csv", "excel", "pdf", "print"),
                                lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
                              ),
                              rownames = FALSE)%>%
      formatStyle('Month',
                  target = "row",
                  backgroundColor = 'white', fontWeight = 'bold')%>%
      formatStyle(columns = colnames(aDFmonth), color = "black")%>%
    formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = ",")
  })
  
  #output$plot1<-renderPlot({
  #  df<-data.frame("Year"=seq(input$loan_term),"Rent"=(input$unit1*(input$rent_increase/100))+input$unit*input$loan_term)
 #   print(df)
#    ggplot(data=df)+geom_point(aes(x=Year,y=Rent))
#  })
  
#  output$plot2<-renderPlot({
#    # Create Data
#    data <- data.frame(
#      group=c("PITI","CapEx","Repairs/Maintenance",
#              "Vacancy","Management","PMI"),
#      value=c((input$taxes+input$insurance+input$princ+input$inter),input$capex,
#              input$repairs_mait,input$vacancy,input$management,input$pmi_dollar)
#    )
#    
#    # Basic piechart
#    ggplot(data, aes(x="", y=value, fill=group)) +
#      geom_bar(stat="identity", width=1) +
#      coord_polar("y", start=0)+
#      theme_void()+
#      labs(fill = "Expenses")+
#      scale_fill_brewer(palette="Set1")
    
  #})
  
})




