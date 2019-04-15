#Savings Calculator Application

library(shiny)

ui <- fluidPage(
   
  #Title of App and sliders/facet options
  titlePanel("Savings Calculator"),
   
    fluidRow(
      
      column( 4,
                 
          sliderInput("initial_amount",
                      "Initial Amount Saved",
                      min = 0,
                      max = 100000,
                      value = 1000,
                      step = 500,
                      pre = "$"),
    
          sliderInput("contribution",
                      "Annual Contribution",
                      min = 0,
                      max = 50000,
                      value = 2000,
                      step = 500,
                      pre = "$")),
         
     column(4, 
                
         sliderInput("return_rate",
                     "Return Rate",
                     min = 0,
                     max = 20,
                     value = 5,
                     step = .1,
                     post = '%'),
         
         sliderInput("growth_rate",
                     "Growth Rate",
                     min = 0,
                     max = 20,
                     value = 2,
                     step = .1,
                     post = '%')),
         
     column(4,
         
         sliderInput("years",
                     "Years",
                     min = 0,
                     max = 50,
                     value = 20,
                     step = 1),
         
         selectInput("facet",
                     "Faceted Graph?",
                     choices = list("Yes" = 1, "No" = 0), selected = 0)
         
         )
      
      ),
        
      mainPanel(
         plotOutput("valuesplot", width = "150%"),
         tableOutput("values")
         
      )
   
  )

server <- function(input, output) {
    
    library(ggplot2)
    library(tidyr)
    
    #Functions to Calculate 3 modes of savings
    future_value <- function(amount, rate, years){
        return (amount*(1+rate)^years)
    }
    
    annuity <- function(contrib, rate, years){
        return((contrib/rate)*(((1+rate)^years)-1))
    }
    
    growing_annuity <- function(contrib, rate, growth, years){
        return((contrib/(rate-growth))*(((1+rate)^years)-((1+growth)^years)))
    }
  
    no_contrib_reg <- c()
    fixed_contrib_reg <- c()
    growing_contrib_reg <- c()
  
    #Without Gathering to allow for facetting
    sliderValues0 <- reactive({
      
        return_rate <- input$return_rate/100
        growth_rate <- input$growth_rate/100
    
    for (i in c(0:input$years)){
      
        no_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i)
        fixed_contrib_reg[i+1] <-future_value(input$initial_amount,return_rate,i) + annuity(input$contribution, return_rate, i)
        growing_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i) + growing_annuity(input$contribution, return_rate, growth_rate, i)
    
    }
    
    data.frame(Year = c(0:input$years), No_Contribution = no_contrib_reg, Fixed_Contribution = fixed_contrib_reg, Growing_Contribution = growing_contrib_reg, stringsAsFactors = FALSE)

  })
  
    #With Gathering to allow for facetting
    sliderValues1 <- reactive({
      
        return_rate <- input$return_rate/100
        growth_rate <- input$growth_rate/100
        
        for (i in c(0:input$years)){
          
            no_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i)
            fixed_contrib_reg[i+1] <-future_value(input$initial_amount,return_rate,i) + annuity(input$contribution, return_rate, i)
            growing_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i) + growing_annuity(input$contribution, return_rate, growth_rate, i)
            
        }
      
        df <- data.frame(Year = c(0:input$years), No_Contribution = no_contrib_reg, Fixed_Contribution = fixed_contrib_reg, Growing_Contribution = growing_contrib_reg, stringsAsFactors = FALSE)
        gather(df, key="Type", value = "Savings", c("No_Contribution", "Fixed_Contribution", "Growing_Contribution"))
    
  })
  
  #Data table of values
  output$values <- renderTable({sliderValues0()})
  
    #Plot without facetting
    output$valuesplot <- renderPlot({ 
      if (input$facet==0){
        
          ggplot(sliderValues0()) +
              geom_line(aes(x = Year, y = No_Contribution, color = "#ff0000")) + geom_point(aes(x = Year, y = No_Contribution, color = "#ff0000")) + 
              geom_line(aes(x = Year, y = Fixed_Contribution, color = "#00ff00")) + geom_point(aes(x = Year, y = Fixed_Contribution, color = "#00ff00")) + 
              geom_line(aes(x = Year, y = Growing_Contribution, color = "#0000ff")) + geom_point(aes(x = Year, y = Growing_Contribution, color = "#0000ff")) + 
              labs(title = 'Three Types of Investing') +
              xlab('Year') +
              ylab('Investment (in $)') +
              scale_color_discrete(name = "Modality", labels = c('No Contribution', 'Fixed Contribution', 'Growing Contribution')) +
              theme_bw()
        
              }
        
    else{
      
        #Plot with facetting
        ggplot(sliderValues1()) +
            geom_line(aes(x = Year, y = Savings, color = Type)) + geom_point(aes(x = Year, y = Savings, color = Type)) + geom_area(aes(x = Year, y = Savings, fill = Type)) +
            labs(title = 'Different Types of Investing') + 
            facet_wrap (.~Type) +
            xlab('Year') +
            ylab('Investment (in $)') +
            scale_color_discrete(name = "Modality", labels = c('Fixed Contribution',  'Growing Contribution', 'No Contribution')) +
            scale_fill_discrete(guide=FALSE) +
            theme_bw()
            
            }

  })
    
}

shinyApp(ui = ui, server = server)

