##########################################################################
# Risk Explorer Shiny App - expert opinion
# Dwight Barry, PhD | Enterprise Analytics
# November 2015 - Version 0.9 - Proof of concept
# Oct 2018 - Version 1.3 
# Changes from 1.0: added data input and mix/max adjustment
# Changes from 1.1: substituted function for mc2d rpert,
#                   removed min values from inputs
# Changes from 1.2: fixed math error for negative min percentage values,
#                   made data reactive,
#                   styling and layout changes
##########################################################################


##### Global #####

# Load Packages
library(shiny)
library(ggplot2)
library(htmlTable)
library(mc2d)


##########################################################################

##### User Interface #####

ui = shinyUI(fluidPage(
  # Title
  titlePanel("Expert Opinion Risk Explorer"),

  sidebarLayout(
    
    #### Sidebar Panel ----

        sidebarPanel(
          
        ## Allow user to input betaPERT parameters ----
    
        # Enter lowest possible value
        numericInput(
            "low",
            label = "Enter Lowest Possible Value:",
            value = -10,
            step = 0.1
        ),

        # Enter most likely value (mode/maximum density)
        numericInput(
            "mode",
            label = "Enter Most Likely Value (Mode):",
            value = 10,
            step = 0.1
        ),

        # Enter highest possible value
        numericInput(
            "high",
            label = "Enter Highest Possible Value:",
            value = 25,
            step = 0.1
        ),

        # Produce empirical histogram/density plot
        numericInput(
            "bins",
            label = "Bin Width (0 for density only):",
            min = 0,
            value = 1,
            step = 1
        ),

        # User inputs for min/max percent adjustment
        numericInput(
            "minperc",
            label = "Minimum adjustment (percent below minimum, decimal):",
            min = 0,
            max = 1,
            value = 0,
            step = 0.05
        ),

        numericInput(
            "maxperc",
            label = "Maximum adjustment (percent above maximum, decimal):",
            min = 0,
            max = 1,
            value = 0,
            step = 0.05
        ),

        # User inputs for simulation reps
            numericInput(
            "reps",
            label = "Simulation Replications:",
            min = 1,
            value = 1000
        ),

      sliderInput(
        "gamma",
        label = HTML(
              "BetaPERT Distribution Uncertainty Parameter (&gamma;): <br>
              <small><i>4 is default PERT
              distribution</i></small>"),
        min = 1,
        max = 10,
        value = 4,
        step = 1
        ),
      
      br(),

      hr(),

      HTML("Contact: <a href='mailto:dwight.barry@seattlechildrens.org'>Dwight Barry, PhD</a>"),

      br()

      ),

    #### Main panel ----
    
    mainPanel(

        ## Plot output ----
        
        fluidRow(
            
            splitLayout(cellWidths = c("45%", "5%", "45%"), 
            # Simulation cdf plot
            plotOutput("cdf_plot", click = "plot_click"), 
            HTML(""),
            # Simulation histogram/density plot
            plotOutput("dist_plot"))
            
            ),
      
        # Click cue
        
        br(),
        
        hr(),
        
        HTML(
            
            "<b>Click on the <font color='darkblue'>CDF line</font> for a given value to obtain
            the probability estimate.</b>"
            
            ),
        
        hr(),
        
        # Show betaPERT values
        fluidRow(
            splitLayout(
                cellWidths = c("45%", "10%", "45%"),
                htmlOutput("info"),
                HTML(""),
                htmlOutput("paramstext"))
            ),

        hr()
        
      )

  )

))


##########################################################################

##### Server #####

server = shinyServer(function(input, output) {
    
    
    ## Create betapert simulation dataset ----
    mydata <- reactive({
    
        sim_df = data.frame(Value = rpert(
            
          input$reps,
          (input$low + (input$low * input$minperc)),
          input$mode,
          (input$high + (input$high * input$maxperc)),
          input$gamma))
    
        })


    ## Simulation distribution plot ----
    output$dist_plot = renderPlot({

        ggplot(mydata(), aes(x = Value), environment = environment()) +
            labs(title = "Simulation Distribution",
                 y = "Density / Count") +
            geom_histogram(
                aes(y = ..density..),
                binwidth = input$bins,
                col = "blue",
                fill = "blue",
                alpha = 0.2,
                na.rm = T) +
            geom_density(
                col = "darkblue",
                fill = "blue",
                alpha = 0.2,
                na.rm = T) +
            theme(
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank())

    })

    
    ## Simulation CDF plot ----
    output$cdf_plot = renderPlot({

        ggplot(mydata(), aes(Value), environment = environment()) +
            labs(title = "Simulation Distribution",
                 y = "Probability") +
            stat_ecdf(lwd = 1.5, na.rm = T, color = "darkblue") +
            theme(
                axis.ticks.y = element_blank(), 
                axis.text.y = element_blank())

    })

    
    ## Text output ----
    
    # Click output
    output$info = renderText({

        req(input$plot_click)
            paste0(
              "<i>The probability of obtaining a value less than<br>
              or equal to <b><font color='darkblue'>",
              txtRound(input$plot_click$x, 1),
              "</font></b> is about <b><font color='darkblue'>",
              txtRound(input$plot_click$y, 2),
              "</font></b>.</i>")

    })
    
    # Details on betaPERT parameters
    output$paramstext = renderText({

        paste0(
          "<b>BetaPERT Inputs:</b>
          <br><i>Simulation mode: ",
          txtRound(input$mode, 1),
          ".<br>Simulation minimum: ",
          txtRound(input$low + (input$low * input$minperc), 1),
          "<br>Simulation maximum: ",
          txtRound(input$high + (input$high * input$maxperc), 1),
          "</i>."
        )

    })

    
})


##########################################################################

##### App #####

## Return the Shiny app ----
shinyApp(ui = ui, server = server)

##########################################################################
