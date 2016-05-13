#############################################################
# Risk Explorer Shiny App
# Dwight Barry, PhD | Enterprise Analytics
# November 2015 - Version 0.9 - Proof of concept
# April 2016 - Version 1.1b - no data available
# Changes from 1.0: added data input and mix/max adjustment
#############################################################


##### Global #####

# Load Packages
library(shiny)
library(ggplot2)
library(htmlTable)


# Function for modified BetaPERT distribution
# Written by J. Heller & G.T. Innocent, University of Glasgow
# Adapted from Vose (2000), pp 170-171.
# in: http://theses.gla.ac.uk/2522/1/2009HellerPhD.pd
rmodbetapert = function(n, min, Mode, max, gamma) {
    if (Mode == (min + max) / 2) {
        Mode = Mode + 0.001
    }
    if ((2 * Mode - min - max) == 0) {
        Mode = Mode + 0.001
    }
    mean = (min + gamma * Mode + max) / (gamma + 2)
    if ((Mode - mean) == 0) {
        Mode = Mode + 0.001
    }
    if ((mean - min) == 0) {
        mean = mean + 0.001
    }
    beta.part = (max - mean) / (mean - min)
    if (beta.part == 0) {
        beta.part = 0.001
    }
    alpha = ((mean - min) * (2 * Mode - min - max)) / 
        ((Mode - mean) * (max - min))
    beta = alpha * beta.part
    value = (rbeta(n, alpha, beta) * (max - min) + min)
}


#######################################################

##### User Interface #####

ui = shinyUI(fluidPage(
    # Title
    titlePanel("Expert Opinion Risk Explorer"),
    
    sidebarLayout(
        # Sidebar Panel
        
        sidebarPanel(
            # Allow user to input betaPERT parameters
            
            # Enter lowest possible value
            numericInput(
                "low",
                label = "Enter Lowest Possible Value:",
                min = 0,
                value = 0,
                step = 0.1
            ),
            
            # Enter most likely value (mode/maximum density)
            numericInput(
                "mode",
                label = "Enter Most Likely Value (Mode):",
                min = 0,
                value = 10,
                step = 0.1
            ),
            
            # Enter highest possible value
            numericInput(
                "high",
                label = "Enter Highest Possible Value:",
                min = 0,
                value = 25,
                step = 0.1
            ),
            
            # Produce empirical histogram/density plot
            numericInput(
                "bins",
                label = "Bin Width (0 for density only):",
                min = 0,
                value = 0.1,
                step = 0.1
            ),
            
            # User inputs for min/max percent adjustment
            numericInput(
                "minperc",
                label = "Minimum adjustment (percent below minimum):",
                min = 0,
                max = 1,
                value = 0
            ),
            
            numericInput(
                "maxperc",
                label = "Maximum adjustment (percent above maximum):",
                min = 0,
                max = 1,
                value = 0
            ),
            
            # Show betaPERT values
            htmlOutput("maxdens"),
            
            br(),
            
            # User inputs for simulation
            numericInput(
                "reps",
                label = "Simulation Replications:",
                min = 1,
                max = 100000,
                value = 10000
            ),
            
            sliderInput(
                "gamma",
                label = HTML(
                    "BetaPERT Distribution Uncertainty Parameter (&gamma;): <br>
                    <small><i>4 is default PERT
                    distribution</i></small>"
                ),
                min = 1,
                max = 10,
                value = 4,
                step = 1
                )
            
            ),
        
        # Main panel
        mainPanel(
            
            # Simulation histogram/density plot
            plotOutput(outputId = "distPlot2"),
            
            # Output of simulation cdf plot click
            HTML(
                "<br><b>Click on the CDF line for a given rate value to obtain
                the probability estimate.</b><br>"
            ),
            
            br(),
            
            # Simulation cdf plot
            plotOutput(outputId = "distPlot3", click = "plot_click"),
            
            # Click output results
            htmlOutput("info")
            
            )
        
    )
    
))


#######################################################

##### Server #####

server = shinyServer(function(input, output) {
    
    # Simulation distribution plot
    output$distPlot2 = renderPlot({
        Rate = c(input$low, input$high, input$mode)
        df = data.frame(Rate)
        
        bob = data.frame(Rate = rmodbetapert(
            input$reps,
            (min(df$Rate) - min(df$Rate) * input$minperc),
            input$mode,
            (max(df$Rate) + max(df$Rate) * input$maxperc),
            input$gamma))
        
        ggplot(bob, aes(Rate)) +
            ggtitle("Simulation Distribution") +
            ylab("Density / Count") +
            geom_histogram(
                aes(y = ..density..),
                binwidth = input$bins,
                col = "blue",
                fill = "blue",
                alpha = 0.2,
                na.rm = T) +
            xlim((min(df$Rate) - min(df$Rate) * input$minperc),
                 (max(df$Rate) + max(df$Rate) * input$maxperc)) +
            geom_density(
                col = "blue",
                fill = "blue",
                alpha = 0.2,
                na.rm = T) +
            theme(
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank())
    })
    
    # Simulation CDF plot
    output$distPlot3 = renderPlot({
        Rate = c(input$low, input$high, input$mode)
        df = data.frame(Rate)
        
        bob = data.frame(Rate = rmodbetapert(
            input$reps,
            (min(df$Rate) - min(df$Rate) * input$minperc),
            input$mode,
            (max(df$Rate) + max(df$Rate) * input$maxperc),
            input$gamma
        ))
        
        ggplot(bob, aes(Rate)) +
            ggtitle("Simulation CDF") +
            ylab("Probability") +
            xlim((min(df$Rate) - min(df$Rate) * input$minperc),
                 (max(df$Rate) + max(df$Rate) * input$maxperc)) +
            stat_ecdf(lwd = 2, na.rm = T) +
            theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
        
    })
    
    # Plot-click result for simulation
    output$info = renderText({
        paste0(
            "<i>The probability of obtaining a rate less than
            or equal to ",
            txtRound(input$plot_click$x, 1),
            " is about <b>",
            txtRound(input$plot_click$y, 2),
            "</b>.</i>"
        )
        
    })
    
    # Details on betaPERT parameters
    output$maxdens = renderText({
        Rate = c(input$low, input$high, input$mode)
        df = data.frame(Rate)
        
        bob = data.frame(Rate = rmodbetapert(
            input$reps,
            (min(df$Rate) - min(df$Rate) * input$minperc),
            input$mode,
            (max(df$Rate) + max(df$Rate) * input$maxperc),
            input$gamma
        ))
        
        
        paste0(
            "<b>BetaPERT Inputs:</b><br><i>Simulation mode: ",
            txtRound(input$mode, 1),
            ".<br>Simulation minimum: ",
            txtRound((min(df$Rate) - min(df$Rate) * input$minperc), 1),
            "<br>Simulation maximum: ",
            txtRound(max(df$Rate) + max(df$Rate) * input$maxperc, 1),
            "</i>."
        )
        
    })
    
})


#######################################################

##### App #####

# Return the Shiny app
shinyApp(ui = ui, server = server)

#######################################################
