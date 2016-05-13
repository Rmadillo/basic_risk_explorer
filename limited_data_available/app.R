#############################################################
# Risk Explorer Shiny App
# Dwight Barry, PhD | Enterprise Analytics
# November 2015 - Version 0.9 - Proof of concept
# April 2016 - Version 1.1
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
    titlePanel("Risk Explorer"),
    
    sidebarLayout(
        # Sidebar Panel
        
        sidebarPanel(
            # Allow user to input data
            textInput(
                'vec1',
                'Enter rate data (comma delimited)',
                "2.4, 1.7, 2.3, 2.4, 2.2, 2.6"
            ),
            
            # Produce empirical histogram/density plot
            plotOutput(outputId = "distPlot", height = 265),
            numericInput(
                "bins",
                label = "Bin Width (0 for density only):",
                min = 0,
                value = 0.1,
                step = 0.1
            ),
            
            hr(),
            
            # User inputs for min/max percent adjustment
            numericInput(
                "minperc",
                label = "Minimum adjustment (percent below
                empirical minimum):",
                min = 0,
                max = 1,
                value = 0.15
        ),
        numericInput(
            "maxperc",
            label = "Maximum adjustment (percent above
            empirical maximum):",
            min = 0,
            max = 1,
            value = 0.05
        ),
        
        # Show betaPERT values
        htmlOutput("maxdens"),
        
        hr(),
        
        # User inputs for simulation
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
    # Empirical distribution plot
    output$distPlot = renderPlot({
        Rate = as.numeric(unlist(strsplit(input$vec1, ",")))
        df = data.frame(Rate)
        
        ggplot(df, aes(Rate)) +
            ggtitle("Empirical Distribution") +
            ylab("Density / Count") +
            geom_histogram(
                aes(y = ..density..),
                binwidth = input$bins,
                col = "blue",
                fill = "blue",
                alpha = 0.4,
                na.rm = T
            ) +
            geom_density(
                col = "blue",
                fill = "blue",
                alpha = 0.2,
                na.rm = T
            ) +
            theme(
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
            )
        
    })
    
    # Simulation distribution plot
    output$distPlot2 = renderPlot({
        Rate = as.numeric(unlist(strsplit(input$vec1, ",")))
        df = data.frame(Rate)
        
        bib = data.frame(wt = density(df$Rate)$x,
                         dens = density(df$Rate)$y)
        
        max_density = as.numeric(bib[which.max(bib[, 2]), ][1])
        
        bob = data.frame(Rate = rmodbetapert(
            input$reps,
            (min(df$Rate) - min(df$Rate) * input$minperc),
            max_density,
            (max(df$Rate) + max(df$Rate) * input$maxperc),
            input$gamma
        ))
        
        ggplot(bob, aes(Rate)) +
            ggtitle("Simulation Distribution") +
            ylab("Density / Count") +
            geom_histogram(
                aes(y = ..density..),
                binwidth = input$bins,
                col = "blue",
                fill = "blue",
                alpha = 0.2,
                na.rm = T
            ) +
            xlim((min(df$Rate) - min(df$Rate) * input$minperc),
                 (max(df$Rate) + max(df$Rate) * input$maxperc)) +
            geom_density(
                col = "blue",
                fill = "blue",
                alpha = 0.2,
                na.rm = T
            ) +
            theme(
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
    
    # Simulation CDF plot
    output$distPlot3 = renderPlot({
        Rate = as.numeric(unlist(strsplit(input$vec1, ",")))
        df = data.frame(Rate)
        
        bib = data.frame(wt = density(df$Rate)$x,
                         dens = density(df$Rate)$y)
        
        bob = data.frame(Rate = rmodbetapert(
            input$reps,
            (min(df$Rate) - min(df$Rate) * input$minperc),
            as.numeric(bib[which.max(bib[, 2]), ][1]),
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
        Rate = as.numeric(unlist(strsplit(input$vec1, ",")))
        df = data.frame(Rate)
        
        bib = data.frame(wt = density(df$Rate)$x,
                         dens = density(df$Rate)$y)
        
        paste0(
            "<i>Maximum density at ",
            txtRound(as.numeric(bib[which.max(bib[, 2]), ][1]), 1),
            "; n=",
            length(df$Rate),
            ".<br>Simulation minimum: ",
            txtRound((
                min(df$Rate) - min(df$Rate) * input$minperc
            ), 1),
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
