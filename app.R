#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)

# Load data
deaths <- read_csv("data/NCHS_-_Leading_Causes_of_Death__United_States.csv")
colnames(deaths) <- c("Year","Long.Cause","Cause","State","Deaths","Age.adj.Death.Rate")
deaths <- deaths[deaths$Year != 2016,]

# Define UI for application that draws a line graph
ui <- fluidPage(
    
    theme = shinytheme("cosmo"),

    # Application title
    titlePanel("U.S. Cause of Death Trends (1999-2015)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("causes",
                               "Cause of Death",
                               choices = sort(unique(deaths$Cause)),
                               selected = "All causes"),
            selectInput("states",
                               "State",
                               choices = c("United States",sort(unique(deaths$State))[-45]),
                               selected = "United States"),
            checkboxInput("mult", "Add States"),
                conditionalPanel(
                    condition = "input.mult == true",
                    numericInput("statenum",NULL,0,width = "18%",min = 0,max = 50),
                    uiOutput("extraStates")
                ),
            selectInput("variable",
                        "Variable of Interest",
                        choices = c("Age-adjusted Death Rate","Number of Deaths","Proportionality of Deaths"),
                        selected = "Age-adjusted Death Rate")
            # TODO make a checkbox grid for selecting multiple states
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Load data
    deaths <- read_csv("data/NCHS_-_Leading_Causes_of_Death__United_States.csv")
    colnames(deaths) <- c("Year","Long.Cause","Cause","State","Deaths","Age.adj.Death.Rate")
    deaths <- deaths[deaths$Year != 2016,]
    deaths$propdeaths <- NA
    for(i in 1:nrow(deaths)){
        deaths$propdeaths[i] <- deaths$Deaths[i]/(deaths$Deaths[deaths$Year==deaths$Year[i] & deaths$State==deaths$State[i] & deaths$Cause=="All causes"])
    }
    
    # Define UI for added states
    output$extraStates <- renderUI({
        if(input$statenum > 0){
            v <- list()
            for (i in 2:(input$statenum+1)){
            v[[i]] <- selectInput(paste0("state",i),
                                  paste("State",i),
                                  choices = c("United States",sort(unique(deaths$State))[-45]),
                                  selected = "United States")
            }
            v
        }
    })


    
    output$distPlot <- renderPlot({
        states <- input$states
        for(i in 2:(input$statenum+1)){
            var <- paste0("state",i)
            states <- append(states,input[[var]])
        }
        # generate plot based on input$states from ui.R
        p <- ggplot(deaths[deaths$Cause %in% input$causes & deaths$State %in% states,],aes(x = Year, color = State)) + theme(legend.text=element_text(size=12),
                                                                                                                             legend.title=element_text(size=15))
        
        if(input$variable == "Number of Deaths"){
            p <- p  + geom_line(aes(y = Deaths),lwd = 2)
        }else if(input$variable == "Age-adjusted Death Rate"){
            p <- p + geom_line(aes(y = Age.adj.Death.Rate),lwd = 2) + ylab("Age-adjusted Death Rate (per 100,000)")
        }else if(input$variable == "Proportionality of Deaths"){
            p <- p + geom_line(aes(y = propdeaths),lwd = 2) + ylab("Proportionality of Deaths")
        }
        print(p)
    })
}

# Remove extra variables
rm(deaths)

# Run the application 
shinyApp(ui = ui, server = server)
