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
library(DT)

# Load data
deaths <- read_csv("data/NCHS_-_Leading_Causes_of_Death__United_States.csv")
colnames(deaths) <- c("Year","Long.Cause","Cause","State","Deaths","Age.adj.Death.Rate")
deaths <- deaths[deaths$Year != 2016,]

# create data.frame of row/column names of futre datable
my_df <- data.frame(nam = c("Favorite1", "Favorite2"))

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
                    numericInput("statenum",NULL,0,width = "70px",min = 0,max = 50),
                    uiOutput("extraStates")
                ),
            selectInput("variable",
                        "Variable of Interest",
                        choices = c("Age-adjusted Death Rate","Number of Deaths","Proportionality of Deaths"),
                        selected = "Age-adjusted Death Rate"),
            # TODO make a checkbox grid for selecting multiple states
                  h2("Checkboxes Datatable"),
                DT::dataTableOutput("mytable", width = "1%")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           # TODO Checkbox stuff
           h2("Selected"),
      tableOutput("checked")
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
    
    # TODO new stuff
    # helper function for making checkbox
    shinyInput <- function(FUN, len, id, ...) { 
      inputs <- character(len) 
      for (i in seq_len(len)) { 
        inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...)) 
      } 
      inputs 
    } 
    # datatable with checkbox
    output$mytable <- DT::renderDataTable( 
      expr = {
        df <- data.frame(
        #  my_df,
          my_df,
          Favorite1 = shinyInput(checkboxInput, nrow(my_df), "cbox1"), 
          Favorite2 = shinyInput(checkboxInput, nrow(my_df), "cbox2")
        )
        names(df)[1] <- " "
        df
      }, 
      rownames = FALSE,
      server = FALSE, 
      escape = FALSE, 
      options = list(
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        preDrawCallback = JS("function() { 
          Shiny.unbindAll(this.api().table().node()); }"
        ), 
        drawCallback = JS("function() { 
          Shiny.bindAll(this.api().table().node()); } "
        ) 
      )
    )

    # helper function for reading checkbox
    shinyValue <- function(id, len) { 
      unlist(
        x = lapply(
          X = seq_len(len), 
          FUN = function(i) { 
            value = input[[paste0(id, i)]] 
            if (is.null(value)) {
              NA
            } else {
              value
            }  
          }
        )
      ) 
    } 
    # output read checkboxes
    output$checked <- renderTable({
      data.frame(
        Favorite1 = shinyValue("cbox1", nrow(my_df)),
        Favorite2 = shinyValue("cbox2", nrow(my_df))
      )
    }
  )
}

# Remove extra variables
rm(deaths)

# Run the application 
shinyApp(ui = ui, server = server)
