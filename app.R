
library(shiny)
library(dplyr)


# Create the ui and server objects

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Scorekeeper"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = 'right',

    # Sidebar panel ----
    
    sidebarPanel(
	  h4("Instructions:"),
	  h5('Face off against each opponent. Use this app to record your score after each game. The students with the top scores have a chance at bonus points.')
    ),

    # Main panel for displaying outputs ----
    
    mainPanel(
	  
	  # Buttons for each play ----
	  
	  br(),
	  
	  radioButtons("your_move", "YOUR move:",
	               c("Attack!" = "hawk",
	                 "Retreat" = "dove")),
	  
	  radioButtons("opponent_move", "Opponent's move:",
	               c("Attack!" = "hawk",
	                 "Retreat" = "dove")),
	  
	  actionButton("run", label = "Score"),

 
	  # Button to restart ----
	  
    h4(textOutput(outputId = "corrTestres", strong)),
	  h6(textOutput(outputId = "ngamesres", strong)),
	  h6(textOutput(outputId = "avgres")),
	  
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  actionButton("dotest", label = "Refresh/restart at 0")
      
    )
  )
)




# Define server logic required to draw a histogram ----
server <- function(input, output) {
	
	alldata <- 0
	
	ticker <- 0
	
	avg <- 0
	
	ngames <<- ticker[1]
	
	x <<- alldata[1]
	
    output$corrTestres <- renderText({paste('Your score = ', sum(x), sep = '')})
    output$ngamesres <- renderText({paste(ngames, ' games played', sep = '')})
    output$avgres <- renderText({paste('')})

  	observeEvent(input$run, {
 
  		x <<- if(input$your_move == 'hawk' & input$opponent_move == 'hawk'){
  			x + 2
  		} else {
  			if(input$your_move == 'hawk' & input$opponent_move == 'dove'){
  			  x + 10
  			} else {
  			  if(input$your_move == 'dove' & input$opponent_move == 'hawk'){
  			    x + 1
  			  } else {
  			    x + 4
  			  }
  			}
  		}
  		
  		ngames <<- ngames + 1
  		x_avg <<- round(x / ngames, 2)
    	
    		output$corrTestres <- renderText({paste('Your score = ', sum(x), sep = '')})
    		output$ngamesres <- renderText({paste(ngames, ' games played', sep = '')})
    		output$avgres <- renderText({paste('Averaging ', x_avg, ' per game', sep = '')})
    	})
    	
    observeEvent(input$dotest, {
		
      output$corrTestres <- renderText({paste('Your score = ', sum(x), sep = '')})
      output$ngamesres <- renderText({paste(ngames, ' games played', sep = '')})
      output$avgres <- renderText({paste('Averaging ', x_avg, ' per game', sep = '')})
   	 	x <<- alldata[1] # start again
   	 	ngames <<- ticker[1]
   	 	x_avg <- avg[1]

    })
    	
}

# 

shinyApp(ui = ui, server = server) # end with a call to the shinyApp function



