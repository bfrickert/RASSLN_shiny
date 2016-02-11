library(shiny)
source('helper.R')

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(theme='bootstrap.css',
  tags$h2('Can WWE Match Outcomes be Predicted based on Opponent Move Sets?'),
  HTML("<strong>Select a wrestler from the menu below</strong> for a reporting dashboard 
replete
       with data about your wrestler's opponents and their move sets. Below that, you'll
       find the results of a predictive logistic regression model that predicts 
       the outcomes (<strong>wins
       </strong> vs. <strong>losses</strong>) of your wrestler's matches based on the 
       move sets of his or her opponents.<br /><br />All the data comes from the amazing <a 
       href='http://wrestlingdata.com'>WrestlingData.com</a>."),
  fluidRow(
    column(3,
      selectInput('Wrestler','',
                  c(wrestler.names), selected='Randy Savage')
    )),
    
    mainPanel(
      tags$hr(),
      tags$div(class='record',uiOutput('Record')),
      tags$br(),
      tags$div(class='record', 
               HTML("<strong>Scatterplot of Opponents featuring Regression of Wins And Losses by Weight and Height of Opponent</strong><br />")),
      HTML("Each dot on the graph below represents an opponent. Perhaps even the 
same opponent multiple time. <strong>Regression lines are then fitted</strong> 
and applied to the graph 
                    providing a sense of how the height and weight of opponents may 
                    have affected whether the wrestler won or lost a match."),
      plotOutput("distPlot"),
      tags$hr(),
      tags$div(class='record', 
               HTML("<strong>Graph Plot of Top 20 Opponents in WWE</strong>")),
      imageOutput("graphPlot"),
      br(),
      br(),
      br(),
      br(),
      tags$hr(),
      tags$div(class='record', 
               uiOutput('beatens.title')),
      fluidRow(column(6,
             
      tableOutput("beatens")),
      column(6, imageOutput("winning.word.cloud"))),
      br(),
      br(),
      br(),
      tags$hr(),
      tags$div(class='record', 
               uiOutput('beatens.by.title')),
      fluidRow(column(5, 
                      
                      tableOutput("beaters")),column(5,
                                                     imageOutput("losing.word.cloud"))),
      br(),
      br(),
      br(),
      tags$hr(),
      tags$div(class='record', uiOutput("Accuracy")),
      tableOutput("Xtable"),
      tags$hr(),
      br(),
      tags$div(class='record', uiOutput("effective.moves.title")),
      br(),
      textOutput("effectiveMoves"),
      tags$hr(),
      tags$div(class='record', uiOutput("ineffective.moves.title")),
      br(),
      textOutput("ineffectiveMoves"),
      tags$hr(),
      tags$div(class='record', HTML("<strong>Performance of Predictive Model</strong>")),
      imageOutput("aucCurve"),
      br(),
      br(),
      br(),
      br(),
      tags$div(class='record', uiOutput("auc")),
      tags$hr(),
      #plotOutput("timeline.outcomes"),
       br()
    )
  ))
