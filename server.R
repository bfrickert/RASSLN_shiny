library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  wrestler.row <- reactive({
    filter(shiny, name == input$Wrestler)
  })
  wrestler.id <- reactive({
    filter(wrestler.row(), name == input$Wrestler)$id
  })
  
  output$distPlot <- renderPlot({
    f.lost <- select(filter(full, loser.name == input$Wrestler & 
                              is.na(winner.wgt)==F & is.na(winner.hgt)==F &
                              winner.wgt != 0 & winner.hgt != 0), winner.wgt,winner.hgt)
    names(f.lost) <- c('wgt','hgt')
    f.lost$result <- 'lost'
    f.won <- select(filter(full, winner.name == input$Wrestler & 
                             is.na(loser.wgt)==F & is.na(loser.hgt)==F &
                             loser.wgt != 0 & loser.hgt != 0),loser.wgt, loser.hgt)
    names(f.won) <- c('wgt','hgt')
    f.won$result <- 'won'
    
    f <- rbind(f.lost,f.won)

    p <- ggplot(f, aes(wgt, hgt))
    p + geom_point(position = position_jitter(w = 0.1, h = 0.1)) + 
      geom_smooth(aes(group=result,colour = factor(result)), method="lm") + theme_bw() +
      labs(y='height (meters)', x='weight (lbs)')  + 
      scale_colour_discrete(name = "")
  })
  
  output$graphPlot <- renderImage({
    # Return a list containing the filename and alt text
    list(src = paste('./viz/graph/',wrestler.id(),'.jpg',sep=''),
         alt = paste("Top Opponents for ", input$Wrestler))
    
  }, deleteFile = FALSE)
  
  output$aucCurve <- renderImage({
    list(src = paste('./viz/auc/',wrestler.id(),'.jpg',sep=''),
         alt = paste("Area Under Curve graph for ", input$Wrestler))
    
  }, deleteFile = FALSE)
  
  output$winning.word.cloud <- renderImage({
    list(src = paste('./viz/cloud/wins/',wrestler.id(),'.jpg',sep=''),
         alt = paste("Inneffective Moves against ", input$Wrestler))
    
  }, deleteFile = FALSE)
  
  output$losing.word.cloud <- renderImage({
    # Return a list containing the filename and alt text
    list(src = paste('./viz/cloud/losses/',wrestler.id(),'.jpg',sep=''),
         alt = paste("Effective Moves against ", input$Wrestler))
    
   }, deleteFile = FALSE)
  
  output$Record <- renderUI({
    HTML(paste('<strong>',input$Wrestler, '\'s Record: ',wrestler.row()$record * 100,'%</strong>',sep=''))
  })
  
  output$effectiveMoves <- renderText({
    wrestler.row()$effective.moves
  })
  
  output$ineffectiveMoves <- renderText({
    wrestler.row()$ineffective.moves
  })
  
  output$beaters <- renderTable({
    get.beaten.by(input$Wrestler)
  })
  
  output$beatens <- renderTable({
    get.beatens(input$Wrestler)
  })
  
  output$Wins <- renderTable({
    wins <- get.wins(input$Wrestler)
    
    beatens <- data.frame(sort(table(wins$loser.name),decreasing=T)[2:5])
    names(beatens) <- 'count'
    beatens
  })
  
  output$Accuracy <- renderUI({
    HTML(paste('<strong>The Model\'s Accuracy at Predicting ', input$Wrestler, 
              '\'s Match Outcomes: ', wrestler.row()$accuracy * 100, '%</strong>',sep=''))
  })
  
  output$auc <- renderUI({
    HTML(paste('<strong>Area Under Curve: ', wrestler.row()$auc, '</strong>', sep=''))
  })
  
  output$timeline.outcomes <- renderPlot({
    wins <- filter(get.wins(input$Wrestler), as.numeric(format(as.Date(date),'%Y')) != 1900)
    losses <- filter(get.losses(input$Wrestler), as.numeric(format(as.Date(date),'%Y')) != 1900)
    
    wins.s <- summarize(group_by(wins, date), n())
    names(wins.s) <- c('date','cnt')
    
    losses.s <- summarise(group_by(losses, date), n())
    names(losses.s) <-c('date','cnt')
    p <- ggplot() + 
      geom_line(data = wins.s, aes(x = as.Date(date), y = cnt, color = "red")) +
      geom_line(data = losses.s, aes(x = as.Date(date), y = cnt, color = "blue"))  +
      xlab('date') +
      ylab('cnt')
    p
  })

  output$Xtable <- renderTable({
    df <- as.data.frame(rbind(c(wrestler.row()$true.positives, wrestler.row()$false.negatives),
                           c(wrestler.row()$false.positives,wrestler.row()$true.negatives)))
    names(df) <- c('predicted wins', 'predicted losses')
    row.names(df) <- c('actual wins', 'actual losses')
    df
      })
  
  output$beatens.title <- renderUI({
    HTML(paste("<strong>Who Was Beaten By ", input$Wrestler, " Most Often?<br />
               What Moves Were Used by Opponents Beaten By ", input$Wrestler, "?</strong><br />",sep=''))
  })
  
  output$beatens.by.title <- renderUI({
    HTML(paste("<strong>Who Beat ", input$Wrestler, " Most Often?<br />
               What Moves Were Used by Opponents who Beat ", input$Wrestler, "?</strong><br />",sep=''))
  })
  
  output$ineffective.moves.title <- renderUI({
    HTML(paste("<strong>Statistically Significant Ineffective Moves against<br />",
          input$Wrestler, "</strong>", sep=''))
  })

  output$effective.moves.title <- renderUI({
    HTML(paste("<strong>Statistically Significant Effective Moves against<br />",
          input$Wrestler,"</strong>",sep=''))
  })
})