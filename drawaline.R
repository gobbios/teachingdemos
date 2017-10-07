

library(shiny)
library(mongolite)

# Define UI for application that draws a histogram
ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("graph1", {
        sidebarLayout(
           sidebarPanel(actionButton("do1", "submit")),
           mainPanel(
              plotOutput("figure1", click = "plot_click1"),
              verbatimTextOutput("info")
           )
        )
      }),
      tabPanel("graph2",{
        sidebarLayout(
          sidebarPanel(actionButton("do2", "submit")),
          mainPanel(
            plotOutput("figure2", click = "plot_click2")
          )
        )
      }),
      tabPanel("graph3",{
        sidebarLayout(
          sidebarPanel(actionButton("do3", "submit")),
          mainPanel(
            plotOutput("figure3", click = "plot_click3")
          )
        )
      }),
      tabPanel("submit", {
        actionButton("submitresults", "submit")
        
      })
    )
  )
  
   # # Application title
   # titlePanel("testing user submissions"),
   # 
   # # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
   #      actionButton("do1", "submit")
   #    ),
   #    
   #    # Show a plot of the generated distribution
   #    mainPanel(
   #       plotOutput("distPlot", click = "plot_click"),
   #       verbatimTextOutput("info")
   #    )
   # )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  userid <- paste0(sample(c(letters, LETTERS), 8), collapse = "")
  today <- as.character(Sys.Date())
  
  vals2 <- reactiveValues(coords = data.frame(fig = 1:3, x1 = 0, x2 = 0, y1 = 0, y2 = 0))
  
  # create figures
  
  output$figure1 <- renderPlot({
    plot(anscombe$x1, anscombe$y1, ann = FALSE, axes = FALSE); box()
    # points(vals$coord[c(1,3)], vals$coord[c(2, 4)], "l", lwd = 2, col = "red")
    points(vals2$coords[1, c(2, 3)], vals2$coords[1, c(4, 5)], "l", lwd = 2, col = "red")
  })
  
  output$figure2 <- renderPlot({
    plot(anscombe$x2, anscombe$y2, ann = FALSE, axes = FALSE); box()
    points(vals2$coords[2, c(2, 3)], vals2$coords[2, c(4, 5)], "l", lwd = 2, col = "red")
  })
  
  output$figure3 <- renderPlot({
    plot(anscombe$x3, anscombe$y3, ann = FALSE, axes = FALSE); box()
    points(vals2$coords[3, c(2, 3)], vals2$coords[3, c(4, 5)], "l", lwd = 2, col = "red")
  })
  

  
  

   clickcounter1 <- reactiveValues(cnt = numeric(3))
   
   observeEvent(input$plot_click1, {
     if(clickcounter1$cnt[1] %%2 == 0) {
       #vals$coord[1] <- input$plot_click1$x
       #vals$coord[2] <- input$plot_click1$y
       vals2$coords[1,2] <- input$plot_click1$x
       vals2$coords[1,4] <- input$plot_click1$y
     }
     if(clickcounter1$cnt[1] %%2 == 1) {
       #vals$coord[3] <- input$plot_click1$x
       #vals$coord[4] <- input$plot_click1$y
       vals2$coords[1,3] <- input$plot_click1$x
       vals2$coords[1,5] <- input$plot_click1$y
       
     }
     clickcounter1$cnt[1] <- clickcounter1$cnt[1] + 1
   })
   
   clickcounter2 <- reactiveValues(cnt = numeric(3))
   observeEvent(input$plot_click2, {
     if(clickcounter2$cnt[1] %%2 == 0) {
       vals2$coords[2,2] <- input$plot_click2$x
       vals2$coords[2,4] <- input$plot_click2$y
     }
     if(clickcounter2$cnt[1] %%2 == 1) {
       vals2$coords[2,3] <- input$plot_click2$x
       vals2$coords[2,5] <- input$plot_click2$y
     }
     clickcounter2$cnt[1] <- clickcounter2$cnt[1] + 1
   })
   
   clickcounter3 <- reactiveValues(cnt = numeric(3))
   observeEvent(input$plot_click3, {
     if(clickcounter3$cnt[1] %%2 == 0) {
       vals2$coords[3, c(2, 4)] <- c(input$plot_click3$x, input$plot_click3$y)
       #vals2$coords[3,4] <- 
     }
     if(clickcounter3$cnt[1] %%2 == 1) {
       vals2$coords[3, c(3, 5)] <- c(input$plot_click3$x, input$plot_click3$y)
       #vals2$coords[3,5] <- 
     }
     clickcounter3$cnt[1] <- clickcounter3$cnt[1] + 1
   })
   
   observeEvent(input$submitresults, {
     #res <- c(userid, today, vals$coord)
     #write.table(res, file = paste(userid, "test1.txt"))
     # write.table(vals2$coords, file = paste(userid, "test1b.txt"))
     m <- mongo("plottingcoordinates", url = "mongodb://gobbios:DwFRW488ts@ds147534.mlab.com:47534/teachingdemo")
     
     
     res <- as.data.frame(vals2$coords)
     res$user <- userid
     res$today <- today
     m$insert(res)
     
     
   })
   

   
}

# Run the application 
shinyApp(ui = ui, server = server)

