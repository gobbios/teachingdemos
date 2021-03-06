library(shiny)
library(mongolite)

ui <- fluidPage(
  titlePanel("guess the line"),
  sidebarLayout(
    sidebarPanel(
      textInput("thepassword", label = h3("enter the password"), value = "here..."),
      hr(),
      h4("once finished submit your guesses"),
      actionButton("submitresults", "submit your results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("graph1", plotOutput("figure1", click = "plot_click1")),
        tabPanel("graph2", plotOutput("figure2", click = "plot_click2")),
        tabPanel("graph3", plotOutput("figure3", click = "plot_click3")),
        tabPanel("graph4", plotOutput("figure4", click = "plot_click4"))
      )
    )
  )
)

server <- function(input, output) {
  userid <- paste0(sample(c(letters, LETTERS), 8), collapse = "")
  today <- as.character(Sys.time())
  vals <- reactiveValues(coords = data.frame(fig = 1:4, x1 = 0, x2 = 0, y1 = 0, y2 = 0))

  # create figures
  output$figure1 <- renderPlot({
    plot(anscombe$x1, anscombe$y1, ann = FALSE, axes = FALSE, xlim = c(2, 17), ylim = c(3, 13), pch = 16, cex = 1.3)
    box()
    points(vals$coords[1, c(2, 3)], vals$coords[1, c(4, 5)], "l", lwd = 2, col = "red")
  })

  output$figure2 <- renderPlot({
    plot(anscombe$x2, anscombe$y2, ann = FALSE, axes = FALSE, xlim = c(2, 17), ylim = c(1, 11), pch = 16, cex = 1.3)
    box()
    points(vals$coords[2, c(2, 3)], vals$coords[2, c(4, 5)], "l", lwd = 2, col = "red")
  })

  output$figure3 <- renderPlot({
    plot(anscombe$x3, anscombe$y3, ann = FALSE, axes = FALSE, xlim = c(2, 17), ylim = c(4, 14), pch = 16, cex = 1.3)
    box()
    points(vals$coords[3, c(2, 3)], vals$coords[3, c(4, 5)], "l", lwd = 2, col = "red")
  })

  output$figure4 <- renderPlot({
    plot(anscombe$x4, anscombe$y4, ann = FALSE, axes = FALSE, xlim = c(6, 21), ylim = c(4, 14), pch = 16, cex = 1.3)
    box()
    points(vals$coords[4, c(2, 3)], vals$coords[4, c(4, 5)], "l", lwd = 2, col = "red")
  })

  clickcounter1 <- reactiveValues(cnt = 0)
  observeEvent(input$plot_click1, {
   if(clickcounter1$cnt[1] %% 2 == 0) vals$coords[1, c(2, 4)] <- c(input$plot_click1$x, input$plot_click1$y)
   if(clickcounter1$cnt[1] %% 2 == 1) vals$coords[1, c(3, 5)] <- c(input$plot_click1$x, input$plot_click1$y)
   clickcounter1$cnt[1] <- clickcounter1$cnt[1] + 1
  })

  clickcounter2 <- reactiveValues(cnt = 0)
  observeEvent(input$plot_click2, {
   if(clickcounter2$cnt[1] %% 2 == 0) vals$coords[2, c(2, 4)] <- c(input$plot_click2$x, input$plot_click2$y)
   if(clickcounter2$cnt[1] %% 2 == 1) vals$coords[2, c(3, 5)] <- c(input$plot_click2$x, input$plot_click2$y)
   clickcounter2$cnt[1] <- clickcounter2$cnt[1] + 1
  })

  clickcounter3 <- reactiveValues(cnt = 0)
  observeEvent(input$plot_click3, {
   if(clickcounter3$cnt[1] %% 2 == 0) vals$coords[3, c(2, 4)] <- c(input$plot_click3$x, input$plot_click3$y)
   if(clickcounter3$cnt[1] %% 2 == 1) vals$coords[3, c(3, 5)] <- c(input$plot_click3$x, input$plot_click3$y)
   clickcounter3$cnt[1] <- clickcounter3$cnt[1] + 1
  })

  clickcounter4 <- reactiveValues(cnt = 0)
  observeEvent(input$plot_click4, {
   if(clickcounter4$cnt[1] %% 2 == 0) vals$coords[4, c(2, 4)] <- c(input$plot_click4$x, input$plot_click4$y)
   if(clickcounter4$cnt[1] %% 2 == 1) vals$coords[4, c(3, 5)] <- c(input$plot_click4$x, input$plot_click4$y)
   clickcounter4$cnt[1] <- clickcounter4$cnt[1] + 1
  })

  observeEvent(input$submitresults, {
    u <- paste0("mongodb://", "imaparticipant:", input$thepassword, "@ds261088.mlab.com:61088/", "teachingappsdata", "?retryWrites=false")
    m <- tryCatch(mongo(collection = "draw_the_line", url = u), error = function(e) FALSE)
    if (inherits(m, "mongo")) {
      res <- as.data.frame(vals$coords)
      res$user <- userid
      res$today <- today
      m$insert(res)
      showModal(modalDialog(title = "Success", "Thank you."))
    } else {
      showModal(modalDialog(title = "Error", "Hmmm. Something went wrong. Did you enter the correct password?"))
    }

  })

}

# Run the application
shinyApp(ui = ui, server = server)
