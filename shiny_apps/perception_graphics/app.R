library(shiny)
library(mongolite)

# load plot function
source("create_graph.R")

# prepare test data for local user
typeorder <- c("square", "bar", "circlesize", "grey", "viridis", "rainbow", "angle", "length", "point")
nreps <- 3
xdata <- data.frame(type = sample(rep(typeorder, nreps)))
xdata$Aval <- round(runif(nrow(xdata), 5, 98), 1)
xdata$Bval <- round(runif(nrow(xdata), 5, 98), 1)
xdata$Aoffs <- 100 - runif(nrow(xdata), 0, 100 - xdata$Aval)
xdata$Boffs <- 100 - runif(nrow(xdata), 0, 100 - xdata$Bval)
xdata$truediff <- abs(xdata$Aval - xdata$Bval)
xdata$user_smaller <- ""
xdata$user_diff <- NA
xdata$user_time <- NA


# generate user interface
ui <- fluidPage(
    titlePanel("perception of graphical elements"),
    conditionalPanel(condition = "output.state == 'none'", includeMarkdown("instructions.Rmd")),
    conditionalPanel(condition = "output.state != 'none' && output.state != 'finished'", plotOutput('xplot')),
    conditionalPanel(condition = "output.state == 'finished'", includeMarkdown("finished.Rmd")),
    textOutput("textout"),
    textOutput("textout2"),
    textOutput("textout3"),
    conditionalPanel(condition = "output.state == 'none'",
        fixedRow(
            column(width = 2, offset = 5, actionButton(inputId = "startbutton", label = "Start", icon = NULL))
        )),
    conditionalPanel(condition = "output.state == 'finished'",
         fixedRow(
             column(width = 2, offset = 5, actionButton(inputId = "submitbutton", label = "Submit data", icon = NULL))
         )),
    conditionalPanel(condition = "output.state == 'whichissmaller'",
                     fluidRow(column(width = 4, offset = 4, uiOutput("whichissmaller_text"))),
                     fluidRow(column(width = 4, offset = 5, uiOutput("whichissmaller"))),
                     fluidRow(column(width = 2, offset = 5, actionButton(inputId = "proceedbutton_1", label = "Continue", icon = NULL)))
        ),
    conditionalPanel(condition = "output.state == 'percentdifference'",
        fluidRow(column(width = 6, offset = 3, uiOutput("percentdifference"))),
        fluidRow(column(width = 2, offset = 5, actionButton(inputId = "proceedbutton_2", label = "Continue", icon = NULL)))
        )
    # ,
    # tableOutput("xdata")
)



# generate server

server <- function(input, output, session) {
    v <- reactiveValues(counter = 0,
                        state = "none",
                        xdata = xdata,
                        finished = FALSE,
                        timer = NA)

    observeEvent({ input$startbutton }, {
        output$whichissmaller <- renderUI({
            tagList(
                radioButtons(inputId = "whichissmaller",
                             label = c("which element represents the smaller value"),
                             choices = c("A", "B"), inline = TRUE)
            )
        })
        v$timer <- Sys.time()
        v$state <- "whichissmaller"
        v$counter <- 1
        output$xplot <- renderPlot({
            par(mar = c(4, 6, 2, 6))
            xline <- v$counter
            create_graph(type = v$xdata$type[xline], A = v$xdata$Aval[xline], B = v$xdata$Bval[xline], offs_A = v$xdata$Aoffs[xline], offs_B = v$xdata$Boffs[xline])
        })

    })

    observeEvent({
        input$proceedbutton_1
    }, {
        output$percentdifference <- renderUI({
            tagList(textInput(inputId = "percentdifference", label = paste("what's the difference in percent between the two?")))
        })

        output$xplot <- renderPlot({
            if (!v$finished) {
                par(mar = c(4, 6, 2, 6))
                xline <- v$counter
                create_graph(type = v$xdata$type[xline], A = v$xdata$Aval[xline], B = v$xdata$Bval[xline], offs_A = v$xdata$Aoffs[xline], offs_B = v$xdata$Boffs[xline])

            }
        })
        v$state <- "percentdifference"
        res <- ifelse(length(input$whichissmaller) == 0, NA, input$whichissmaller)
        v$xdata$user_smaller[v$counter] <- res
    })

    observeEvent({ input$proceedbutton_2 }, {
        output$xplot <- renderPlot({
            if (!v$finished) {
                par(mar = c(4, 6, 2, 6))
                xline <- v$counter
                create_graph(type = v$xdata$type[xline], A = v$xdata$Aval[xline], B = v$xdata$Bval[xline], offs_A = v$xdata$Aoffs[xline], offs_B = v$xdata$Boffs[xline])
                title(main = paste(nreps * length(typeorder) - v$counter + 1, "trials to go"))
            }
        })

        v$state <- "whichissmaller"
        res <- ifelse(input$percentdifference == "", NA, input$percentdifference)
        v$xdata$user_diff[v$counter] <- res
        v$xdata$user_time[v$counter] <- as.numeric(difftime(Sys.time(), v$timer, units = "secs"))
        v$timer <- Sys.time()

        v$counter <- v$counter + 1
        if (v$counter == length(typeorder) * nreps + 1) {
            v$state <- "finished"
            v$finished <- TRUE
        }
    })

    observeEvent({input$submitbutton}, {
        showModal(modalDialog(title = "enter password", passwordInput(inputId = "password", label = NULL),
                              footer = actionButton("passwordenter", "Continue.")))
    })

    observeEvent( {input$passwordenter}, {
        u <- paste0("mongodb://", "imaparticipant:", input$password, "@ds261088.mlab.com:61088/", "teachingappsdata", "?retryWrites=false")
        m <- tryCatch(mongo(collection = "perceptionetal", url = u), error = function(e) FALSE)
        if (inherits(m, "mongo")) {
            res <- as.data.frame(v$xdata)
            res$user <- paste(sample(c(LETTERS, letters), 8, replace = TRUE), collapse = "")
            res$today <- as.character(Sys.time())
            m$insert(res)
            showModal(modalDialog(title = "Success", "Thank you.", footer = actionButton("showreport", "Done."), easyClose = TRUE))
        } else {
            showModal(modalDialog(title = "Error", "Hmmm. Something went wrong. Did you enter the correct password?"))
        }
    })

    observeEvent({ input$showreport }, {
        v$state <- "showreport"
        pdata <- data.frame(v$xdata)
        pdata$user_smaller <- as.character(pdata$user_smaller)
        pdata$user_diff <- as.numeric(as.character(pdata$user_diff))
        pdata$user_time <- as.numeric(as.character(pdata$user_time))
        pdata$type <- factor(as.character(pdata$type),
                             levels = c("bar", "angle", "circlesize", "square", "viridis", "grey", "rainbow"))

        # formula from Heer and Bostock 2010
        # subjects were instructed first to identify the smaller of two marked
        # values, and then “make a quick visual judgment” to estimate what
        # percentage the smaller was of the larger.
        pdata$error <- log(abs(pdata$user_diff - pdata$truediff) + 1/8, base = 2)

        # output$xplot <- renderPlot({
        #     par(mar = c(4, 6, 2, 6), family = "serif")
        #     plot(as.numeric(pdata$type), pdata$error, xlim = c(0.5, 7.5), xlab = "category", ylab = "error", axes = FALSE)
        #     axis(1, at = 1:7, levels(pdata$type), tcl = 0)
        #     axis(2, las = 1)
        #     box()
        # })
        output$xdata <- renderTable(pdata)

    })

    observeEvent( {input$quit}, {
        stopApp()
    })


    # output$textout <- renderText(v$finished)
    output$xdata <- renderTable(v$xdata)

    output$state <- reactive({
        v$state
    })

    outputOptions(output, "state", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
