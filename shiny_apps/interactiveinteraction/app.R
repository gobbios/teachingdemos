
library(shiny)
library(effects)

# Define UI
ui <- fluidPage(

   # Application title
   titlePanel("colorful interaction"),

   # Sidebar slider inputs for angle and fixing values along x and y
   sidebarLayout(
      sidebarPanel(
        sliderInput("theta", "angle theta:",min = -180, max = 180,value = -20, step = 10),
        sliderInput("xval","fixed x (pred1):",min = 1, max = 11,value = 1),
        sliderInput("yval","fixed y (pred2):",min = 1, max = 11,value = 1)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot3d")
      )
   )
)

# Define server
server <- function(input, output) {

   output$plot3d <- renderPlot({

     set.seed(123)
     N <- 500
     pred1 <- rnorm(N)
     pred2 <- rnorm(N)
     resp <- pred1 * pred2 + rnorm(N, sd = 3)

     GRIDRES <- 11
     THETA <- input$theta
     YCUT <- input$yval
     XCUT <- input$xval

     xdata <- data.frame(resp, pred1=scale(pred1), pred2 = scale(pred2)); rm(pred1, pred2, resp, N); summary(res <- lm(resp ~ pred1*pred2, xdata))
     newx <- seq(min(xdata$pred1), max(xdata$pred1), length.out = GRIDRES); newy <- seq(min(xdata$pred2), max(xdata$pred2), length.out = GRIDRES)
     pdata <- effect("pred1:pred2", res, xlevels=list(pred1=newx, pred2 = newy))


     layout(matrix(c(1,2,1,3), ncol=2), heights = c(5,3))


     perspmat <- persp(x = newx, y = newy, z = matrix(pdata[[5]], ncol=length(newx)), theta=THETA, phi=10, expand=0.6, r=10, xlab="predictor1", ylab="predictor2", zlab="response", zlim=c(-11, 11), ticktype = "detailed")

     xcols <- rainbow(100)[cut(xdata$resp, breaks = 100)]

     points(trans3d(xdata$pred1, xdata$pred2, xdata$resp, perspmat), pch=16, col=xcols)



     # fix pred2 and show relation between pred1 and resp
     xcuts <- cut(xdata$pred2, breaks = GRIDRES-1)
     xmid <- (as.numeric( sub("\\((.+),.*", "\\1", levels(xcuts)) ) + as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(xcuts)) )) * 0.5
     xpos <- which(as.numeric(xcuts) == YCUT)
     plot(xdata$pred1[xpos], xdata$resp[xpos], pch=16, col=xcols[xpos], xlab="pred1", ylab="response", xlim=range(xdata$pred1), ylim=c(-11,11), main=paste("pred2= ", round(xmid[YCUT],2)))

     pd <- data.frame(effect("pred1:pred2", res, xlevels=list(pred2 = xmid, pred1 = newx )))
     pd <-pd[round(pd$pred2 - xmid[YCUT], 4)== 0.0000, ]
     points(pd$pred1, pd$fit, type="l")


     # fix pred1 and show relation between pred2 and resp
     xcuts <- cut(xdata$pred1, breaks = GRIDRES-1)
     xmid <- (as.numeric( sub("\\((.+),.*", "\\1", levels(xcuts)) ) + as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(xcuts)) )) * 0.5
     xpos <- which(as.numeric(cut(xdata$pred1, breaks = GRIDRES-1)) == XCUT)
     plot(xdata$pred2[xpos], xdata$resp[xpos], pch=16, col=xcols[xpos], xlab="pred2", ylab="response", xlim=range(xdata$pred2), ylim=c(-11,11), main=paste("pred1= ", round(xmid[XCUT],2)))


     pd <- data.frame(effect("pred1:pred2", res, xlevels=list(pred2 = newy, pred1 = xmid )))
     pd <-pd[round(pd$pred1 - xmid[XCUT], 4)== 0.0000, ]
     points(pd$pred2, pd$fit, type="l")
   })
}

# Run the application
shinyApp(ui = ui, server = server)

