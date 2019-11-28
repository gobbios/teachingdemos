library(shiny)
library(lme4)
library(effects)

ui <- fluidPage(

   # Application title
   titlePanel("2-way interaction"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("nindividuals", "N individuals", min = 6, max = 12, value = 7),
         sliderInput("nobs", "N observations", min = 10, max = 1000, value = 400, step = 10),
         sliderInput("intercept", "intercept", min = -10, max = 10, value = 0, step = 0.1),
         sliderInput("pred1", "predictor 1", min = -10, max = 10, value = 0.5, step = 0.1),
         sliderInput("pred2", "predictor 2", min = -10, max = 10, value = -0.5, step = 0.1),
         sliderInput("interaction", "interaction", min = -10, max = 10, value = 2, step = 0.1),
         sliderInput("resolution", "resolution", min = 3, max = 50, value = 3, step = 1),
         sliderInput("theta", "rotation theta", min =10, max = 60, value = 20, step = 1),
         sliderInput("pred1fix", "fix pred 1", min = -1, max = 1, value = 0, step = 0.1),
         sliderInput("pred2fix", "fix pred 2", min = -1, max = 1, value = 0, step = 0.1),
         checkboxInput("plotranef", "plot random intercepts", value = FALSE),
         radioButtons("showwhich", "show which predictor", choices = c("pred1", "pred2"), selected = "pred2")

      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("PLOT")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    createdata <- reactive({
      set.seed(123)
      pred1 <- runif(n = input$nobs, -1, 1)
      pred2 <- runif(n = input$nobs, -1, 1)
      subject <- sample(letters[1:input$nindividuals], size = input$nobs, replace = TRUE)
      ri <- rnorm(n = input$nindividuals)
      names(ri) <- letters[1:input$nindividuals]
      xdata <- data.frame(pred1, pred2, subject)
      xdata$ri <- ri[subject]
      errorterm <- rnorm(input$nobs)
      xdata$resp <- input$intercept + xdata$ri + input$pred1*pred1 + input$pred2*pred2  + input$interaction*xdata$pred1* xdata$pred2 + errorterm

      # prepare data further (binning etc..)
      # along predictor 1
      midpoints1 <- seq(min(xdata$pred1), max(xdata$pred1), length.out = input$resolution)
      x <- diff(midpoints1)[1]/2
      binrange <- range(xdata$pred1) + x*c(-1, 1)
      bins1 <- seq(binrange[1], binrange[2], length.out = input$resolution + 1)
      xdata$cutpred1 <- cut(xdata$pred1, breaks = bins1)
      xdata$cutpred1mp <- midpoints1[as.numeric(xdata$cutpred1)]

      # along predictor 2
      midpoints2 <- seq(min(xdata$pred2), max(xdata$pred2), length.out = input$resolution)
      x <- diff(midpoints2)[1]/2
      binrange <- range(xdata$pred2) + x*c(-1, 1)
      bins2 <- seq(binrange[1], binrange[2], length.out = input$resolution + 1)
      xdata$cutpred2 <- cut(xdata$pred2, breaks = bins2)
      xdata$cutpred2mp <- midpoints2[as.numeric(xdata$cutpred2)]

      # colours for subjects
      xcols <- rainbow(input$nindividuals)
      names(xcols) <- letters[1:input$nindividuals]
      xdata$colors <- xcols[subject]

      return(list(xdata = xdata, midpoints1 = midpoints1, midpoints2 = midpoints2))
    })

    runmodels <- reactive({
      xdata <- createdata()$xdata
      res <- lmer(resp ~ pred1 * pred2 + (1|subject), data = xdata, REML = FALSE)
      red <- lmer(resp ~ pred1 + pred2 + (1|subject), data = xdata, REML = FALSE)
      null <- lmer(resp ~ 1 + (1|subject), data = xdata, REML = FALSE)
      return(list(res = res, red = red, null = null, xdata = xdata))
    })


   output$PLOT <- renderPlot({
     mods <- runmodels()
     xdata <- mods$xdata
     res <- mods$res
     ifelse(input$showwhich == "pred1", mp <- createdata()$midpoints2, mp <- createdata()$midpoints1)

     newx1 <- seq(from = min(xdata$pred1), to = max(xdata$pred1), length.out = input$resolution + 1)
     newx2 <- seq(from = min(xdata$pred2), to = max(xdata$pred2), length.out = input$resolution + 1)


     newdata <- expand.grid(pred1 = newx1, pred2 = newx2)
     newdata$resp <- predict(res, newdata = newdata, re.form = NA)
     pmat <- matrix(newdata$resp, ncol = input$resolution + 1)
     zrange <- range(c(pmat), xdata$resp)

     par(mfcol = c(1, 2))
     tmat <- persp(x = newx1, y = newx2, z = pmat, theta = input$theta, phi = 10, expand = 0.6, r = 10,
                   xlab = "predictor 1", ylab = "predictor 2", zlab = "response", zlim = zrange, ticktype = "detailed")

     # fix one predictor
     if(input$showwhich == "pred2") {
       newdata <- expand.grid(pred1 = input$pred1fix, pred2 = newx2)
       xrange <- range(xdata$pred2)
       xlab <- "predictor 2"
       rdata <- xdata[xdata$cutpred1mp == mp[which.min(abs(mp - input$pred1fix))], ]

     }
     if(input$showwhich == "pred1") {
       newdata <- expand.grid(pred1 = newx1, pred2 = input$pred2fix)
       xrange <- range(xdata$pred1)
       xlab <- "predictor 1"
       rdata <- xdata[xdata$cutpred2mp == mp[which.min(abs(mp - input$pred2fix))], ]

     }

     newdata$fit <- predict(res, newdata = newdata, re.form = NA)
     plot(0, 0, type = "n", xlim = xrange, ylim = zrange, xlab = xlab, ylab = "response", las = 1)
     points(rdata[, ifelse(input$showwhich == "pred1", "pred1", "pred2")], rdata$resp, col = rdata$colors, cex = 0.8, lwd = 0.8)

     if(input$plotranef) {
       re <- ranef(res)$subject
       for(i in names(xcols)) {
         points(newdata[, input$showwhich], newdata$fit + re[i, 1], type = "l", col = xcols[i])
       }
     }

     points(newdata[, input$showwhich], newdata$fit, type = "l", lwd = 2)



   })
}

# Run the application
shinyApp(ui = ui, server = server)

