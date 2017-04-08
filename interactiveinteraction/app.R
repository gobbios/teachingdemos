library(shiny)


# create data and run model
set.seed(123)
N <- 1000
pred1 <- runif(N)
pred2 <- runif(N)
#create linear predictor (basically the model wrt the effects of the predictors on the response): 
# lp=as.vector(scale(pred1))+as.vector(scale(pred1))*(0.75+as.vector(scale(pred2))) 
lp <- scale(pred1) + scale(pred1) * (0.75 + scale(pred2))

resp <- rbinom(N, size=1, prob=exp(lp)/(1+exp(lp)))#create response

#put all variables involved (including the two predictors z-transformed) into a data frame:
xdata <- data.frame(pred1, pred2, resp, zpred1=scale(pred1), zpred2=scale(pred2))
rm(N, pred1, pred2, resp, lp)#... and remove them from the workspace 
res <- glm(resp ~ zpred1*zpred2, family=binomial, data=xdata) 




ui <- fluidPage(
   
   # Application title
   titlePanel("GLMM with two-way interaction"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("bins", "number of bins:",min = 3, max = 33, value = 11),
        sliderInput("theta", "angle theta:",min = -180, max = 180,value = -20, step = 10),
        checkboxInput("redline", label = "red line", value = FALSE),
        sliderInput("x", "pred1:", min = round(min(xdata$zpred1),1), max = round(max(xdata$zpred1),1), value = 0, step = 0.1),
        checkboxInput("blueline", label = "blue line", value = FALSE),
        sliderInput("y", "pred2:", min = round(min(xdata$zpred2),1), max = round(max(xdata$zpred2),1), value = 0, step = 0.1)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic 
server <- function(input, output) {
   
   output$distPlot <- renderPlot({

     
     xp1 <- seq(from=min(xdata$zpred1), to=max(xdata$zpred1), length.out=input$bins)
     yp2 <- seq(from=min(xdata$zpred2), to=max(xdata$zpred2), length.out=input$bins)
     
     coeffs=coefficients(res)#store model coefficients in a vector with a short name
     
     pred.mat=outer(X=xp1, Y=yp2, FUN=function(x, y){
       coeffs["(Intercept)"]+coeffs["zpred1"]*x+coeffs["zpred2"]*y+coeffs["zpred1:zpred2"]*x*y })
     pred.mat=exp(pred.mat)/(1+exp(pred.mat)) 
     
     tmat=persp(x=xp1, y=yp2, z=pred.mat, theta=input$theta, phi=10, expand=0.6, r=10, xlab="predictor1",
                ylab="predictor2", zlab="response", zlim=c(0, 1))
     

     
     
     if(input$redline) {
       pred.mat=outer(X=input$x, Y=yp2, FUN=function(x, y){
         coeffs["(Intercept)"]+coeffs["zpred1"]*x+coeffs["zpred2"]*y+coeffs["zpred1:zpred2"]*x*y })
       pred.mat=exp(pred.mat)/(1+exp(pred.mat))
       points(trans3d(x=input$x, y=yp2, z=t(pred.mat), pmat=tmat), type="l", col="red", lwd=2)
       
     }
     
     if(input$blueline) {
       pred.mat=outer(X=xp1, Y=input$y, FUN=function(x, y){
         coeffs["(Intercept)"]+coeffs["zpred1"]*x+coeffs["zpred2"]*y+coeffs["zpred1:zpred2"]*x*y })
       pred.mat=exp(pred.mat)/(1+exp(pred.mat))
       points(trans3d(x=xp1, y=input$y, z=pred.mat, pmat=tmat), type="l", col="blue", lwd=2)
       
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

