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

# Define server logic required to draw a histogram
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
     
      
     #bin predictor 1:
     bin.width=diff(xp1)[1]#determine bin width for the predictor 
     binned.pv1=cut(x=xdata$zpred1, breaks=xp1, labels=F, include.lowest=T) 
     binned.pv1=min(xp1)+bin.width/2+(binned.pv1-1)*bin.width
     #bin predictor 2:
     bin.width=diff(yp2)[1]#determine bin width for the predictor 
     binned.pv2=cut(x=xdata$zpred2, breaks=yp2, labels=F, include.lowest=T) 
     binned.pv2=min(yp2)+bin.width/2+(binned.pv2-1)*bin.width
     
     #average the response separately per combinaton of the values of binned.pv1 and binned.pv2:
     observed=aggregate(x=xdata$resp, by=list(pv1=binned.pv1, pv2=binned.pv2), FUN=mean) #determine sample size per combination of values of the two predictors (not counting NAs): 
     N=aggregate(x=!is.na(xdata$resp), by=list(pv1=binned.pv1, pv2=binned.pv2), FUN=sum)
     
     expected=coeffs["(Intercept)"]+coeffs["zpred1"]*observed$pv1+coeffs["zpred2"]*observed$pv2+coeffs["zpred1:zpred2"]*observed$pv1*observed$pv2
     expected=exp(expected)/(1+exp(expected))#transform from link to probability space
     
     points(trans3d(x=observed$pv1, y=observed$pv2, z=observed$x, pmat=tmat),
            pch=c(1, 19)[1+as.numeric(observed$x>expected)], cex=0.5*N$x^(1/3)) 
     
     #points(trans3d(x=observed$pv1, y=observed$pv2, z=observed$x, pmat=tmat))
     
     
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

