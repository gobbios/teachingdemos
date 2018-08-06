# requires igraph in addition to shiny
library(shiny)
library(igraph)
# the data matrix
mat <- structure(c(0, 0.007, 0.266, 0.129, 2.196, 0.634, 0.011, 0, 0,
                   0, 0, 0, 0, 0, 0.151, 0, 0.532, 0.044, 0.17, 0.145, 0.104, 0,
                   0, 0, 0, 0, 0.457, 0.09, 0.023, 0.191, 0, 0.142, 0, 0, 0.1, 0,
                   0, 0, 0, 0, 0, 0, 0.559, 1.027, 0.195, 0, 0.054, 0.133, 0.135,
                   0, 0, 0, 0, 0, 2.891, 0.007, 3.411, 1.187, 0.407, 1.17, 0, 0.364,
                   0.05, 1.139, 0.187, 0.035, 0.321, 0.067, 0.556, 1.252, 2.509,
                   0.766, 0.4, 1.434, 1.963, 0, 0, 0.181, 0.287, 0.063, 0.383, 0.116,
                   0.451, 0.428, 0.157, 0.702, 0.012, 0.25, 0.08, 0.553, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0.777, 0.207, 0, 0, 0, 0.036, 0.436,
                   0.066, 0, 0, 0, 0, 0, 0, 0.073, 0.02, 0, 0.212, 0, 0.04, 1.8,
                   0.162, 0, 0, 0, 0, 0, 0, 0.413, 0.44, 0, 0.14, 0.212, 0, 0.711,
                   0.077, 0, 0, 0, 0, 0, 0, 0.188, 0.118, 0, 0.082, 1.787, 0.005,
                   0, 0.077, 0, 0, 0, 0, 0, 0, 0.576, 0.087, 0, 0.44, 0.152, 0.28,
                   0.253, 0, 0, 0, 0, 0.096, 0, 0.324, 0.228, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0.719, 0, 0.45, 0, 0.518, 0, 0, 0, 0, 0, 0, 0.285,
                   0),
                 .Dim = c(14L, 14L),
                 .Dimnames = list(c("h", "j", "a", "k", "t", "u", "w", "m", "n", "x", "l", "z", "i", "q"),
                                  c("h", "j", "a", "k", "t", "u", "w", "m", "n", "x", "l", "z", "i", "q")))
ids <- sort(colnames(mat))
idlist <- lapply(ids, function(X)X)
names(idlist) <- ids

# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("shinynetwork"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput("weightwidth", label = "edge weight display", min = 0, max = 5, value = 1, step = 0.05),
      checkboxInput("graphdirected", label = "directed network", value = FALSE),
      checkboxInput("considerweights", label = "weighted network", value = TRUE),
      selectInput("clusteralgo", label = "cluster algorithm",
                  choices = list("none" = "none", "fast_greedy" = "fast_greedy", "leading_eigen" = "leading_eigen"), selected = "none"),
      selectInput("centrmeas", label = "centrality measure",
                  choices = list("none" = 1, "eigenvector" = 2, "betweenness" = 3, "closeness" = 4, "degree" = 5), selected = 1),
      checkboxGroupInput(inputId = "testids", label = "IDs", choices = idlist, selected = ids)
    ),

    # three tabs: first 2 with figures, last a readme
    mainPanel(
      tabsetPanel(
        tabPanel("network",       fixedRow(column(12, plotOutput("network")))),
        tabPanel("modelresults",  fixedRow(column(12, plotOutput("model")))),
        tabPanel("read me",       fixedRow(includeMarkdown("infosheet.Rmd")))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # response variable for model
  g <- graph_from_adjacency_matrix(adjmatrix = mat, weighted = TRUE, mode = "directed")
  e <- eigen_centrality(g)$vector
  set.seed(123)
  xdata <- data.frame(parasiteload = e * 3 + rnorm(length(e)) + 3, id = names(e))

  # create the actual graph
  makegraph <- reactive({
    g <- graph_from_adjacency_matrix(adjmatrix = mat[input$testids, input$testids],
                                     weighted = TRUE,
                                     mode = ifelse(input$graphdirected, "directed", "undirected"))
    if(!input$considerweights) E(g)$weight <- 1
    # deal with centrality
    ncolors <- 100
    if(input$centrmeas == 1) { #none
      V(g)$xcol <- "red"
      maxval <- NA
      legendcolors <- NA
    }
    if(input$centrmeas == 2) { #eigen
      e <- eigen_centrality(g)$vector
      colvals <- as.numeric(cut(e, breaks = seq(-0.00001, max(e) + 0.0000001, length.out = ncolors + 1)))
      V(g)$xcol <- colorRampPalette(colors = c("blue", "red"))(ncolors)[colvals]
      maxval <- max(e)
      legendcolors <- colorRampPalette(colors = c("blue", "red"))(ncolors)
    }
    if(input$centrmeas == 3) { #between
      e <- betweenness(g, normalized = TRUE)
      colvals <- as.numeric(cut(e, breaks = seq(-0.00001, max(e) + 0.0000001, length.out = ncolors + 1)))
      V(g)$xcol <- colorRampPalette(colors = c("blue", "red"))(ncolors)[colvals]
      maxval <- max(e)
      legendcolors <- colorRampPalette(colors = c("blue", "red"))(ncolors)
    }
    if(input$centrmeas == 4) { #closeness
      e <- closeness(g, normalized = TRUE)
      colvals <- as.numeric(cut(e, breaks = seq(-0.00001, max(e) + 0.0000001, length.out = ncolors + 1)))
      V(g)$xcol <- colorRampPalette(colors = c("blue", "red"))(ncolors)[colvals]
      maxval <- max(e)
      legendcolors <- colorRampPalette(colors = c("blue", "red"))(ncolors)
    }
    if(input$centrmeas == 5) { #degree
      e <- degree(g, normalized = TRUE)
      colvals <- as.numeric(cut(e, breaks = seq(-0.00001, max(e) + 0.0000001, length.out = ncolors + 1)))
      V(g)$xcol <- colorRampPalette(colors = c("blue", "red"))(ncolors)[colvals]
      maxval <- max(e)
      legendcolors <- colorRampPalette(colors = c("blue", "red"))(ncolors)
    }

    # deal with clustering
    if(input$clusteralgo == "fast_greedy" | input$clusteralgo == "leading_eigen") {
      if(input$clusteralgo == "fast_greedy") clusterresults <- cluster_fast_greedy(g)
      if(input$clusteralgo == "leading_eigen") clusterresults <- cluster_leading_eigen(g)
      clusterlist <- list()
      for(i in 1:length(clusterresults)) clusterlist[[i]] <- clusterresults[[i]]
    }
    if(input$clusteralgo == "none") {
      clusterlist <- list()
      clusterlist[[1]] <- colnames(mat)
    }

    # return network and relevant information for plotting
    list(g = g, maxval = maxval, legendcolors = legendcolors, clusterlist = clusterlist)
  })

  # plot network
  output$network <- renderPlot({
    # get the network and extract relevant info
    temp <- makegraph()
    G <- temp$g
    xcolors <- temp$legendcolors
    xmax <- temp$maxval
    grplabels <- NULL
    if(input$clusteralgo != "none") grplabels <- temp$clusterlist

    # two options: with or without legend
    if(input$centrmeas == 1) { # no legend...
      par(mar = c(0, 0, 0, 0))
      set.seed(123)
      plot(G, vertex.color = V(G)$xcol, mark.groups = grplabels, edge.width = E(G)$weight * input$weightwidth, label.color = "white")
    }
    if(input$centrmeas > 1) {
      # create layout for a second plot that will contain the legend for the colour gradient
      layout(matrix(1:2, nrow = 1), widths = c(5, 2))
      par(mar = c(0, 0, 0, 0))
      set.seed(123)
      plot(G, vertex.color = V(G)$xcol, mark.groups = grplabels, edge.width = E(G)$weight * input$weightwidth, label.color = "white")
      par(mar = c(0.5, 0.5, 0.5, 0.5))
      # legend
      plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(1, 101), axes = FALSE, ann = FALSE)
      for(i in 1:101) {
        rect(xleft = 0.1, ybottom = i, xright = 0.5, ytop = i + 1, border = NA, col = xcolors[i])
      }
      text(x = 0.3, y = 1, adj = c(0.5, 0), col = "white", font = 2, labels = "0")
      text(x = 0.3, y = 101, adj = c(0.5, 1), col = "white", font = 2, labels = round(xmax, 3))
      textlabel <- NULL
      if(input$centrmeas == 2) textlabel <- "eigenvector centrality"
      if(input$centrmeas == 3) textlabel <- "betweenness centrality"
      if(input$centrmeas == 4) textlabel <- "closeness centrality"
      if(input$centrmeas == 5) textlabel <- "degree"
      text(x = 0.8, y = 51, labels = textlabel, srt = 270, family = "serif", cex = 1.5, font = 2)
    }

  })

  # handle the model output...
  output$model <- renderPlot({
    yrange <- range(xdata$parasiteload)
    xdata$zen <- NA

    if(input$centrmeas == 1) {
      plot(0, 0, type = "n", ylim = yrange, xlim = c(-3, 3), xlab = "", ylab = "parasite load", las = 1)
      text(0, mean(yrange), labels = "please select a centrality measure")
    } else {
      # create predictor variable, i.e. the centrality measure
      G <- makegraph()$g
      if(input$centrmeas == 2) { #eigen (on which the response depended)
        zen <- eigen_centrality(G)$vector
        for(i in 1:nrow(xdata)) xdata$zen[i] <- zen[as.character(xdata$id[i])]
        xl <- "eigenvector centrality"
      }
      if(input$centrmeas == 3) { # between
        zen <- betweenness(g, normalized = TRUE)
        for(i in 1:nrow(xdata)) xdata$zen[i] <- zen[as.character(xdata$id[i])]
        xl <- "betweenness centrality"
      }
      if(input$centrmeas == 4) { # closeness
        zen <- closeness(g, normalized = TRUE)
        for(i in 1:nrow(xdata)) xdata$zen[i] <- zen[as.character(xdata$id[i])]
        xl <- "closeness centrality"
      }
      if(input$centrmeas == 5) { # degree
        zen <- degree(g, normalized = TRUE)
        for(i in 1:nrow(xdata)) xdata$zen[i] <- zen[as.character(xdata$id[i])]
        xl <- "degree centrality"
      }
      # standardize predictor
      xdata$zen <- as.numeric(scale(xdata$zen))

      # select subset and fit model
      xdata <- subset(xdata, xdata$id %in% input$testids)
      mod <- lm(parasiteload ~ zen, data = xdata)
      newdata <- data.frame(zen = range(xdata$zen))
      newdata$predicted <- predict(mod, newdata = newdata)

      # start plot
      plot(xdata$zen, xdata$parasiteload, type = "n", ylim = yrange, xlim = c(-3, 3), xlab = xl, ylab = "parasite load", las = 1)
      text(xdata$zen, xdata$parasiteload, labels = xdata$id)
      # draw model
      points(newdata$zen, newdata$predicted, type = "l", lwd = 2, col = "red")
      # add model results
      Fs <- summary(mod)$fstatistic

      text(-3, 6.9, labels = bquote(parasite~load == .(round(coef(mod)[1], 2)) +  .(round(coef(mod)[2], 2))~centrality), adj=0, cex = 0.6)
      text(-3, 6.7, labels = bquote(italic(F)[list(.(Fs[2]), .(Fs[3]))] == .(round(Fs[1], 2))), adj=0, cex = 0.6)
      text(-3, 6.5, labels = bquote(italic(p) == .(sprintf("%.4f", coefficients(summary(mod))[2, 4]))), adj=0, cex = 0.6)

    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

