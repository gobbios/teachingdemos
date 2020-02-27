library(shiny)
library(igraph)

source("helpers.R")
source("helpers2.R")


shinyServer(function(input, output) {
    # handle data values
    rdata <- reactiveValues()
    rdata$xdata <- matrix()
    rdata$dataexists <- FALSE
    rdata$randomizationsexist <- FALSE
    rdata$AI <- matrix()
    rdata$amats <- NA
    # handle timer
    timer <- reactiveValues()
    timer$timer <- reactiveTimer()

    observeEvent(input$nind, {
        g <- matrix(1:input$nind^2, ncol = input$nind)
        g <- graph_from_adjacency_matrix(g)
        rdata$network_layout <- layout_nicely(g)
    })

    observeEvent(eventExpr = input$gendata, {
        # input <- list(nind = 6, nobs = 20, npres = 0.3, prefdyad=T,avoiddyad=F, ai_meth = "SRI", checkerboardsonly=TRUE); rdata <- list();
        rdata$xdata <- data_gen(nind = input$nind,
                                nobs = input$nobs,
                                npres = input$npres,
                                pref = input$prefdyad,
                                avoid = input$avoiddyad)
        temp <- AIrand(mat = rdata$xdata,
                       runs = 1,
                       checkerboards_only = input$checkerboardsonly,
                       ai = "SRI")
        rdata$AI <- AI2mat(temp$ores)

        rdata$randomizationsexist <- FALSE
        rdata$dataexists <- TRUE
    })
    # input$nrand=10
    observeEvent(eventExpr = input$genrand, {
        if (rdata$dataexists) {
            rdata$rand_data <- AIrand(mat = rdata$xdata,
                                      runs = input$nrand,
                                      checkerboards_only = input$checkerboardsonly,
                                      ai = "SRI")
            rdata$randomizationsexist <- TRUE
        }
    })

    # reactive values for chaining
    chaindata <- reactiveValues()
    chaindata$smat <- NA
    chaindata$dyad1 <- NA
    chaindata$dyad2 <- NA
    chaindata$svals <- NA

    chaindata$chaindataexist <- FALSE

    observeEvent(eventExpr = input$runchains, {
        # input <- list(nswaps2 = 100, nchains=5, checkerboardsonly2=T); chaindata <- list(); chaindata$smat <- NA
        if (rdata$dataexists) {
            for (i in seq_len(input$nchains)) {
                temp <- AIrand(mat = rdata$xdata,
                               runs = input$nswaps2,
                               checkerboards_only = input$checkerboardsonly2,
                               ai = "SRI")
                if (i == 1) {
                    chaindata$smat <- as.matrix(temp$svals)
                } else {
                    chaindata$smat <- cbind(chaindata$smat, temp$svals)
                }
                chaindata$dyad1[i] <- temp$randmat[1, input$nswaps2]
                chaindata$dyad2[i] <- temp$randmat[10, input$nswaps2]
                chaindata$svals[i] <- temp$svals[input$nswaps2]
            }
            chaindata$chaindataexist <- TRUE

        }
    })

    domcent_data <- reactiveValues()
    domcent_data$data_exists <- FALSE
    domcent_data$rand_exists <- FALSE

    # input <- list(nind_domcentr = 25, nobs_domcentr=200,nruns_dom=50);domcent_data <- list()
    observeEvent(eventExpr = input$gen_domcentr, {
        domcent_data$mat <- data_gen_dom(nind = input$nind_domcentr, nobs = input$nobs_domcentr, domeff = 2, std = 1)
        domcent_data$data_exists <- TRUE
        domcent_data$rand_exists <- FALSE
    })
    observeEvent(eventExpr = input$run_domcentr, {
        xres <- AIrand(mat = domcent_data$mat$mat, runs = input$nruns_dom, checkerboards_only = TRUE, ai = "SRI")
        ests <- matrix(ncol = 2, nrow = ncol(xres$randmat))
        for (i in 1:length(ests[, 1])) {
            temp <- xres$ores
            temp$AI <- xres$randmat[, i]
            temp <- rowSums(AI2mat(temp))
            ests[i, ] <- coefficients(lm(temp ~ domcent_data$mat$ids$dom))[1:2]
        }
        domcent_data$rand_exists <- TRUE
        # domcent_data$odata <- domcent_data$mat$ids
        domcent_data$ests <- ests
        domcent_data$rand_data <- xres
    })

    output$domcentr <- renderPlot({
        if (domcent_data$data_exists) {
            par(family = "serif", mfrow = c(1, 2), mar = c(4.5, 4.5, 4.5, 1))
            temp <- domcent_data$mat$ids

            mod <- lm(sri ~ dom, data = temp)
            pdata <- coefficients(mod)[1] + range(temp$dom) * coefficients(mod)[2]
            plot(temp$dom, temp$sri, las = 1, xlab = "dominance rank", ylab = "centrality (strength)", ylim = range(pdata, temp$sri), pch = 16)
            points(range(temp$dom), pdata, type = "l", col = "red")
            estb <- sprintf(fmt = "%.2f", coefficients(mod)[2])
            estp <- sprintf(fmt = "%.4f", coefficients(summary(mod))[2, 4])
            lab <- bquote(plain("linear regression: ")*beta==.(estb)*plain(",")~italic(p)==.(estp))
            mtext(text = lab, side = 3, line = 0)



            if (domcent_data$rand_exists) {
                par(family = "serif", mar = c(4.5, 2.5, 4.5, 1))
                breaks <- seq(min(domcent_data$ests[, 2]), max(domcent_data$ests[, 2]), length.out = 41)
                y <- hist(domcent_data$ests[, 2], breaks = breaks, plot = FALSE)
                plot(y, col = "grey", border = "white", ylim = c(0, max(y$counts) * 1.02),
                     yaxs = "i", axes = FALSE, xlab = "", ylab = "", main = "", xlim = range(breaks))
                axis(1, tcl = 0, mgp = c(1, 0.2, 0))
                title(xlab = "parameter estimate from model", line = 1.2, cex.lab = 1.3)
                title(ylab = "frequency", line = 0.5, cex.lab = 1.3)
                abline(v = domcent_data$ests[1, 2], col = "red", lwd = 2, xpd = FALSE)
                pval <- sum(domcent_data$ests[, 2] >= domcent_data$ests[1, 2]) / nrow(domcent_data$ests)
                pval <- sprintf("%.3f", pval)
                lab <- bquote(plain("randomizations: ")*.(input$nruns_dom)*plain(",")~italic(p)==.(pval))
                mtext(text = lab, side = 3, line = 0)
                box(bty = "l")

            } else {
                par(mar = c(0, 0, 0, 0), family = "serif")
                mat <- AI2mat(AIrand(domcent_data$mat$mat, runs = 1)$ores)
                g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
                # plot(g)
                domseq <- seq(from = min(temp$dom) - 0.01, to = max(temp$dom) + 0.01, length.out = 101)
                allcols <- hcl.colors(n = 101, palette = "Blues 2", rev = TRUE)
                # plot(g)
                vsize <- rowSums(mat)
                vsize <- vsize - min(vsize) + 1
                vsize <- vsize * input$nind_domcentr
                #
                ewidth <- 1/10
                vcol <- allcols[cut(temp$dom, breaks = domseq)]
                plot(g, vertex.label = "",
                     vertex.color = vcol,
                     vertex.size = vsize,
                     vertex.frame.color = NA, edge.width = ewidth)
            }
        }


    })


    output$distPlot <- renderPlot({
        if (rdata$dataexists) {
            if (input$display_step <= input$nrand) {
                if (rdata$randomizationsexist) {
                    lmat <- matrix(c(1, 1, 1, 1, 2, 3, 5, 7,  2, 4, 6, 7), ncol = 3)
                    layout(lmat, widths = c(4, 2, 2), heights = c(0.8, 4, 4, 4))
                    layout.show(max(lmat))
                    par(mar = c(2, 2, 2, 8))
                    # matrix and swaps
                    plot_matrix(res = rdata$rand_data, step = input$display_step,
                                init = rdata$xdata, textcex = input$textcex)
                    # text
                    par(mar = c(0, 0, 0, 0), family = "serif")
                    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
                    x <- rdata$rand_data$swaplocs[1:input$display_step, 5]
                    lab <- paste0("selected matrices: ", length(x), "; swapped checkerboards: ", sum(x))
                    text(0, 0, labels = lab, cex = 1.5)

                    # individual strength
                    par(mar = c(2.5, 1.5, 4, 1))
                    plot_hist_id(res = rdata$rand_data$adjmats,
                                 step = input$display_step,
                                 id = "A")
                    plot_hist_id(res = rdata$rand_data$adjmats,
                                 step = input$display_step,
                                 id = "C")

                    # dyadic indices
                    par(mar = c(2.5, 1.5, 4, 1))
                    plot_hist_dyad(res = rdata$rand_data,
                                   step = input$display_step,
                                   dyad = c("A", "B"))
                    plot_hist_dyad(res = rdata$rand_data,
                                   step = input$display_step,
                                   dyad = c("C", "D"))

                    # group level
                    plot_hist_global(res = rdata$rand_data,
                                     step = input$display_step)
                } else {
                    par(mfrow = c(1, 2))
                    plot_matrix(res = rdata$xdata,
                                step = NULL,
                                init = NULL,
                                textcex = input$textcex)
                    g <- graph_from_adjacency_matrix(adjmatrix = rdata$AI,
                                                     weighted = TRUE,
                                                     mode = "undirected")
                    plot(g,
                         edge.width = E(g)$weight/0.1,
                         layout = rdata$network_layout)
                }
            } else {
                par(family = "serif", mar = c(0, 0, 0, 0))
                plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
                text(0, 0, "display value is higher than number of randomizations", cex = 2)
            }
        } else {
            par(family = "serif", mar = c(0, 0, 0, 0))
            plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
            text(0, 0, "generate data first,\nthen run randomizations", cex = 2)
        }
    }, height = 800)

    output$chainplot <- renderPlot({
        if (chaindata$chaindataexist) {
            histbreaks <- seq(-0.000001, 1.000001, length.out = 21)
            set.seed(123)
            h <- AIrand(mat = rdata$xdata, runs = 100, checkerboards_only = input$checkerboardsonly2, ai = "SRI")
            rm(.Random.seed, envir=globalenv())
            par(mfrow = c(2, 2), family = "serif")
            par(mar = c(2, 2, 2.5, 1))
            nice_hist(chaindata$dyad1, obs = h$ores$AI[1], breaks = histbreaks)
            title(main = "A - B")
            nice_hist(chaindata$dyad2, obs = h$ores$AI[10], breaks = histbreaks)
            title(main = "C - D")

            par(mar = c(4, 3, 2.5, 1))
            xlims <- range(c(range(chaindata$svals), h$S)) * c(0.98, 1.02)
            hist(chaindata$svals, main = "", xlab = bquote(italic("S")), xlim = xlims, tcl = 0, mgp = c(1.5, 0.5, 0), breaks = 21, xpd = TRUE, yaxs = "i", ylab = "frequency", axes = FALSE)
            axis(1, mgp = c(1.5, 0.5, 0), tcl = 0)
            box(bty = "l")
            abline(v = h$S, col = "red", lwd = 2)
            plot(0, 0, type = "n", xlim = c(0, input$nswaps2), ylim = range(chaindata$smat), xlab = "swap trial", ylab = bquote(italic("S")), las = 1)
            for (i in seq_len(input$nchains)) {
                points(chaindata$smat[, i], type = "l", lwd = 0.2)
            }
        } else {
            if (rdata$dataexists) {
                par(family = "serif", mar = c(0, 0, 0, 0))
                plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
                text(0, 0, "data exists,\nyou can run chains", cex = 2)
            } else {
                par(family = "serif", mar = c(0, 0, 0, 0))
                plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
                text(0, 0, "generate data first,\nthen run chains", cex = 2)
            }
        }
    })

    output$networkplot <- renderPlot({
        if (ndata$nexists) {
            set.seed(1)
            l <- layout_nicely(ndata$n)
            rm(.Random.seed, envir=globalenv())
            plot(ndata$n, edge.width = E(ndata$n)$weight * 20, layout = l)
        }


    })
})
