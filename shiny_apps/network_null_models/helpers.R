# init <- matrix(0, ncol = 5, nrow = 15)
# colnames(init) <- c(LETTERS[1:ncol(init)])
# set.seed(1234)
# while (0 %in% rowSums(init) | 0 %in% colSums(init)) {
#   init[] <- sample(c(0, 1), length(init), replace = TRUE, prob = c(0.6, 0.4))
# }
#
#
# xmat <- init
seq2mat <- function(xmat) {
  outmat <- matrix(ncol = ncol(xmat), nrow = ncol(xmat), 0)
  colnames(outmat) <- colnames(xmat)
  rownames(outmat) <- colnames(xmat)
  xmat <- xmat[rowSums(xmat) > 1, ]
  for (i in seq_len(nrow(xmat))) {
    temp <- xmat[i, ]
    temp <- names(temp)[temp == 1]
    temp <- combn(temp, 2)
    for (k in seq_len(ncol(temp))) {
      outmat[temp[1, k], temp[2, k]] <- outmat[temp[1, k], temp[2, k]] + 1
    }
  }
  outmat
}

# mat <- seq2mat(init)

# library(igraph)
# g <- graph_from_adjacency_matrix(adjmatrix = mat, weighted = TRUE, mode = "undirected")
# set.seed(1234)
# templout <- layout_nicely(graph = g, dim = 3)
# # templout <- layout_nicely(graph = g)
# plot(g, layout = templout, edge.width = E(g)$weight)



# data generation ---------------------------------------------------------

# input <- list(nind = 6, nobs = 20, npres = 0.3); rdata <- list();
data_gen <- function(nind, nobs, npres, pref = FALSE, avoid = FALSE) {
  init <- matrix(0, ncol = nind, nrow = nobs)
  colnames(init) <- c(LETTERS[1:ncol(init)])
  # set.seed(1234)
  while (0 %in% rowSums(init) | 0 %in% colSums(init)) {
    init[] <- sample(x = c(0, 1),
                     size = length(init),
                     replace = TRUE,
                     prob = c(1 - npres, npres))
    if (pref) {
      init[, 2] <- rbinom(n = nobs, size = 1, prob = init[, 1] * 0.7)
    }
    if (avoid) {
      # cbind(init[, 3:4], rbinom(n = nobs, size = 1, prob = (init[, 3] * (-1) + 1) * 0.8))
      init[, 4] <- rbinom(n = nobs, size = 1, prob = (init[, 3] * (-1) + 1) * 0.8)
    }

  }
  init
}



# AI function ----------------------------------------------------------

# mat <- data_gen(6, nobs = 20, 0.3, pref = TRUE)
#
# nswaps=1
# checkerboards_only=TRUE

swap_association_matrix <- function(mat, nswaps = 1, checkerboards_only = TRUE) {

  swaplocs <- matrix(0, ncol = 5, nrow = nswaps)

  if (!checkerboards_only) {
    xmat <- mat
    for (i in 1:nswaps) {
      # select two random rows and two random columns
      rs <- sort(sample(1:nrow(mat), 2))
      cs <- sort(sample(1:ncol(mat), 2))
      swaplocs[i, 1:2] <- rs
      swaplocs[i, 3:4] <- cs
      M <- xmat[rs, cs]
      # M; is_checkerboard(M)
      # swap only if checkerboard, else do nothing
      if (is_checkerboard(M)) {
        xmat[rs, cs] <- xmat[rs, rev(cs)]
        swaplocs[i, 5] <- 1
      }
      rm(rs, cs, M)
    }
    return(list(mat = xmat, swaplocs = swaplocs))
  }

  if (checkerboards_only) {
    xmat <- mat
    nsw <- 0
    # trials <- 0
    while(nsw < nswaps) {
      # select two individuals
      dyad <- sort(sample(1:ncol(mat), 2))
      # select two rows
      nr <- which(xmat[, dyad[1]] != xmat[, dyad[2]])
      if (length(nr) > 1) {
        r <- sort(sample(nr, 2))
        if (is_checkerboard(xmat[r, dyad])) {
          xmat[r, dyad] <- xmat[r, rev(dyad)]
          nsw <- nsw + 1
          swaplocs[nsw, 1:2] <- r
          swaplocs[nsw, 3:4] <- dyad
          swaplocs[nsw, 5] <- 1
        }
        rm(r)
      }
      # trials <- trials + 1
      rm(dyad, nr)
    }
    # mat[swaplocs[1, 1:2], swaplocs[1, 3:4]]
    # xmat[swaplocs[1, 1:2], swaplocs[1, 3:4]]

    return(list(mat = xmat, swaplocs = swaplocs))
  }
}

AI2mat <- function(res) {
  res$i1 <- as.character(res$i1)
  res$i2 <- as.character(res$i2)
  allids <- unique(c(res$i1, res$i2))
  resmat <- matrix(0, ncol = length(allids), nrow = length(allids))
  colnames(resmat) <- allids
  rownames(resmat) <- allids

  for (i in seq_len(nrow(res))) {
    resmat[res$i1[i], res$i2[i]] <- res$AI[i]
    resmat[res$i2[i], res$i1[i]] <- res$AI[i]
  }
  resmat
}

is_checkerboard <- function(m) {
  if (sum(m) == 2) {
    if (sum(diag(m)) == 2 | sum(diag(m)) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

AI <- function(mat, AI = "HWI") {
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("i", sprintf("%03.f", 1:ncol(mat)))
  }
  if (AI == "HWI") mult <- 0.5
  if (AI == "SRI") mult <- 1

  dyads <- combn(colnames(mat), 2)
  res <- numeric(ncol(dyads))
  # fix yab to 0
  yab <- 0

  for (i in seq_len(ncol(dyads))) {
    X <- sum(mat[, dyads[1, i]] == 1 & mat[, dyads[2, i]] == 1)
    ya <- sum(mat[, dyads[1, i]] == 1 & mat[, dyads[2, i]] == 0)
    yb <- sum(mat[, dyads[2, i]] == 1 & mat[, dyads[1, i]] == 0)
    res[i] <- X / (X + yab + (ya + yb) * mult)
  }
  data.frame(i1 = dyads[1, ], i2 = dyads[2, ], AI = res)
}

# runs = 7
# checkerboards_only=FALSE
AIrand <- function(mat, runs = 1000, checkerboards_only = TRUE, ai = "HWI") {
  res <- AI(mat, AI = ai)
  randmat <- matrix(nrow = nrow(res), ncol = runs)
  xmat <- mat
  swaplocs <- matrix(ncol = 5, nrow = runs)
  adjmats <- list()
  adjmats[[1]] <- AI2mat(res)
  for (i in 1:runs) {
    temp <- swap_association_matrix(mat = xmat, nswaps = 1, checkerboards_only = checkerboards_only)
    xmat <- temp$mat
    swaplocs[i, 1:5] <- temp$swaplocs
    randmat[, i] <- AI(xmat, AI = ai)$AI
    adjmats[[i + 1]] <- AI2mat(AI(xmat, AI = ai))
    rm(temp)
  }

  # add observed values as first column of randomization results
  # (and remove last column to keep nrand at its value)
  randmat <- cbind(res$AI, randmat[, -ncol(randmat)])

  res$expected <- rowMeans(randmat)
  S <- sum((res$AI - res$expected) ^ 2 / ncol(mat) ^ 2)
  S
  svals <- colSums(((randmat - res$expected) ^ 2) / (ncol(mat) ^ 2))

  # get dyadic p values
  res$dyadic_p <- rowSums(randmat >= res$AI) / runs

  # global p
  S_p <- sum(svals >= S) / runs

  # adjacency matrices
  adjmats <- adjmats[-runs]

  list(ores = res, randmat = randmat, S = S, svals = svals, S_p = S_p, nind = ncol(mat), swaplocs = swaplocs, adjmats = adjmats)

}


# plotting functions ------------------------------------------------------
# res = AIrand(mat = mat, runs = 12, checkerboards_only = TRUE, AI = "HWI")
# step = 1
# init = mat
plot_matrix <- function(res, step, init, textcex = 1) {

  if (is.null(init)) {
    init <- res
    par(family = "serif", mar = c(1, 1, 2, 1))
    plot(0, 0, type = "n", xlim = c(-2, ncol(init) + 4), ylim = c(nrow(init) + 1, 0), axes = FALSE, ann = FALSE)
    text(x = 1:ncol(init), y = -0.5, labels = colnames(init), cex = 1 * textcex)
    text(x = rep(1:ncol(init), each = nrow(init)),
         y = rep(1:nrow(init), ncol(init)),
         labels = as.character(init), col = "black", cex = 1 * textcex)
    return(NULL)
  }
  swapmat <- init < -1
  colmat <- apply(swapmat, 2, function(x)c("grey", "black")[x + 1])
  plotmat <- init
  plotframe <- function(step) {
    par(family = "serif", mar = c(1, 1, 2.5, 1))
    plot(0, 0, type = "n", xlim = c(0.5, ncol(init) + 4), ylim = c(nrow(init) + 1, 0), main = step, axes = FALSE, ann = FALSE)

    if (step > 0) {
      xcent <- par()$usr[2] - (par()$usr[2] - ncol(init)) / 2

      # boxes
      ybox <- seq(from = par()$usr[4], to = par()$usr[3], length.out = 9)[c(2, 4, 6, 8)]
      xbox <- xcent + c(-1.2, 1.2)
      rect(xleft = xbox[1], ybottom = ybox[1], xright = xbox[2], ytop = ybox[2])
      rect(xleft = xbox[1], ybottom = ybox[3], xright = xbox[2], ytop = ybox[4])

      # numbers
      ytext <- seq(from = par()$usr[4],
                   to = par()$usr[3],
                   length.out = 17)[c(4, 6, 12, 14, 2, 10)]
      text(x = xcent - diff(xbox) * 0.3 * c(1, 1, -1, -1),
           y = ytext[c(1, 2, 1, 2)],
           labels = as.character(M1),
           cex = 1.3 * textcex,
           font = 2)
      text(x = xcent - diff(xbox) * 0.3 * c(-1, 1),
           y = ytext[5],
           labels = rev(colnames(M1)),
           col = "grey50",
           font = 3,
           cex = 0.8 * textcex)
      text(x = xcent - 1.5,
           y = ytext[1:2],
           labels = rownames(M1),
           col = "grey50",
           font = 3,
           cex = 0.8 * textcex)

      if (temp[5] == 1) {
        text(x = xcent - diff(xbox) * 0.3 * c(1, 1, -1, -1),
             y = ytext[c(3, 4, 3, 4)],
             labels = M2,
             cex = 1.3 * textcex,
             font = 2)
        # text(x = xcent - diff(xbox)*0.3 * c(-1, 1), y = ytext[6], labels = colnames(M1), col = "darkgrey", font = 3)
        # text(x = xcent - 1.5, y = ytext[1:2], labels = rownames(M1), col = "darkgrey", font = 3)

      } else {
        points(rep(xbox, 2), ybox[c(3,4,4,3)], type = "l")
      }
      arrows(xcent, ybox[3] * 0.95, xcent, ybox[2] * 1.05, code = 1, lwd = 2, length = 0.2)

    }

  }

  if (step == 0) {
    plotframe(step = step)
    text(x = rep(1:ncol(init), each = nrow(init)),
         y = rep(1:nrow(init), ncol(init)),
         labels = as.character(init), col = colmat, cex = 1 * textcex)
    title(main = step)
    text(x = 1:ncol(init), y = -0.5, labels = colnames(init), cex = 1 * textcex)
  }

  if (step > 0) {
    # swapmat <- init < -1
    # colmat <- apply(swapmat, 2, function(x)c("grey", "black")[x + 1])
    # plotmat <- init
    # step=2
    # i=2
    for (i in 1:step) {
      temp <- res$swaplocs[i, ]
      rs <- sort(temp[1:2])
      cs <- sort(temp[3:4])
      M1 <- plotmat[rs, cs]
      rownames(M1) <- rs
      M2 <- M1[, 2:1]
      colnames(M2) <- colnames(M1)

      if (temp[5] == 0) {
        colmat2 <- colmat
        colmat2[rs, cs] <- "red"
        if (i == step) {
          plotframe(step = step)
          text(x = 1:ncol(init),
               y = -0.5,
               labels = colnames(init),
               col = c("black", "red")[1:ncol(init) %in% cs + 1],
               cex = 1 * textcex)
          text(x = rep(1:ncol(init), each = nrow(init)),
               y = rep(1:nrow(init), ncol(init)),
               labels = as.character(plotmat),
               col = colmat2,
               cex = 1 * textcex)
        }
      }

      if (temp[5] == 1) {
        swapmat[rs, cs] <- TRUE
        colmat <- apply(swapmat, 2, function(x)c("grey", "black")[x + 1])
        colmat2 <- colmat
        colmat2[rs, cs] <- "red"
        if (i == step) {
          plotframe(step = step)
          text(x = 1:ncol(init),
               y = -0.5,
               labels = colnames(init),
               col = c("black", "red")[1:ncol(init) %in% cs + 1],
               cex = 1 * textcex)
          text(x = rep(1:ncol(init), each = nrow(init)),
               y = rep(1:nrow(init), ncol(init)),
               labels = as.character(plotmat),
               col = colmat2,
               cex = 1 * textcex)
        }
        plotmat[rs, cs] <- plotmat[rs, rev(cs)]
      }
      # plotmat[c(6,12),c("E","F")]
      # init[c(6,12),c("E","F")]
    }
  }
}

plot_hist_global <- function(res, step) {
  randdata <- res$randmat
  evals <- rowMeans(randdata)
  S <- sum((res$ores$AI - evals) ^ 2 / res$nind ^ 2)
  xlims <- range(colSums(((randdata - evals) ^ 2) / (res$nind ^ 2)))

  if (step == 0) {
    svals <- colSums(((res$ores[, "AI", drop = FALSE] - evals) ^ 2) / (res$nind ^ 2))
    # svals <- 1
    S_p <- 1
  }

  if (step > 0) {
    # randdata <- res$randmat[, 1:step, drop = FALSE]
    svals <- colSums(((randdata[, 1:step, drop = FALSE] - evals) ^ 2) / (res$nind ^ 2))
    S_p <- sum(S <= svals) / step
  }


  y <- hist(svals, breaks = seq(xlims[1]*0.9999, xlims[2]*1.0001, length.out = 21), plot = FALSE)
  # y <- hist(svals,  plot = FALSE)
  plot(y, col = "grey30", border = "white", ylim = c(0, max(y$counts) * 1.02), yaxs = "i",
       axes = FALSE, xlab = "", ylab = "", xlim = xlims, main = "global")
  axis(1, tcl = 0, mgp = c(1, 0.2, 0))
  title(xlab = bquote(italic("S")), line = 1.2, cex.lab = 1.3)
  title(ylab = "frequency", line = 0.5, cex.lab = 1.3)

  abline(v = S, col = "red", lwd = 2)


  S_p <- sprintf("%.3f", S_p)

  text(xlims[2], max(y$counts) * 1.05, labels = bquote(italic("p")==.(S_p)), xpd = TRUE, adj = 1)
  box(bty = "l")

}

plot_hist_dyad <- function(res, step, dyad = c("A", "B")) {
  xline <- which(res$ores$i1 == sort(dyad)[1] & res$ores$i2 == sort(dyad)[2])
  x <- c(res$ores$AI[xline], res$randmat[xline, 1:step])
  y <- hist(x, breaks = seq(-0.000001, 1.000001, length.out = 21), plot = FALSE)
  plot(y, col = "grey", border = "white", ylim = c(0, max(y$counts) * 1.02), yaxs = "i",
       main = paste0(sort(dyad)[1], " - ", sort(dyad)[2]), axes = FALSE, xlab = "", ylab = "")
  axis(1, tcl = 0, mgp = c(1, 0.2, 0))
  title(xlab = "association index", line = 1.2, cex.lab = 1.3)
  title(ylab = "frequency", line = 0.5, cex.lab = 1.3)
  # axis(2)
  abline(v = res$ores$AI[xline], col = "red", lwd = 2)
  pval <- sum(res$ores$AI[xline] <= res$randmat[xline, 1:step]) / step
  if (step == 0) pval <- 1
  pval <- sprintf("%.3f", pval)

  text(0.8, max(y$counts) * 1.05, labels = bquote(italic("p")==.(pval)), xpd = TRUE)
  box(bty = "l")
}

plot_hist_id <- function(res, step, id = "A") {
  xrange <- range(unlist(lapply(res, function(x)range(rowSums(x)))))

  x <- unlist(lapply(res[1:step], function(x) sum(x[id, ])))
  xbreaks <- seq(from = floor(xrange[1] * 10) / 10, to = ceiling(xrange[2] * 10) / 10, length.out = 31)
  y <- hist(x, breaks = xbreaks, plot = FALSE)
  plot(y, col = "grey", border = "white", ylim = c(0, max(y$counts) * 1.02), yaxs = "i",
       main = id, axes = FALSE, xlab = "", ylab = "")
  axis(1, tcl = 0, mgp = c(1, 0.2, 0))
  title(xlab = "centrality (strength)", line = 1.2, cex.lab = 1.3)
  title(ylab = "frequency", line = 0.5, cex.lab = 1.3)

  abline(v = x[1], col = "red", lwd = 2)
  pval <- sum(x >= x[1]) / step
  if (step == 0) pval <- 1
  pval <- sprintf("%.3f", pval)

  text(max(xbreaks), max(y$counts) * 1.05, labels = bquote(italic("p")==.(pval)), xpd = TRUE, adj = 1)
  box(bty = "l")
}

nice_hist <- function(xdata, obs = NULL, breaks = NULL) {
  if (!is.null(obs)) xdata[1] <- obs
  if (is.null(breaks)) breaks <- seq(min(xdata), max(xdata), length.out = 21)
  y <- hist(xdata, breaks = breaks, plot = FALSE)

  plot(y, col = "grey", border = "white", ylim = c(0, max(y$counts) * 1.02),
       yaxs = "i", axes = FALSE, xlab = "", ylab = "", main = "")
  axis(1, tcl = 0, mgp = c(1, 0.2, 0))
  title(xlab = "association index", line = 1.2, cex.lab = 1.3)
  title(ylab = "frequency", line = 0.5, cex.lab = 1.3)

  if (!is.null(obs)) {
    abline(v = obs, col = "red", lwd = 2)
    pval <- sum(xdata >= obs) / length(xdata)
    pval <- sprintf("%.3f", pval)
    text(0.8, max(y$counts) * 1.05,
         labels = bquote(italic("p")==.(pval)), xpd = TRUE)

  }
  box(bty = "l")
}


