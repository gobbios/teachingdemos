
create_graph <- function(type, A, B, offs_A = NULL, offs_B = NULL) {
  par(family = "serif")
  create_circle <- function(A, B, Acol, Bcol) {
    rvals <- seq(0, 2 * pi, length.out = 101)
    xp1 <- 20 * cos(rvals) + 20
    yp1 <- 20 * sin(rvals) + 30
    xp2 <- 20 * cos(rvals) + 80
    yp2 <- 20 * sin(rvals) + 30
    pd <- data.frame(xp1, yp1, xp2, yp2)
    polygon(pd$xp1, pd$yp1, border = NA, col = Acol)
    polygon(pd$xp2, pd$yp2, border = NA, col = Bcol)
  }
  color_legend <- function(allcols) {
    res <- seq(0, 100, length.out = 101)
    for (i in 1:100) {
      rect(xleft = res[i + 1], ybottom = 85, xright = res[i + 2], ytop = 95, border = NA, col = allcols[i], xpd = TRUE)
    }
    rect(xleft = 1, ybottom = 85, xright = 100, ytop = 95, border = "grey", xpd = TRUE)
    text(x = -5, y = 90, labels = "0", xpd = TRUE)
    text(x = 105, y = 90, labels = "100", xpd = TRUE)
  }


  plot(0, 0, type = "n", xlim = c(-1, 101), ylim = c(0, 100), yaxs = "i", xaxs = "i", axes = FALSE, ann = FALSE, asp = 1)
  box()

  if (type == "length") {
    ytop <- offs_A
    ybot <- ytop - A
    segments(x0 = 20, y0 = ybot, x1 = 20, y1 = ytop, lwd = 2)

    ytop <- offs_B
    ybot <- ytop - B
    segments(x0 = 80, y0 = ybot, x1 = 80, y1 = ytop, lwd = 2)
    axis(2, at = c(0, 50, 100), las = 1)

  }

  if (type == "angle") {
    rvals <- seq(0, 0.5 * pi, length.out = 100)
    xp <- cos(rvals)
    yp <- sin(rvals)
    points(c(0 + 20, xp[round(A)] * 30 + 20), c(0 + 30, yp[round(A)] * 30 + 30), type = "l", lwd = 2)
    points(c(0 + 20, xp[1] * 30 + 20), c(0 + 30, yp[1] * 30 + 30), type = "l", lwd = 2)

    points(c(0 + 80, xp[round(B)] * 30 + 80), c(0 + 30, yp[round(B)] * 30 + 30), type = "l", lwd = 2)
    points(c(0 + 80, xp[1] * 30 + 80), c(0 + 30, yp[1] * 30 + 30), type = "l", lwd = 2)

    # legend
    points(c(0 + 45, xp[1]   * 10 + 45), c(0 + 80,   yp[1] * 10 + 80), type = "l")
    points(c(0 + 45, xp[50]  * 10 + 45), c(0 + 80,  yp[50] * 10 + 80), type = "l")
    points(c(0 + 45, xp[100] * 10 + 45), c(0 + 80, yp[100] * 10 + 80), type = "l")
    text(57, 80, "0", cex = 0.7)
    text(45, 92, "100", cex = 0.7)
    text(54, 87, "50", cex = 0.7)
  }


  if (type == "circlesize") {
    exfac <- 3
    # area: pi*r^2
    rvals <- seq(0, 2 * pi, length.out = 101)
    # xp <- rad * cos(rvals) + x
    # yp <- rad * sin(rvals) + y + rad
    xp1 <- sqrt(A*exfac/pi) * cos(rvals) + 20
    yp1 <- sqrt(A*exfac/pi) * sin(rvals) + 40
    xp2 <- sqrt(B*exfac/pi) * cos(rvals) + 80
    yp2 <- sqrt(B*exfac/pi) * sin(rvals) + 40
    polygon(xp1, yp1, border = NA, col = "grey")
    polygon(xp2, yp2, border = NA, col = "grey")

    # legend
    for (i in seq(20, 100, by = 20)) {
      xp <- sqrt(i*exfac/pi) * cos(rvals)
      yp <- sqrt(i*exfac/pi) * sin(rvals)
      polygon(xp + i - 10, yp + 85, border = NA, col = "grey")
      text(i - 10, 85, labels = i, cex = 0.7)
    }

  }

  if (type == "viridis") {
    # create colour scale
    # allcols <- hcl.colors(n = 100, palette = "viridis")
    allcols <-  c("#4B0055", "#4B0057", "#4B015A", "#4A075C", "#4A0E5F", "#491461", "#481964", "#471D66", "#462169", "#45256B", "#44286E", "#422C70", "#402F72", "#3E3375", "#3C3677", "#3A3979", "#373D7B", "#33407D", "#30437F", "#2B4681", "#264983", "#204C85", "#185086", "#0B5388", "#00568A", "#00598B", "#005C8D", "#005F8E", "#00628F", "#006490", "#006791", "#006A92", "#006D93", "#007094", "#007395", "#007696", "#007896", "#007B97", "#007E97", "#008197", "#008398", "#008698", "#008998", "#008B98", "#008E98", "#009097", "#009397", "#009597", "#009896", "#009A95", "#009D95", "#009F94", "#00A193", "#00A492", "#00A691", "#00A890", "#00AA8F", "#00AC8D", "#00AF8C", "#00B18A", "#00B389", "#00B587", "#00B785", "#00B983", "#00BB81", "#00BD7F", "#00BE7D", "#00C07A", "#00C278", "#00C476", "#14C673", "#2BC770", "#3AC96D", "#46CA6B", "#51CC68", "#5BCE64", "#64CF61", "#6CD05E", "#74D25B", "#7CD357", "#84D454", "#8BD650", "#93D74D", "#9AD849", "#A1D946", "#A7DA42", "#AEDB3E", "#B5DC3B", "#BBDD38", "#C2DE34", "#C8DF32", "#CEE02F", "#D4E12D", "#DBE12C", "#E1E22B", "#E7E22B", "#ECE32C", "#F2E32E", "#F8E330", "#FDE333")

    # draw circles
    create_circle(A, B, Acol = allcols[round(A)], Bcol = allcols[round(B)])
    # legend
    color_legend(allcols = allcols)
  }

  if (type == "rainbow") {
    # create colour scale
    allcols <- rainbow(100)
    # draw circles
    create_circle(A, B, Acol = allcols[round(A)], Bcol = allcols[round(B)])
    # legend
    color_legend(allcols = allcols)
  }

  if (type == "grey") {
    # create colour scale
    allcols <- gray.colors(n = 100, start = 1, end = 0)
    # draw circles
    create_circle(A, B, Acol = allcols[round(A)], Bcol = allcols[round(B)])
    # legend
    color_legend(allcols = allcols)
  }

  if (type == "point") {
    axis(2, at = c(0, 50, 100), las = 1)
    points(20, A, pch = 16, cex = 1.5)
    points(80, B, pch = 16, cex = 1.5)
  }

  if (type == "bar") {
    axis(2, at = c(0, 50, 100), las = 1)
    rect(15, 0, 25, A, border = NA, col = "grey")
    rect(75, 0, 85, B, border = NA, col = "grey")
    # axis(1, at = c(20, 80), lwd = 0, labels = c("A", "B"), line = -1)
  }

  if (type == "square") {
    Ax <- sqrt(A)
    Bx <- sqrt(B)
    rect(xleft = 20 - Ax/2,
         ybottom = 50 - Ax/2,
         xright = 20 + Ax/2,
         ytop = 50 + Ax/2,
         border = NA, col = "grey")
    rect(xleft = 80 - Bx/2,
         ybottom = 50 - Bx/2,
         xright = 80 + Bx/2,
         ytop = 50 + Bx/2,
         border = NA, col = "grey")
    # legend
    xleg <- seq(20, 80, length.out = 5)
    sizes <- seq(20, 100, by = 20)
    for (i in 1:5) {
      rect(xleft = xleg[i] - sqrt(sizes[i])/2, ybottom = 85 - sqrt(sizes[i])/2, xright = xleg[i] + sqrt(sizes[i])/2, ytop = 85 + sqrt(sizes[i])/2, border = NA, col = "grey")
      text(xleg[i], 95, labels = sizes[i], cex = 0.7)
    }
  }
  axis(1, at = c(20, 80), lwd = 0, labels = c("A", "B"), line = -1)

}
