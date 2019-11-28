
# type = "bar"
# A = 80
# B = 24
create_graph <- function(type, A, B) {
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
      rect(xleft = res[i + 1], ybottom = 85, xright = res[i + 2], ytop = 95, border = NA, col = allcols[i])
    }
    text(x = -5, y = 90, labels = "0", xpd = TRUE)
    text(x = 105, y = 90, labels = "100", xpd = TRUE)
  }


  plot(0, 0, type = "n", xlim = c(0, 100), ylim = c(0, 100), yaxs = "i", xaxs = "i", axes = FALSE, ann = FALSE, asp = 1)
  box()


  if (type == "angle") {
    rvals <- seq(0, 0.5 * pi, length.out = 100)
    xp <- cos(rvals)
    yp <- sin(rvals)
    # A=50
    # B = 100
    points(c(0 + 20, xp[round(A)] * 30 + 20), c(0 + 30, yp[round(A)] * 30 + 30), type = "l", lwd = 2)
    points(c(0 + 80, xp[round(B)] * 30 + 80), c(0 + 30, yp[round(B)] * 30 + 30), type = "l", lwd = 2)
    # points(xp * 30 + 20, yp * 30 +40, cex=0.1)
    # points(xp * 30 + 80, yp * 30 +40, cex=0.1)

    # legend
    points(c(0 + 45, xp[1]   * 10 + 45), c(0 + 80,   yp[1] * 10 + 80), type = "l")
    points(c(0 + 45, xp[50]  * 10 + 45), c(0 + 80,  yp[50] * 10 + 80), type = "l")
    points(c(0 + 45, xp[100] * 10 + 45), c(0 + 80, yp[100] * 10 + 80), type = "l")
    text(57, 80, "0", cex = 0.7)
    text(45, 92, "100", cex = 0.7)
    text(54, 87, "50", cex = 0.7)

    # points(xp * 10 + 45, yp * 10 +80, cex=0.1)
    # abline(h = 80)
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
    allcols <- hcl.colors(n = 101, palette = "viridis")
    # draw circles
    create_circle(A, B, Acol = allcols[round(A)], Bcol = allcols[round(B)])
    # legend
    color_legend(allcols = allcols)
  }

  if (type == "rainbow") {
    # create colour scale
    allcols <- rainbow(101)
    # draw circles
    create_circle(A, B, Acol = allcols[round(A)], Bcol = allcols[round(B)])
    # legend
    color_legend(allcols = allcols)
  }

  if (type == "grey") {
    # create colour scale
    allcols <- gray.colors(n = 101, start = 1, end = 0)
    # draw circles
    create_circle(A, B, Acol = allcols[round(A)], Bcol = allcols[round(B)])
    # legend
    color_legend(allcols = allcols)
  }

  if (type == "bar") {
    axis(2, at = c(0, 100), las = 1)
    rect(15, 0, 25, A, border = NA, col = "grey")
    rect(75, 0, 85, B, border = NA, col = "grey")
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
  }
  axis(1, at = c(20, 80), lwd = 0, labels = c("A", "B"), line = -1)

}
