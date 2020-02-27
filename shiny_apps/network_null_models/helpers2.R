data_gen_dom <- function(nind = 20, nobs = 100, domeff = 1, std = domeff/2) {
  xdata <- data.frame(dom = rnorm(nind))
  xdata$centr <- nind + xdata$dom * domeff + rnorm(nind, sd = std)
  while(min(xdata$centr) < 0) {
    xdata$centr <- nind + xdata$dom * domeff + rnorm(nind, sd = std)
  }

  pmat <- matrix(ncol = nind, nrow = nind, NA)
  diag(pmat) <- 0
  # make pairwise AI
  success <- FALSE
  while(!success) {
    pmat[] <- NA
    diag(pmat) <- 0
    for (i in 1:(nrow(pmat) - 1)) {
      x <- which(is.na(pmat[i, ]))
      already <- sum(pmat[i, ], na.rm = TRUE)
      temp <- rgamma(n = length(x), shape = 1)
      temp <- temp/sum(temp)
      temp <- temp * (xdata$centr[i] - already)
      sum(temp)
      pmat[i, x] <- temp
      pmat[x, i] <- temp
    }
    if(min(pmat) >= 0) success <- TRUE
    if(rowSums(pmat)[nind] > mean(rowSums(pmat)[-nind]) + 1.5 * sd(rowSums(pmat)[-nind])) success <- FALSE
    if(rowSums(pmat)[nind] < mean(rowSums(pmat)[-nind]) - 1.5 * sd(rowSums(pmat)[-nind])) success <- FALSE
  }


  xdata$centr <- rowSums(pmat)
  xdata$dom <- xdata$centr / domeff + rnorm(nind, mean = 0, sd = std)
  xdata$dom <- as.numeric(scale(xdata$dom))
  #xdata$dom <- (xdata$centr - rnorm(nind, mean = 0, sd = std)) / domeff
  xdata$id <- paste0("i_", sprintf("%02.f", 1:nind))


  mat <- matrix(ncol = nind, nrow = nobs, 0)
  colnames(mat) <- paste0("i_", sprintf("%02.f", 1:nind))

  group_size <- sample(c(1 : (nind)), nobs, replace=TRUE)

  smat <- matrix(1:length(pmat), ncol = ncol(pmat))
  diag(smat) <- 0
  smat[lower.tri(smat)] <- 0
  pmat[lower.tri(pmat)] <- 0
  # N <- ncol(pmat)
  IDS <- colnames(mat)

  for (i in seq_len(nrow(mat))) {
    x <- sample(smat, size = group_size[i] * 2, prob = pmat)
    temp <- c()
    for (k in 1:length(x)) {
      temp <- c(temp, IDS[x[k] %% nind])
      temp <- c(temp, IDS[x[k] %/% nind + 1])
    }
    temp <- unique(temp)
    if (length(temp) > group_size[i]) temp <- temp[1:group_size[i]]
    if (length(temp) < group_size[i]) {
      temp <- c(temp, sample(IDS[-c(which(IDS %in% temp))], group_size[i] - length(temp)))
    }
    mat[i, temp] <- 1
    rm(x, temp)
  }

  xdata$true_obs <- colSums(mat)
  xdata$sri <- rowSums(AI2mat(AI(mat, AI = "SRI")))
  xdata$hwi <- rowSums(AI2mat(AI(mat, AI = "HWI")))

  list(ids = xdata, mat = mat)
}
