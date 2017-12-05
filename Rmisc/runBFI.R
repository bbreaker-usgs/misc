waterYEar <- function (x, numeric = FALSE) 
{
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if (numeric) 
    return(yr)
  ordered(yr)
}

runBFI <- function(flow, dates, f = 0.9, N = 5L) {
  flow <- pmax(flow[sel], 10^(-2.5))
  by <- match.arg(by, c("water year", "calendar year", "continuous"))
  if (by == "calendar year") {
    Cut <- c(seq(0, 360, by = N, ), 366)
    Yr <- year(dates)
    dayno <- yday
  }
  else if (by == "water year") {
    Cut <- c(seq(0, 360, by = N, ), 366)
    Yr <- waterYear(dates, numeric = TRUE)
    dayno <- function(x) {
      Jul <- as.integer(x)
      lWY <- waterYear(x, numeric = TRUE) - 1L
      baseWY <- as.integer(as.Date(ISOdate(lWY, 9, 30)))
      return(Jul - baseWY)
    }
  }
  else {
    Cut <- c(seq(0, length(flow) + N, by = N))
    Yr <- rep(1L, length(flow))
    dayno <- function(x) seq(1L, by = 1L, length.out = length(x))
  }
  DF <- data.frame(dates = dates, Q = flow)
  ret <- by(DF, Yr, function(DF) {
    Jul <- dayno(DF$dates)
    Grp <- cut(Jul, Cut, labels = FALSE)
    retval <- tapply(DF$Q, Grp, function(x) {
      Min <- min(x)
      Wch <- which(Min == x)[1L]
      return(c(Min, Wch))
    })
    retval <- do.call("rbind", retval)
    retval[, 2L] <- retval[, 2L] + Cut[unique(Grp)]
    return(retval)
  })
  Yrtbl <- cumsum(c(0, table(Yr)))
  for (i in seq(length(ret))) 
    ret[[i]][, 2L] <- ret[[i]][, 2L] + Yrtbl[i] 
  Jstrt <- dayno(dates[1L])
  if (Jstrt > 1L) 
    ret[[1L]][, 2L] <- ret[[1L]][, 2L] - Jstrt + 1L
  ret <- do.call("rbind", ret)
  TP <- rep(FALSE, nrow(ret))
  for (i in seq(2L, nrow(ret) - 1L)) {
    if (ret[i, 1L] == 0) {
      TP[i] <- TRUE
    }
    else if (ret[i - 1L, 1L] == 0) {
      TP[i] <- f * ret[i, 1L] <= ret[i + 1L, 1L]
    }
    else if (ret[i + 1L, 1L] == 0) {
      TP[i] <- f * ret[i, 1L] <= ret[i - 1L, 1L]
    }
    else {
      TP[i] <- f * ret[i, 1L] <= min(ret[i - 1L, 1L], ret[i + 1L, 1L])
    }
  }
  TPdat <- ret[TP, ]
  BaseQ <- rep(NA, length = length(flow))
  for (i in seq(1L, nrow(TPdat) - 1L)) {
    Rng <- seq(TPdat[i, 2L], TPdat[i + 1L, 2L])
    if (TPdat[i, 1L] == 0 || TPdat[i + 1L, 1L] == 0) {
      BaseQ[Rng] <- pmin(Flow[Rng], 
                         seq(TPdat[i, 1L], TPdat[i + 1L, 1L], 
                             length.out = TPdat[i + 1L, 2L] - TPdat[i, 2L] + 1L))
    }
    else BaseQ[Rng] <- pmin(Flow[Rng], 
                            exp(seq(log(TPdat[i, 1L]),
                                    log(TPdat[i + 1L, 1L]), 
                                    length.out = TPdat[i + 1L, 2L] - TPdat[i, 2L] + 1L)))
  }
  retval <- BaseQ
  return(retval)
}
