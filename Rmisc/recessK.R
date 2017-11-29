recessK <- function(flow, dates) {
  if (any(is.na(flow))) {
    retVal <- NA
  } else {
    testDF <- data.frame(dates = dates, flow = flow)
    testDF$diffQ <- c(0, diff(flow)) 
    testDF$diffLog <- if_else(testDF$diffQ < 0, "a", "b")
    testRle <- data.frame(lengths = rle(testDF$diffLog)$lengths,
                          vals = rle(testDF$diffLog)$values)
    testRle$cumVal <- cumsum(testRle$lengths)
    limVal <- quantile(testRle[which(testRle$vals == "a"), 1], probs = 0.97)
    
}