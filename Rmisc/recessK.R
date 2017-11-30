recessK <- function(flow, dates) {
  
  library(dplyr)
  
  if (any(is.na(flow))) {
    
    retVal <- NA
    
  } else {
    
    testDF <- data.frame(dates = dates, flow = flow)
    
    testDF$diffQ <- c(0, diff(flow)) 
    
    testDF$diffLog <- if_else(testDF$diffQ < 0, "fall", "notFall")
    
    testRle <- data.frame(lengths = rle(testDF$diffLog)$lengths,
                          vals = rle(testDF$diffLog)$values)
    
    testRle$cumVal <- cumsum(testRle$lengths)
    
    limVal <- quantile(testRle[which(testRle$vals == "fall"), 1], 
                       probs = 0.99)
    
    testRleDF <- testRle %>%
      mutate(qualK = if_else(vals != "fall", 0, 
                             if_else(lengths < limVal, 0, 1)))
    
    
  }
}