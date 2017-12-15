recessK <- function(flow, baseQ, dates) {
  
  library(dplyr)
  
  if (any(is.na(flow))) {
    
    retVal <- NA
    
  } else {
    
    testDF <- data.frame(dates = dates, flow = flow, baseQ = baseQ)
    
    testDF$diffQ <- testDF$flow - testDF$baseQ
    
    testDF$diffLog <- if_else(testDF$diffQ == 0, "base", "event")
    
    testRle <- data.frame(lengths = rle(testDF$diffLog)$lengths,
                          vals = rle(testDF$diffLog)$values)
    
    testRle$cumVal <- cumsum(testRle$lengths)
    
    #testRleDF <- testRle %>%
    #  mutate(qualK = if_else(vals != "fall", 0, 
    #                         if_else(lengths < limVal, 0, 1))) %>% 
    #  group_by(qualK) %>% 
    #  mutate(Index = ifelse(qualK == 0, 0, 1:n())) %>% 
    #  data.frame()
    
    testRleDF <- testRle %>% 
      mutate(qualk = if_else(vals == "base", 0, 1)) %>% 
      group_by(qualk) %>% 
      mutate(index = ifelse(qualk == 0, 0, 1:n())) %>% 
      data.frame()
    
    retVal <- as.numeric()

    for (i in seq(1, nrow(dplyr::filter(testRleDF, qualK == 1)), 1)) {
      
      chunk <- testRleDF[(which(testRleDF$Index == i) - 1):which(testRleDF$Index == i), ]
      
      chunk <- testDF[(chunk[1, 3] + 1):chunk[2, 3], ]
      
      chunk <- chunk[(nrow(chunk) * 0.4):(nrow(chunk) * 0.8),]
      
      kVal <- coef(lm(flow ~ dates, data = chunk))[2]
      
      kVal <- signif(kVal, 3)
      
      retVal <- append(retVal, kVal)
      
    }
    
  }
  
  return(mean(retVal, na.rm = TRUE))
  
}