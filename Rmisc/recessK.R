recessK <- function(flow, baseQ, dates) {
  
  library(dplyr, quietly = TRUE)
  
  if (any(is.na(flow))) {
    
    retVal <- NA
    
  } else {
    
    testDF <- data.frame(dates = dates, flow = flow, baseQ = baseQ)
    
    testDF$diffQ <- testDF$flow - testDF$baseQ
    
    testDF$diffLog <- dplyr::if_else(testDF$diffQ == 0, "base", "event")
    
    testRle <- data.frame(lengths = rle(testDF$diffLog)$lengths,
                          vals = rle(testDF$diffLog)$values)
    
    testRle$cumVal <- cumsum(testRle$lengths)
    
    testRleDF <- testRle %>% 
      mutate(qualk = if_else(vals == "base", 0, 1)) %>% 
      group_by(qualk) %>% 
      mutate(index = ifelse(qualk == 0, 0, 1:n())) %>% 
      data.frame()
    
    for (i in seq(1, max(testRleDF$index) - 1, 1)) {
      
      chunk <- testRleDF[(which(testRleDF$index == i)):(which(testRleDF$index == i) + 1), ]
      
      chunk <- testDF[(chunk[1, 3]):(chunk[2, 3] + 1), ]
      
      kValEvent <- as.numeric()
      
      if (nrow(chunk) <= 4) {
        
        kValEvent_ <- NA
        
      } else {
        
        for (k in seq(2, nrow(chunk) - 2, 1)) {
          
          kValEvent_ <- chunk[(k + 1), 3] / chunk[k, 3]
          
          kValEvent <- append(kValEvent, kValEvent_)
          
        }
        
      }
      
      kVal <- signif(mean(kValEvent, na.rm = TRUE), 3)
      
    }
    
  }
  
  return(kVal)
  
}
