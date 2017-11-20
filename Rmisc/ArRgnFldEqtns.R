###################################### region A  ###############################################

aep50rgnA <- function(DRNAREA) {
  x <- (10^2.322)*(DRNAREA^0.732)
  return(x)
}

aep20rgnA <- function(DRNAREA, ELEV) {
  x <- (10^1.822)*(DRNAREA^0.714)*(ELEV^0.273)
  return(x)
}

aep10rgnA <- function(DRNAREA, ELEV) {
  x <- (10^1.603)*(DRNAREA^0.706)*(ELEV^0.400)
  return(x)
}

aep4rgnA <- function(DRNAREA, ELEV) {
  x <- (10^1.379)*(DRNAREA^0.696)*(ELEV^0.532)
  return(x)
}

aep2rgnA <- function(DRNAREA, ELEV) {
  x <- (10^1.245)*(DRNAREA^0.689)*(ELEV^0.613)
  return(x)
}

aep1rgnA <- function(DRNAREA, ELEV) {
  x <- (10^1.126)*(DRNAREA^0.683)*(ELEV^0.684)
  return(x)
}

aep0.2rgnA <- function(DRNAREA, ELEV) {
  x <- (10^0.903)*(DRNAREA^0.672)*(ELEV^0.821)
  return(x)
}


###################################### region B1 ###############################################

aep50rgnB1 <- function(DRNAEA, BSHAPE) {
  x <- (10^2.850)*(DRNAREA^0.761)*(BSHAPE^-0.462) 
  return(x)
} 

aep20rgnB1 <- function(DRNAEA, BSHAPE) {
  x <- (10^3.171)*(DRNAREA^0.737)*(BSHAPE^-0.510) 
  return(x)
} 

aep10rgnB1 <- function(DRNAEA, BSHAPE) {
  x <- (10^3.329)*(DRNAREA^0.723)*(BSHAPE^-0.526) 
  return(x)
}

aep4rgnB1 <- function(DRNAEA, BSHAPE) {
  x <- (10^3.490)*(DRNAREA^0.707)*(BSHAPE^-0.535) 
  return(x)
}

aep2rgnB1 <- function(DRNAEA, BSHAPE) {
  x <- (10^3.589)*(DRNAREA^0.696)*(BSHAPE^-0.537) 
  return(x)
}

aep1rgnB1 <- function(DRNAEA, BSHAPE) {
  x <- (10^3.674)*(DRNAREA^0.686)*(BSHAPE^-0.536) 
  return(x)
}

aep0.2rgnB1 <- function(DRNAEA, BSHAPE) {
  x <- (10^3.839)*(DRNAREA^0.665)*(BSHAPE^-0.525) 
  return(x)
}

###################################### region B2 ###############################################

aep50rgnB2 <- function(DRNAREA, PRECIP, SOILINDEX, LC11DVOPN, ALVM) {
  x <- (DRNAREA^0.605)*(PRECIP^4.208)*(10^((0.479*(SOILINDEX))+(5.175*(0.01*LC11DVOPN+1))-(0.314*(0.01*ALVM+1))-11.495)) 
  return (x)
}

aep20rgnB2 <- function(DRNAREA, PRECIP, SOILINDEX, LC11DVOPN, ALVM) {
  x <- (DRNAREA^0.608)*(PRECIP^5.309)*(10^((0.412*(SOILINDEX))+(4.380*(0.01*LC11DVOPN+1))-(0.284*(0.01*ALVM+1))-12.141)) 
  return (x)
}

aep10rgnB2 <- function(DRNAREA, PRECIP, SOILINDEX, LC11DVOPN, ALVM) {
  x <- (DRNAREA^0.610)*(PRECIP^5.745)*(10^((0.380*(SOILINDEX))+(4.085*(0.01*LC11DVOPN+1))-(0.269*(0.01*ALVM+1))-12.376)) 
  return (x)
}

aep4rgnB2 <- function(DRNAREA, PRECIP, SOILINDEX, LC11DVOPN, ALVM) {
  x <- (DRNAREA^0.611)*(PRECIP^6.102)*(10^((0.348*(SOILINDEX))+(3.862*(0.01*LC11DVOPN+1))-(0.254*(0.01*ALVM+1))-12.546)) 
  return (x)
}

aep2rgnB2 <- function(DRNAREA, PRECIP, SOILINDEX, LC11DVOPN, ALVM) {
  x <- (DRNAREA^0.612)*(PRECIP^6.261)*(10^((0.329*(SOILINDEX))+(3.805*(0.01*LC11DVOPN+1))-(0.247*(0.01*ALVM+1))-12.626)) 
  return (x)
}

aep1rgnB2 <- function(DRNAREA, PRECIP, SOILINDEX, LC11DVOPN, ALVM) {
  x <- (DRNAREA^0.612)*(PRECIP^6.383)*(10^((0.312*(SOILINDEX))+(3.809*(0.01*LC11DVOPN+1))-(0.242*(0.01*ALVM+1))-12.719)) 
  return (x)
}

aep0.2rgnB2 <- function(DRNAREA, PRECIP, SOILINDEX, LC11DVOPN, ALVM) {
  x <- (DRNAREA^0.612)*(PRECIP^6.593)*(10^((0.280*(SOILINDEX))+(3.879*(0.01*LC11DVOPN+1))-(0.233*(0.01*ALVM+1))-12.917)) 
  return (x)
}

###################################### region C  ###############################################

aep50rgnC <- function(DRNAREA, BSHAPE, LC11PAST, UPZ) {
  x <- (DRNAREA^0.666)*(10^(2.636-(0.297*(0.01*LC11PAST+1))+(0.111*(0.01*UPZ+1)))) 
  return (x)
}

aep20rgnC <- function(DRNAREA, BSHAPE, LC11PAST, UPZ) {
  x <- (DRNAREA^0.675)*(10^(2.893-(0.307*(0.01*LC11PAST+1))+(0.127*(0.01*UPZ+1)))) 
  return (x)
}

aep10rgnC <- function(DRNAREA, BSHAPE, LC11PAST, UPZ) {
  x <- (DRNAREA^0.725)*(BSHAPE^-0.321)*(10^(3.159-(0.272*(0.01*LC11PAST+1))+(0.136*(0.01*UPZ+1)))) 
  return(x)
}

aep4rgnC <- function(DRNAREA, BSHAPE, LC11PAST, UPZ) {
  x <- (DRNAREA^0.745)*(BSHAPE^-0.416)*(10^(3.329-(0.258*(0.01*LC11PAST+1))+(0.142*(0.01*UPZ+1)))) 
  return(x)
}

aeprgnC <- function(DRNAREA, BSHAPE, LC11PAST, UPZ) {
  x <- (DRNAREA^0.757)*(BSHAPE^-0.474)*(10^(3.434-(0.249*(0.01*LC11PAST+1))+(0.147*(0.01*UPZ+1)))) 
  return(x)
}

aeprgnC <- function(DRNAREA, BSHAPE, LC11PAST, UPZ) {
  x <- (DRNAREA^0.768)*(BSHAPE^-0.522)*(10^(3.522-(0.242*(0.01*LC11PAST+1))+(0.152*(0.01*UPZ+1)))) 
  return(x)
}

aep0.2rgnC <- function(DRNAREA, BSHAPE, LC11PAST, UPZ) {
  x <- (DRNAREA^0.789)*(BSHAPE^-0.616)*(10^(3.692-(0.224*(0.01*LC11PAST+1))+(0.160*(0.01*UPZ+1)))) 
  return(x)
}

###################################### region D  ###############################################

aep50rgnD <- function(DRNAREA, SLPFM, BSHAPE) {
  x <- (10^2.183)*(DRNAREA^0.678)*(SLPFM^0.239)*(BSHAPE^-0.324) 
  return(x)
}

aep20rgnD <- function(DRNAREA, SLPFM, BSHAPE) {
  x <- (10^2.310)*(DRNAREA^0.694)*(SLPFM^0.344)*(BSHAPE^-0.348) 
  return(x)
}

aep10rgnD <- function(DRNAREA, SLPFM, BSHAPE) {
  x <- (10^2.374)*(DRNAREA^0.702)*(SLPFM^0.399)*(BSHAPE^-0.359) 
  return(x)
}

aep4rgnD <- function(DRNAREA, SLPFM, BSHAPE) {
  x <- (10^2.441)*(DRNAREA^0.709)*(SLPFM^0.457)*(BSHAPE^-0.370) 
  return(x)
}

aep2rgnD <- function(DRNAREA, SLPFM, BSHAPE) {
  x <- (10^2.483)*(DRNAREA^0.713)*(SLPFM^0.494)*(BSHAPE^-0.376) 
  return(x)
}

aep1rgnD <- function(DRNAREA, SLPFM, BSHAPE) {
  x <- (10^2.520)*(DRNAREA^0.716)*(SLPFM^0.527)*(BSHAPE^-0.381) 
  return(x)
}

aep0.2rgnD <- function(DRNAREA, SLPFM, BSHAPE) {
  x <- (10^2.594)*(DRNAREA^0.722)*(SLPFM^0.592)*(BSHAPE^-0.391) 
  return(x)
}

