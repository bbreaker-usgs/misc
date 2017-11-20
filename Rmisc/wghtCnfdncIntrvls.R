########################################### REGION C #######################################################
Xi <- matrix(data=c(1, log10(1072), log10(128), (0.01*14.3) + 1, (0.01*98.9) + 1), nrow=1, ncol=5)
#matRegC10
RRE <- RRE10
EMA <- 106200
Vreg <- 0.028
Vsite <- 0.0022
q10 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.024174612000,	0.000702356110,	-0.008624702100,	-0.010166176000,	-0.002429211500),
                c2 = c(0.000702356110,	0.000687059200,	-0.002513714500,	0.000443399400,	-0.000035419213),
                c3 = c(-0.008624702100,	-0.002513714500,	0.017632494000,	-0.001907968200,	-0.000211953610),
                c4 = c(-0.010166176000,	0.000443399400,	-0.001907968200,	0.009363413400,	-0.000436503030),
                c5 = c(-0.002429211500,	-0.000035419213,	-0.000211953610,	-0.000436503030,	0.002431138000))
U <- as.matrix(U)
t <- 1.665
MEV <- 0.024
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt10 <- RRE/T
upperInt10 <- RRE*T
Xsite <- log10(86370)
Xreg <- log10(lowerInt10)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(133200)
Xreg <- log10(upperInt10)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q10

#matRegC4
RRE <- RRE4
EMA <- 147000
Vreg <- 0.028
Vsite <- 0.0031
q4 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.025364785000,	0.000776223610,	-0.009254320000,	-0.010039544000,	-0.002730015100),
                c2 = c(0.000776223610,	0.000708962530,	-0.002603126100,	0.000461401130,	-0.000038390150),
                c3 = c(-0.009254320000,	-0.002603126100,	0.018230876000,	-0.001961157800,	-0.000209024890),
                c4 = c(-0.010039544000,	0.000461401130,	-0.001961157800,	0.009340801600,	-0.000453927460),
                c5 = c(-0.002730015100,	-0.000038390150,	-0.000209024890,	-0.000453927460,	0.002685125300))
U <- as.matrix(U)
t <- 1.665
MEV <- 0.023
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt4 <- RRE/T
upperInt4 <- RRE*T
Xsite <- log10(116400)
Xreg <- log10(lowerInt4)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(196400)
Xreg <- log10(upperInt4)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q4

#matRegC1
RRE <- RRE1
EMA <- 216500
Vreg <- 0.028
Vsite <- 0.0056
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.030560984000,	0.000963429820,	-0.011289236000,	-0.011586197000,	-0.003463236000),
                c2 = c(0.000963429820, 0.000844515070, -0.003109763000,	0.000556049610,	-0.000046184338),
                c3 = c(-0.011289236000,	-0.003109763000,	0.021754983000,	-0.002335228500,	-0.000250580890),
                c4 = c(-0.011586197000,	0.000556049610,	-0.002335228500,	0.010871475000,	-0.000546072040),
                c5 = c(-0.003463236000,	-0.000046184338,	-0.000250580890,	-0.000546072040,	0.003365953700))
U <- as.matrix(U)
t <- 1.665
MEV <- 0.024
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(161500)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(328700)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q1

########################################### REGION A #######################################################

Xi <- matrix(data=c(1, log10(34.4), log10(1300)), nrow=1, ncol=3)
#matRegA10
RRE <- RRE10
EMA <- 5211
Vreg <- 0.019
Vsite <- 0.0061
q10 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.173417730000,	0.002571548100,	-0.060411569000),
                c2 = c(0.002571548100,	0.000436143300,	-0.001135499500),
                c3 = c(-0.060411569000,	-0.001135499500,	0.021334926000))
U <- as.matrix(U)
t <- 1.669
MEV <- 0.017
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt10 <- RRE/T
upperInt10 <- RRE*T
Xsite <- log10(3833)
Xreg <- log10(lowerInt10)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(8500)
Xreg <- log10(upperInt10)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q10

#matRegA4
RRE <- RRE4
EMA <- 6676
Vreg <- 0.020
Vsite <- 0.0085
q4 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.195152600000,	0.003022656000,	-0.068155157000),
                c2 = c(0.003022656000,	0.000483771900,	-0.001325218900),
                c3 = c(-0.068155157000,	-0.001325218900,	0.024140914000))
U <- as.matrix(U)
t <- 1.669
MEV <- 0.017
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt4 <- RRE/T
upperInt4 <- RRE*T
Xsite <- log10(4765)
Xreg <- log10(lowerInt4)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(12650)
Xreg <- log10(upperInt4)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q4

#matRegA1
RRE <- RRE1
EMA <- 8957
Vreg <- 0.024
Vsite <- 0.0136
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.252263590000,	0.004001944500,	-0.088197911000),
                c2 = c(0.004001944500,	0.000616122030,	-0.001743968200),
                c3 = c(-0.088197911000,	-0.001743968200,	0.031274713000))
U <- as.matrix(U)
t <- 1.669
MEV <- 0.020
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(6016)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(21420)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q1

########################################### REGION B1 #######################################################

Xi <- matrix(data=c(1, log10(2680), log10(10.49)), nrow=1, ncol=3)
#matRegB110
RRE <- 186440
EMA <- 44370
Vreg <- 0.027
Vsite <- 0.0014
q10 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.023356932000,	0.000205587030,	-0.023785620000),
                c2 = c(0.000205587030,	0.001064609600,	-0.002577943800),
                c3 = c(-0.023785620000,	-0.002577943800,	0.031196476000))
U <- as.matrix(U)
t <- 1.692
MEV <- 0.025
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt10 <- RRE/T
upperInt10 <- RRE*T
Xsite <- log10(38340)
Xreg <- log10(lowerInt10)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(55430)
Xreg <- log10(upperInt10)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q10

#matRegB14
RRE <- 233080
EMA <- 54550
Vreg <- 0.030
Vsite <- 0.0024
q4 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.027140861000,	0.000130902430,	-0.027285908000),
                c2 = c(0.000130902430,	0.001255371000,	-0.002959310800),
                c3 = c(-0.027285908000,	-0.002959310800,	0.035804076000))
U <- as.matrix(U)
t <- 1.692
MEV <- 0.025
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt4 <- RRE/T
upperInt4 <- RRE*T
Xsite <- log10(45840)
Xreg <- log10(lowerInt4)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(76880)
Xreg <- log10(upperInt4)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q4

#matRegB11
RRE <- 300950
EMA <- 70800
Vreg <- 0.034
Vsite <- 0.0050
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.252263590000,	0.004001944500,	-0.088197911000),
                c2 = c(0.004001944500,	0.000616122030,	-0.001743968200),
                c3 = c(-0.088197911000,	-0.001743968200,	0.031274713000))
U <- as.matrix(U)
t <- 1.692
MEV <- 0.029
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(56210)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(125800)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q1

########################################### REGION D #######################################################
#DRNAREA, SLPFM, BSHAPE
Xi <- matrix(data=c(1, log10(1137), log10(1.04), log10(60)), nrow=1, ncol=4)
#matRegD10
RRE <- RRE10
EMA <- 7985
Vreg <- 0.024
Vsite <- 0.0004
q10 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.008712242300, 0.000420754700,	-0.003214500500,	-0.007622486000),
                c2 = c(0.000420754700, 0.000793972980,	0.000245713690,	-0.001726102300),
                c3 = c(-0.003214500500, 0.000245713690, 0.003796728100,	0.001780234300),
                c4 = c(-0.007622486000, -0.001726102300, 0.001780234300,	0.009919235700))
U <- as.matrix(U)
t <- 1.691
MEV <- 0.021
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt10 <- RRE/T
upperInt10 <- RRE*T
Xsite <- log10(7414)
Xreg <- log10(lowerInt10)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(9268)
Xreg <- log10(upperInt10)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q10

#matRegD2
RRE <- RRE2
EMA <- 8893
Vreg <- 0.037
Vsite <- 0.0010
q4 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.016430437000,	0.000781834050,	-0.006165508500, -0.014366426000),
                c2 = c(0.000781834050,	0.001519732000,	0.000490948230,	-0.003288149500),
                c3 = c(-0.006165508500, 0.000490948230,	0.007301038200,	0.003354098900),
                c4 = c(-0.014366426000, -0.003288149500,	0.003354098900,	0.018755365000))
U <- as.matrix(U)
t <- 1.691
MEV <- 0.032
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt4 <- RRE/T
upperInt4 <- RRE*T
Xsite <- log10(7927)
Xreg <- log10(lowerInt4)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(11310)
Xreg <- log10(upperInt4)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q4

#matRegD1
RRE <- RRE10
EMA <- 9226
Vreg <- 0.037
Vsite <- 0.0013
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(0.019103223000,	0.000907549580,	-0.007187895200,	-0.016704249000),
                c2 = c(0.000907549580,	0.001770663800,	0.000574473580,	-0.003828937000),
                c3 = c(-0.007187895200,	0.000574473580,	0.008508913500,	0.003901638300),
                c4 = c(-0.016704249000,	-0.003828937000,	0.003901638300,	0.021817852000))
U <- as.matrix(U)
t <- 1.691
MEV <- 0.037
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(8090)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(12280)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q1

########################################### REGION B2 #######################################################

Xi <- matrix(data=c(1, log10(24.1), log10(56.9), (2.68), (0.01*5.38) + 1, (0.01*0) + 1), nrow=1, ncol=6)

#matRegB22
RRE <- 186440
EMA <- 44370
Vreg <- 0.027
Vsite <- 0.0014
q10 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(15.4165760000,  0.0022875563, -7.3246402000, -0.0106979460, -2.2927335000, -0.0235299400),
                c2 = c( 0.0022875563,  0.0004617545, -0.0030746748, -0.0002262749,  0.0024091212,  0.0004386855),
                c3 = c(-7.3246402000, -0.0030746748,  3.9133557000,  0.0432996030,  0.4075633900, -0.0017089317),
                c4 = c(-0.0106979460, -0.0002262749,  0.0432996030,  0.0052091874,  0.0168314330, -0.0012588263),
                c5 = c(-2.2927335000,  0.0024091212,  0.4075633900,  0.0168314330,  1.4514484000,  0.0208231910),
                c6 = c(-0.0235299400,  0.0004386855, -0.0017089317, -0.0012588263,  0.0208231910,  0.0073444606))

U <- as.matrix(U)
t <- 1.676
MEV <- 0.021
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt10 <- RRE/T
upperInt10 <- RRE*T
Xsite <- log10(38340)
Xreg <- log10(lowerInt10)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(55430)
Xreg <- log10(upperInt10)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q2

#matRegB24
RRE <- 233080
EMA <- 54550
Vreg <- 0.030
Vsite <- 0.0024
q4 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(13.1326960000,  0.0029207513, -6.3625334000, -0.0871479720, -1.7679156000, -0.0157931960),
                c2 = c( 0.0029207513,  0.0003878490, -0.0032030541, -0.0002026209,  0.0021049064,  0.0003723216),
                c3 = c(-6.3625334000, -0.0032030541,  3.4517718000,  0.0352794450,  0.2776530600, -0.0028808990),
                c4 = c(-0.0871479720, -0.0002026209,  0.0352794450,  0.0042776791,  0.0135463230, -0.0010071926),
                c5 = c(-1.7679156000,  0.0021049064,  0.2776530600,  0.0135463230,  1.1774179000,  0.0160544240),
                c6 = c(-0.0157931960,  0.0003723216, -0.0028808990, -0.0010071926,  0.0160544240,  0.0060171382))
U <- as.matrix(U)
t <- 1.676
MEV <- 0.015
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt4 <- RRE/T
upperInt4 <- RRE*T
Xsite <- log10(45840)
Xreg <- log10(lowerInt4)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(76880)
Xreg <- log10(upperInt4)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q5

#matRegB210
RRE <- 300950
EMA <- 70800
Vreg <- 0.034
Vsite <- 0.0050
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(14.3975800000,  0.0038403898, -7.0450728000, -0.0938189220, -1.8326658000, -0.0139353150),
                c2 = c( 0.0038403898,  0.0004169905, -0.0038922071, -0.0002234062,  0.0023431740,  0.0003985861),
                c3 = c(-7.0450728000, -0.0038922071,  3.8488811000,  0.0381646470,  0.2680109700, -0.0043453467),
                c4 = c(-0.0938189220, -0.0002234062,  0.0381646470,  0.0045852581,  0.0142821040, -0.0010592567),
                c5 = c(-1.8326658000,  0.0023431740,  0.2680109700,  0.0142821040,  1.2524882000,  0.0163710800),
                c6 = c(-0.0139353150,  0.0003985861, -0.0043453467, -0.0010592567,  0.0163710800,  0.0064337446))
U <- as.matrix(U)
t <- 1.676
MEV <- 0.015
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(56210)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(125800)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q10

#matRegB225
RRE <- 300950
EMA <- 70800
Vreg <- 0.034
Vsite <- 0.0050
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(16.3290350000,  0.0051690606, -8.0784164000, -0.1044652500, -1.9442024000, -0.0115202980),
                c2 = c( 0.0051690606,  0.0004620910, -0.0049171053, -0.0002540411,  0.0027299803,  0.0004375924),
                c3 = c(-8.0784164000, -0.0049171053,  4.4457790000,  0.0428130320,  0.2594211400, -0.0063278122),
                c4 = c(-0.1044652500, -0.0002540411,  0.0428130320,  0.0050661071,  0.0154207610, -0.0011503897),
                c5 = c(-1.9442024000,  0.0027299803,  0.2594211400,  0.0154207430,  1.3689177000,  0.0168888290),
                c6 = c(-0.0115202980,  0.0004375924, -0.0063278122, -0.0011503897,  0.0168888290,  0.0070600125))
U <- as.matrix(U)
t <- 1.676
MEV <- 0.015
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(56210)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(125800)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q25

#matRegB250
RRE <- 300950
EMA <- 70800
Vreg <- 0.034
Vsite <- 0.0050
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c(18.6386230000,  0.0063241418, -9.2562905000, -0.1187756000, -2.1653653000, -0.0109190450),
                c2 = c( 0.0063241418,  0.0005203089, -0.0058488614, -0.0002888047,  0.0031149996,  0.0004894731),
                c3 = c(-9.2562905000, -0.0058488614,  5.1049723000,  0.0489315450,  0.2807799100, -0.0079798694),
                c4 = c(-0.1187756000, -0.0002888047,  0.0489315450,  0.0057231083,  0.0171860010, -0.0012903941),
                c5 = c(-2.1653653000,  0.0031149996,  0.2807799100,  0.0171860010,  1.5383222000,  0.0184647840),
                c6 = c(-0.0109190450,  0.0004894731, -0.0079798694, -0.0012903941,  0.0184647840,  0.0079385458))
U <- as.matrix(U)
t <- 1.676
MEV <- 0.017
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(56210)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(125800)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q50

#matRegB2100
RRE <- 8806
EMA <- 70800
Vreg <- 0.034
Vsite <- 0.0050
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c( 21.7881790000,  0.0076434952, -10.8267130000, -0.1391719100, -2.5217369000, -0.0114773420),
                c2 = c(  0.0076434952,  0.0006025355,  -0.0069354230, -0.0003357077,  0.0035779857,  0.0005643553),
                c3 = c(-10.8267130000, -0.0069354230,   5.9705865000,  0.0575566080,  0.3288740700, -0.0098417906),
                c4 = c( -0.1391719100, -0.0003357077,   0.0575566080,  0.0066702272,  0.0198581020, -0.0014983140),
                c5 = c( -2.5217369000,  0.0035779857,   0.3288740700,  0.0198581020,  1.7894204000,  0.0212394280),
                c6 = c( -0.0147734200,  0.0005643553,  -0.0098417906, -0.0014983140,  0.0212394280,  0.0092281114))
U <- as.matrix(U)
t <- 1.676
MEV <- 0.019
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(56210)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(125800)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q100

#matRegB2500
RRE <- 300950
EMA <- 70800
Vreg <- 0.034
Vsite <- 0.0050
q1 <- signif(10^(((log10(EMA)*Vreg)+(log10(RRE)*Vsite))/(Vsite+Vreg)),3)
U <- data.frame(c1 = c( 28.0141710000,  0.0106626620, -13.9705740000, -0.1787427000, -3.1659104000, -0.0107248850),
                c2 = c(  0.0106626620,  0.0007607080,  -0.0093207560, -0.0004295010,  0.0044999613,  0.0007059275),
                c3 = c(-13.9705740000, -0.0093207560,   7.7162266000,  0.0745022540,  0.4062445900, -0.0140282770),
                c4 = c( -0.0178742740, -0.0004295010,   0.0745022540,  0.0084834840,  0.0247301230, -0.0018927986),
                c5 = c( -3.1659104000,  0.0044999613,   0.4062445900,  0.0247301230,  2.2591334000,  0.0258901830),
                c6 = c( -0.0107248850,  0.0007059275,  -0.0140282770, -0.0018927986,  0.0258901830,  0.0116318320))
U <- as.matrix(U)
t <- 1.676
MEV <- 0.023
XiP <- t(Xi)
UXiP <- U %*% XiP
XiUXiP <- Xi %*% UXiP
Si <- (MEV + XiUXiP)^0.5
T <- 10^(t*Si)
lowerInt1 <- RRE/T
upperInt1 <- RRE*T
Xsite <- log10(56210)
Xreg <- log10(lowerInt1)
#lower10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
Xsite <- log10(125800)
Xreg <- log10(upperInt1)
#upper10
signif(10^(((Xsite*Vreg)+(Xreg*Vsite))/(Vsite+Vreg)), 3)
q500