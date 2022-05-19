x  <-  c(0.1615055828898457, 0.3230111657796915, 0.2907100492017223,
         0.1550453595742519, 0.05426587585098816, 0.01302381020423716,
         0.002170635034039527, 2.48072575318803*10^-4,
         1.860544314891023*10^-5, 8.2690858439601*10^-7,
         1.65381716879202*10^-8, 0.0, 0.1938066994678149,
         0.3488520590420667, 0.2790816472336534, 0.1302381020423716,
         0.03907143061271148, 0.007814286122542296,
         0.001041904816338973, 8.930612711476907*10^-5,
         4.465306355738455*10^-6, 9.922903012752121*10^-8, 0.0, 0.0,
         0.2325680393613778, 0.3721088629782046, 0.2604762040847432,
         0.1041904816338973, 0.02604762040847432, 0.004167619265355891,
         4.167619265355891*10^-4, 2.381496723060509*10^-5,
         5.953741807651273*10^-7,0.0, 0.0, 0.0, 0.2790816472336534,
         0.3907143061271148, 0.2344285836762689, 0.07814286122542295,
         0.01562857224508459, 0.001875428669410151,
         1.250285779606767*10^-4, 3.572245084590763*10^-6,0.0, 0.0,
         0.0, 0.0, 0.3348979766803841, 0.4018775720164609,
         0.2009387860082305, 0.05358367626886145, 0.008037551440329218,
         6.430041152263375*10^-4, 2.143347050754458*10^-5,0.0, 0.0,
         0.0, 0.0, 0.0, 0.4018775720164609, 0.4018775720164609,
         0.1607510288065844, 0.03215020576131687, 0.003215020576131687,
         1.286008230452675*10^-4,0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.4822530864197531, 0.3858024691358025, 0.1157407407407407,
         0.0154320987654321, 7.716049382716049*10^-4,0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.5787037037037037, 0.3472222222222222,
         0.06944444444444445, 0.004629629629629629,0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.6944444444444444, 0.2777777777777778,
         0.02777777777777778,0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.8333333333333334, 0.1666666666666667,0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0)
P  <-  t( matrix(x, 11, 11) )

PPow  <- P
tenzicdf  <- c(PPow[1,11])
for (i in 2:50) {
  PPow  <- PPow %*% P;
  tenzicdf  <- c(tenzicdf, PPow[1,11])
}

tenzipdf  <-  tenzicdf[2:50] - tenzicdf[1:49]
tenzipdf  <- c(tenzicdf[1], tenzipdf)

sum(tenzipdf)
tenzimean  <- (1:50) %*% tenzipdf

Q  <-  P[1:10, 1:10]
I10  <-  diag(10)
N  <- solve( I10 - Q, I10)

mean  <- N %*% rep(1,10)
Varw  <- ((2 * N - I10) %*% N) %*% rep(1, 10) - ( N %*% rep(1, 10) )^2
sdw  <- sqrt(Varw)
cat("Mean waiting time: ", mean[1], "\nSD of waiting time:", sdw[1], "\n");

plot(x = 1:50, tenzicdf, col="red", xlab="Rolls", ylab="Probability")
points(1:50, tenzicdf, type="l", col="red")
points(1:50, tenzipdf, col="blue")
points(1:50, tenzipdf, type="l", col="blue")

## NAME: tenzi.R -- Compute probabilities and statistics of Tenzi game
## USAGE: within R, at interactive prompt
##        source("tenzi.R")
## REQUIRED ARGUMENTS: none
## OPTIONS:  none
## DESCRIPTION: Compute probabilities and statistics of Tenzi game
##   using the transition probability matrix and the normal matrix.           
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: R base, including simple graphics
## DEPENDENCIES: R base
## INCOMPATIBILITIES: none known
## PROVENANCE: Steve Dunbar, created on 26 Oct 2020 10:23:22 AM CDT
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS: ggplot2 makes prettier plots
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Mon 26 Oct 2020 10:23:48 AM CDT
## KEYWORDS: Markov chain, matrix power, pdf, cdf, waiting time

