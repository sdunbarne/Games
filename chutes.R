library("markovchain")

rotvec <- function(vec) vec[ c(length(vec), 1:(length(vec)-1)) ]

nStates <- 100
chutesladdersStates <- as.character( c(1:nStates) )

diceRoll <- matrix(0, nStates, nStates)
p1 <- c(0,
        1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
        numeric(nStates-7)
        )
for (i in 1:(nStates-5)) {
    diceRoll[i, ] <- p1
    p1 <- rotvec(p1)
}
diceRoll[100, 100] <- 1

diceRoll[99, 99] <- 5/6; diceRoll[99, 100] <- 1/6

diceRoll[98, 98] <- 4/6; diceRoll[98, 99] <- 1/6; diceRoll[98, 100] <- 1/6

diceRoll[97, 97] <- 3/6; diceRoll[97, 98] <- 1/6
diceRoll[97, 99] <- 1/6; diceRoll[97, 100] <- 1/6

diceRoll[96, 96] <- 2/6; diceRoll[96, 97] <- 1/6
diceRoll[96, 98] <- 1/6; diceRoll[96, 99] <- 1/6; diceRoll[96, 100] <- 1/6

ladders <- diag(nStates)
ladderIn <-  c( 1,  4,  9, 21, 28, 36, 51, 71,  80)
ladderOut <- c(38, 14, 31, 42, 84, 44, 67, 91, 100)
ladders[ cbind(ladderIn, ladderIn) ] <- 0
ladders[ cbind(ladderIn, ladderOut) ] <- 1

chutes <- diag(nStates)
chuteIn <-  c(16, 47, 49, 56, 62, 64, 87, 93, 95, 98)
chuteOut <- c( 6, 26, 11, 53, 19, 60, 24, 73, 75, 78)
chutes[ cbind(chuteIn, chuteIn) ] <- 0
chutes[ cbind(chuteIn, chuteOut) ] <- 1

chutesladdersMat <-  diceRoll %*% ladders %*% chutes

chutesladders <- new("markovchain", states = chutesladdersStates, byrow = TRUE,
                     transitionMatrix = chutesladdersMat, name = "ChutesLadders")
meanMC  <- meanAbsorptionTime(object=chutesladders)

Q  <- chutesladdersMat[1:99, 1:99]
I99  <-  diag(99)
N  <- solve( I99 - Q, I99)
absorptionTimeMat  <-  N %*% rep(1, 99)
meanMat  <- mean( absorptionTimeMat[ c(38, 2, 3, 14, 5, 6)] )
varabsorptionTimeMat  <- ((2 * N - I99) %*% N) %*% rep(1, 99) - ( N %*% rep(1, 99) )^2
sdAbsorptionTime  <-  sqrt( mean( varWait[ c(38, 2, 3, 14, 5, 6) ] ) )

cat("Mean waiting time from markovchain library: ", meanMC[1], "\nMean waiting time from normal matrix: ", meanMat + 1, "\nSD of waiting time:", sdAbsorptionTime[1], "\n");

## NAME: chutes.R -- Compute statistics of Chutes and Ladders game
## USAGE: within R, at interactive prompt
##        source("chutes.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: Compute statistics of the Chutes and Ladders game
##        using the R markovchain library and also the normal matrix      
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: R, markovchain
## DEPENDENCIES:  R, markovchain
## INCOMPATIBILITIES: None known
## PROVENANCE: Steve Dunbar, created on Tue 27 Oct 2020 08:58:48 AM CDT
## BUGS AND LIMITATIONS: None known
## FEATURES AND POTENTIAL IMPROVEMENTS: Maybe add some graphics
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Tue 27 Oct 2020 08:59:29 AM CDT
## KEYWORDS: Markov chain, absorbing states, waiting time absorption








