boardLength <- 40

animalList <- c("C", "D", "P", "S", "T")
NAnimals <- length(animalList) + 1  # one more for fox

blueSquares <- numeric(boardLength)
blueSquares[4] <- 1
blueSquares[8] <- 1
blueSquares[22] <- 1
blueSquares[35] <- 1
blueSquares[39] <- 1

totalNumberOfBlues <- sum(blueSquares)

# Setting up the board
board <- character(boardLength)

board[1] <- "B"
board[2] <- "S"
board[3] <- "P"
board[4] <- "T"
board[5] <- "C"
board[6] <- "D"
board[7] <- "P"
board[8] <- "C"
board[9] <- "D"
board[10] <- "S"
board[11] <- "T"
board[12] <- "B"
board[13] <- "C"
board[14] <- "P"
board[15] <- "B"
board[16] <- "B"
board[17] <- "B"
board[18] <- "T"
board[19] <- "B"
board[20] <- "T"
board[21] <- "D"
board[22] <- "S"
board[23] <- "C"
board[24] <- "D"
board[25] <- "P"
board[26] <- "T"
board[27] <- "B"
board[28] <- "S"
board[29] <- "C"
board[30] <- "B"
board[31] <- "B"
board[32] <- "T"
board[33] <- "P"
board[34] <- "S"
board[35] <- "D"
board[36] <- "B"
board[37] <- "S"
board[38] <- "C"
board[39] <- "P"
board[40] <- "CDPST"

# Count how many states we have
statesIndex <- 1  #initial state (0,0)
statesCount <- 1
statesHolder <- matrix(0, boardLength * totalNumberOfBlues, 2)

for (i in 1:(boardLength - 1)) {
    # don't use last space, the 'coop'
    if (board[i] != "B") {
        bLi <- sum(blueSquares[1:i])  #blueSquares to Left of i, including i
        bRi <- sum(blueSquares[(i + 1):boardLength])  #blueSquares to Right of i
        
        maxChickens <- min(boardLength, i + bLi)
        minChickens <- max(0, i - bRi)
        
        statesCount <- statesCount + maxChickens - minChickens + 1
        for (j in minChickens:maxChickens) {
            statesIndex <- statesIndex + 1
            statesHolder[statesIndex, ] <- c(i, j)
        }
    }
}
statesHolder[statesCount + 1, ] <- c(Inf, Inf)  # win state
statesHolder[statesCount + 2, ] <- c(-Inf, -Inf)  # lose state
statesCount <- statesCount + 2  #add final won and loss states
states <- statesHolder[1:statesCount, ]
rm(statesHolder)

Q <- matrix(0, statesCount - 2, statesCount - 2)
R <- matrix(0, statesCount - 2, 2)
I <- diag(2)
Z <- matrix(0, 2, statesCount - 2)

for (i in 1:(statesCount - 2)) {
    currentSquare <- states[i, 1]
    chicksInCoop <- states[i, 2]
    currentState <- i
    
    # This sets transition probabilities from spinning a fox
    if (chicksInCoop == 0) {
        Q[i, i] <- 1/NAnimals  # stay in current state, prob 1/NAnimals
    } else if (states[i - 1, 1] == currentSquare) {
        Q[i, i - 1] <- 1/NAnimals  # remove 1 chick, fill trans prob
    } else {
        # too few chicks, must lose
        R[i, 2] <- R[i, 2] + 1/NAnimals  # so add to lose probability
    }
    
    # Now set transition probabilities from other five animals.
    for (animal in animalList) {
        
        nextSquare <- grep(animal, board[(currentSquare + 1):boardLength])[1] + currentSquare
        
        if (blueSquares[nextSquare] == 0) {
            # not Blue, just add advance
            updatedChicksInCoop <- chicksInCoop + (nextSquare - currentSquare)
            if (updatedChicksInCoop > boardLength) 
                updatedChicksInCoop <- boardLength
        } else {
            # Blue square, so add extra chick
            updatedChicksInCoop <- chicksInCoop + (nextSquare - currentSquare) + 
                1
            if (updatedChicksInCoop > boardLength) 
                updatedChicksInCoop <- boardLength
        }
        
        in_states <- FALSE
        
        for (p in 1:statesCount) {
            if (states[p, 1] == nextSquare && states[p, 2] == updatedChicksInCoop) {
                in_states <- TRUE
                newLocation <- p
                break
            }
        }
        
        if (in_states == TRUE) {
            Q[i, newLocation] <- 1/NAnimals
        } else {
            if (nextSquare == boardLength && updatedChicksInCoop == boardLength) 
                {
                  R[i, 1] <- R[i, 1] + 1/NAnimals
                }  # win
 else {
                R[i, 2] <- R[i, 2] + 1/NAnimals  #lose
            }
        }
    }
}

# Calculate win-lose probabilities with normal matrix: (I-Q)^{-1} R
IsC <- diag(statesCount - 2)
matrixProbs <- solve(IsC - Q, R)


## NAME: countyourchickens.R
## USAGE: within R, at interactive prompt
##        source("countyourchickens.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: None
## DESCRIPTION: Use the normal matrix and transition probability
##  matrix partition to calculate probability of winning the game.            
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: R base
## DEPENDENCIES:  R base
## INCOMPATIBILITIES: None known
## PROVENANCE: Steve Dunbar as of Tue 27 Oct 2020 10:59:52 AM CDT
## BUGS AND LIMITATIONS:  None
## FEATURES AND POTENTIAL IMPROVEMENTS: Add waiting time to absorption
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Tue 27 Oct 2020 11:01:05 AM CDT
## KEYWORDS: Markov chain, normal matrix, win probability

