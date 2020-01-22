decklist <- read.csv(file="C:/Users/jacek.korpikiewicz/Desktop/symulator/deck.csv",header=TRUE)

order <- 1:60
seeds <- runif(100000)*1000000

shuffledDeck <- lapply(seeds,function(aSeed){
  set.seed(aSeed)
  sample(order)
})

deck <- decklist
handno <- shuffledDeck[[1]][1:7]
hand <- as.vector(deck[handno,])
# l.cnt <- length(which(hand[3] == 'L'))
# dork.cnt <- length(which(hand[4] == 1))
# dork.bl <- IsDorkPlayableTurnTwo(hand)

IsDorkPlayableTurnTwo <- function(hand){
  sourceG <- sum(hand[which(hand[3] == 'L'),]$G)
  untap2 <- sum(hand[which(hand[3] == 'L'),]$Untap)
  if (sourceG > 0 && untap2 > 0) {
    IsDorkPlayableTurnTwo = TRUE
  }
  else {
    IsDorkPlayableTurnTwo = FALSE
  }
}

IsMulligan <- function(hand){
  l.cnt <- length(which(hand[3] == 'L'))
  dork.cnt <- length(which(hand[4] == 1))
  dork.bool <- IsDorkPlayableTurnTwo(hand)
  if ((l.cnt < 2) || (l.cnt > 4)) {
    IsMulligan = TRUE
  } 
   else if ((l.cnt == 2) && (dork.cnt > 0) && (dork.bool) == FALSE) {
     IsMulligan = TRUE
    } 
     else if (l.cnt + dork.cnt > 5) { 
      IsMulligan = TRUE
    } 
     else {
      IsMulligan = FALSE
    }
}





#####STARE GENEROWANIE ROZKLADOW#####
decklist1 <- read.csv(file="C:/Users/jacek.korpikiewicz/Desktop/symulator/deck.csv",header = TRUE)
decklist2 <- read.csv(file="C:/Users/jacek.korpikiewicz/Desktop/symulator/deck2.csv",header = TRUE)

deck <- decklist2

df <- data.frame(
  Cnt = integer()
)

df[1,1] <- 0
df[2,1] <- 0
df$Name <- 'Mull'
df[1,2] <- 'Keep'


for (i in 1:100000) {
  handno <- shuffledDeck[[i]][1:7]
  hand <- as.vector(deck[handno,])
  bool <- IsMulligan(hand)
  if (bool == TRUE) {
    df[2,1] <- df[2,1] + 1
  } else {
    df[1,1] <- df[1,1] + 1
  }
}

