install.packages("rlist")

decklist <- read.csv(file="C:/Users/PC/Desktop/MullSim-master/deck.csv",header=TRUE)
deck <- decklist


##STATA
  df.dk <- data.frame(
    color = character()
    ,lands = double()
    ,cards = double()
    ,stringsAsFactors = FALSE
  )

  lands.dk <- decklist[which(decklist[3] == 'L'),]
  nonlands.dk <- decklist[which(decklist[3] != 'L'),]
  
  for (i in 1:5) {
    df.dk[i,1] <- as.character(colnames(decklist[5+i]))
    df.dk[i,2] <- sum(lands.dk[5+i])
    df.dk[i,3] <- sum(nonlands.dk[5+i])
  }
  df.dk$card_val <- df.dk$cards/sum(df.dk$cards)
  
  a <- lands[,6:10]
  
  df.mull <- data.frame(
    hand = list()
    ,mull = integer()
    ,stringsAsFactors = FALSE
  )
  
  
##TESTOWANIE

for (i in 1:10000) {
a <- StartGame(decklist)
hand <- a[[1]]
deck <- a[[2]]
mull.bool <- IsMulligan(hand)
mull.cnt <- 0
print(mull.bool)
# tmp.row <- as.data.frame(t(c(as.character(hand$Name),mull.bool * 1)))
# colnames(tmp.row) <- c('a1','a2','a3','a4','a5','a6','a7','mull')
# df.mull <- rbind(df.mull, tmp.row)

if(mull.bool == TRUE){
  hand.tmp <- hand
  a <- StartGame(decklist)
  hand <- as.data.frame(a[[1]])
  deck <- as.data.frame(a[[2]])
  mull.bool <- IsMulligan(hand)
  card.bottom <- as.data.frame(PutCardOnBottom(hand))
  hand <- hand[hand$Id != card.bottom$Id,]
  deck <- rbind(deck,card.bottom)
  mull.cnt <- 1
  
  print('asdf')
  tmp.row <- as.data.frame(t(c(as.character(hand.tmp$Name),as.character(card.bottom$Name))))
  colnames(tmp.row) <- c('a1','a2','a3','a4','a5','a6','a7','mull')
  df.mull <- rbind(df.mull, tmp.row)
}
}


#funkcje

ShuffleDeck <- function(i){
  no <- 1:i
  seeds <- runif(1)*1043040
  order <- lapply(seeds,function(aSeed){
    set.seed(aSeed) 
    sample(no)
  }
  )
  return(order)
}

StartGame <- function(deck){
  order <- ShuffleDeck(60)
  hand <- as.vector(deck[order[[1]][1:7],])
  deckvar <- as.vector(deck[order[[1]][8:60],])
  ret <- list(hand, deckvar)
  return(ret)
}

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
  fourcmc.cnt <- length(which(hand[5]==4))
  if ((l.cnt < 2) || (l.cnt > 4)) {
    IsMulligan = TRUE
  } 
   else if ((l.cnt == 2) && (dork.cnt > 0) && (dork.bool) == FALSE) {
     IsMulligan = TRUE
    } 
     else if (l.cnt + dork.cnt > 5 
              #&& fourcmc.cnt == 0
              ) { 
      IsMulligan = TRUE
    } 
     else {
      IsMulligan = FALSE
    }
}

SelectWeakestLand <- function(lands, df.dk){
  l <- lands
  l$val <-  t(t(l[,6:10])*df.dk$card_val)
  l$val.sum <- 0
  for (i in 1:nrow(l)){
    l[i,'val.sum'] <- sum(l[i,'val'])
  }
  return(l[which(l$val.sum == min(l$val.sum)),])
}

SelectWeakestNonLand <-function(nonlands){
  return(nonlands[which(nonlands$CMC == max(nonlands$CMC)),][1,])
}


PutCardOnBottom <- function(hand){
  lands <- hand[which(hand[3] == 'L'),]
  nonlands <- hand[which(hand[3] != 'L'),]
  dorks <- hand[which(hand[4] == 1),]
  if (nrow(lands) > 5) {
    landbottom.bool <- TRUE
  } else if (nrow(lands) > 4 && nrow(dorks) > 0) {
    landbottom.bool <- TRUE
  } else {
    landbottom.bool <- FALSE
  }
  if (landbottom.bool == TRUE){
    return(SelectWeakestLand(lands,df.dk))
  } else {
    return(SelectWeakestNonLand(nonlands))
  }
}


DrawCard <- function(hand, order, deck){
  
}



#####STARE GENEROWANIE ROZKLADOW#####
decklist1 <- read.csv(file="C:/Users/PC/Desktop/MullSim-master/deck.csv",header = TRUE)
decklist2 <- read.csv(file="C:/Users/PC/Desktop/MullSim-master/deck2.csv",header = TRUE)


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

