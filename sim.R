decklist <- read.csv(file="C:/Users/PC/Desktop/MullSim-master/deck.csv",header=TRUE)

deck <- decklist
color.list <- as.data.frame(rbind(c('W','Plains'),c('U','Island'),c('B','Swamp'),c('R','Mountain'),c('G','Forest')))
colnames(color.list) <- c('short','basic')

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

df.mull <- data.frame(
  hand = list()
  ,mull = integer()
  ,stringsAsFactors = FALSE
)

rep.hand <- data.frame(
  turn=integer()
  ,type=character()
  ,C1 = character()
  ,C2 = character()
  ,C3 = character()
  ,C4 = character()
  ,C5 = character()
  ,C6 = character()
  ,C7 = character()
  ,C8 = character()
  ,C9 = character()
  ,C10 = character()
)
rep.mana <- data.frame(
  turn=integer()
  ,W=integer()
  ,U=integer()
  ,B=integer()
  ,R=integer()
  ,G=integer()
)


##TESTOWANIE

# for (i in 1:10000) {
  dlist <- StartGame(decklist)
  
  
  
  # tmp.row <- as.data.frame(t(c(as.character(hand$Name),mull.bool * 1)))
  # colnames(tmp.row) <- c('a1','a2','a3','a4','a5','a6','a7','mull')
  # df.mull <- rbind(df.mull, tmp.row)
  
  # if(mull.bool == TRUE){
  #   hand.tmp <- hand
  #   a <- StartGame(decklist)
  #   hand <- as.data.frame(a[[1]])
  #   deck <- as.data.frame(a[[2]])
  #   mull.bool <- IsMulligan(hand)
  #   card.bottom <- as.data.frame(PutCardOnBottom(hand))
  #   hand <- hand[hand$Id != card.bottom$Id,]
  #   deck <- rbind(deck,card.bottom)
  #   mull.cnt <- 1
  #   
  #   print('asdf')
  #   tmp.row <- as.data.frame(t(c(as.character(hand.tmp$Name),as.character(card.bottom$Name))))
  #   colnames(tmp.row) <- c('a1','a2','a3','a4','a5','a6','a7','mull')
  #   df.mull <- rbind(df.mull, tmp.row)
  # }
# }
##PIERWSZA TURA

hand <- dlist[[1]]
deck <- dlist[[2]]
mull.bool <- IsMulligan(hand)
print(hand)
print(mull.bool)
field <- hand[0,]
played <- hand[0,]

for (turn in 1:5){
print(turn)
if(turn != 1){
  field$Tap <- 0
  draw.arr <- DrawCard(hand,deck)
  hand <- draw.arr[[1]]
  deck <- draw.arr[[2]]
}
land.to.play <- SelectLandToPlay(hand,field,turn)[1]
play.arr <- PlayCard(hand,field,SelectLandToPlay(hand,field,turn)[1])
field <- play.arr[[3]]
hand <- play.arr[[2]]

if (nrow(field[which(field$Name == 'Fabled Passage'),])>0) {
  play.arr <- FetchLand(deck,field,WhatToFetch(hand,field,color.list))
  field <- play.arr[[1]]
  deck <- play.arr[[2]]
}

play.card <- SelectCardToPlay(hand,field)[1]
if (!is.na(play.card)) {
  card.name <- as.character(hand[which(hand$Id == play.card),2])
  play.arr <- PlayCard(hand,field,play.card[1])
  field <- play.arr[[3]]
  hand <- play.arr[[2]]
  if (card.name == 'Uro'){
    draw.arr <- DrawCard(hand,deck)
    hand <- draw.arr[[1]]
    deck <- draw.arr[[2]]
    play.arr <- PlayCard(hand,field,SelectLandToPlay(hand,field,2)[1])
    field <- play.arr[[3]]
    hand <- play.arr[[2]]
  }
}
manasource <- field[which(field$T == 'L' | field$Dork == 1),]
rep.mana[turn,1] <- turn
rep.mana[turn,2] <- sum(as.numeric(manasource$W))
rep.mana[turn,3] <- sum(as.numeric(manasource$U))
rep.mana[turn,4] <- sum(as.numeric(manasource$B))
rep.mana[turn,5] <- sum(as.numeric(manasource$G))
rep.mana[turn,6] <- sum(as.numeric(manasource$R))
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
    return(TRUE)
  }
  else {
    return(FALSE)
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
  else if ((l.cnt == 2) && (dork.cnt > 0) && (dork.bool == FALSE)) {
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


DrawCard <- function(hand, deck){
  h <- rbind(hand,deck[1,])
  d <- deck[2:nrow(deck),]
  return(list(h,d))
}

PlayCard <- function(hand, field, ID){
  if (!is.numeric(ID)) {
    return(list(NULL,hand,field))
  }
  
  pc <- hand[which(hand$Id == ID),]
  if (pc$Dork == 1) {
    pc$W <- 1
    pc$U <- 1
    pc$B <- 1
    pc$R <- 1
    pc$G <- 1
  }
  h <- hand[which(hand$Id != ID),]
  f <- rbind(field, pc)
  return(list(pc,h,f))
}

CountMana <- function(field){
  lands <- field[which(field$T == 'L' & field$Tap == 0),]
  dorks <- field[which(field$Dork == 1 & field$Tap == 0),]
  cnt <- nrow(lands) + nrow(dorks)
  mana <- colSums(lands[,6:10]) + nrow(dorks)
  return(list(cnt,mana))
}

SelectLandToPlay <- function(hand, field, turn){
  l <- hand[which(hand$T == 'L'),]
  cnt.mana <- CountMana(field)[[1]]
  
  if (nrow(l) == 0) {return(hand[0,])}
  
  cr <- hand[which(hand$T != 'L' & hand$CMC <= cnt.mana + min(nrow(l),1)),]
  mana.need <- colSums(t(t(cr[,6:10]) - colSums(field[,6:10])))
  col.missing <- colnames(t(mana.need[which(mana.need == max(mana.need))]))
  col.missing.df <- as.data.frame(col.missing)
  
  if (turn == 1) {
    l.tap <- l[which(l$Tap == 1),]
    if (nrow(l.tap) > 0) {
      return(l.tap[which(l.tap$Mana_val == max(l.tap$Mana_val)),1][1])
    } else {
      l.g <- l[which(l$G == 1),]
      return(l.g[which(l.g$Mana_val == max(l.g$Mana_val)),1][1])
    }
  } else {
    l.w <- l[which(l$Mana_val == max(l$Mana_val)),]
    if (nrow(col.missing.df) == 0){
      l.u <- l[which(l[col.missing] == 1 & l$Untap == 1),]
      l.t <- l[which(l[col.missing] == 1),]
    } else {
      l.u <- l[0,]
      l.t <- l[0,]
      for (i in 1:nrow(col.missing.df)){
        l.u <- rbind(l.u, l[which(l[as.character(col.missing.df[i,])] == 1 & l$Untap == 1),])
        l.t <- rbind(l.u, l[which(l[as.character(col.missing.df[i,])] == 1),])
      }
    }
    
    if (nrow(l.u) > 0) {
      return(l.u[which(l.u$Mana_val == max(l.u$Mana_val)),1])
    } else if (nrow(l.t) > 0){
      return(l.t[which(l.t$Mana_val == max(l.t$Mana_val)),1])
    } else {
      return(l.w[which(l.w$Mana_val == max(l.w$Mana_val)),1])
    }
  }
}

SelectCardToPlay <- function(hand, field){
  tmp <- CountMana(field)
  cnt.mana <- tmp[[1]]
  col.mana <- as.data.frame(t(tmp[[2]]))
  cr <- hand[which(hand$CMC <= cnt.mana & hand$T != 'L'),]
  if (col.mana$G > 0) {
  cr.dork <- cr[which(cr$Dork == 1),] }
  if (col.mana$G > 0 && col.mana$U > 0){
  cr.uro <- cr[which(cr$Name == 'Uro'),]}
  if (nrow(cr) == 0) {
    return(hand[0,1])
  } else if (nrow(cr.uro) > 0) {
    return(cr.uro[1,1])
  } else if (nrow(cr.dork) > 0) {
    return(cr.dork[1,1])
  } else {
    return(cr[1,1])
  }
}

colMax <- function(data) sapply(data, max, na.rm = TRUE)

WhatToFetch <- function(hand,field,aux){
  lands.hand <- hand[which(hand$T == 'L'),]
  lands.field <- field[which(field$T == 'L' & field$F != 1),]
  dorks.field <- field[which(field$Dork == 1),]
  mana.sources <- rbind(lands.hand, lands.field, dorks.field)
  mana.gen <- colSums(mana.sources[6:10])
  mana.req <- colMax(hand[which(hand$T != 'L'),][6:10])
  mx <- mana.req - mana.gen
  req.color <- colnames(mana.sources)[5+which(mx == max(mx))[[1]]]
  return(as.character(aux[which(aux$short == req.color),][[2]]))
}

FetchLand <- function(deck,field,name){
  fetched.land <- deck[which(deck$Name == name),]
  field.new <- rbind(field[which(field$Name != 'Fabled Passage')],fetched.land)
  deck.new <- deck[which(deck$Name != name),]
  return (list(field.new, deck.new))
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
