library(lattice)
#library(matlab)

# ANalyze basic simulations
source("functions.R")

# Analysis of non-competitive model

data <- read.table("sims-noncompetitive/sims-positive-negative-alpha-egs-noncomp.txt", 
                   header=T,
                   sep=","
                   )


# Simpify for analysis
s_data <- aggregate(data[c("ChooseA", "AvoidB", 
                           "PickA", "PickB",
                           "PickC", "PickD",
                           "PickE", "PickF")], 
                    list(Alpha=data$Alpha, 
                         EGS = data$EGS, 
                         PosRwrd=data$PosRwrd, 
                         NegRwrd=data$NegRwrd), 
                    mean)

## PSP

approx.equal <- function(x, y) {
  if (abs(x-y)/abs(min(c(x, y))) < 0.5) {
    T
  } else {
    F
  }
}

approx.equal <- function(x, y) {
  if (abs(x-y) < 0.05) {
    T
  } else {
    F
  }
}


approx.order <- function(x, y) {
  if (approx.equal(x, y)) {
    0
  } else {
    sign(x - y)
  }
}

vpattern <- Vectorize(approx.order)

s_data$Pattern <- vpattern(s_data$ChooseA, s_data$AvoidB)

psp <- tapply(s_data$ChooseA, s_data$Pattern, length)

round(100 * psp / (sum(c(psp))), 2)

# No significant cases where Avoid > Choose
# --------------------------
# Equal      Choose > Avoid
# 0          1 
# 82.29      17.71 
# --------------------------


paratu <- function() {
  hist(s_data$ChooseA - s_data$AvoidB, 
       xlim=c(-0.25, 1), col="grey", 
       border = "white", 
       breaks = seq(-0.25, 1, 0.025),
       xlab = "Difference between Choose and Avoid",
       ylab = "Frequency",
       main = "Distribution of simulation results")
  box(bty="o")
  grid()
  abline(h=0)
}



## Individual values

a_data1 <- s_data
a_data2 <- s_data

a_data1$AvoidB <- NULL
names(a_data1)[5] <- "Accuracy"
a_data1$Measure <- "Choose"

a_data2$ChooseA <- NULL
names(a_data2)[5] <- "Accuracy"
a_data2$Measure <- "Avoid"

a_data <- merge(a_data1, a_data2, all=T)

a_data <- subset(a_data, a_data$Alpha < 0.1)
a_data <- subset(a_data, a_data$EGS > 0)

## Original Data from Michael Frank

frank2004 <- data.frame(Choose = c(67.74144062, 79.02238338, 64.68865491),
                        Avoid = c(63.62980266, 58.10961579, 82.43114148),
                        ChooseSE = c(7.051112196, 5.05299491, 6.930827697),
                        AvoidSE = c(7.402203473, 11.27815355, 7.988982637),
                        Group=c("Controls", "PD, On Medication", "PD, Off Medication"))

frank2007a <- data.frame(Choose = c(75.8930991, 66.60729804),
                         Avoid = c(66.76050086, 74.05648741),
                         ChooseSE = c(3.316427604, 5.050997524),
                         AvoidSE = c(3.750035042, 4.030509742),
                         Group=c("A/A", "G/G, G/A"))

frank2007b <- data.frame(Choose = c(73.0866648, 68.80067095),
                         Avoid = c(66.81073525, 78.08624546),
                         ChooseSE = c(3.36734694, 5.357142855),
                         AvoidSE = c(3.44387755, 3.69897959),
                         Group=c("C/C, C/T", "T/T"))


## ------------------------------------------------------------------------- ##
## PARKINSON
## ------------------------------------------------------------------------- ##

pd_sims <- read.table("sims-competitive/sims-d1-d2-alpha-0.008-egs-0.1-comp.txt", 
                      header=T, 
                      sep=",")
apd <- aggregate(pd_sims[c("ChooseA", "AvoidB")], 
                 list(D1=pd_sims$D1, D2=pd_sims$D2), 
                 mean)


# Calculate energy
energy2004 <- function(set, d1, d2) {
  m <- midpoint(d1, d2)
  data1 <- c(set$ChooseA[set$D1 == m & set$D2 == m], 
             set$AvoidB[set$D1 == m & set$D2 == m])
  data2 <- c(set$ChooseA[set$D1 == d1 & set$D2 == d2], 
             set$AvoidB[set$D1 == d1 & set$D2 == d2])
  data3 <- c(set$ChooseA[set$D1 == d2 & set$D2 == d1], 
             set$AvoidB[set$D1 == d2 & set$D2 == d1])
  e1 <- sum((data1 - c(67.74144062/100,  63.62980266/100))**2)
  e2 <- sum((data2 - c(64.68865491/100,  82.43114148/100))**2)
  e3 <- sum((data3 - c(79.02238338/100,  58.10961579/100))**2)
  
  e1 + e2 + e3
}

energy2004b <- function(set, d1, d2) {
  m <- midpoint(d1, d2)
  data1 <- c(set$ChooseA[set$D1 == m & set$D2 == m], 
             set$AvoidB[set$D1 == m & set$D2 == m])
  data2 <- c(set$ChooseA[set$D1 == d1 & set$D2 == d2], 
             set$AvoidB[set$D1 == d1 & set$D2 == d2])
  data3 <- c(set$ChooseA[set$D1 == d2 & set$D2 == d1], 
             set$AvoidB[set$D1 == d2 & set$D2 == d1])
  e1 <- sum((data1 - c(67.74144062/100,  63.62980266/100))**2)
  e2 <- sum((data2 - c(64.68865491/100,  82.43114148/100))**2)
  e3 <- sum((data3 - c(79.02238338/100,  58.10961579/100))**2)
  
  if (m == 1) {
    e1 + e2 + e3
  } else {
    1
  }
}

correl2004 <- function(set, d1, d2) {
  m <- midpoint(d1, d2)
  data1 <- c(set$ChooseA[set$D1 == m & set$D2 == m], 
             set$AvoidB[set$D1 == m & set$D2 == m])
  data2 <- c(set$ChooseA[set$D1 == d1 & set$D2 == d2], 
             set$AvoidB[set$D1 == d1 & set$D2 == d2])
  data3 <- c(set$ChooseA[set$D1 == d2 & set$D2 == d1], 
             set$AvoidB[set$D1 == d2 & set$D2 == d1])
  cor(c(data1, data2, data3),
      c(67.74144062/100, 63.62980266/100, 
        64.68865491/100,  82.43114148/100,
        79.02238338/100,  58.10961579/100))
}

midpoint <- function(a, b) {
  round(mean(c(a, b)) * 20, 0) / 20
}

apd$Energy2004 <- 0 
apd$Energy2004b <- 0
apd$Correl2004 <- 0

for (d1 in unique(apd$D1)) {
  for (d2 in unique(apd$D2)) {
    apd$Energy2004[apd$D1 == d1 & apd$D2 == d2] <- energy2004(apd, d1, d2)
    apd$Energy2004b[apd$D1 == d1 & apd$D2 == d2] <- energy2004b(apd, d1, d2)
    apd$Correl2004[apd$D1 == d1 & apd$D2 == d2] <- correl2004(apd, d1, d2)
  }
}

subset(apd, apd$Energy2004b == min(apd$Energy2004b))
subset(apd, apd$Energy2004 == min(apd$Energy2004))

# Transform the energy value into RMSE in percent points
subset(apd, apd$Energy2004 == min(apd$Energy2004))$Energy2004  * (100 ** 2) / 3

mpdA <- pd_sims
mpdA$AvoidB <- NULL
mpdA$Measure <- "Choose"
names(mpdA)[3] <- "Accuracy"

mpdB <- pd_sims
mpdB$ChooseA <- NULL
mpdB$Measure <- "Avoid"
names(mpdB)[3] <- "Accuracy"

mpd <- merge(mpdA, mpdB, all=T)


mapdA <- apd
mapdA$AvoidB <- NULL
mapdA$Measure <- "Choose"
names(mapdA)[3] <- "Accuracy"

mapdB <- apd
mapdB$ChooseA <- NULL
mapdB$Measure <- "Avoid"
names(mapdB)[3] <- "Accuracy"

mapd <- merge(mapdA, mapdB, all=T)

## ------------------------------------------------------------------------- ##
## GENETICS
## ------------------------------------------------------------------------- ##

gen_sims <- read.table("sims-competitive/sims-d1-d2-alpha-0.018-egs-0.1-comp.txt", 
                       header=T, 
                       sep=",")
agen <- aggregate(gen_sims[c("ChooseA", "AvoidB")], 
                  list(D1=gen_sims$D1, 
                       D2=gen_sims$D2), 
                  mean)




# Calculate energy
energy2007 <- function(set, d1, d2) {
  # Approximate the midpoint
  midpoint <- midpoint(as.numeric(as.character(d1)), 
                       as.numeric(as.character(d2)))
  
  # Simulation for D1 up or down
  data1 <- c(set$ChooseA[set$D1 == d1 & set$D2 == midpoint], 
             set$AvoidB[set$D1 == d1 & set$D2 == midpoint])
  data2 <- c(set$ChooseA[set$D1 == d2 & set$D2 == midpoint], 
             set$AvoidB[set$D1 == d2 & set$D2 == midpoint])
  
  # Simulation for D2 up or down 
  data3 <- c(set$ChooseA[set$D1 == midpoint & set$D2 == d1], 
             set$AvoidB[set$D1 == midpoint & set$D2 == d1])
  data4 <- c(set$ChooseA[set$D1 == midpoint & set$D2 == d2], 
             set$AvoidB[set$D1 == midpoint & set$D2 == d2])
  e1 <- sum((data1 - c(75.8930991/100,  66.76050086/100)) ** 2)
  e2 <- sum((data2 - c(66.60729804/100,  74.05648741/100)) ** 2)
  e3 <- sum((data4 - c(73.0866648/100,  66.81073525/100)) ** 2)
  e4 <- sum((data3 - c(68.80067095/100,  78.08624546/100)) ** 2)
  
  if (midpoint != 1) {
  #  e1 + e2 + e3 + e4
    1
  } else {
    e1 + e2 + e3 + e4
  }
}

correl2007 <- function(set, d1, d2) {
  # Approximate the midpoint
  midpoint <- midpoint(as.numeric(as.character(d1)), 
                       as.numeric(as.character(d2)))
  
  # Simulation for D1 up or down
  data1 <- c(set$ChooseA[set$D1 == d1 & set$D2 == midpoint], 
             set$AvoidB[set$D1 == d1 & set$D2 == midpoint])
  data2 <- c(set$ChooseA[set$D1 == d2 & set$D2 == midpoint], 
             set$AvoidB[set$D1 == d2 & set$D2 == midpoint])
  
  # Simulation for D2 up or down 
  data3 <- c(set$ChooseA[set$D1 == midpoint & set$D2 == d1], 
             set$AvoidB[set$D1 == midpoint & set$D2 == d1])
  data4 <- c(set$ChooseA[set$D1 == midpoint & set$D2 == d2], 
             set$AvoidB[set$D1 == midpoint & set$D2 == d2])
  
  cor(c(data1, data2, data3, data4),
      c(75.8930991/100,  66.76050086/100,
        66.60729804/100,  74.05648741/100,
        68.80067095/100,  78.08624546/100,
        73.0866648/100,  66.81073525/100))
}


agen$Energy2007 <- 0
agen$Correl2007 <- 0

for (d1 in unique(agen$D1)) {
  for (d2 in unique(agen$D2)) {
    agen$Energy2007[agen$D1 == d1 & agen$D2 == d2] <- energy2007(agen, d1, d2)
    agen$Correl2007[agen$D1 == d1 & agen$D2 == d2] <- correl2007(agen, d1, d2)
  }
}

subset(agen, agen$Energy2007 == min(agen$Energy2007))

magenA <- agen
magenA$AvoidB <- NULL
magenA$Measure <- "Choose"
names(magenA)[3] <- "Accuracy"

magenB <- agen
magenB$ChooseA <- NULL
magenB$Measure <- "Avoid"
names(magenB)[3] <- "Accuracy"

magen <- merge(magenA, magenB, all=T)

mgenA <- gen_sims
mgenA$AvoidB <- NULL
mgenA$Measure <- "Choose"
names(mgenA)[3] <- "Accuracy"

mgenB <- gen_sims
mgenB$ChooseA <- NULL
mgenB$Measure <- "Avoid"
names(mgenB)[3] <- "Accuracy"

mgen <- merge(mgenA, mgenB, all=T)


calculate.q.2007 <- function(val) {
  q = 2 - val
  z<-subset(agen, abs(agen$D1 - val) < 0.04 & abs(agen$D2 - q) < 0.04) $Energy2007
  z/4
} 

calculate.q.2004 <- function(val) {
  z <- min(subset(apd, abs(apd$D1 - val) < 0.04)$Energy2004)
  z/3
} 


data2 <- read.table("sims-noncompetitive-reverse/sims-positive-negative-alpha-egs-noncompetitive-reverse.txt", 
                   header=T,
                   sep=","
)


# Simplify for analysis
s_data2 <- aggregate(data2[c("ChooseA", "AvoidB", 
                           "PickA", "PickB",
                           "PickC", "PickD",
                           "PickE", "PickF")], 
                    list(Alpha=data2$Alpha, 
                         EGS = data2$EGS, 
                         PosRwrd=data2$PosRwrd, 
                         NegRwrd=data2$NegRwrd), 
                    mean)

## Individual values

ac_data1 <- s_data2
ac_data2 <- s_data2

ac_data1$AvoidB <- NULL
names(ac_data1)[5] <- "Accuracy"
ac_data1$Measure <- "Choose"

ac_data2$ChooseA <- NULL
names(ac_data2)[5] <- "Accuracy"
ac_data2$Measure <- "Avoid"

ac_data <- merge(ac_data1, ac_data2, all=T)

ac_data <- subset(ac_data, ac_data$Alpha < 0.1)
ac_data <- subset(ac_data, ac_data$EGS > 0)

s_data2$Pattern <- vpattern(s_data2$ChooseA, s_data2$AvoidB)

acpsp <- tapply(s_data2$ChooseA, s_data2$Pattern, length)

round(100 * acpsp / (sum(c(acpsp))), 2)
