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
# 80.12431   19.87569 
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
#s_data <- subset(s_data, s_data$PosRwrd == 1)
#s_data <- subset(s_data, s_data$NegRwrd == -1)


plot.by.2factors(a_data, "Accuracy", "Measure", "Alpha", rng=c(0.4, 0.7, 0.1), legpos="topleft")

plot.by.2factors(a_data, "Accuracy", "Measure", "EGS", rng=c(0.4, 0.7, 0.1), legpos="topleft")

plot.by.2factors(a_data, "Accuracy", "Measure", "PosRwrd", rng=c(0.5, 0.7, 0.1), legpos="topleft")

plot.by.2factors(a_data, "Accuracy", "Measure", "NegRwrd", rng=c(0.5,0.7,0.1), legpos="topleft")

plot.by.2factors(s_data, "PickA", "EGS", "Alpha", legpos="topleft", rng=c(-0.2,0.1,0.1))

plot.by.2factors(s_data, "PickA", "PosRwrd", "NegRwrd", legpos="topleft", rng=c(-0.7,0.3,0.1))

plot.by.2factors(s_data, "PickB", "EGS", "Alpha", rng=c(-1,0.1,0.1), legpos="bottomleft")

plot.by.2factors(subset(s_data, s_data$PosRwrd==1 & s_data$NegRwrd==-1), "PickB", "EGS", "Alpha", rng=c(-1,0.1,0.1), legpos="bottomleft")


plot.by.2factors(s_data, "PickB", "PosRwrd", "NegRwrd", legpos="topleft", rng=c(-0.7,0.3,0.1))


## Analysis of utility

u_data <- NA
for (option in c("A", "B", "C", "D", "E", "F")) {
  sub <- s_data[1:6]
  sub$Option <- rep(option, dim(sub)[1])
  sub$Utility <- s_data[[paste("Pick", option, sep="")]]
  if (! is.na(u_data)) {
    u_data <- merge(u_data, sub, all=T)
  } else {
    u_data <- sub
  }
}

plot.by.2factors(u_data, "Utility", "Alpha", "Option", legpos="topleft", rng=c(-0.7,0.3,0.1))
plot.by.2factors(u_data, "Utility", "EGS", "Option", legpos="topleft", rng=c(-0.7,0.3,0.1))
plot.by.2factors(u_data, "Utility", "PosRwrd", "Option", legpos="topleft", rng=c(-0.7,0.3,0.1))
plot.by.2factors(u_data, "Utility", "NegRwrd", "Option", legpos="topleft", rng=c(-0.7,0.3,0.1))


z <- subset(a_data, a_data$Alpha <= 0.1 & a_data$EGS == 0.2)

plot.by.2factors(subset(z, z$PosRwrd %in% c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8) & z$NegRwrd %in% c(-0.5)), "Accuracy", "Measure", "PosRwrd", rng=c(0.5,  1, 0.1), legpos="topleft")


# Simplified analysis

s_an <- subset(s_data, s_data$Alpha %in% (seq(0, 10, 1) / 10) &
                 s_data$EGS %in% (seq(0, 10, 1)/10) &
                 s_data$PosRwrd %in% (seq(0, 10, 1) / 10) &
                 s_data$NegRwrd %in% (seq(0, -10, -1) / 10)
)
s_an$Model <- paste("Run", 1:dim(s_an)[1]) 
s_an$Alpha <- factor(s_an$Alpha)
s_an$EGS <- factor(s_an$EGS)
s_an$PosRwrd <- factor(s_an$PosRwrd)
s_an$NegRwrd <- factor(s_an$NegRwrd)
#summary(aov(Accuracy ~ (Measure * Alpha * EGS * PosRwrd * NegRwrd) + Error(Model), s_an))

#summary(aov(Accuracy ~ (Measure * Alpha * EGS * PosRwrd) + Error(Model), s_an))



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




data <- read.table("simulations-new-alpha-egs.txt", header=T)
plot(aggregate(data$AvoidB, list(data$Alpha), mean), type="l")
plot(aggregate(data$Choose, list(data$Alpha), mean), type="l")

plot(aggregate(data$AvoidB, list(data$EGS), mean), type="l")
plot(aggregate(data$Choose, list(data$EGS), mean), type="l")

plot.by.2factors(data, "AvoidB", "Alpha", "EGS")
plot.by.2factors(data, "ChooseA", "Alpha", "EGS")

findings <- c(0.6766, 0.6325)

data$DAvoid <- data$AvoidB - 0.6766
data$DChoose <- data$ChooseA - 0.6325
data$Energy <- data$DAvoid ** 2 + data$DChoose ** 2

penergy <- function(a, e, av, ch) {
  ((a - 0.2)**2)/100  + ((e - 0.0)**2)/100 + ((av - 0.7) ** 2 + (ch - 0.7) ** 2)
  #(av - 0.70) ** 2 + (ch - 0.70) ** 2
}

energy <- function(a, e, av, ch) {
  ((av - 0.7) ** 2 + (ch - 0.7) ** 2)
  #(av - 0.70) ** 2 + (ch - 0.70) ** 2
}


data$PEnergy <- penergy(data$Alpha, data$EGS, data$AvoidB, data$ChooseA)
data$Energy <- energy(data$Alpha, data$EGS, data$AvoidB, data$ChooseA)

subset(data, data$Energy == min(data$Energy))
subset(data, data$PEnergy == min(data$PEnergy))

e <- tapply(data$Energy, list(data$Alpha, data$EGS), mean)
e <- e[2:21, 1:21]
e <- sqrt(e)

filled.contour(e, color.palette = jet.colors, 
               xlim = c(0.05, 1), ylim = c(0, 1),
               xlab = "Alpha", ylab ="EGS",
               main = "Energy")

pe <- tapply(data$PEnergy, list(data$Alpha, data$EGS), mean)
pe <- pe[2:21, 1:21]
pe <- sqrt(pe)

filled.contour(pe, color.palette = jet.colors, 
               xlim = c(0.05, 1), ylim = c(0, 1),
               xlab = "Alpha", ylab ="EGS",
               main = "Energy")



plot.by.2factors(subset(data, data$EGS <= 0.3), "Energy", "Alpha", "EGS", legpos="topright", rng=c(0,0.1,0.01))


E <- min(data$Energy[data$Alpha <= 0.2])
subset(data, data$Energy == E)

E <- min(data$Energy[data$Alpha <= 0.1])
subset(data, data$Energy == E)

E <- min(data$Energy)
subset(data, data$Energy == E)

plot.by.2factors(subset(data, data$Alpha <= 0.2), "Energy", "EGS", "Alpha", legpos="topright", rng=c(0, 0.04, 0.01))


asA <- data[c(1:3)]
asB <- data[c(1:2,4)]
names(asA)[3] <- "Accuracy"
names(asB)[3] <- "Accuracy"
asA$Strategy <- "Choose"
asB$Strategy <- "Avoid"
as <- merge(asA, asB, all=T)

plot.by.2factors(subset(as, as$Alpha %in% seq(0,2,0.1)), variable = "Accuracy", factor1 = "Strategy", factor2 = "Alpha", rng=c(0.5, 0.9, .1))

plot.by.2factors(subset(as, as$EGS %in% seq(0,2,0.1) & as$Alpha > 0), variable = "Accuracy", factor1 = "Strategy", factor2 = "EGS", rng=c(0.5, 0.9, .1))


## Other analysis: D1 vs. D2
## =========================

base.data <- read.table("sims-alpha-<0,0.1,0.001>-comp.txt", header=T, sep=",")
a_base <- aggregate(base.data[c("ChooseA", "AvoidB")], list(Alpha=base.data$Alpha, EGS=base.data$EGS), mean)



energy.seniors <- function(set, alpha) {
  sub <- colMeans(subset(set, set$Alpha == alpha))
  data <- sub[c("ChooseA", "AvoidB")]
  sum((data - c(67.74144062/100,  63.62980266/100))**2)
}

a_base$EnergySeniors <- 0

for (a in unique(a_base$Alpha)) {
  a_base$EnergySeniors[a_base$Alpha == a] <- energy.seniors(a_base, a)
}


subset(a_base, a_base$EnergySeniors == min(a_base$EnergySeniors))

# Now the genetic data

a <- colMeans(frank2007a[1:2])
b <- colMeans(frank2007b[1:2])

genetics <- colMeans(rbind(a, b))

energy.genetics <- function(set, alpha) {
  sub <- colMeans(subset(set, set$Alpha == alpha))
  data <- sub[c("ChooseA", "AvoidB")]
  sum((data - c(71.09693/100, 71.42849/100))**2)
}

a_base$EnergyGenetics <- 0


for (a in unique(a_base$Alpha)) {
  a_base$EnergyGenetics[a_base$Alpha == a] <- energy.genetics(a_base, a)
}


subset(a_base, a_base$EnergyGenetics == min(a_base$EnergyGenetics))

## ------------------------------------------------------------------------- ##
## PARKINSON
## ------------------------------------------------------------------------- ##

pd_sims <- read.table("sims-d1-d2-alpha-0.008-egs-0.1-comp.txt", 
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


plot.by.2factors(subset(mapd, mapd$D1 %in% c(0.1) & mapd$D2 %in% c(2)), 
                 "Accuracy", "Measure", "D1", rng=c(0.5,  0.9, 0.1), legpos="topleft")
plot.by.2factors(subset(mapd, mapd$D1 %in% c(2) & mapd$D2 %in% c(0.1)), 
                 "Accuracy", "Measure", "D1", rng=c(0.5,  0.9, 0.1), legpos="topleft")

z <- tapply(apd$Energy2004, list(D1=apd$D1, D2=apd$D2), mean)
z[z==1] <- 0.11
filled.contour(sqrt(z), color.palette = jet.colors)
lines(x = c(0,1), y=c(1,0))

## ------------------------------------------------------------------------- ##
## GENETICS
## ------------------------------------------------------------------------- ##

gen_sims <- read.table("sims-d1-d2-alpha-0.018-egs-0.1-comp.txt", 
                       header=T, 
                       sep=",")
agen <- aggregate(gen_sims[c("ChooseA", "AvoidB")], 
                  list(D1=gen_sims$D1, 
                       D2=gen_sims$D2), 
                  mean)

midpoint <- function(a, b) {
  round(mean(c(a, b)) * 20, 0) / 20
}



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


plot.by.2factors(subset(magen, 
                        magen$D1 %in% c(0.5, 1.5) & magen$D2 %in% c(1.1)), 
                 "Accuracy", "Measure", "D1", 
                 rng=c(0.5,  0.9, 0.1), 
                 legpos="topleft")

plot.by.2factors(subset(magen, 
                        magen$D2 %in% c(0.5, 1.5) & magen$D1 %in% c(1)), 
                 "Accuracy", "Measure", "D2", 
                 rng=c(0.5,  0.9, 0.1), legpos="topleft")


mgenA <- gen_sims
mgenA$AvoidB <- NULL
mgenA$Measure <- "Choose"
names(mgenA)[3] <- "Accuracy"

mgenB <- gen_sims
mgenB$ChooseA <- NULL
mgenB$Measure <- "Avoid"
names(mgenB)[3] <- "Accuracy"

mgen <- merge(mgenA, mgenB, all=T)


plot.by.2factors(subset(mgen, 
                        mgen$D1 %in% c(0.5, 1.5) & mgen$D2 %in% c(1)), 
                 "Accuracy", "Measure", "D1", 
                 rng=c(0.5,  0.9, 0.1), 
                 legpos="topleft")

plot.by.2factors(subset(mgen, 
                        mgen$D2 %in% c(0.5, 1.5) & mgen$D1 %in% c(1)), 
                 "Accuracy", "Measure", "D2", 
                 rng=c(0.5,  0.9, 0.1), legpos="topleft")

z <- tapply(agen$Energy2007, list(D1=agen$D1, D2=agen$D2), mean)
z[z==1] <- 0.11
filled.contour(sqrt(z), color.palette = jet.colors)

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

a_data1 <- s_data2
a_data2 <- s_data2

a_data1$AvoidB <- NULL
names(a_data1)[5] <- "Accuracy"
a_data1$Measure <- "Choose"

a_data2$ChooseA <- NULL
names(a_data2)[5] <- "Accuracy"
a_data2$Measure <- "Avoid"

a_data <- merge(a_data1, a_data2, all=T)

a_data <- subset(a_data, a_data$Alpha < 0.1)
a_data <- subset(a_data, a_data$EGS > 0)
