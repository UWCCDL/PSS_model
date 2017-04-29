library(lattice)
library(matlab)

# ANalyze basic simulations
source("functions.R")

# Analysis of non-competitive model

canonical <- read.table(unzip("sims-noncompetitive/sims-positive-negative-alpha-egs-noncomp.zip"),
                        header=T,
                        sep=","
                        )


# Simpify for analysis
a_canonical <- aggregate(canonical[c("ChooseA", "AvoidB", 
                           "PickA", "PickB",
                           "PickC", "PickD",
                           "PickE", "PickF")], 
                    list(Alpha = canonical$Alpha, 
                         EGS = canonical$EGS, 
                         PosRwrd = canonical$PosRwrd, 
                         NegRwrd = canonical$NegRwrd), 
                    mean)

## PSP


approx.equal <- function(a, b) {
  b <- binom.test(x=round(250 * a, 0), b, n=250)
  if (b$p.value > 0.05) {
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

a_canonical$Pattern <- vpattern(a_canonical$ChooseA, a_canonical$AvoidB)

psp <- tapply(a_canonical$ChooseA, a_canonical$Pattern, length)

round(100 * psp / (sum(c(psp))), 2)

# No significant cases where Avoid > Choose
# --------------------------
# Equal      Choose > Avoid
# 0          1 
# 80.12431   19.87569 
# --------------------------


a_data1 <- a_canonical
a_data2 <- a_canonical

a_data1$AvoidB <- NULL
names(a_data1)[5] <- "Accuracy"
a_data1$Measure <- "Choose"

a_data2$ChooseA <- NULL
names(a_data2)[5] <- "Accuracy"
a_data2$Measure <- "Avoid"

a_data_canonical <- merge(a_data1, a_data2, all=T)

a_data_canonical <- subset(a_data_canonical, a_data_canonical$Alpha < 0.1)
a_data_canonical <- subset(a_data_canonical, a_data_canonical$EGS > 0)

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






## COMPETITIVE MODEL
## =================
## Other analysis: D1 vs. D2
## =========================

#base.data <- read.table(unzip("sims-competitive/sims-alpha-0,0.1,0.001-comp.zip"), header=T, sep=",")
base.data <- read.table(unzip("sims-competitive-alternative/sims-alpha.zip"), header=T, sep=",")
a_base <- aggregate(base.data[c("ChooseA", "AvoidB")], list(Alpha=base.data$Alpha, EGS=base.data$EGS), mean)


midpoint <- function(a, b) {
  round(mean(c(a, b)) * 20, 0) / 20
}


energy.seniors <- function(set, alpha) {
  sub <- colMeans(subset(set, set$Alpha == alpha))
  data <- sub[c("ChooseA", "AvoidB")]
  sum((data - c(67.74144062/100,  63.62980266/100))**2)
}

a_base$EnergySeniors <- 0

for (a in unique(a_base$Alpha)) {
  a_base$EnergySeniors[a_base$Alpha == a] <- energy.seniors(a_base, a)
}


# Results is alpha = 0.008
subset(a_base, a_base$EnergySeniors == min(a_base$EnergySeniors))

# -------------------------------------------------------
# And now, let's find the best match for the genetic data
# -------------------------------------------------------

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

# Best match is for alpha = 0.018

subset(a_base, a_base$EnergyGenetics == min(a_base$EnergyGenetics))

## ------------------------------------------------------------------------- ##
## PARKINSON DATA (ALPHA = 0.008, EGS = 0.1)
## ------------------------------------------------------------------------- ##

pd_sims <- read.table(unzip("sims-competitive/sims-d1-d2-alpha-0.008-egs-0.1-comp.zip"), 
                      header=T, 
                      sep=",")
pd_sims <- read.table(unzip("sims-competitive-alternative/final-pd.zip"), 
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



## ------------------------------------------------------------------------- ##
## GENETICS
## ------------------------------------------------------------------------- ##

gen_sims <- read.table(unzip("sims-competitive/sims-d1-d2-alpha-0.018-egs-0.1-comp.zip"), 
                       header=T, 
                       sep=",")
gen_sims <- read.table(unzip("sims-competitive-alternative/final-genetics.zip"), 
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

## --------------------------------------------------------------- ##
## MODEL COMPARISON
## --------------------------------------------------------------- ##

frank2004points <- c(frank2004$Choose, frank2004$Avoid)

frank2007points <- c(frank2007a$Choose, frank2007a$Avoid, frank2007b$Choose, frank2007b$Avoid)

mybic <- function(predictions, data, npars) {
  n <- length(predictions)
  k <- npars
  
  rss <- sum((predictions - data) ** 2)
  n + n * log(2 * pi) + n * log(rss / n) + log(n) * (k + 1)
}

rmse <- function(predictions, data) {
  sqrt(mean((predictions - data) ** 2))
}


## Calculate a vector of values for Competitive model

z1 <- subset(mpd, mpd$D1==1 & mpd$D2 == 1)
z2 <- subset(mpd, mpd$D1==0.2 & mpd$D2 == 2)
z3 <- subset(mpd, mpd$D1==2 & mpd$D2 == 0.2)
z <- merge(z1, z2, all=T)
z <- merge(z, z3, all=T)
az <- aggregate(z[c("Accuracy")], list(D1=z$D1, Measure=z$Measure), mean)

c2004points <- az$Accuracy[c(5,6,4,2,3,1)] * 100

## The best theoretical predictiojns for the canonical model: 
## "Off" PD data is simply replaced by control data

## Calculate a vector of values for Competitive model
w1 <- subset(mgen, mgen$D1==0.5 & mgen$D2 == 1)
w2 <- subset(mgen, mgen$D1==1.5 & mgen$D2 == 1)
w3 <- subset(mgen, mgen$D2==0.5 & mgen$D1 == 1)
w4 <- subset(mgen, mgen$D2==1.5 & mgen$D1 == 1)
w <- merge(w1, w2, all=T)
w <- merge(w, w3, all=T)
w <- merge(w, w4, all=T)
aw <- aggregate(w[c("Accuracy")], list(D1=w$D1, D2=w$D2, Measure=w$Measure), mean)

c2007points <- aw$Accuracy[c(7,6,3,2,5,8,1,4)] * 100


# Separate fitting for the three conditions of PD

s_data$PD_Controls_Energy <- 0
s_data$PD_On_Energy <- 0
s_data$PD_Off_Energy < 0

s_data$Gen_D1_Up <-0 
s_data$Gen_D1_Down <-0 
s_data$Gen_D2_Up <-0 
s_data$Gen_D2_Down <-0 

for (a in unique(s_data$Alpha)) {
  for (s in unique(s_data$EGS)) {
    for (rplus in unique(s_data$PosRwrd)) {
      for (rminus in unique(s_data$NegRwrd)) {
        psub <- subset(s_data, s_data$Alpha == a &
                        s_data$EGS == s &
                        s_data$PosRwrd == rplus &
                        s_data$NegRwrd == rminus)
        pdata <- psub[c("ChooseA", "AvoidB")] * 100
        
        # Parkinson's data
        
        esub <- subset(frank2004, frank2004$Group == "Controls")
        edata <- esub[c("Choose", "Avoid")] 
        s_data$PD_Controls_Energy[s_data$Alpha == a &
                                    s_data$EGS == s &
                                    s_data$PosRwrd == rplus &
                                    s_data$NegRwrd == rminus] <- sum(c((pdata - edata)**2))
        
        esub <- subset(frank2004, frank2004$Group == "PD, On Medication")
        edata <- esub[c("Choose", "Avoid")] 
        s_data$PD_On_Energy[s_data$Alpha == a &
                              s_data$EGS == s &
                              s_data$PosRwrd == rplus &
                              s_data$NegRwrd == rminus] <- sum(c((pdata - edata)**2))
        
        esub <- subset(frank2004, frank2004$Group == "PD, Off Medication")
        edata <- esub[c("Choose", "Avoid")] 
        s_data$PD_Off_Energy[s_data$Alpha == a &
                               s_data$EGS == s &
                               s_data$PosRwrd == rplus &
                               s_data$NegRwrd == rminus] <- sum(c((pdata - edata)**2))
        
        # Genetics dataset
        esub <- subset(frank2007a, frank2007a$Group == "A/A")
        edata <- esub[c("Choose", "Avoid")] 
        s_data$Gen_D1_Up[s_data$Alpha == a &
                               s_data$EGS == s &
                               s_data$PosRwrd == rplus &
                               s_data$NegRwrd == rminus] <- sum(c((pdata - edata)**2))
        
        esub <- subset(frank2007a, frank2007a$Group == "G/G, G/A")
        edata <- esub[c("Choose", "Avoid")] 
        s_data$Gen_D1_Down[s_data$Alpha == a &
                           s_data$EGS == s &
                           s_data$PosRwrd == rplus &
                           s_data$NegRwrd == rminus] <- sum(c((pdata - edata)**2))
        
        esub <- subset(frank2007b, frank2007b$Group == "T/T")
        edata <- esub[c("Choose", "Avoid")] 
        s_data$Gen_D2_Up[s_data$Alpha == a &
                           s_data$EGS == s &
                           s_data$PosRwrd == rplus &
                           s_data$NegRwrd == rminus] <- sum(c((pdata - edata)**2))
        
        esub <- subset(frank2007b, frank2007b$Group == "C/C, C/T")
        edata <- esub[c("Choose", "Avoid")] 
        s_data$Gen_D2_Down[s_data$Alpha == a &
                             s_data$EGS == s &
                             s_data$PosRwrd == rplus &
                             s_data$NegRwrd == rminus] <- sum(c((pdata - edata)**2))
        
      }
    }
  }
}

subset(s_data, s_data$PD_Controls_Energy == min(s_data$PD_Controls_Energy))
subset(s_data, s_data$PD_On_Energy == min(s_data$PD_On_Energy))
subset(s_data, s_data$PD_Off_Energy == min(s_data$PD_Off_Energy))

points <- rbind(subset(s_data, s_data$PD_Controls_Energy == min(s_data$PD_Controls_Energy)),
                subset(s_data, s_data$PD_On_Energy == min(s_data$PD_On_Energy)),
                subset(s_data, s_data$PD_Off_Energy == min(s_data$PD_Off_Energy)))

nc2004e_points <- c(points$ChooseA, points$AvoidB)

mybic(nc2004e_points, frank2004points, 4)
mybic(c2004points, frank2004points, 6)
mybic(nc2004e_points, frank2004points, 4) - mybic(c2004points, frank2004points, 6)



# Genetics data

subset(s_data, s_data$Gen_D1_Up == min(s_data$Gen_D1_Up))
subset(s_data, s_data$Gen_D1_Down == min(s_data$Gen_D1_Down))
subset(s_data, s_data$Gen_D2_Up == min(s_data$Gen_D2_Up))
subset(s_data, s_data$Gen_D2_Down == min(s_data$Gen_D2_Down))

apoints <- rbind(subset(s_data, s_data$Gen_D1_Up == min(s_data$Gen_D1_Up)),
                 subset(s_data, s_data$Gen_D1_Down == min(s_data$Gen_D1_Down)))

bpoints <- rbind(subset(s_data, s_data$Gen_D2_Down == min(s_data$Gen_D2_Down)),
                 subset(s_data, s_data$Gen_D2_Up == min(s_data$Gen_D2_Up)))

nc2007e_points <- c(apoints$ChooseA, apoints$AvoidB, bpoints$ChooseA, bpoints$AvoidB)

mybic(nc2007e_points, frank2007points, 4)
mybic(c2007points, frank2007points, 6)
mybic(nc2007e_points, frank2007points, 4) - mybic(c2007points, frank2007points, 6)


## Results from reverse simulations 


anticanonical <- read.table(unzip("sims-noncompetitive-reverse/sims-positive-negative-alpha-egs-noncompetitive-reverse.zip"), 
                            header=T,
                            sep=","
)


# Simplify for analysis
a_anticanonical <- aggregate(anticanonical[c("ChooseA", "AvoidB", 
                             "PickA", "PickB",
                             "PickC", "PickD",
                             "PickE", "PickF")], 
                     list(Alpha=anticanonical$Alpha, 
                          EGS = anticanonical$EGS, 
                          PosRwrd=anticanonical$PosRwrd, 
                          NegRwrd=anticanonical$NegRwrd), 
                     mean)

## Individual values

a_data1 <- a_anticanonical
a_data2 <- a_anticanonical

a_data1$AvoidB <- NULL
names(a_data1)[5] <- "Accuracy"
a_data1$Measure <- "Choose"

a_data2$ChooseA <- NULL
names(a_data2)[5] <- "Accuracy"
a_data2$Measure <- "Avoid"

a_data_anticanonical <- merge(a_data1, a_data2, all=T)

a_data_anticanonical <- subset(a_data_anticanonical, a_data_anticanonical$Alpha < 0.1)
a_data_anticanonical <- subset(a_data_anticanonical, a_data_anticanonical$EGS > 0)


a_anticanonical$Pattern <- vpattern(a_anticanonical$ChooseA, a_anticanonical$AvoidB)

psp2 <- tapply(a_anticanonical$ChooseA, a_anticanonical$Pattern, length)

round(100 * psp2 / (sum(c(psp2))), 2)

biological <- merge(pd_sims, gen_sims, all=T)
biological$Pattern <- vpattern(biological$ChooseA, biological$AvoidB)

psp3 <- tapply(biological$ChooseA, biological$Pattern, length)

round(100 * psp3 / (sum(c(psp3))), 2)



s_data$Pattern <- vpattern(s_data$ChooseA, s_data$AvoidB)

psp <- tapply(s_data$ChooseA, s_data$Pattern, length)

round(100 * psp / (sum(c(psp))), 2)


