library(lattice)
library(matlab)

# ANalyze basic simulations
source("functions.R")


## -----------------------------------------
## Basic data from Frank's experiments
## -----------------------------------------

## Original Data from Michael Frank

frank2004 <- data.frame(Choose = c(67.74144062, 79.02238338, 64.68865491),
                        Avoid = c(63.62980266, 58.10961579, 82.43114148),
                        ChooseSE = c(7.051112196, 5.05299491, 6.930827697),
                        AvoidSE = c(7.402203473, 11.27815355, 7.988982637),
                        Group=c("Senior Controls", "PD, On Medication", "PD, Off Medication"))

frank2007a <- data.frame(Choose = c(75.8930991, 66.60729804),
                         Avoid = c(66.76050086, 74.05648741),
                         ChooseSE = c(3.316427604, 5.050997524),
                         AvoidSE = c(3.750035042, 4.030509742),
                         Group=c("A/A (More D1)", "G/G, G/A (Less D1)"))

frank2007b <- data.frame(Choose = c(73.0866648, 68.80067095),
                         Avoid = c(66.81073525, 78.08624546),
                         ChooseSE = c(3.36734694, 5.357142855),
                         AvoidSE = c(3.44387755, 3.69897959),
                         Group=c("C/C, C/T (Less D2)", "T/T (More D2)"))

frank.plot <- function(data = frank2004, cols = c("black", "#22BB22", "red"), 
                       inc=0, title = "Frank", pch.cex = 1.5, ...) {
  xs <- c(0.25, 1.75)
  plot.new()
  plot.window(xlim=c(0, 2), ylim=c(50, 90))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(50, 90, 10), 
       labels = paste(seq(50, 90, 10), "%", sep = ""))
  box(bty="o")
  grid()
  #cols <- c("black", "green", "red")
  for (i in 1:3) {
    xs <- xs + i * inc
    ys <- as.numeric(data[c("Choose", "Avoid")][i,])
    ses <- as.numeric(data[c("ChooseSE", "AvoidSE")][i,])
    points(x = xs, y = ys, bg=cols[i], pch=21, col = cols[i], cex=pch.cex)
    lines(x = xs, y = ys, col=cols[i], ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
           angle = 90, length = 0.1, col=cols[i], ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
           angle = 90, length = 0.1, col=cols[i], ...)
  }
  title(main = title, ylab = "Accuracy")
  legend(x = "topleft", col = cols, 
         legend = data$Group, lwd = 1, lty = 1, 
         pch = 21, bty = "n", pt.bg = cols)
}

#tiff("figure4abc.tiff", width=6, height=4, res=150, units="in")
frank.triple <- function(...) {
  layout(matrix(1:3, nrow = 1, ncol = 3), widths = 1)
  par(mar = c(4, 3, 3, 1.4))
  frank.plot(frank2004, title="(A) Parkinson's Disease\n(Frank et al., 2004)", ...)
  frank.plot(frank2007a, title = "(B) DARPP-32 Polymorphism\n(Frank et al., 2007) ", cols = c("#22BB22", "black"), ...)
  frank.plot(frank2007b, title = "(C) DRD2 Polymorphisms\n(Frank et al., 2007)", cols = c("black", "red"), ...)
}

figure04 <- function() {
  frank.triple()
} 

## ------------------------------------------------------------------------- ##
## The Non-Competitive Model Simulations
## ------------------------------------------------------------------------- ##

nc.plot <- function(data = a_data, factor = "Alpha", pch.cex = 1.5, ...) {
  xs <- c(0.25, 1.75)
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.5, 0.7))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(0.5, 0.7, 0.05), 
       labels = paste(seq(50, 70, 5), "%", sep = ""))
  box(bty="o")
  grid()
  cols <- grey.colors(length(unique(data[[factor]])))
  i <- 1
  for (val in unique(data[[factor]])) {
    sub <- subset(data, data[[factor]] == val)
    ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
    ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
    points(x = xs, y = ys, bg=cols[i], pch=21, col = cols, cex=pch.cex)
    lines(x = xs, y = ys, col=cols[i], ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
           angle = 90, length = 0.1, col=cols[i], ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
           angle = 90, length = 0.1, col=cols[i], ...)
    i <- (i + 1)
  }

}


#tiff("figure7abc.tiff", width=8, height=4, res=150, units="in")
nc.fourplot <- function(data=a_data_canonical) {
  layout(matrix(1:4, nrow = 1, ncol = 4), widths = 1)
  par(mar = c(4, 3, 3, 1.4))
  nc.plot(data, factor = "Alpha")
  title(main = expression(paste(bold("(A) Effect of "), alpha)))
  vals = parse(text = paste("alpha == ", unique(a_data$Alpha)))
  legend(x = "topleft", legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21, 
         bty = "n", 
         col = grey.colors(n = length(unique(a_data$Alpha))),
         pt.bg = grey.colors(n = length(unique(a_data$Alpha))))
  
  nc.plot(data, factor="EGS")
  title(main = expression(paste(bold("(B) Effect of "), italic("s"))))
  vals = parse(text = paste("italic('s') == ", unique(a_data$EGS)))
  legend(x = 1, y=0.708, legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21, 
         bty = "n", 
         col = grey.colors(n = length(unique(a_data$EGS))),
         pt.bg = grey.colors(n = length(unique(a_data$EGS))))
  
  nc.plot(data, factor="PosRwrd")
  title(main = expression(paste(bold("(C) Effect of "),
                                italic(R) ^ plain("+")
                                )))
  vals = parse(text = paste("italic('R') ^ plain('+') == ", unique(a_data$PosRwrd)))
  legend(x = 0.8, y = 0.708, legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21, 
         bty = "n",
         ncol = 1,
         col = grey.colors(n = length(unique(a_data$PosRwrd))),
         pt.bg = grey.colors(n = length(unique(a_data$PosRwrd))))
  
  nc.plot(data, factor="NegRwrd")
  title(main = expression(paste(bold("(D) Effect of "),
                                italic(R) ^ plain("-"))))
  vals = parse(text = paste("italic('R') ^ plain('-') == ", unique(a_data$NegRwrd)))
  legend(x = 0.76, y = 0.708,  legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21, 
         bty = "n", 
         ncol = 1,
         col = grey.colors(n = length(unique(a_data$NegRwrd))),
         pt.bg = grey.colors(n = length(unique(a_data$NegRwrd))))
  
}

figure07 <- function() {
  nc.fourplot(a_data)
}

plot.alpha.optimized <- function(data = a_base) {
  plot.new()
  plot.window(xlim=c(min(data$Alpha), max(data$Alpha)), 
              ylim=c(0, 0.06))
  axis(1, at = seq(0, 0.1, 0.01))
  axis(2, at = seq(0, 0.08, 0.01))
  box(bty="o")
  grid()
  
  lines(x = data$Alpha, 
        y = data$EnergySeniors, lwd = 2, col = "black")
  lines(x = data$Alpha, 
        y = data$EnergyGenetics, lwd = 2, col = "grey")
  
  abline(v = data$Alpha[data$EnergySeniors == min(data$EnergySeniors)], 
         lwd = 1, col = "black", lty = 2)
  
  abline(v = data$Alpha[data$EnergyGenetics == min(data$EnergyGenetics)], 
         lwd = 1, col = "grey", lty = 2)
  
  points(x = data$Alpha[data$EnergyGenetics == min(data$EnergyGenetics)],
         y = min(data$EnergySeniors),
         col = "white",
         cex = 1.5,
         pch =21,
         bg = "grey")
  
  points(x = data$Alpha[data$EnergySeniors == min(data$EnergySeniors)],
         y = min(data$EnergySeniors),
         col = "white",
         cex = 1.5,
         pch =21,
         bg = "black")
  
  title(main = "Fit for Baseline Data",
        xlab = expression(paste(plain("Learning rate "), alpha)),
        ylab = "Root mean squared error")
}


# Model plots

model.plot.pd <- function(data = mpd, pch.cex = 1.5, frank=T, ...) {
  xs <- c(0.25, 1.75)
  cols <- c("#000000", "#22BB22", "#FF0000")
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.5, 0.9))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(0.5, 0.9, 0.1), 
       labels = paste(seq(50, 90, 10), "%", sep = ""))
  box(bty="o")
  grid()
  i <- 1
  
  # if FRANK, plots the original Frank data
  if (frank) {
    ncols <- paste(cols, "77", sep="")
    for (i in 1:3) {
      xs <- xs #+ i * inc
      ys <- as.numeric(frank2004[c("Choose", "Avoid")][i,]) / 100
      ses <- as.numeric(frank2004[c("ChooseSE", "AvoidSE")][i,]) / 100
      points(x = xs, y = ys, bg=ncols[i], pch=21, col = ncols[i], cex=pch.cex)
      lines(x = xs, y = ys, col=ncols[i], lty = 5, lwd=1, ...)
      arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
             angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
      arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
             angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
    }
  }
  
  # Seniors
  sub <- subset(data, data$D1 == 1 & data$D2 == 1)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg=cols[1], pch=21, col = cols[1], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[1], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)

  
  # PD On
   sub <- subset(data, data$D1 == 2 & data$D2 == 0.2)
  #sub <- subset(data, data$D1 == 1.15 & data$D2 == 1.85)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  print(ses)
  points(x = xs, y = ys, bg=cols[2], pch=21, col = cols[2], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[2], lwd=3)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[2], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[2], lwd=2,...)
  
  # PD Off
  #sub <- subset(data, data$D1 == 0.2 & data$D2 == 2.0)
  sub <- subset(data, data$D1 == 1.85 & data$D2 == 1.15)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg=cols[3], pch=21, col = cols[3], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[3], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[3], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[3], lwd=2, ...)
  
  #legend(x = "topleft", col = cols, 
  #       legend = c("Controls", 
  #                  "PD, On Medicatioin", 
  #                  "PD, Off medication"), 
  #       lwd = 1, lty = 1, 
  #       pch = 21, bty = "n", pt.bg = cols) 
  
  legend(x = "topleft", col = cols, 
         legend = c(expression(paste(plain("Controls ("), italic(d)[1] == 1, ",", italic(d)[2] == 1, ")")),
                    expression(paste(plain("On ("), italic(d)[1] == 1.8, ",", italic(d)[2] == 0.2, ")")), 
                    expression(paste(plain("Off ("), italic(d)[1] == 0.2, ",", italic(d)[2] == 1.8, ")"))),
                    lwd = 1, lty = 1, 
         pch = 21, bty = "n", pt.bg = cols)
  
}


model.plot.d1 <- function(data = mgen, low=0.5, high=0.5, pch.cex = 1.5, frank=T,...) {
  xs <- c(0.25, 1.75)
  cols <- c("#000000", "#22BB22", "#FF0000")
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.5, 0.9))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(0.5, 0.9, 0.1), 
       labels = paste(seq(50, 90, 10), "%", sep = ""))
  box(bty="o")
  grid()
  i <- 1
  
  # if FRANK, plots the original Frank data
  if (frank) {
    ncols <- rev(paste(cols[1:2], "77", sep=""))
    for (i in 1:3) {
      xs <- xs #+ i * inc
      ys <- as.numeric(frank2007a[c("Choose", "Avoid")][i,]) / 100
      ses <- as.numeric(frank2007a[c("ChooseSE", "AvoidSE")][i,]) / 100
      points(x = xs, y = ys, bg=ncols[i], pch=21, col = ncols[i], cex=pch.cex)
      lines(x = xs, y = ys, col=ncols[i], lty = 5, lwd=1, ...)
      arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
             angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
      arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
             angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
    }
  }
  
  
  # A/A
  sub <- subset(data, data$D1 == high & data$D2 == 1.0)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg=cols[2], pch=21, col = cols[2], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[2], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[2], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[2], lwd=2, ...)
  
  # A/G, G/G
  sub <- subset(data, data$D1 == low & data$D2 == 1)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg=cols[1], pch=21, col = cols[1], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[1], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)
  #legend(x = "topleft", col = cols[c(2,1)], 
  #       legend = c("A/A", "A/G, G/G"), 
  #       lwd = 1, lty = 1, 
  #       pch = 21, bty = "n", pt.bg = cols[c(2,1)])
  
  legend(x = "topleft", col = cols[c(3,1)], 
         legend = c(expression(paste(plain("A/A ("), italic(d)[1] == 1.5, ")")), 
                    expression(paste(plain("A/G, G/G ("), italic(d)[1] == 0.5, ")"))), lwd = 1, lty = 1, 
         pch = 21, bty = "n", pt.bg = cols[c(3,1)])
}


model.plot.d2 <- function(data = mgen, low = 0.5, high = 1.5, pch.cex = 1.5, frank=T, ...) {
  xs <- c(0.25, 1.75)
  cols <- c("#FF0000", "#22BB22", "#000000")
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.5, 0.9))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(0.5, 0.9, 0.1), 
       labels = paste(seq(50, 90, 10), "%", sep = ""))
  box(bty="o")
  grid()
  i <- 1
  
  # if FRANK, plots the original Frank data
  if (frank) {
    ncols <- paste(cols[c(1,3)], "77", sep="")
    for (i in 1:3) {
      xs <- xs #+ i * inc
      ys <- as.numeric(frank2007b[c("Choose", "Avoid")][i,]) / 100
      ses <- as.numeric(frank2007b[c("ChooseSE", "AvoidSE")][i,]) / 100
      points(x = xs, y = ys, bg=ncols[i], pch=21, col = ncols[i], cex=pch.cex)
      lines(x = xs, y = ys, col=ncols[i], lty = 5, lwd=1, ...)
      arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
             angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
      arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
             angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
    }
  }
  
  
  # T/T
  sub <- subset(data, data$D1 == 1.0 & data$D2 == high)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg=cols[3], pch=21, col = cols[3], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[3], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.1, col=cols[3], ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.1, col=cols[3], ...)
  
  # C/C, C/T
  sub <- subset(data, data$D1 == 1.0 & data$D2 == low)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg=cols[1], pch=21, col = cols[1], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[1], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)
  legend(x = "topleft", col = cols[c(3,1)], 
         legend = c(expression(paste(plain("C/C, C/T ("), italic(d)[2] == 0.5, ")")), 
                    expression(paste(plain("T/T ("), italic(d)[2] == 1.5, ")"))), lwd = 1, lty = 1, 
         pch = 21, bty = "n", pt.bg = cols[c(3,1)])
}

#tiff("figure6abc.tiff", width=6, height=4, res=150, units="in")

model.triple <- function(...) {
  layout(matrix(1:3, nrow = 1, ncol = 3), widths = 1)
  par(mar = c(4, 3, 3, 1.4))
  model.plot.pd(...)
  title(main="(A) Parkinson's Disease\n(Simulations and Data)")
  model.plot.d1(...)
  title(main = "(B) DARPP-32 Polymorphism\n(Simulations and Data)")
  model.plot.d2(...)
  title(main = "(C) DRD2 Polymorphism\n(Simulations and Data)")
}

super.figure <- function(cols = c("#000000", "#22BB22", "#FF0000"), pch.cex = 1.5, ...) {
  data <- frank2004
  xs <- c(0.25, 1.75) 
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.5, 0.9))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(0.5, 0.9, 0.1), 
       labels = paste(seq(50, 90, 10), "%", sep = ""))
  box(bty="o")
  grid()
  ncols <- paste(cols, "77", sep="")
  for (i in 1:3) {
    xs <- xs #+ i * inc
    ys <- as.numeric(data[c("Choose", "Avoid")][i,]) / 100
    ses <- as.numeric(data[c("ChooseSE", "AvoidSE")][i,]) / 100
    points(x = xs, y = ys, bg=ncols[i], pch=21, col = ncols[i], cex=pch.cex)
    lines(x = xs, y = ys, col=ncols[i], lty = 5, lwd=1, ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
           angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
           angle = 90, length = 0.1, col=ncols[i], lty=5, ...)
  }
  
  # Model
  data <- mpd
  # Seniors
  sub <- subset(data, data$D1 == 1 & data$D2 == 1)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se)) 
  points(x = xs, y = ys, bg=cols[1], pch=21, col = cols[1], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[1], lwd=2,...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[1], lwd=2, ...)
  
  
  # PD On
  sub <- subset(data, data$D1 == 2 & data$D2 == 0.2)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  print(ses)
  points(x = xs, y = ys, bg=cols[2], pch=21, col = cols[2], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[2], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[2], lwd=3, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[2], lwd=3,...)
  
  # PD Off
  sub <- subset(data, data$D1 == 0.2 & data$D2 == 2.0)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg=cols[3], pch=21, col = cols[3], cex=pch.cex)
  lines(x = xs, y = ys, col=cols[3], lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.05, col=cols[3], lwd=2,...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.05, col=cols[3], lwd=2,...)
}



figure08 <- function() {
  layout(matrix(c(1,2,3,3), byrow=T, nrow=2))
  par(mar=c(1,2,2,2))
  rpsp1 <- round(100 * psp / (sum(c(psp))), 2)
  pie(rpsp1, labels=c("Choose = Avoid", "Choose > Avoid"), 
      col=rev(grey.colors(3))[2:3], border="white", edges = 30000)
  text(x=c(-0.25, 0.35), y=c(0.25, -0.25), 
       labels= paste(rpsp1, "%", sep=""), col=c("black",  "white"))
  title(main="(A) Canonical Model (Fig. 6A)")
  
  
  rpsp3 <- round(100 * psp3 / (sum(c(psp3))), 2)
  pie(rpsp3, labels=c("Choose < Avoid", "Choose = Avoid", "Choose > Avoid"), 
      col=rev(grey.colors(3)), border="white", edges = 30000)
  text(x=c(0.15, -0.45, 0.15), y=c(0.35,0,  -0.35), 
       labels= paste(rpsp3, "%", sep=""), col=c("black", "black", "white"))
  title(main="(B) Biological, Competitive Model (Fig. 6B)")
  
  rpsp2 <- round(100 * psp2 / (sum(c(psp2))), 2)
  pie(rpsp2, labels=c("Choose < Avoid", "Choose = Avoid"), 
      col=rev(grey.colors(3))[1:2], border="white", edges = 30000)
  text(x=c(0.35, -0.25), y=c(0.25, -0.25), 
       labels= paste(rpsp2, "%", sep=""), col=c("black"))
  title(main="(C) Anti-Canonical Model", sub="Parameter Space Partitioning")
  
}

figure9.d1 <- function(data = mgen, colfunc=colorRampPalette(c("lightgreen", "darkgreen")),...) {
  xs <- c(0.25, 1.75)
  cols <- c("red", "#22BB22", "black")
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.5, 0.9))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(0.5, 0.9, 0.1), 
       labels = paste(seq(50, 90, 10), "%", sep = ""))
  box(bty="o")
  grid()
  i <- 1
  
  
  d <- subset(data, data$D1 %in% c(0, 0.25, 0.5, 0.75, 1.25, 1.5, 1.75, 2) 
              &  data$D2 == 1)
  n <- length(unique(d$D1))
  #cols <- rgb(red=0, green=(1:(n + 2))/(n + 2), blue=0)
  cols <- colfunc(n)
  print(cols)
  for (i in 1:n) {
    sub <- subset(d, d$D1 == unique(d$D1)[i])
    ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
    ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
    points(x = xs, y = ys, bg=cols[i], pch=21, col = cols[i], cex=1.5)
    lines(x = xs, y = ys, col=cols[i], lty=2, ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
           angle = 90, length = 0.1, col=cols[i], ...)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
           angle = 90, length = 0.1, col=cols[i], ...)
  }
  
  sub <- subset(data, data$D1 == 1 & data$D2 == 1)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg="black", pch=21, col = "black", cex=1.5)
  lines(x = xs, y = ys, col="black", lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.1, col="black", ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.1, col="black", ...)
  #vals = parse(text = paste("italic(d)[1] == ", unique(d$D1)))
  #vals = parse(text = paste("paste(italic(d)[1] ==\"", formatC(unique(d$D1), digits = 2, format="f"), "\",',  ', italic(d)[2] == \"1.00\")"))
  vals = parse(text = paste("italic(d)[1] ==\"", formatC(unique(d$D1), digits = 2, format="f"), "\"", sep=""))
  
  legend(x = 0.8, y = 0.9 + 0.4*0.04,  
         legend = c(vals, expression(paste(italic(d)[1], " = ", italic(d)[2] == 1))),
         col = c(cols, "black"), 
         lty = 1, pch = 21, 
         pt.bg = c(cols, "black"), 
         lwd = c(rep(1, length(vals)), 2),
         bty="n")
  
}

figure9.d2 <- function(data = mgen, colfunc=colorRampPalette(c("gold", "darkred")), ...) {
  xs <- c(0.25, 1.75)
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.5, 0.9))
  axis(1, at = xs, labels = c("\nChoose Accuracy", "\nAvoid Accuracy"))
  axis(2, at = seq(0.5, 0.9, 0.1), 
       labels = paste(seq(50, 90, 10), "%", sep = ""))
  box(bty="o")
  grid()
  i <- 1
  
  
  d <- subset(data, data$D2 %in% c(0, 0.25, 0.5, 0.75, 1.25, 1.5, 1.75, 2) 
              & data$D1 == 1)
  n <- length(unique(d$D2))
  #cols <- rgb(red=(2:(n + 2))/(n + 2), green=0, blue=0)
  cols <- colfunc(n)
  print(cols)
  for (i in 1:n) {
    sub <- subset(d, d$D2 == unique(d$D2)[i])
    ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
    ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
    points(x = xs, y = ys, bg=cols[i], pch=21, col = cols[i], cex=1.5)
    lines(x = xs, y = ys, col=cols[i], lty=2)
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
           angle = 90, length = 0.1, col=cols[i])
    arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
           angle = 90, length = 0.1, col=cols[i], ...)
  }
  
  sub <- subset(data, data$D1 == 1 & data$D2 == 1)
  ys <- rev(tapply(sub$Accuracy, sub$Measure, mean))
  ses <- rev(tapply(sub$Accuracy, sub$Measure, se))
  points(x = xs, y = ys, bg="black", pch=21, col = "black", cex=1.5)
  lines(x = xs, y = ys, col="black", lwd=2, ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys + ses, 
         angle = 90, length = 0.1, col="black", ...)
  arrows(x0 = xs, y0 = ys, x1 = xs, y1 = ys - ses, 
         angle = 90, length = 0.1, col="black", ...)
#vals = parse(text = paste("italic(d)[2] == ", unique(d$D2)))
  #vals = parse(text = paste("paste(italic(d)[1] ==\"1.00\", ',  '", ', ', 'italic(d)[2] == \"', formatC(unique(d$D2), digits = 2, format="f"), "\")"))
  vals = parse(text = paste("italic(d)[2] ==\"", formatC(unique(d$D2), digits = 2, format="f"), "\"", sep=""))
  
  legend(x="topleft",
         legend = c(vals, expression(paste(italic(d)[1], " = ", italic(d)[2] == 1))), 
  #legend(x="topleft", legend=c(vals, expression(paste(italic(d)[1] == 1, ".00,  ", italic(d)[2] == 1, ".00"))), col = c(cols, "black"),
        col = c(cols, "black"),
        lwd = c(rep(1, length(vals)), 2),
        lty = 1, pch = 21, pt.bg = c(cols, "black"), bty="n")
  
  
}

#tiff("figure8ab.tiff", width=6, height=4, res=150, units="in")
figure09 <- function(data = merge(mpd, mgen, all=T)) {
  layout(matrix(1:4, nrow = 1, ncol = 4), widths = c(1/2, 1, 1, 1/2))
  par(mar = c(4,3,3,1))
  plot.new()
  figure8.d1(data)
  title(main = expression(paste(bold("(A) Effects of "), bold(italic(d))[1])))
  figure8.d2(data)
  title(main = expression(paste(bold("(B) Effects of "), bold(italic(d))[2])))
  plot.new()

}



#tiff("figureB1.tiff", width=6, height=4, res=150, units="in")

figb1 <- function() {
  plot.new()
  plot.window(xlim=c(0,0.1), ylim=c(0.0, 0.05))
  axis(1, at = seq(0, 0.1, 0.01))
  axis(2, at = seq(0.0, 0.05, 0.025), 
       labels = seq(0, 0.05, 0.025))
  box(bty="o")
  grid()
  
  # Seniors
  lines(x = a_base$Alpha, y=a_base$EnergySeniors, col="black", lwd=2)
  m <- min(a_base$EnergySeniors)
  points(x = a_base$Alpha[a_base$EnergySeniors == m],
         y = m,
         cex = 1.5,
         pch = 21,
         bg = "black",
         col = "white")
  abline(v = a_base$Alpha[a_base$EnergySeniors == m], 
         lwd=1, col="black", lty=5)
  
  # Genetics
  lines(x = a_base$Alpha, y=a_base$EnergyGenetics, col="grey", lwd=2)
  m <- min(a_base$EnergyGenetics)
  points(x = a_base$Alpha[a_base$EnergyGenetics == m],
         y = m,
         cex = 1.5,
         pch = 21,
         bg = "grey",
         col = "white")
  abline(v = a_base$Alpha[a_base$EnergyGenetics == m], 
         lwd=1, col="grey", lty=5)
  
  legend(x=0.02, y=0.05, 
         legend=c("PD dataset (controls)",
                  "Genetic dataset (group average)"),
         col = c("black", "grey"),
         bty="n",
         pch = 21, pt.bg = c("black", "grey"),
         lwd=1)
  title(main = expression(paste(bold("Data fit for different values of "), alpha)),
        xlab = expression(paste(plain("Learning rate "), alpha)),
        ylab = "MSE")
}

figure10 <- function() {
  nc.fourplot(a_data2)
}

figure11 <- function() {
  model.triple()
}

figb2 <- function() {
  plot.new()
  plot.window(xlim=c(0,2), ylim=c(0.0, 0.03))
  axis(1, at = seq(0, 2, 0.2))
  axis(2, at = seq(0.0, 0.03, 0.005), 
       labels = seq(0, 0.03, 0.005))
  box(bty="o")
  grid()
  
  # PD experiment

  qvals <- sort(unique(apd$D1))
  fvals <- sapply(qvals, calculate.q.2004) 
  lines(x = qvals, y=fvals, col="black", lwd=2)
  points(x = qvals[fvals == min(fvals)],
         y = min(fvals),
         cex = 1.5,
         pch = 21,
         bg = "black",
         col = "white")
  abline(v = qvals[fvals == min(fvals)], 
         lwd=1, col="black", lty=5)
  
  # Genetics
  vals <- sort(unique(agen$D1))
  qvals <- calculate.q.2007(vals) 
  lines(x = vals, y=qvals, col="grey", lwd=2)
  points(x = vals[qvals == min(qvals)],
         y = min(qvals),
         cex = 1.5,
         pch = 21,
         bg = "grey",
         col = "white")
  abline(v = vals[qvals == min(qvals)], 
         lwd=1, col="grey", lty=5)
  
  legend(x=0.6, y=0.025,
         legend=c("PD dataset",
                  "Genetics dataset"),
         col = c("black", "grey"),
         bty="n",
         pch = 21, pt.bg = c("black", "grey"),
         lwd=1)
  title(main = expression(paste(bold("Data fit for different values of "), italic(q))),
        xlab = expression(paste(plain("Meta-parameter "), italic(q))),
        ylab = "MSE")
}



