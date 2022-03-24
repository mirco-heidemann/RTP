## ---------------------------------------------------------------------
##  Darstellung der Eintretenswahrscheinlichkeit von Ereignissen
##  verschiedener Wiederkehrperiode bei unterschiedlicher Lebensdauer
##
##  Jan Kleinn, 11/2009
##  Mirco Heidemann, 12/2014
## ---------------------------------------------------------------------

setwd('J:/MHE/03 Statistische Analysen/Wiederkehrperioden')

ps.file <- 'pdf/EP_RetPer_Lifetime_DE.ps'

##wkp <- c(10, 30, 50, 100, 300, 475)
wkp <- c(10, 20, 30, 50, 100, 300, 475, 975)
lebdauer <- 1:100
ew <- ew.pois <- ew.binom <- ew.bin2 <- array(dim = c(length(wkp), length(lebdauer)))
for (i in 1:length(wkp)) {
  ew[i,] <- 100 * (1 - (1 - (1 / wkp[i]))^lebdauer)
  lambda <- lebdauer / wkp[i]
  ##ew.pois[i,] <- 100 * (1 - exp(-lambda))
  ##ew.pois[i,] <- 100 * (1 - ppois(0, lambda))
  ew.pois[i,] <- 100 * ppois(0, lambda, lower.tail = FALSE)
  ew.binom[i,] <- 100 * pbinom(0, lebdauer, 1/wkp[i], lower.tail = FALSE)
  p <- 1/wkp[i]
  binom0 <- choose(lebdauer, 0) * p^0 * (1-p)^(lebdauer-0)
  ew.bin2[i,] <- 100 * (1 - binom0)
}

postscript(ps.file, paper = 'a4', horizontal = TRUE, colormodel = 'rgb')
par(mar = c(3.5, 3.5, 3, 1), las = 1)

##cols <- c('green3', 'blue', 'darkorchid', 'red', 'orange', 'saddlebrown')
cols <- rainbow((length(wkp) + 1))[-3]
##leg.txt <- paste(wkp, 'years')
leg.txt <- paste(wkp, 'Jahre')

plot(0, 0, type = 'n', xlim = c(0, 100), ylim = c(0, 100),
     xaxs = 'i', yaxs = 'i', ann = FALSE, axes = FALSE)
abline(h = seq(5, 95, 10), col = 'grey80')
abline(v = seq(5, 95, 10), col = 'grey80')
abline(h = seq(10, 90, 10), col = 'grey60')
abline(v = seq(10, 90, 10), col = 'grey60')
for (i in 1:dim(ew)[1]) {
  ##lines(lebdauer, ew.pois[i,], lwd = 2, col = 'grey40')
  lines(lebdauer, ew[i,], lwd = 2, col = cols[i])
  ##lines(lebdauer, ew.bin2[i,], lwd = 2, col = 'black', lty = 3)
}
##legend('bottomright', leg.txt, col = cols, lwd = 2, bg = 'white',
##       title = 'Return Period (T)', ncol = 2)
legend('bottomright', leg.txt, col = cols, lwd = 2, bg = 'white',
       title = 'Wiederkehrperiode (T)', ncol = 2)
axis(1, seq(0, 100, 5), label = FALSE)
axis(1, seq(0, 100, 10), tick = FALSE)
axis(2, seq(0, 100, 5), label = FALSE)
axis(2, seq(0, 100, 10), tick = FALSE)
axis(4, seq(0, 100, 5), label = FALSE)
##axis(4, seq(0, 100, 10), tick = FALSE)
##main.txt <- paste('Exceedance probability', '\n', 
##                  'for different return periods of events',
##                  'and different life times of an object')
main.txt <- paste('Eintretens-Wahrscheinlichkeit', '\n', 
                  'bei unterschiedlicher Wiederkehrperiode des Ereignisses',
                  'und unterschiedlicher Lebensdauer des Objektes')
box()
title(main = main.txt, line = 0.5)
##title(xlab = 'life time of an object (n)', line = 2.3)
title(xlab = 'Lebensdauer des Objektes in Jahren (n)', line = 2.3)
##title(ylab = expression(paste('Exceedance probability in percent, ',
title(ylab = expression(paste('Eintretens-Wahrscheinlichkeit in Prozent, ',
    p == 1 - (1 - 1/T)^n)), line = 2.2)

dev.off()

## convert PostScript to PDF
system(paste(Sys.getenv("COMSPEC"),"/c ps2pdf", ps.file))
file.remove(ps.file)

## R beenden ohne etwas zu speichern
q('no')
