## ---------------------------------------------------------------------
##  Darstellung der Eintretenswahrscheinlichkeit von Ereignissen
##  verschiedener Wiederkehrperiode bei definierter Lebensdauer
##
##  Jan Kleinn, 11/2009
##  Mirco Heidemann, 12/2014
## ---------------------------------------------------------------------

setwd('J:/MHE/03 Statistische Analysen/Wiederkehrperioden')

ps.file <- 'pdf/Binom_WKP-Lebendsdauer_DE.ps'

##wkp <- c(10, 30, 50, 100, 300, 475)
wkp <- c(10, 20, 30, 50, 100, 300, 475, 975)
lebdauer <- 50
nevents <- 0:12

ew <- array(dim = c(length(wkp), length(nevents)))
for (i in 1:length(wkp)) {
  p <- 1/wkp[i]
  ew[i,] <- choose(lebdauer, nevents) * p^nevents * (1-p)^(lebdauer-nevents)
}

## convert to percent
ew <- 100 * ew

nplots <- ceiling(length(wkp)/4)

##cols <- c('green3', 'blue', 'darkorchid', 'red', 'orange', 'saddlebrown')
cols <- rainbow((length(wkp) + 1))[-3]
##leg.txt <- paste(wkp, 'years')
leg.txt <- paste(wkp, 'Jahre')

xmin <- 0.5
xmax <- (4 + 1) * length(nevents) + 0.5
ymin <- 0
ymax <- ceiling(max(ew[,-1]))

## reduce probabilities > ymax to ymax for nicer barplot
ind <- which(ew > ymax)
if (length(ind) > 0) ew[ind] <- ymax

postscript(ps.file, paper = 'a4', horizontal = TRUE, colormodel = 'rgb')
par(mar = c(3.5, 3.5, 3, 1), las = 1, lend = 2)

for (i in 1:nplots) {
  ind <- (1:4) + (4 * (i - 1))
  plot(0, 0, type = 'n', xlim = c(xmin, xmax), ylim = c(ymin, ymax),
       xaxs = 'i', yaxs = 'i', ann = FALSE, axes = FALSE)
  abline(h = seq(1, ymax, 1), col = 'grey80')
  abline(h = seq(5, ymax, 5), col = 'grey60')
  abline(v = seq(5.5, 5*(length(nevents)-1) + 0.5, 5), col = 'grey60')
  barplot(ew[ind,], beside = TRUE, col = cols[ind], border = FALSE, add = TRUE)
  legend('topright', leg.txt[ind], col = cols[ind], lwd = 10, bg = 'white',
         title = 'Wiederkehrperiode (T)')
  axis(1, seq(xmin, xmax, 5), label = FALSE)
  axis(1, seq((xmin + 2.5), xmax, 5), tick = FALSE, label = nevents,
       line = -0.3)
  main.txt <- paste('Eintretens-Wahrscheinlichkeit', '\n', 
                    'bei unterschiedlicher Wiederkehrperiode des Ereignisses',
                    'und Lebensdauer des Objektes von', lebdauer, 'Jahren')
  box()
  title(main = main.txt, line = 0.5)
  title(xlab = 'Anzahl Ereignisse in der Lebensdauer', line = 2.3)
  title(ylab = 'Eintretens-Wahrscheinlichkeit in Prozent', line = 2.2)
}
dev.off()

## convert PostScript to PDF
system(paste(Sys.getenv("COMSPEC"),"/c ps2pdf", ps.file))
file.remove(ps.file)

## R beenden ohne etwas zu speichern
q('no')
