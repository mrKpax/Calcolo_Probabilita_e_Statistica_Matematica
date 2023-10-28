if (!require(readxl)) {
  install.packages("readxl")
}

setwd("/Users/francesco/Desktop/ProgettoCPSM")
getwd()

library(readxl)

statSerieA <- read_excel("raccoltaDati.xlsx")
View(statSerieA)

# Filtra le squadre vincitrici
squadreV <- statSerieA[statSerieA$Posizionamento == "V", ]

# Calcolo statistiche delle Vincitrici
mediaDRV <- mean(squadreV$`Diff. reti`)
medianaDRV <- median(squadreV$'Diff. reti')
massimoDRV <- max(squadreV$'Diff. reti')
minimoDRV <- min(squadreV$'Diff. reti')
varianzaDRV <- var(squadreV$'Diff. reti')
conteggioDRV <- length(squadreV$'Diff. reti')

# Realizzazione barplot
barplot(squadreV$'G. fatti', main = "Analisi Statistiche Vincitrici", names.arg=squadreV$Stagione, las = 2, cex.names = 0.9, col="red", ylim = c(0, 100))
axis(2, at = seq(0, 100, by = 10), las = 2)
barplot(squadreV$'Diff. reti', las = 2, col = "blue", add = TRUE)
abline(h = mediaDRV, col = "green")
legend("topright", legend = c("G. subiti", "Diff. Reti", "Media D. Reti"), fill = c("red", "blue", "green"), xpd = TRUE, cex = 0.55, y.intersp = 0.8, horiz = TRUE)

# Analisi dati
cat("\n")
cat("Media Diff. reti Vincitrici:", mediaDRV, "\n")
cat("Mediana Diff. reti Vincitrici:", medianaDRV, "\n")
cat("Massima Diff. reti Vincitrici:", massimoDRV, "\n")
cat("Minima Diff. reti Vincitrici:", minimoDRV, "\n")
cat("Varianza Diff. reti Vincitrici:", varianzaDRV, "\n")
cat("Conteggio Diff. reti Vincitrici:", conteggioDRV, "\n")

tabStatsDRV <- data.frame(Statistica = c("Media", "Mediana", "Massima", "Minima", "Varianza", "Conteggio"),
                          Valore = round(c(mediaDRV, medianaDRV, massimoDRV, minimoDRV, varianzaDRV, conteggioDRV),2))
View(tabStatsDRV)

# -------------------------------------------------------------

# Filtra le squadre retrocesse
squadreR <- statSerieA[statSerieA$Posizionamento == "R", ]

# Calcolo statistiche delle Retrocesse
mediaGSR <- mean(squadreR$`G. subiti`)
medianaGSR <- median(squadreR$'G. subiti')
massimoGSR <- max(squadreR$'G. subiti')
minimoGSR <- min(squadreR$'G. subiti')
varianzaGSR <- var(squadreR$'G. subiti')
conteggioGSR <- length(squadreR$'G. subiti')
modaGSR <- as.numeric(names(which.max(table(squadreR$'G. subiti'))))
rangeGSR <- max(squadreR$'G. subiti') - min(squadreR$'G. subiti')
sommaGSR <- sum(squadreR$'G. subiti')

cat("Media Goal Subiti dalle Retrocesse:", mediaGSR, "\n")
cat("Mediana Goal Subiti dalle Retrocesse:", medianaGSR, "\n")
cat("Massimo Goal Subiti dalle Retrocesse:", massimoGSR, "\n")
cat("Minimo Goal Subiti dalle Retrocesse:", minimoGSR, "\n")
cat("Varianza Goal Subiti dalle Retrocesse:", varianzaGSR, "\n")
cat("Conteggio campioni Goal Subiti dalle Retrocesse:", conteggioGSR, "\n")
cat("Moda Goal Subiti dalle Retrocesse:", modaGSR, "\n")
cat("Range Goal Subiti dalle Retrocesse:", rangeGSR, "\n")
cat("Somma Goal Subiti dalle Retrocesse:", sommaGSR, "\n")

tabStatsGSR <- data.frame(Statistica = c("Media", "Mediana", "Massimo", "Minimo", "Varianza", "Conteggio", "Moda", "Range", "Somma"),
                          Valore = round(c(mediaGSR, medianaGSR, massimoGSR, minimoGSR, varianzaGSR, conteggioGSR, modaGSR, rangeGSR, sommaGSR),2))
View(tabStatsGSR)

# Crea un diagramma a colonne
barplot(squadreR$'G. subiti', main = "Andamento Goal Subiti Retrocesse", names.arg=squadreR$'Stagione', las = 2, cex.names = 0.9, ylab = "G. subiti", col = "red", ylim = c(0, 100))
axis(2, at = seq(0, 100, by = 10), las = 2)
abline(h = mediaGSR, col = "blue")
legend("topright", legend = c("G. subiti", "Media "), fill = c("red", "blue"), xpd = TRUE, cex = 0.7, y.intersp = 0.8)

# Import package
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Calcolo statistiche stagionali di goal subiti da Retrocesse

GSxStagione <- squadreR %>%
  group_by(Stagione) %>%
  summarise(SommaGS = sum(`G. subiti`))

mediaGSxStagione <- mean(GSxStagione$SommaGS)
varianzaGSxStagione <- var(GSxStagione$SommaGS)
cat("Media Goal Subiti dalle Retrocesse per stagione: ", mediaGSxStagione, "\n")
cat("Varianza Goal Subiti dalle Retrocesse per stagione: ", varianzaGSxStagione, "\n")

# Diagramma a torta con goal subiti dalle Retrocesse per ciascuna stagione
graficoTorta <- ggplot(GSxStagione, aes(x ="", y = SommaGS, fill = Stagione)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Somma di G. Subiti dalle Retrocesse per Stagione")

graficoTorta + geom_text(aes(label = scales::percent(SommaGS/sum(SommaGS), accuracy = 0.01)), position = position_stack(vjust = 0.5))

# Grafico a linee per andamento goal subiti dalle Retrocesse per Stagione
graficoLinee <- ggplot(GSxStagione, aes(x = Stagione, y = SommaGS)) +
  geom_point(color = "red") +
  geom_line(group = 'Stagione') +
  labs(title = "Somma di G. Subiti dalle Retrocesse per Stagione",
       x = "Stagione",
       y = "Somma di G. Subiti") +
  scale_y_continuous(limits = c(0, 300))

print(graficoLinee)

