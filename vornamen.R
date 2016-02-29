#####################################
# 2015 in Berlin vergebene Vornamen #
#####################################

# Packages installieren
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr", dep=T)}
#if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table", dep=T)}
if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio", dep=T)}

# Packages laden
library(dplyr)
#library(data.table)
library(rio)

# Daten einlesen

## Option 1: Daten manuell von http://daten.berlin.de/datensaetze/liste-der-h%C3%A4ufigen-vornamen-2015 
  ## herunterladen und einlesen

#setwd("D:/Berlin-Data/Vornamen")
#c.w <-  read.csv2("charlottenburg-wilmersdorf.csv", encoding="UTF-8")
#f.k <- read.csv2("friedrichshain-kreuzberg.csv", encoding="UTF-8")
#lichtenberg <- read.csv2("lichtenberg.csv", encoding="UTF-8")
#m.h <- read.csv2("marzahn-hellersdorf.csv", encoding="UTF-8")
#mitte <- read.csv2("mitte.csv", encoding="UTF-8")
#neukoelln <- read.csv2("neukoelln.csv", encoding="UTF-8")
#pankow <- read.csv2("pankow.csv", encoding="UTF-8")
#reinickendorf <- read.csv2("reinickendorf.csv", encoding="UTF-8")
#spandau <- read.csv2("spandau.csv", encoding="UTF-8")
##standesamt1 <- read.csv2("standesamt-i.csv", encoding="UTF-8")
#s.z <- read.csv2("steglitz-zehlendorf.csv", encoding="UTF-8")
#t.s <- read.csv2("tempelhof-schoeneberg.csv", encoding="UTF-8")
#t.k <- read.csv2("treptow-koepenick.csv", encoding="UTF-8")

## Option 2: Daten direkt von daten.berlin.de einlesen
c.w <-  read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/charlottenburg-wilmersdorf.csv", encoding="UTF-8")
f.k <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/friedrichshain-kreuzberg.csv", encoding="UTF-8")
lichtenberg <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/lichtenberg.csv", encoding="UTF-8")
m.h <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/marzahn-hellersdorf.csv", encoding="UTF-8")
mitte <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/mitte.csv", encoding="UTF-8")
neukoelln <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/neukoelln.csv", encoding="UTF-8")
pankow <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/pankow.csv", encoding="UTF-8")
reinickendorf <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/reinickendorf.csv", encoding="UTF-8")
spandau <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/spandau.csv", encoding="UTF-8")
#standesamt1 <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/standesamt-i.csv", encoding="UTF-8")
s.z <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/steglitz-zehlendorf.csv", encoding="UTF-8")
t.s <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/tempelhof-schoeneberg.csv", encoding="UTF-8")
t.k <- read.csv2("http://www.berlin.de/daten/liste-der-vornamen-2015/treptow-koepenick.csv", encoding="UTF-8")


# Bezirks-Variable einfügen
c.w <- c.w %>% mutate(bezirk = 1)
f.k <- f.k %>% mutate(bezirk = 2)
lichtenberg <- lichtenberg %>% mutate(bezirk = 3)
m.h <- m.h %>% mutate(bezirk = 4)
mitte <- mitte %>% mutate(bezirk = 5)
neukoelln <- neukoelln %>% mutate(bezirk = 6)
pankow <- pankow %>% mutate(bezirk = 7)
reinickendorf <- reinickendorf %>% mutate(bezirk = 8)
spandau <- spandau %>% mutate(bezirk = 9)
s.z <- s.z %>% mutate(bezirk = 10)
t.s <- t.s %>% mutate(bezirk = 11)
t.k <- t.k %>% mutate(bezirk = 12)

# Gesamttabelle erstellen
total <- rbind(c.w, f.k, lichtenberg, m.h, mitte, neukoelln, pankow, reinickendorf, spandau, s.z, t.s, t.k) 


# Merge erstellen
# (wie oft wurden die einzelnen Vornamen in den Bezirken und insgesamt vergeben?)

## Merge-Funktionen erstellen
### Erster Merge
merge.initial <- function(arg1, arg2){
  merge <- merge(arg1, arg2, by=c("vorname","geschlecht"), all=T) # Datensatz 1+2 mergen 
  anzahl <- rowSums(cbind(merge$anzahl.x, merge$anzahl.y), na.rm=TRUE) # Gesamtanzahl ermitteln
  merge <- cbind(merge, anzahl) # Gesamtzahl an Datensatz anfügen
  merge <- select(merge, -bezirk.x) # Bezirk löschen
  merge <- select(merge, -bezirk.y) # Bezirk löschen
  return(merge) # gemergten Datensatz ausgeben
}

### Alle weiteren Merges
merge.following <- function(arg1, arg2){
  merge <- merge(arg1, arg2, by=c("vorname","geschlecht"), all=T) # Datensatz 1 + 2 mergen
  anzahl <- rowSums(cbind(merge$anzahl.x, merge$anzahl.y), na.rm=TRUE) # Gesamtanzahl ermitteln
  merge <- cbind(merge, anzahl) # Gesamtzahl an Datensatz anfügen
  merge <- select(merge, -bezirk) # Bezirk löschen
  merge <- select(merge, -anzahl.x) # Anzahl aus Datensatz 1 löschen
}

# Mergen (immer einen weiteren Bezirk hinzufügen)
merge <- merge.initial(c.w, f.k) # Die ersten beiden Datensätze mergen
merge <- merge %>% rename(anzahl.c.w = anzahl.x) # Anzahl umbennen
merge <- merge %>% rename(anzahl.f.k = anzahl.y) # Anzahl umbennen
merge <- merge.following(merge, lichtenberg) # Weiteren Datensatz anfügen
merge <- merge %>% rename(anzahl.lichtenberg = anzahl.y) # Anzahl umbennen
merge <- merge.following(merge, m.h)
merge <- merge %>% rename(anzahl.m.h = anzahl.y)
merge <- merge.following(merge, mitte)
merge <- merge %>% rename(anzahl.mitte = anzahl.y)
merge <- merge.following(merge, neukoelln)
merge <- merge %>% rename(anzahl.neukoellen = anzahl.y)
merge <- merge.following(merge, pankow)
merge <- merge %>% rename(anzahl.pankow = anzahl.y)
merge <- merge.following(merge, reinickendorf)
merge <- merge %>% rename(anzahl.reinickendorf = anzahl.y)
merge <- merge.following(merge, s.z)
merge <- merge %>% rename(anzahl.s.z = anzahl.y)
merge <- merge.following(merge, spandau)
merge <- merge %>% rename(anzahl.spandau = anzahl.y)
merge <- merge.following(merge, t.k)
merge <- merge %>% rename(anzahl.t.k = anzahl.y)
merge <- merge.following(merge, t.s)
merge <- merge %>% rename(anzahl.t.s = anzahl.y)


# Datensatz sortieren
merge <- merge[order(-merge$anzahl),] 


# Datensatz nach Geschlechtern trennen
merge.m <- merge %>% filter(grepl("m",geschlecht))
merge.w <- merge %>% filter(grepl("w",geschlecht))

# Datensatz mit einmaligen Vornamen erstellen
merge.unique <- merge %>% filter(merge$anzahl == 1)


# Daten exportiern
export(merge, "merge.csv")
export(merge.unique, "merge_unique.csv")
