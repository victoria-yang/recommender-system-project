# title: "Recommeder System"
# author: "Chieh-An Yang"

library(lsa)
library(tidyr)
library(corrplot)

## Data
D1<-read.csv("difficulty.csv")
I1<-read.csv("interest.csv")

## Collaborative filter

##  Matrix operations
D1<-D1[!duplicated(D1$name),]
I1<-I1[!duplicated(I1$name),]

row.names(D1) <- D1$name
row.names(I1) <- I1$name

as.matrix(D1)
as.matrix(I1)

D2<-D1[,-1]
I2<-I1[,-1]

D2[is.na(D2)]<-0
I2[is.na(I2)]<-0

## Student's Interest
I2 <- t(I2)
I.SIM <- cosine(I2)
diag(I.SIM)


## Find out which students are most similar to you
my.name <- "Chieh-An Yang" # input your name
head(rownames(I.SIM[order(I.SIM[my.name,], decreasing = TRUE),]), n = 2)

## Unit based recommendation
D2<-as.matrix(D2)
D.SIM <- cosine(D2)
diag(D.SIM) <- NA

head(rownames(D.SIM[order(D.SIM["pred.dif",], decreasing = TRUE),]), n = 2)

# composite measure from interest and difficulty, then construct a similarity matrix using this measure

C1<-data.frame(merge(D2,I2))

COR <- cor(C1)

tiff("COR.tiff",units="in",width=5, height=5, res=300)
corrplot(as.matrix(C1), method = "circle")
dev.off()

I3 <- gather(I1,unit,interest, 2:7)
D3 <- gather(D1, name, difficulty)

C1 <- data.frame(I3$name, I3$unit, I3$interest, D3$difficulty)
names(C1) <- c("name", "unit", "interest", "difficulty")
C1 <- filter(C1, difficulty > 0)
C2 <- select(C1, "interest", "difficulty")

#Run PCA
pc <- prcomp(C2)
#Extract PC1 loadings as new measure and attach to stid & unit
C3 <- data.frame(C1$name, C1$unit, pc$x)
C4 <- select(C3, C1.name, C1.unit, PC1)
#Remove int from unit label
C4$C1.unit <- gsub(".int", "", C4$C1.unit)

#Recreate unit by student matrix
C5 <- spread(C4, C1.name, PC1)
row.names(C5) <- C5$C1.unit
C5$C1.unit <- NULL
C5 <- as.matrix(C5)
C5 <- ifelse(is.na(C5), 0, C5)
C5 <- t(C5)

#Generate cosine similarity matrix for units
C.SIM <- cosine(C5)
diag(C.SIM) <- NA

#Search for most similar unit to "neural"
head(rownames(C.SIM[order(C.SIM["neural",], decreasing = TRUE),]), n = 1)