rm(list = ls())
setwd("~/gss/code")

library(foreign)

fnames <- dir("../data/sav")
ffidx <- grep("sav", dir("../data/sav"))

dats <- list()
for (ii in seq_along(ffidx)) {
    dats[[ii]] <- read.spss(paste0("../data/sav/", fnames[ffidx[ii]]))
}

vars <- c("abany", "cappun", "grass", "homosex", "bible", "prayer", "premarsx", "happy", "uswary")

contVars <- rep(0, length(dats))

for (ii in seq_along(dats)) {
    contVars[ii] <- sum(names(dats[[ii]]) %in% vars)
}

selYrs <- which(contVars == 9)
selData <- do.call(rbind.data.frame, lapply(dats[selYrs], function(x) do.call(cbind.data.frame, x[which(names(x) %in% vars )])))

for (ii in 1:ncol(selData)) {
    selData[ , ii] <- as.character(selData[ , ii])
}

fullData <- selData[-which(rowSums(selData == "NA") > 0 | rowSums(selData == "DK") > 0), ]

compData <- fullData

set.seed(12345)

for (ii in 1:ncol(compData)) {
  cc <- compData[ , ii]
  iapIdx <- cc == "IAP"
  pp <- table(cc[!iapIdx]) / sum(table(cc[!iapIdx]))
  cc[iapIdx] <- sample(unique(cc[!iapIdx]), sum(iapIdx), replace = TRUE, prob = pp)
  compData[ , ii] <- cc
}

idxHappy <- compData$happy == "PRETTY HAPPY" | compData$happy == "VERY HAPPY"
compData$happy[idxHappy] <- "yes"
compData$happy[!idxHappy] <- "no"

idxBible <- compData$bible == "INSPIRED WORD" | compData$bible == "WORD OF GOD"
compData$bible[idxBible] <- "yes"
compData$bible[!idxBible] <- "no"

idxPre <- compData$premarsx == "ALMST ALWAYS WRG" | compData$premarsx == "ALWAYS WRONG"
compData$premarsx[idxPre] <- "wrong"
compData$premarsx[!idxPre] <- "right"

idxHomo <- compData$homosex == "ALMST ALWAYS WRG" | compData$homosex == "ALWAYS WRONG"
compData$homosex[idxHomo] <- "wrong"
compData$homosex[!idxHomo] <- "right"

compData$happy <- ordered(compData$happy, levels = c("yes", "no"))
compData$bible <- ordered(compData$bible, levels = c("yes", "no"))
compData$cappun <- ordered(compData$cappun, levels = c("FAVOR", "OPPOSE"))
compData$grass <- ordered(compData$grass, levels = c("LEGAL", "NOT LEGAL"))
compData$premarsx <- ordered(compData$premarsx, levels = c("right", "wrong"))
compData$prayer <- ordered(compData$prayer, levels = c("APPROVE", "DISAPPROVE"))
compData$uswary <- ordered(compData$uswary, levels = c("YES", "NO"))
compData$homosex <- ordered(compData$homosex, levels = c("right", "wrong"))
compData$abany <- ordered(compData$abany, levels = c("YES", "NO"))

saveRDS(compData, "../data/compData.rds")
saveRDS(fullData, "../data/fullData.rds")

matCompData <- sapply(compData, as.numeric)
write.table(matCompData, "../data/mat_comp_data.csv", row.names = FALSE, col.names = FALSE, sep = ",")

ncv <- 10

cvidx <- sample(1:ncv, nrow(matCompData), replace = TRUE)

test <- vector("list", ncv)
train <- vector("list", ncv)
for (ii in 1:ncv) {
  cvpart <- (1:nrow(matCompData))[cvidx == ii]
  test[[ii]] <- matCompData[cvpart, ]
  train[[ii]] <- matCompData[-cvpart, ]
}

saveRDS(train, "../data/train_gss.rds")
saveRDS(test, "../data/test_gss.rds")

for (ii in 1:ncv) {
  write.table(train[[ii]], paste0("../data/csv_train/full/train_", ii, ".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(test[[ii]], paste0("../data/csv_test/full/test_", ii, ".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
}

#### partition the data into k = 20 subsets

rm(list = ls())

train <- readRDS("../data/train_gss.rds")

nparts <- 20
ncv <- 10

set.seed(12345)

partTrain <- vector("list", ncv)
for (ii in 1:ncv) {
  partTrain[[ii]] <- vector("list", nparts)
}

for (ii in 1:ncv) {
  pidx <- sample(1:nparts, nrow(train[[ii]]), replace = TRUE)
  for (jj in 1:nparts) {
    ppart <- (1:nrow(train[[ii]]))[pidx == jj]
    partTrain[[ii]][[jj]] <- train[[ii]][ppart, ]
    write.table(partTrain[[ii]][[jj]], paste0("../data/csv_train/sub20/", ii, "/train_", jj, ".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
  }
}

saveRDS(partTrain, "../data/part20_train_gss.rds")

#### partition the data into k = 10 subsets

rm(list = ls())

train <- readRDS("../data/train_gss.rds")

nparts <- 10
ncv <- 10

set.seed(12345)

partTrain <- vector("list", ncv)
for (ii in 1:ncv) {
  partTrain[[ii]] <- vector("list", nparts)
}

for (ii in 1:ncv) {
  pidx <- sample(1:nparts, nrow(train[[ii]]), replace = TRUE)
  for (jj in 1:nparts) {
    ppart <- (1:nrow(train[[ii]]))[pidx == jj]
    partTrain[[ii]][[jj]] <- train[[ii]][ppart, ]
    write.table(partTrain[[ii]][[jj]], paste0("../data/csv_train/sub10/", ii, "/train_", jj, ".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
  }
}

saveRDS(partTrain, "../data/part10_train_gss.rds")
