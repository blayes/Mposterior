cmdArgs <- commandArgs(trailingOnly = TRUE)

mtd <- as.numeric(cmdArgs[1])
id <- as.numeric(cmdArgs[2])

library(Mposterior)

setwd('/Users/ssrivastva/gss/code/')

if (mtd == 1) {
    cvs <- rep(1:10, each = 9)
    cats <- rep(1:9, times = 10)
    cvdata <- readRDS("../data/marg_sub10.rds")
    train <- cvdata[[cvs[id]]][[cats[id]]]
    res <- vector("list", 2)
    startTime <- proc.time()
    for (cc in 1:2) {
        res[[cc]] <- findWeiszfeldMedian(lapply(train, function(x) as.matrix(x[sample(1:1000, 200), cc])))
    }
    endTime <- proc.time()
    fname <- paste0("/Shared/ssrivastva/gss/result/mpost/sub10/marg_cat_", cats[id], "_cv_", cvs[id], ".rds")
    saveRDS(list(res = res, time = endTime - startTime), fname)
} else if (mtd == 2) {
    cvs <- rep(1:10, each = 9)
    cats <- rep(1:9, times = 10)
    cvdata <- readRDS("../data/marg_sub20.rds")
    train <- cvdata[[cvs[id]]][[cats[id]]]
    res <- vector("list", 2)
    startTime <- proc.time()
    for (cc in 1:2) {
        res[[cc]] <- findWeiszfeldMedian(lapply(train, function(x) as.matrix(x[sample(1:1000, 200), cc])))
    }
    endTime <- proc.time()
    fname <- paste0("/Shared/ssrivastva/gss/result/mpost/sub20/marg_cat_", cats[id], "_cv_", cvs[id], ".rds")
    saveRDS(list(res = res, time = endTime - startTime), fname)
} else if (mtd == 3) {
    cvs <- rep(1:10, each = 36)
    cats <- rep(1:36, times = 10)
    cvdata <- readRDS("../data/joint_sub10.rds")
    train <- cvdata[[cvs[id]]][[cats[id]]]
    res <- vector("list", 4)
    startTime <- proc.time()
    for (cc in 1:4) {
        res[[cc]] <- findWeiszfeldMedian(lapply(train, function(x) as.matrix(x[sample(1:1000, 200), cc])))
    }
    endTime <- proc.time()
    fname <- paste0("/Shared/ssrivastva/gss/result/mpost/sub10/joint_cat_", cats[id], "_cv_", cvs[id], ".rds")
    saveRDS(list(res = res, time = endTime - startTime), fname)
} else {
    cvs <- rep(1:10, each = 36)
    cats <- rep(1:36, times = 10)
    cvdata <- readRDS("../data/joint_sub20.rds")
    train <- cvdata[[cvs[id]]][[cats[id]]]
    res <- vector("list", 4)
    startTime <- proc.time()
    for (cc in 1:4) {
        res[[cc]] <- findWeiszfeldMedian(lapply(train, function(x) as.matrix(x[sample(1:1000, 200), cc])))
    }
    endTime <- proc.time()
    fname <- paste0("/Shared/ssrivastva/gss/result/mpost/sub20/joint_cat_", cats[id], "_cv_", cvs[id], ".rds")
    saveRDS(list(res = res, time = endTime - startTime), fname)
}
