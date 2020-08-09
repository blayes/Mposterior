setwd("~/gss/code/")

## m = 10
rm(list = ls())

# marginals
catMargs <- vector("list", 10)
for (dd in 1:10) {
  catMargs[[dd]] <- vector("list", 9)
  for (cc in 1:9) {
    catMargs[[dd]][[cc]] <- vector("list", 10)
    for (mm in 1:10) {
      mdf <- read.table(paste0('/Shared/ssrivastva/gss/data/sub10/', dd, '/marg/sub_', mm, '_marg_', cc, '.csv'), sep = ',')
      colnames(mdf) <- c("cat1", "cat2")
      catMargs[[dd]][[cc]][[mm]] <- mdf
    }
  }
}

saveRDS(catMargs, "../data/marg_sub10.rds")

rm(list = ls())

# joints
catJoints <- vector("list", 10)
for (dd in 1:10) {
  catJoints[[dd]] <- vector("list", 36)
  for (cc in 1:36) {
    catJoints[[dd]][[cc]] <- vector("list", 10)
    for (mm in 1:10) {
      mdf <- read.table(paste0('/Shared/ssrivastva/gss/data/sub10/', dd, '/joint/sub_', mm, '_joint_', cc, '.csv'), sep = ',')
      colnames(mdf) <- c("cat11", "cat12", "cat21", "cat22")
      catJoints[[dd]][[cc]][[mm]] <- mdf
    }
  }
}

saveRDS(catJoints, "../data/joint_sub10.rds")


## m = 20
rm(list = ls())

# marginals
catMargs <- vector("list", 10)
for (dd in 1:10) {
  catMargs[[dd]] <- vector("list", 9)
  for (cc in 1:9) {
    catMargs[[dd]][[cc]] <- vector("list", 20)
    for (mm in 1:20) {
      mdf <- read.table(paste0('/Shared/ssrivastva/gss/data/sub20/', dd, '/marg/sub_', mm, '_marg_', cc, '.csv'), sep = ',')
      colnames(mdf) <- c("cat1", "cat2")
      catMargs[[dd]][[cc]][[mm]] <- mdf
    }
  }
}

saveRDS(catMargs, "../data/marg_sub20.rds")

rm(list = ls())

# joints
catJoints <- vector("list", 10)
for (dd in 1:10) {
  catJoints[[dd]] <- vector("list", 36)
  for (cc in 1:36) {
    catJoints[[dd]][[cc]] <- vector("list", 20)
    for (mm in 1:20) {
      mdf <- read.table(paste0('/Shared/ssrivastva/gss/data/sub20/', dd, '/joint/sub_', mm, '_joint_', cc, '.csv'), sep = ',')
      colnames(mdf) <- c("cat11", "cat12", "cat21", "cat22")
      catJoints[[dd]][[cc]][[mm]] <- mdf
    }
  }
}

saveRDS(catJoints, "../data/joint_sub20.rds")

## overall posterior

rm(list = ls())

# marginals
catMargs <- vector("list", 10)
for (dd in 1:10) {
  catMargs[[dd]] <- vector("list", 9)
  for (cc in 1:9) {
      mdf <- read.table(paste0('/Shared/ssrivastva/gss/result/full/marg/', dd, '/marg_', cc, '.csv'), sep = ',')
      colnames(mdf) <- c("cat1", "cat2")
      catMargs[[dd]][[cc]] <- mdf
  }
}

saveRDS(catMargs, "../data/marg_full.rds")

rm(list = ls())

# joints
catJoints <- vector("list", 10)
for (dd in 1:10) {
  catJoints[[dd]] <- vector("list", 36)
  for (cc in 1:36) {
    mdf <- read.table(paste0('/Shared/ssrivastva/gss/result/full/joint/', dd, '/joint_', cc, '.csv'), sep = ',')
    colnames(mdf) <- c("cat11", "cat12", "cat21", "cat22")
    catJoints[[dd]][[cc]] <- mdf
  }
}

saveRDS(catJoints, "../data/joint_full.rds")
