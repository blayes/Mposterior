rm(list = ls())

setwd('/Users/ssrivastva/gss/code/')

fdata <- readRDS("../data/compData.rds")
catNames <- colnames(fdata)
catLevels <- toupper(sapply(fdata, levels))

margOpost <- readRDS("../data/marg_full.rds")
for (dd in 1:10) {
  names(margOpost[[dd]]) <- catNames
  for (cc in 1:9) {
    colnames(margOpost[[dd]][[cc]]) <- catLevels[ , cc]
  }
}

margSpost10 <- vector("list", 10)
margSpost20 <- vector("list", 10)
mtime10 <- vector("list", 10)
mtime20 <- vector("list", 10)
for (dd in 1:10) {
    margSpost10[[dd]] <- vector("list", 9)
    names(margSpost10[[dd]]) <- catNames
    margSpost20[[dd]] <- vector("list", 9)
    names(margSpost20[[dd]]) <- catNames
    mtime10[[dd]] <- vector("list", 9)
    names(mtime10[[dd]]) <- catNames
    mtime20[[dd]] <- vector("list", 9)
    names(mtime20[[dd]]) <- catNames
    for (cc in 1:9) {
        res10 <- readRDS(paste0("/Shared/ssrivastva/gss/result/mpost/sub10/marg_cat_", cc, "_cv_", dd, ".rds"))
        mpost10 <- res10$res
        margSpost10[[dd]][[cc]] <- matrix(0.0, 5000, 2)
        colnames(margSpost10[[dd]][[cc]]) <- colnames(margOpost[[dd]][[cc]])
        for (ll in 1:2) {
            pp <- as.numeric(mpost10[[ll]]$weiszfeldWts)
            pp[pp < 1/20] <- 0
            aa <- as.numeric(mpost10[[ll]]$medianAtoms)
            margSpost10[[dd]][[cc]][ , ll] <- sample(aa, 5000, prob = pp[rep(1:10, each = 200)], replace = TRUE)
        }
        mtime10[[dd]][[cc]] <- res10$time[3]
        res20 <- readRDS(paste0("/Shared/ssrivastva/gss/result/mpost/sub20/marg_cat_", cc, "_cv_", dd, ".rds"))
        mpost20 <- res20$res
        margSpost20[[dd]][[cc]] <- matrix(0.0, 5000, 2)
        colnames(margSpost20[[dd]][[cc]]) <- colnames(margOpost[[dd]][[cc]])
        for (ll in 1:2) {
            pp <- as.numeric(mpost20[[ll]]$weiszfeldWts)
            pp[pp < 1/40] <- 0
            aa <- as.numeric(mpost20[[ll]]$medianAtoms)
            margSpost20[[dd]][[cc]][ , ll] <- sample(aa, 5000, prob = pp[rep(1:20, each = 200)], replace = TRUE)
        }
        mtime20[[dd]][[cc]] <- res20$time[3]
    }
}

jointOpost <- readRDS("../data/joint_full.rds")

jtCatNames <- character(36)
jtCatLevels <- matrix(NA_character_, 36, 4)
cnt <- 0
for (ii in 1:8) {
  for (jj in (ii + 1):9) {
    cnt <- cnt + 1
    jtCatNames[cnt] <- paste(catNames[ii], catNames[jj], sep = "-")
    jtCatLevels[cnt, ] <- paste(rep(catLevels[, ii], each = 2),
                              rep(catLevels[ , jj], times = 2),
                              sep = "-")
  }
}

for (dd in 1:10) {
  names(jointOpost[[dd]]) <- jtCatNames
  for (cc in 1:36) {
    colnames(jointOpost[[dd]][[cc]]) <- jtCatLevels[cc, ]
  }
}

jointSpost10 <- vector("list", 10)
jointSpost20 <- vector("list", 10)
jtime10 <- vector("list", 10)
jtime20 <- vector("list", 10)
for (dd in 1:10) {
  jointSpost10[[dd]] <- vector("list", 36)
  names(jointSpost10[[dd]]) <- jtCatNames
  jointSpost20[[dd]] <- vector("list", 36)
  names(jointSpost20[[dd]]) <- jtCatNames
  jtime10[[dd]] <- vector("list", 36)
  names(jtime10[[dd]]) <- jtCatNames
  jtime20[[dd]] <- vector("list", 36)
  names(jtime20[[dd]]) <- jtCatNames
  for (cc in 1:36) {
    res10 <- readRDS(paste0("/Shared/ssrivastva/gss/result/mpost/sub10/joint_cat_", cc, "_cv_", dd, ".rds"))
    mpost10 <- res10$res
    jointSpost10[[dd]][[cc]] <- matrix(0.0, 1000, 4)
    colnames(jointSpost10[[dd]][[cc]]) <- jtCatLevels[cc, ]
    for (ll in 1:4) {
        pp <- as.numeric(mpost10[[ll]]$weiszfeldWts)
        pp[pp < 1/20] <- 0
        aa <- as.numeric(mpost10[[ll]]$medianAtoms)
        jointSpost10[[dd]][[cc]][ , ll] <- sample(aa, 1000, prob = pp[rep(1:10, each = 200)], replace = TRUE)
    }
    jtime10[[dd]][[cc]] <- res10$time[3]
    res20 <- readRDS(paste0("/Shared/ssrivastva/gss/result/mpost/sub20/joint_cat_", cc, "_cv_", dd, ".rds"))
    mpost20 <- res20$res
    jointSpost20[[dd]][[cc]] <- matrix(0.0, 1000, 4)
    colnames(jointSpost20[[dd]][[cc]]) <- jtCatLevels[cc, ]
    for (ll in 1:4) {
        pp <- as.numeric(mpost20[[ll]]$weiszfeldWts)
        pp[pp < 1 / 40] <- 0
        aa <- as.numeric(mpost20[[ll]]$medianAtoms)
        jointSpost20[[dd]][[cc]][ , ll] <- sample(aa, 1000, prob = pp[rep(1:20, each = 200)], replace = TRUE)
    }
    jtime20[[dd]][[cc]] <- res20$time[3]
  }
}

jtime <- matrix(0.0, 10, 3)
jtime[, 2] <- sapply(lapply(jtime10, function(x) sapply(x, '[[', 1)), mean)
jtime[, 3] <- sapply(lapply(jtime20, function(x) sapply(x, '[[', 1)), mean)

for (ii in 1:10) {
  tbl <- unlist(read.table(paste0("/Shared/ssrivastva/gss/result/full/time_", ii, ".csv")))
  jtime[ii , 1] <- tbl
  t10 <- rep(0, 10)
  for (ss in 1:10) {
    t10[ss] <- unlist(read.table(paste0("/Shared/ssrivastva/gss/data/sub10/", ii, "/time_", ss, ".csv")))
  }
  jtime[ii , 2] <- mean(t10) + jtime[ii, 2]
  t20 <- rep(0, 20)
  for (ss in 1:20) {
    t20[ss] <- unlist(read.table(paste0("/Shared/ssrivastva/gss/data/sub20/", ii, "/time_", ss, ".csv")))
  }
  jtime[ii , 3] <- mean(t20) + jtime[ii, 3]
}

runTime <- log10(jtime)

pdf("~/gss/result/gss_time.pdf", 7, 8)
par(cex = 1)
par(mar = c(0, 0, 0, 0), oma = c(3, 5.5, 0.2, 0.2))
par(tcl = -0.02)
par(mgp = c(2, 0.6, 0))
boxplot(runTime, ylab = NA, axes = FALSE, lwd = 2, ylim = c(3.5, 5))
grid(lwd=4)
box(col = "grey40", lwd = 4)
axis(side = 1, tck = -.01, labels = NA)
mtext(c("Overall", "M (m = 10)", "M (m = 20)"), at = 1:3, side = 1, line = 1, cex = 2.2)
axis(side = 2, tck = -0.01, labels = NA, lwd = 3)
mtext(format(seq(3, 5, by = 0.5)), at = seq(3, 5, by = 0.5), side = 2, line = 0.3, las = 1, cex = 2.5)
mtext(expression(log[10] * " Seconds"), side = 2, outer = TRUE, cex = 2.5, line = 3)
dev.off()


catJoints10 <- readRDS("~/gss/data/joint_sub10.rds")
catJoints20 <- readRDS("~/gss/data/joint_sub20.rds")

## cv 1, happy-bible, sub 10/1, dim 1
catJoints10[[1]][[9]][[1]][ , 1]
## cv 1, happy-bible, sub 20/1, dim 1
catJoints20[[1]][[9]][[1]][ , 1]


library(RColorBrewer)
cols <- brewer.pal(6, "Set1")

cats <- 35
cnames <- c("uswar-yes, abort-yes", "uswar-yes, abort-no", "uswar-no, abort-yes", "uswar-no, abort-no")

pdf(paste0("~/gss/result/us_abort_plot.pdf"), 20, 8)
par(mfrow = c(1, 4))
par(cex = 1)
par(mar = c(3, 3, 0, 0), oma = c(1.5, 1.5, 0.4, 0.4))
par(tcl = -0.02)
par(mgp = c(2, 0.6, 0))
for (comp in 1:4) {
  plot(bkde(jointOpost[[1]][[cats]][ , comp]), lwd = 5, lty = "solid", col = cols[1], xlim = c(range(jointOpost[[1]][[cats]][ , comp])[1] - 0.05, range(jointOpost[[1]][[cats]][ , comp])[2] + 0.05), type = "l", ylab = NA, xlab = NA, axes = FALSE)
  mtext(cnames[comp], side = 3, line = -3.5, adj = 0.01, cex = 1.2)
  grid(lwd=3)
  xxlab <- axTicks(1)
  yylab <- axTicks(2)
  axis(side = 2, tck = -0.01, lwd = 3, cex = 2, labels = NA)
  mtext(format(yylab), at = yylab, side = 2, line = 0.5, cex = 1.5, las = 2)
  axis(side = 1, tck = -0.01, lwd = 3, cex = 2, labels = NA)
  mtext(format(round(xxlab, 2)), at = round(xxlab, 2), side = 1, line = 1.2, cex = 1.5)
  box(col = "grey40", lwd = 5)
  for (ii in 1:10) {
    lines(bkde(catJoints10[[1]][[cats]][[ii]][ , comp]), lwd = 2, lty = "dashed", col = cols[2])
  }
  lines(bkde(jointSpost10[[1]][[cats]][ , comp]), lwd = 5, lty = "solid", col = cols[2])
  for (ii in 1:20) {
    lines(bkde(catJoints20[[1]][[cats]][[ii]][ , comp]), lwd = 2, lty = "dashed", col = cols[4])
  }
  lines(bkde(jointSpost20[[1]][[cats]][ , comp]), lwd = 5, lty = "solid", col = cols[4])
  if (comp == 1) {
    mtext("Density", side = 2, outer = TRUE, cex = 2, line = -1)
    legend("topright", c("Overall Posterior", "M-Posterior (m=10)", "M-Posterior (m=20)"), col = cols[c(1, 2, 4)],
             lty = "solid", lwd = 3, bty = "n", cex = 1.2)
  }
}
dev.off()


cats <- 16
cnames <- c("cap-yes, grass-yes", "cap-yes, grass-no", "cap-no, grass-yes", "cap-no, grass-no")

pdf(paste0("~/gss/result/cap_grass_plot.pdf"), 20, 8)
par(mfrow = c(1, 4))
par(cex = 1)
par(mar = c(3, 3, 0, 0), oma = c(1.5, 1.5, 0.4, 0.4))
par(tcl = -0.02)
par(mgp = c(2, 0.6, 0))
for (comp in 1:4) {
  plot(bkde(jointOpost[[1]][[cats]][ , comp]), lwd = 5, lty = "solid", col = cols[1], xlim = c(range(jointOpost[[1]][[cats]][ , comp])[1] - 0.05, range(jointOpost[[1]][[cats]][ , comp])[2] + 0.05), type = "l", ylab = NA, xlab = NA, axes = FALSE)
  mtext(cnames[comp], side = 3, line = -3.5, adj = 0.01, cex = 1.2)
  grid(lwd=3)
  xxlab <- axTicks(1)
  yylab <- axTicks(2)
  axis(side = 2, tck = -0.01, lwd = 3, cex = 2, labels = NA)
  mtext(format(yylab), at = yylab, side = 2, line = 0.5, cex = 1.5, las = 2)
  axis(side = 1, tck = -0.01, lwd = 3, cex = 2, labels = NA)
  mtext(format(round(xxlab, 2)), at = round(xxlab, 2), side = 1, line = 1.2, cex = 1.5)
  box(col = "grey40", lwd = 5)
  for (ii in 1:10) {
    lines(bkde(catJoints10[[1]][[cats]][[ii]][ , comp]), lwd = 2, lty = "dashed", col = cols[2])
  }
  lines(bkde(jointSpost10[[1]][[cats]][ , comp]), lwd = 5, lty = "solid", col = cols[2])
  for (ii in 1:20) {
    lines(bkde(catJoints20[[1]][[cats]][[ii]][ , comp]), lwd = 2, lty = "dashed", col = cols[4])
  }
  lines(bkde(jointSpost20[[1]][[cats]][ , comp]), lwd = 5, lty = "solid", col = cols[4])
  if (comp == 1) {
    mtext("Density", side = 2, outer = TRUE, cex = 2, line = -1)
    legend("topright", c("Overall Posterior", "M-Posterior (m=10)", "M-Posterior (m=20)"), col = cols[c(1, 2, 4)],
             lty = "solid", lwd = 3, bty = "n", cex = 1.2)
  }
}
dev.off()

test <- readRDS("../data/test_gss.rds")

cvcov <- list()
cvlen <- list()
for (ss in 1:10) {
  cvcov[[ss]] <- list()
  cvlen[[ss]] <- list()
  for (pp in 1:3) {
    cvcov[[ss]][[pp]] <- matrix(0.0, 36, 4)
    cvlen[[ss]][[pp]] <- matrix(0.0, 36, 4)
  }
}

aaa <- 0.025
for (ss in 1:10) {
  cnt <- 0
  for (ii in 1:8) {
    for (jj in (ii + 1):9) {
      cnt <- cnt + 1
      op <- matrix(0.0, 2, 4)
      mp1 <- matrix(0.0, 2, 4)
      mp2 <- matrix(0.0, 2, 4)
      for (comp in 1:4) {
        op[, comp] <- quantile(jointOpost[[ss]][[cnt]][ , comp], probs = c(aaa/4, 1 - aaa / 4))
        mp1[, comp] <- quantile(jointSpost10[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
        mp2[, comp] <- quantile(jointSpost20[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
      }
      dats <- test[[ss]][ , c(ii, jj)]
      pp <- as.numeric(xtabs( ~ dats[ , 1] + dats[ , 2]) / sum(xtabs( ~ dats[ , 1] + dats[ , 2])))
      cvlen[[ss]][[1]][cnt, ] <- op[2, ] - op[1, ]
      cvlen[[ss]][[2]][cnt, ] <- mp1[2, ] - mp1[1, ]
      cvlen[[ss]][[3]][cnt, ] <- mp2[2, ] - mp2[1, ]
      for (comp in 1:4) {
        cvcov[[ss]][[1]][cnt, comp] <- op[2, comp] > pp[comp]  & op[1, comp] < pp[comp]
        cvcov[[ss]][[2]][cnt, comp] <- mp1[2, comp] > pp[comp] & mp1[1, comp] < pp[comp]
        cvcov[[ss]][[3]][cnt, comp] <- mp2[2, comp] > pp[comp] & mp2[1, comp] < pp[comp]
      }
    }
  }
}

oplen <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[1]]), colMeans))
mplen1 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[2]]), colMeans))
mplen2 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[3]]), colMeans))

opcov <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[1]])))
mpcov1 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[2]])))
mpcov2 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[3]])))


lentbl1 <- rbind(paste0(format(round(mean(oplen) * 100, 2), nsmall = 2), " (", format(round(sd(oplen) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen1) * 100, 2), nsmall = 2), " (", format(round(sd(mplen1) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen2) * 100, 2), nsmall = 2), " (", format(round(sd(mplen2) * 100, 2), nsmall = 2), ")"))

covtbl1 <- rbind(paste0(format(round(mean(opcov), 2), nsmall = 2), " (", format(round(sd(opcov), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov1), 2), nsmall = 2), " (", format(round(sd(mpcov1), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov2), 2), nsmall = 2), " (", format(round(sd(mpcov2), 2), nsmall = 2), ")"))


aaa <- 0.10 / 2
for (ss in 1:10) {
  cnt <- 0
  for (ii in 1:8) {
    for (jj in (ii + 1):9) {
      cnt <- cnt + 1
      op <- matrix(0.0, 2, 4)
      mp1 <- matrix(0.0, 2, 4)
      mp2 <- matrix(0.0, 2, 4)
      for (comp in 1:4) {
        op[, comp] <- quantile(jointOpost[[ss]][[cnt]][ , comp], probs = c(aaa/4, 1 - aaa / 4))
        mp1[, comp] <- quantile(jointSpost10[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
        mp2[, comp] <- quantile(jointSpost20[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
      }
      dats <- test[[ss]][ , c(ii, jj)]
      pp <- as.numeric(xtabs( ~ dats[ , 1] + dats[ , 2]) / sum(xtabs( ~ dats[ , 1] + dats[ , 2])))
      cvlen[[ss]][[1]][cnt, ] <- op[2, ] - op[1, ]
      cvlen[[ss]][[2]][cnt, ] <- mp1[2, ] - mp1[1, ]
      cvlen[[ss]][[3]][cnt, ] <- mp2[2, ] - mp2[1, ]
      for (comp in 1:4) {
        cvcov[[ss]][[1]][cnt, comp] <- op[2, comp] > pp[comp]  & op[1, comp] < pp[comp]
        cvcov[[ss]][[2]][cnt, comp] <- mp1[2, comp] > pp[comp] & mp1[1, comp] < pp[comp]
        cvcov[[ss]][[3]][cnt, comp] <- mp2[2, comp] > pp[comp] & mp2[1, comp] < pp[comp]
      }
    }
  }
}

oplen <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[1]]), colMeans))
mplen1 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[2]]), colMeans))
mplen2 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[3]]), colMeans))

opcov <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[1]])))
mpcov1 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[2]])))
mpcov2 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[3]])))

lentbl2 <- rbind(paste0(format(round(mean(oplen) * 100, 2), nsmall = 2), " (", format(round(sd(oplen) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen1) * 100, 2), nsmall = 2), " (", format(round(sd(mplen1) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen2) * 100, 2), nsmall = 2), " (", format(round(sd(mplen2) * 100, 2), nsmall = 2), ")"))

covtbl2 <- rbind(paste0(format(round(mean(opcov), 2), nsmall = 2), " (", format(round(sd(opcov), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov1), 2), nsmall = 2), " (", format(round(sd(mpcov1), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov2), 2), nsmall = 2), " (", format(round(sd(mpcov2), 2), nsmall = 2), ")"))

aaa <- 0.15 / 2
for (ss in 1:10) {
  cnt <- 0
  for (ii in 1:8) {
    for (jj in (ii + 1):9) {
      cnt <- cnt + 1
      op <- matrix(0.0, 2, 4)
      mp1 <- matrix(0.0, 2, 4)
      mp2 <- matrix(0.0, 2, 4)
      for (comp in 1:4) {
        op[, comp] <- quantile(jointOpost[[ss]][[cnt]][ , comp], probs = c(aaa/4, 1 - aaa / 4))
        mp1[, comp] <- quantile(jointSpost10[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
        mp2[, comp] <- quantile(jointSpost20[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
      }
      dats <- test[[ss]][ , c(ii, jj)]
      pp <- as.numeric(xtabs( ~ dats[ , 1] + dats[ , 2]) / sum(xtabs( ~ dats[ , 1] + dats[ , 2])))
      cvlen[[ss]][[1]][cnt, ] <- op[2, ] - op[1, ]
      cvlen[[ss]][[2]][cnt, ] <- mp1[2, ] - mp1[1, ]
      cvlen[[ss]][[3]][cnt, ] <- mp2[2, ] - mp2[1, ]
      for (comp in 1:4) {
        cvcov[[ss]][[1]][cnt, comp] <- op[2, comp] > pp[comp]  & op[1, comp] < pp[comp]
        cvcov[[ss]][[2]][cnt, comp] <- mp1[2, comp] > pp[comp] & mp1[1, comp] < pp[comp]
        cvcov[[ss]][[3]][cnt, comp] <- mp2[2, comp] > pp[comp] & mp2[1, comp] < pp[comp]
      }
    }
  }
}

oplen <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[1]]), colMeans))
mplen1 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[2]]), colMeans))
mplen2 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[3]]), colMeans))

opcov <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[1]])))
mpcov1 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[2]])))
mpcov2 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[3]])))

lentbl3 <- rbind(paste0(format(round(mean(oplen) * 100, 2), nsmall = 2), " (", format(round(sd(oplen) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen1) * 100, 2), nsmall = 2), " (", format(round(sd(mplen1) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen2) * 100, 2), nsmall = 2), " (", format(round(sd(mplen2) * 100, 2), nsmall = 2), ")"))

covtbl3 <- rbind(paste0(format(round(mean(opcov), 2), nsmall = 2), " (", format(round(sd(opcov), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov1), 2), nsmall = 2), " (", format(round(sd(mpcov1), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov2), 2), nsmall = 2), " (", format(round(sd(mpcov2), 2), nsmall = 2), ")"))



aaa <- 0.20 / 2
for (ss in 1:10) {
  cnt <- 0
  for (ii in 1:8) {
    for (jj in (ii + 1):9) {
      cnt <- cnt + 1
      op <- matrix(0.0, 2, 4)
      mp1 <- matrix(0.0, 2, 4)
      mp2 <- matrix(0.0, 2, 4)
      for (comp in 1:4) {
        op[, comp] <- quantile(jointOpost[[ss]][[cnt]][ , comp], probs = c(aaa/4, 1 - aaa / 4))
        mp1[, comp] <- quantile(jointSpost10[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
        mp2[, comp] <- quantile(jointSpost20[[ss]][[cnt]][ , comp], probs = c(aaa / 4, 1 - aaa / 4))
      }
      dats <- test[[ss]][ , c(ii, jj)]
      pp <- as.numeric(xtabs( ~ dats[ , 1] + dats[ , 2]) / sum(xtabs( ~ dats[ , 1] + dats[ , 2])))
      cvlen[[ss]][[1]][cnt, ] <- op[2, ] - op[1, ]
      cvlen[[ss]][[2]][cnt, ] <- mp1[2, ] - mp1[1, ]
      cvlen[[ss]][[3]][cnt, ] <- mp2[2, ] - mp2[1, ]
      for (comp in 1:4) {
        cvcov[[ss]][[1]][cnt, comp] <- op[2, comp] > pp[comp]  & op[1, comp] < pp[comp]
        cvcov[[ss]][[2]][cnt, comp] <- mp1[2, comp] > pp[comp] & mp1[1, comp] < pp[comp]
        cvcov[[ss]][[3]][cnt, comp] <- mp2[2, comp] > pp[comp] & mp2[1, comp] < pp[comp]
      }
    }
  }
}

oplen <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[1]]), colMeans))
mplen1 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[2]]), colMeans))
mplen2 <- do.call(rbind, lapply(lapply(cvlen, function(x) x[[3]]), colMeans))

opcov <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[1]])))
mpcov1 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[2]])))
mpcov2 <- colMeans(do.call(rbind, lapply(cvcov, function(x) x[[3]])))

lentbl4 <- rbind(paste0(format(round(mean(oplen) * 100, 2), nsmall = 2), " (", format(round(sd(oplen) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen1) * 100, 2), nsmall = 2), " (", format(round(sd(mplen1) * 100, 2), nsmall = 2), ")"),
                paste0(format(round(mean(mplen2) * 100, 2), nsmall = 2), " (", format(round(sd(mplen2) * 100, 2), nsmall = 2), ")"))

covtbl4 <- rbind(paste0(format(round(mean(opcov), 2), nsmall = 2), " (", format(round(sd(opcov), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov1), 2), nsmall = 2), " (", format(round(sd(mpcov1), 2), nsmall = 2), ")"),
                paste0(format(round(mean(mpcov2), 2), nsmall = 2), " (", format(round(sd(mpcov2), 2), nsmall = 2), ")"))

xtable::xtable(cbind(lentbl1, lentbl2, lentbl3, lentbl4))
xtable::xtable(cbind(covtbl1, covtbl2, covtbl3, covtbl4))
