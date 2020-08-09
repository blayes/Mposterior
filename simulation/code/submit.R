cmdArgs <- commandArgs(trailingOnly = TRUE)

mtd <- as.numeric(cmdArgs[1])
id <- as.numeric(cmdArgs[2])

if (mtd == 1) {
    source("full_t_sampler.R")

    cv <- rep(1:50, each = 25)
    out <- rep(1:25, times = 50)

    wid <- cbind(cv = cv, out = out)
    cid <- wid[id, 1]
    oid <- wid[id, 2]

    sim <- readRDS("../data/sims_jmlr.rds")

    train <- unlist(sim[[cid]][[oid]])

    res <- sampleFullPost(train, 1, 3, niter = 10000, nburn = 5000, nthin = 5, id)
    fname <- paste0("/Shared/ssrivastva/mposterior/full/rep_", cid, "_out_", oid, ".rds")
    saveRDS(res, fname)
} else if (mtd == 2) {
    source("mpost_t_sampler.R")
    library(Mposterior)
    library(matrixStats)

    cv <- rep(1:50, each = 25)
    out <- rep(1:25, times = 50)

    wid <- cbind(cv = cv, out = out)
    cid <- wid[id, 1]
    oid <- wid[id, 2]

    sim <- readRDS("../data/sims_jmlr.rds")

    train <- sim[[cid]][[oid]]
    subs <- list()
    for (ss in 1:10) {
        subs[[ss]] <- sampleMpost(train[[ss]], 1, 3, 10, niter = 10000, nburn = 5000, nthin = 5, id)
    }

    strt1 <- proc.time()
    resMpost <- findWeiszfeldMedian(lapply(subs,
                                           function(x) {
                                               yy <- matrix(x$samples, ncol = 1)
                                               yy[sample(1:nrow(yy), 500), , drop = FALSE]
                                           }), maxit = 50, tol = 1e-10)

    wts <- as.numeric(resMpost$weiszfeldWts)
    wts[wts < 1 / (2 * length(wts))] <- 0.0
    meds <- sample(1:nrow(resMpost$medianAtoms), size = 10000, replace = TRUE, prob = rep(wts, times = as.numeric(resMpost$natoms)))
    mpost <- resMpost$medianAtoms[meds, ]
    end1 <- proc.time()

    strt2 <- proc.time()
    wasp <- rowMeans(do.call(cbind, lapply(subs,
                                           function(x) {
                                               yy <- x$samples
                                               quantile(yy, probs = seq(0, 1, length = 2000))
                                           })))
    end2 <- proc.time()

    fname <- paste0("/Shared/ssrivastva/mposterior/mpost/rep_", cid, "_out_", oid, ".rds")
    saveRDS(list(mpost = mpost, time = (end1[3] - strt1[3]) + mean(sapply(subs, function(x) x$time[3]))), fname)

    fname <- paste0("/Shared/ssrivastva/mposterior/wasp/rep_", cid, "_out_", oid, ".rds")
    saveRDS(list(wasp = wasp, time = (end2[3] - strt2[3]) + mean(sapply(subs, function(x) x$time[3]))), fname)
} else if (mtd == 3) {
    source("comp_t_sampler.R")
    library(parallelMCMCcombine)
    library(matrixStats)

    cv <- rep(1:50, each = 25)
    out <- rep(1:25, times = 50)

    wid <- cbind(cv = cv, out = out)
    cid <- wid[id, 1]
    oid <- wid[id, 2]

    sim <- readRDS("../data/sims_jmlr.rds")

    train <- sim[[cid]][[oid]]
    subs <- list()
    for (ss in 1:10) {
        subs[[ss]] <- sampleCompPost(train[[ss]], 1, 3, 10, niter = 10000, nburn = 5000, nthin = 5, id)
    }

    consSamp <- array(NA, dim = c(1, 1000, 10))
    consSamp[1, , ] <- do.call(cbind, lapply(subs, function(x) x$samples))

    strt1 <- proc.time()
    cons <- drop(consensusMCindep(consSamp))
    end1 <- proc.time()

    fname <- paste0("/Shared/ssrivastva/mposterior/cmc/rep_", cid, "_out_", oid, ".rds")
    saveRDS(list(mpost = cons, time = (end1[3] - strt1[3]) + mean(sapply(subs, function(x) x$time[3]))), fname)

} else if (mtd == 4) {
    source("full_norm_sampler.R")

    cv <- rep(1:50, each = 25)
    out <- rep(1:25, times = 50)

    wid <- cbind(cv = cv, out = out)
    cid <- wid[id, 1]
    oid <- wid[id, 2]

    sim <- readRDS("../data/sims_jmlr.rds")

    train <- unlist(sim[[cid]][[oid]])

    res <- sampleFullPost(train, 1, niter = 10000, nburn = 5000, nthin = 5, id)
    fname <- paste0("/Shared/ssrivastva/mposterior/full/norm_rep_", cid, "_out_", oid, ".rds")
    saveRDS(res, fname)
} else if (mtd == 5) {
    source("mpost_norm_sampler.R")
    library(Mposterior)
    library(matrixStats)

    cv <- rep(1:50, each = 25)
    out <- rep(1:25, times = 50)

    wid <- cbind(cv = cv, out = out)
    cid <- wid[id, 1]
    oid <- wid[id, 2]

    sim <- readRDS("../data/sims_jmlr.rds")

    train <- sim[[cid]][[oid]]
    subs <- list()
    for (ss in 1:10) {
        subs[[ss]] <- sampleMpost(train[[ss]], 1, 10, niter = 10000, nburn = 5000, nthin = 5, id)
    }

    strt1 <- proc.time()
    resMpost <- findWeiszfeldMedian(lapply(subs,
                                           function(x) {
                                               yy <- matrix(x$samples, ncol = 1)
                                               yy[sample(1:nrow(yy), 500), , drop = FALSE]
                                           }), maxit = 50, tol = 1e-10)

    wts <- as.numeric(resMpost$weiszfeldWts)
    wts[wts < 1 / (2 * length(wts))] <- 0.0
    meds <- sample(1:nrow(resMpost$medianAtoms), size = 10000, replace = TRUE, prob = rep(wts, times = as.numeric(resMpost$natoms)))
    mpost <- resMpost$medianAtoms[meds, ]
    end1 <- proc.time()

    strt2 <- proc.time()
    wasp <- rowMeans(do.call(cbind, lapply(subs,
                                           function(x) {
                                               yy <- x$samples
                                               quantile(yy, probs = seq(0, 1, length = 2000))
                                           })))
    end2 <- proc.time()

    fname <- paste0("/Shared/ssrivastva/mposterior/mpost/norm_rep_", cid, "_out_", oid, ".rds")
    saveRDS(list(mpost = mpost, time = (end1[3] - strt1[3]) + mean(sapply(subs, function(x) x$time[3]))), fname)

    fname <- paste0("/Shared/ssrivastva/mposterior/wasp/norm_rep_", cid, "_out_", oid, ".rds")
    saveRDS(list(wasp = wasp, time = (end2[3] - strt2[3]) + mean(sapply(subs, function(x) x$time[3]))), fname)
} else if (mtd == 6) {
    source("comp_norm_sampler.R")
    library(parallelMCMCcombine)
    library(matrixStats)

    cv <- rep(1:50, each = 25)
    out <- rep(1:25, times = 50)

    wid <- cbind(cv = cv, out = out)
    cid <- wid[id, 1]
    oid <- wid[id, 2]

    sim <- readRDS("../data/sims_jmlr.rds")

    train <- sim[[cid]][[oid]]
    subs <- list()
    for (ss in 1:10) {
        subs[[ss]] <- sampleCompPost(train[[ss]], 1, 10, niter = 10000, nburn = 5000, nthin = 5, id)
    }

    consSamp <- array(NA, dim = c(1, 1000, 10))
    consSamp[1, , ] <- do.call(cbind, lapply(subs, function(x) x$samples))

    strt1 <- proc.time()
    cons <- drop(consensusMCindep(consSamp))
    end1 <- proc.time()

    fname <- paste0("/Shared/ssrivastva/mposterior/cmc/norm_rep_", cid, "_out_", oid, ".rds")
    saveRDS(list(mpost = cons, time = (end1[3] - strt1[3]) + mean(sapply(subs, function(x) x$time[3]))), fname)
} else {
    print("peace")
}
