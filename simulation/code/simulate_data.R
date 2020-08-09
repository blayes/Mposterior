rm(list = ls())

genData <- function (dist1 = "rnorm", pars1 = list(mean = 0, sd = 1), rng, nobs = 200, nout = 4, nsub = 10) {
    res <- vector("list", nsub)
    names(res) <- as.character(seq_len(nsub))
    ok <- do.call(dist1, args = c(n = nobs - nout, pars1))
    outliers <- abs(sort(ok, decreasing = TRUE))[1:nout] * rng
    idxSub <- sample(seq_len(nsub), size = nobs, replace = TRUE)
    mixData <- c(ok, outliers)
    names(mixData) <- c(paste("ok", seq_along(ok), sep = "."), paste("out", seq_along(outliers), sep = "."))
    for (i in 1:nsub) {
        res[[i]] <- mixData[which(idxSub == i)]
    }
    res
}

set.seed(12345)
sims <- list()
for (ss in 1:50) {
    sims[[ss]] <- lapply(seq(1, 25, length = 25),
                         function (x) {
                             genData(rng = x, nobs = 200, nout = 10, nsub = 10)
                         })
}

saveRDS(sims, "../data/sims_jmlr.rds")
