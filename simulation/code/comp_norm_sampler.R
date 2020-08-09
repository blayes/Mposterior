sampleCompPost <- function (yvec, sigma, nsub, niter, nburn, nthin, id) {
    library(inline)
    library(Rcpp)
    library(rstan)

    simList <- list(N = length(yvec), y = yvec, sigma = sigma, nsub = nsub)

    seeds <- (1:5000) * as.numeric(gsub(":", "", substr(Sys.time(), 12, 19)))

    startTime <- proc.time()
    mdl <- stan(file = "comp_norm.stan", data = simList, iter = niter, warmup = nburn, chains = 1, thin = nthin, seed = seeds[id], control = list(adapt_delta = 0.80))
    endTime <- proc.time()

    lst <- mdl@sim$samples[[1]]
    bs <- grep("mu", names(lst))
    samp <- lst[[bs]]

    list(samples = samp[(length(samp) - (niter - nburn) / nthin + 1):length(samp)], time = endTime - startTime)
}
