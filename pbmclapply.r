#Script which parallelizes the pbapply function. Or, alternatively, the mclapply function with a progress bar.

pbmclapply<-function (X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, 
    mc.silent = FALSE, mc.cores = 1L, mc.cleanup = TRUE, mc.allow.recursive = TRUE){
	require(pbapply)
	require(parallel)
    cores <- as.integer(mc.cores)
    if (cores < 1L) 
        stop("'mc.cores' must be >= 1")
    if (cores > 1L) 
        stop("'mc.cores' > 1 is not supported on Windows")
    pblapply(X, FUN, ...)
}