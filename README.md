# Rscripts

pbmclapply.r: Function which adds a progress bar to the parallized lapply function, pbapply. Based on functions from the library pbapply and parallel.

phylosig for individuals.r: Function to calculate the phylogenetic signal in a trait for individuals, rather than for species. The function contains script to generate a phylogeny for individuals. This individual tree and the traits of individuals are then fed through the function "phylosig" in the R library (phytools) to measure Blomberg's K.
