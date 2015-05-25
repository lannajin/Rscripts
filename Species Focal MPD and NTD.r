library(ape)
library(picante)

#samp: dataframe with sites as rownames and species as columns; or community matrix with sites as rows and species as columns
#dis: cophenetic matrix of phylogenetic tree

#Function to calculate species focal measures of Nearest Neighbor/Taxon Distance (NTD) and Mean Pairwise Distance (MPD) for each plot.
NTD<-function (samp, dis){
	if(is.matrix(samp)){
		samp<-as.data.frame(samp)
	}else if(!is.data.frame(samp)){
		stop("object 'samp' not a matrix or data frame")
	}
	
	output<-do.call(rbind.data.frame,lapply(1:dim(samp)[1], function(i){	#For each plot...
		sppInSample <- names(samp[i, samp[i, ] > 0])	#select species which share plot
		if (length(sppInSample) > 1) {	#Can only calculate MPD and NTD focal for plots with 2 or more species
			sample.dis <- dis[sppInSample, sppInSample]	#Phylogenetic distance for 
			diag(sample.dis) <- NA
			do.call(rbind.data.frame,lapply(1:length(sppInSample), function(n){
				cbind(
					Plot = rownames(samp)[i],
					Scientific.Name = sppInSample[n], 
					NearestNeighbor = ifelse(
						names(which(sample.dis[n,]== min(sample.dis[n,],na.rm=TRUE)))>1, 
						names(which(sample.dis[n,]== min(sample.dis[n,],na.rm=TRUE)))[1], 
						names(which(sample.dis[n,]== min(sample.dis[n,],na.rm=TRUE)))), 
					NTD = min(sample.dis[n,],na.rm=TRUE), 
					MPD = mean(sample.dis[n,],na.rm=TRUE)
				)
			}))
		}	else{
           data.frame(cbind(Plot=rownames(samp)[i],Scientific.Name=NA,NearestNeighbor=NA,NTD=NA,MPD=NA))
        }
	}))
	output[,4:5]<-sapply(4:5, function(x) as.numeric(as.character(output[,x])))	#Force NTD and MPD to numeric
	output<-unique(output)	#Remove duplicates
	names(output$Scientific.Name)<-NULL; names(output$Plot)<-NULL; names(output$NearestNeighbor)<-NULL
    return(output)
}

