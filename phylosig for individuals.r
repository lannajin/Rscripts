#Function to calculate the phylogenetic signal in a trait for individuals, rather than for species.

phylosig.indiv<-function(tree, x,  method = "K", test = TRUE, nsim = 1000, se = NULL, 
    start = NULL, control = list()){
    require(phytools)
	if (class(tree) != "phylo") 
        stop("tree object must be of class 'phylo'.")
	if(class(x) != "data.frame")
		stop("x object must be of class 'data.frame'.")
	
	x<-unique(x)
	phy<-tree
	
	#Add individuals as tips to phylogenetic tree
	for(i in unique(x[,1])){
		tt<-unique(x[which(x[,1]==i),2])
			for(j in 1:length(tt)){
				phy<-bind.tip(phy, tip.label=paste(i,tt[j],sep="_"), edge.length=0, where=which(phy$tip==i))
		}
	}
	#trim phy to remove just species names (without trait info)
	phy<-drop.tip(phy,setdiff(phy$tip.label,unique(sapply(1:dim(x)[1],function(y)paste(x[y,1],x[y,2],sep="_")))))
	
	traits<-x[,2]
	names(traits)<-sapply(1:dim(x)[1],function(y)paste(x[y,1],x[y,2],sep="_"))
	
	#Calculate Phylogenetic Signal
	blombergk<-phylosig(phy, traits, method=method, test=test, nsim=nsim, se=se, start=start)
	return(blombergk)
}
	
