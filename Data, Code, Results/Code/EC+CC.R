#Give paths to input underlying graphs and infection graphs
#Set the working directory
load(file="./Data, Code, Results/Graphs/Facebook.RData")
load(file="./Data, Code, Results/Infection Graphs/Facebook_Hetero_2.RData")
#Replace Facebook_Hetero_2 in the rest of the code according to the graph and infection size. For example if the 
#underlying graph is Regular and infection size is 40-60%, replace it with Regular_Hetero_40.

library(igraph)

#Code for EC+CC

ecc_sum=0
ecc_clo_FB_Ht_2=list() #Replace ecc_clo_FB_Ht_2 according to graph and infection size
JC_sum=0
i=1
while(i<=100){
	source=V(graph)[Facebook_Hetero_2[[i]][1]]$name
	sg=induced_subgraph(graph, Facebook_Hetero_2[[i]], impl = c("copy_and_delete"))
	ecc=eccentricity(sg, vids = V(sg), mode = c("all"))
	estimated_ecc=V(sg)[as.numeric(which(min(ecc)==ecc))]$name
	clo=closeness(sg, vids = as.character(estimated_ecc), mode = c( "all"),weights = NULL, normalized = TRUE)
	index=which(max(clo)==clo)
 	estimated_ecc=V(sg)[as.character(estimated_ecc[index[1]])]$name
 	d_ecc=distances(sg, v = as.character(source), to = as.character(estimated_ecc), mode = c("all"), weights=NULL, algorithm = c("unweighted"))
 	print(i)
 	print((d_ecc))
	ecc_clo_FB_Ht_2[[i]]=d_ecc
	ecc_sum=ecc_sum+as.numeric(d_ecc[1])

	if(i%%1==0){
		
		#save(ecc_clo_FB_Ht_2,file="./Data, Code, Results/Result Objects/ecc_clo_FB_Ht_2.RData")
	
	}
	i=i+1
}
