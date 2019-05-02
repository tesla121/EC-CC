#Give paths to input underlying graphs and infection graphs
#Set the working directory
load(file="./Data, Code, Results/Graphs/Facebook.RData")
load(file="./Data, Code, Results/Infection Graphs/Facebook_Hetero_2.RData")
#Replace Facebook_Hetero_2 in the rest of the code according to the graph and infection size. For example if the 
#underlying graph is Regular and infection size is 40-60%, replace it with Regular_Hetero_40.

library(igraph)

#Code for DC+CC

deg_sum=0
deg_clo_FB_Ht_2=list() #Replace deg_clo_FB_Ht_2 according to graph and infection size
JC_sum=0
i=1
while(i<=100){
	source=V(graph)[Facebook_Hetero_2[[i]][1]]$name
	sg=induced_subgraph(graph, Facebook_Hetero_2[[i]], impl = c("copy_and_delete"))
	deg=degree(sg)
	estimated_deg=V(sg)[as.numeric(which(max(deg)==deg))]$name
	clo=closeness(sg, vids = as.character(estimated_deg), mode = c( "all"),weights = NULL, normalized = TRUE)
	index=which(max(clo)==clo)
 	estimated_deg=V(sg)[as.character(estimated_deg[index[1]])]$name
 	d_deg=distances(sg, v = as.character(source), to = as.character(estimated_deg), mode = c("all"), weights=NULL, algorithm = c("unweighted"))
 	print(i)
 	print((d_deg))
	deg_clo_FB_Ht_2[[i]]=d_deg
	deg_sum=deg_sum+as.numeric(d_deg[1])

	if(i%%1==0){
		
		#save(deg_clo_FB_Ht_2,file="./Data, Code, Results/Result Objects/deg_clo_FB_Ht_2.RData")
	
	}
	i=i+1
}
