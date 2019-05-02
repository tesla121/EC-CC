#Give paths to input underlying graphs and infection graphs
#Set the working directory
load(file="./Data, Code, Results/Graphs/Facebook.RData")
load(file="./Data, Code, Results/Infection Graphs/Facebook_Hetero_2.RData")
#Replace Facebook_Hetero_2 in the rest of the code according to the graph and infection size. For example if the 
#underlying graph is Regular and infection size is 40-60%, replace it with Regular_Hetero_40.

library(igraph)

#Code for BC, CC, DC, EVC and EC

d_sum=0
e_sum=0
b_sum=0
c_sum=0
ecc_sum=0
bet_FB_Ht_2=list() #Replace bet_FB_Ht_2 according to graph and infection size
clo_FB_Ht_2=list() #Replace clo_FB_Ht_2 according to graph and infection size
eig_FB_Ht_2=list() #Replace eig_FB_Ht_2 according to graph and infection size
deg_FB_Ht_2=list() #Replace deg_FB_Ht_2 according to graph and infection size
ecc_FB_Ht_2=list() #Replace ecc_FB_Ht_2 according to graph and infection size
i=1
while(i<=100){
	source=V(graph)[Facebook_Hetero_2[[i]][1]]$name
	sg=induced_subgraph(graph, Facebook_Hetero_2[[i]], impl = c("copy_and_delete"))
	bet=betweenness(sg, v = V(sg), directed = FALSE, weights = NULL,nobigint = TRUE, normalized = FALSE)
	clo=closeness(sg, vids = V(sg), mode = c( "all"),weights = NULL, normalized = FALSE)
	eig=eigen_centrality(sg, directed = FALSE, scale = TRUE, weights = NULL,options = arpack_defaults)$vector
	deg=degree(sg)
	ecc=eccentricity(sg, vids = V(sg), mode = c("all"))
	estimated_ecc=V(sg)[as.numeric(which(min(ecc)==ecc))]$name
	estimated_deg=V(sg)[as.numeric(which(max(deg)==deg))]$name
	estimated_eig=V(sg)[as.numeric(which(max(eig)==eig))]$name
	estimated_clo=V(sg)[as.numeric(which(max(clo)==clo))]$name
	estimated_bet=V(sg)[as.numeric(which(max(bet)==bet))]$name
	d_bet=distances(sg, v = as.character(source), to = as.character(estimated_bet), mode = c("all"), weights=NULL, algorithm = c("unweighted"))
	d_clo=distances(sg, v = as.character(source), to = as.character(estimated_clo), mode = c("all"), weights=NULL, algorithm = c("unweighted"))
	d_eig=distances(sg, v = as.character(source), to = as.character(estimated_eig), mode = c("all"), weights=NULL, algorithm = c("unweighted"))
	d_deg=distances(sg, v = as.character(source), to = as.character(estimated_deg), mode = c("all"), weights=NULL, algorithm = c("unweighted"))
	d_ecc=distances(sg, v = as.character(source), to = as.character(estimated_ecc), mode = c("all"), weights=NULL, algorithm = c("unweighted"))
	#print((d_eig))
	bet_FB_Ht_2[[i]]=d_bet
	clo_FB_Ht_2[[i]]=d_clo
	eig_FB_Ht_2[[i]]=d_eig
	deg_FB_Ht_2[[i]]=d_deg
	ecc_FB_Ht_2[[i]]=d_ecc
	if(i%%5==0){
		#save(bet_FB_Ht_2,file="./Data, Code, Results/Result Objects/bet_FB_Ht_2.RData")
		#save(clo_FB_Ht_2,file="./Data, Code, Results/Result Objects/clo_FB_Ht_2.RData")
		#save(eig_FB_Ht_2,file="./Data, Code, Results/Result Objects/eig_FB_Ht_2.RData")
		#save(deg_FB_Ht_2,file="./Data, Code, Results/Result Objects/deg_FB_Ht_2.RData")
		#save(ecc_FB_Ht_2,file="./Data, Code, Results/Result Objects/ecc_FB_Ht_2.RData")
	}
	print(i)
	b_sum=b_sum+as.numeric(d_bet[1])
	c_sum=c_sum+as.numeric(d_clo[1])
	e_sum=e_sum+as.numeric(d_eig[1])
	d_sum=d_sum+as.numeric(d_deg[1])
	ecc_sum=ecc_sum+as.numeric(d_ecc[1])
	i=i+1
}
