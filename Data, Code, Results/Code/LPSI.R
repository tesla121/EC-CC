#Give paths to input underlying graphs and infection graphs
#Set the working directory
load(file="./Data, Code, Results/Graphs/Facebook.RData")
load(file="./Data, Code, Results/Infection Graphs/Facebook_Hetero_2.RData")
#Replace Facebook_Hetero_2 in the rest of the code according to the graph and infection size. For example if the 
#underlying graph is Regular and infection size is 40-60%, replace it with Regular_Hetero_40.

library(igraph)
library(expm)

#Code for Label Propagation based Source Identification (LPSI) - Single Source

#Calculating S, the normalized weighted matrix of a graph, takes time if the graph size is large.
#We have already calculated S for each graph and saved it. Therefore S can be directly loaded.

#CODE TO CALCULATE S
#Code starts
"%^%" <- function(S, power) 
   with(eigen(S), vectors %*% (values^power * t(vectors))) 

W=as.matrix(get.adjacency(graph)) #Line 1 of Algorithm 1 Label Propagation based Source Identification(LPSI) from the main paper
D=diag(rowSums(W))
S=(D%^%-1/2)%*%(W)%*%(D%^%-1/2) #Line 2 of Algorithm 1 Label Propagation based Source Identification(LPSI)
#Code ends

#Either calculate S using above code, but for convenience, load S directly. 
#Replace S_FB in the rest of the code according to the graph. For example if the graph is Regular, replace with it S_Reg.
load(file="./Data, Code, Results/S/S_FB.RData")
S=S_FB

alpha=0.5 #Constant throughout the network
lpsi_FB_Ht_2=list() #Replace lpsi_FB_Ht_2 according to graph and infection size
lpsi_sum=0
k=1
A=as.matrix(get.adjacency(graph))
while(k<=100){
Y=c(rep(-1,length(V(graph))))
Y[c(Facebook_Hetero_2[[k]])]=c(rep(1,length(Facebook_Hetero_2[[k]]))) 
df=NULL
df=as.data.frame(rbind(Y))

iter=2
while(iter<=5){ #In our experimentations, the convergence takes place in 4 iterations.
	G=c(rep(0,length(V(graph))))
	
	for(i in sample(1:length(V(graph)))) {
		summation=0
		nbr=as.numeric(which(A[i,]==1))

		for(j in nbr){
			summation=summation+unlist(S[i,j])*unlist(df[iter-1,j]) # Part of Line 6 of Algorithm 1 Label Propagation based Source Identification(LPSI)
		}

		G[[i]]=alpha*summation+(1-alpha)*unlist(Y[i]) # Part of Line 6 of Algorithm 1 Label Propagation based Source Identification(LPSI)
	}

	df=as.data.frame(rbind(df,unlist(G)))
	iter=iter+1
}

estimated=which(max(df[5,])==df[5,])
d_lpsi=distances(graph, v = Facebook_Hetero_2[[k]][1], to = estimated, mode = c("all"), weights=NULL, algorithm = c("unweighted"))
print(d_lpsi)
print(k)
lpsi_sum=lpsi_sum+as.numeric(d_lpsi[1])
lpsi_FB_Ht_2[[k]]=d_lpsi

	if(k%%5==0){
		#save(lpsi_FB_Ht_2, file="./Data, Code, Results/Result Objects/lpsi_FB_Ht_2.RData")
	}

k=k+1
}
