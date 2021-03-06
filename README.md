There are 5 folders in this directory. 

"Code" folder contains all the code files written in R: state-of-the-art source identifcation techniques ("DA.R", "DA (Fast).R", "LPSI.R", "MDL.R" and "MDL (Fast).R") as well as classical graph centrality measures ("BC_CC_DC_EVC_EC.R", "DC+CC.R" and "EC+CC.R"). Besides, there is code for SI model of infection (SI.R).
"SI.R" takes input a graph object from "Graph" folder, simulates infection spreading process and outputs the corresponding infection graphs object (100 in every case) which is saved in "Infection Graphs" folder.

Each source identification technique (code files found in "Code" folder) takes input an underlying graph from "Graph" folder and the corresponding infection graphs object (100) from "Infection Graphs" folder and estimates the source. It then outputs the corresponding "hop error" (from actual source to estimated source) and saves the result object in "Result Objects" folder. Since each infection graphs object contains 100 infection graphs, therefore, 100 "hop errors" are generated and saved in one object. 
Note: A random node is generated in SI infection process (SI.R) and is considered as the actual source, which serves as the first node in the infection graph object thus produced. Therefore, in every infection graph object, the first node is the actual infection source. 

------------

"Graph" folder contains all the underlying graph objects. These are labeled and weighted graphs; weights are uniformly distributed over (0,1). 

------------

"Infection Graphs" contains all the SI generated infection graph objects. 

------------

"Result Objects" contains all the experimental results and "Result Files" contains Excel files of those results constructed in accordance with the modules of our paper. "Phase I.csv" contains all the relevant results of Phase I of our analysis where we examine the performance of classical graph centrality measures in source identification. "Phase II.csv" contains results from Phase II of our experiments where we compare classical graph measures with state-of-the-art techniques. "Impact of Infection Size.csv" and "Impact of Density.csv" contains results from our experiments where we analyze the impact of infection size and density on source identification. Besides containing the "hop error" results, the .csv files provide results on accuracy (@hop 0), accuracy (@hops 0-1) and average hop error.

"Result Objects" nomenclature:
Barring EC+CC and DC+CC, "Result Objects" contains file names with two patterns: "w_x_y_z.RData" and "w_x1_x2_y_z.RData". "w_x_y_z.RData" type of objects are generated in Phase I, Phase II and when we analyze the impact of infection size. "w_x1_x2_y_z.RData" type objects are generated when impact of density on source identification is analyzed. 

"w_x_y_z.RData": w represents the source identification, x represents the underlying graph, y represents the type of SI model and z represents infection size (in percent). For example, "DA_Ran_Ht_20.RData" is a result object generated by DA.R (or DA (Fast.R)), where the underlying graph is Random (Random.RData in Graphs folder), SI model of infection is heterogeneous and the infection size is 20-25% of the underlying graph.
"w_x1_x2_y_z.RData": w, y, z represent the same as above. x1 and x2 together represent the underlying graph, where x1 represents the topology and x2 represents the type of density. For example, "lpsi_Ran_D_Ht_5.RData" is result object generated by LPSI.R, where the underlying graph is Random Dense (Random_D.RData in Graphs folder), SI model of infection is heterogeneous and the infection size is 5%.

For EC+CC and DC+CC objects, ecc_clo and deg+clo represent the w part in each of two file name patterns.

------------

"Infection Graphs" nomenclature:
This contains file names with two patterns: "x_y_z.RData" and "x1_x2_y_z.RData". x represents the underlying graph. y represents the type of SI model and z represents the infection size. x1_x2 together represent x, where x1 is the underlying graph and x2 is its density.

Discussions on infection size and density can be found in the paper "A Revisit to the Source Identification Problem under Classical Graph Centrality Measures".    

------------

"S" folder contains the normalized weighted matrices of very large underlying graphs, i.e., Facebook, US Power Grid, Regular and Random. This is specifically created for LPSI.R. We can either generate S for each graph or we can load it directly from the "S" folder. 

------------

Instructions for running the code files:

All code files are written R. We shall provide instructions for running the same by considering "MDL.R" for an example. Below are the instructions:

1. Set the path correctly to input the underlying graph and the corresponding infection graph. Please note that each underlying graph has a corresponding infection graph and if the proper corresponding graphs aren't input, the results will be erroneous. For example, Facebook graph will have corresponding infection graphs with file name patterns as "Facebook_y_z.RData" ("Infection Graphs" folder). Similarly, Random will have "Random_y_z.RData". If we have to analyze the density, the underlying graphs will be "Random_VS.RData", "Random_S.RData", "Random_D.RData" and "Random_VD.RData". Their corresponding infection graphs objects will be of the pattern "Random_x2_y_z.RData". 

2. When you input a particular infection graphs object, say "Random_y_z.RData", make sure to make the necessary corresponding replacements in the rest of the code. For example, if the code file already contains "Facebook_y_z", replace it "Random_y_z" wherever necessary.

3. If the underlying graph is Random and the infection graphs object is "Random_y_z", and we are using "MDL.R" file, the corresponding output object, for the sake of simplicity, should be like "mdl_Ran_y_z.RData" (which can be found in "Result Objects" folder). The results thus achieved can be verified using the .csv files found in "Result Files" folder.

------------

More on this can be found in the paper "A Revisit to the Source Identification Problem under Classical Graph Centrality Measures".
