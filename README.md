# Case study on input sensitivity of x264 for performance prediction of its configurations 

The *data* folder contains data (performance measurements: execution time and size of the output video)

Paper (in progress)
https://www.overleaf.com/10673417tkmtdmhyvmrs

Research questions: ("inputs" means here "videos")
 * (RQ1) Do inputs change the performance distributions? Do inputs change the ranking? Do inputs change the influential options? Do inputs change the interactions between options?
 in a nutshell we want characterize the phenomenon: should inputs be taken seriously into account when predicting performance? 
 * (RQ2) Can we identify "cheaper" inputs? (same distribution, but less costly to measure) Can we group together inputs? (same distribution, so no need to transfer) 
 * (RQ3) Can we transfer performance predictions among inputs? (in all cases?) What is an optimal way of building the transfer model? Several factors to consider: sampling size, "linear model" (polynomial), and videos selection (clusters) for transferring ("closer inputs") 

