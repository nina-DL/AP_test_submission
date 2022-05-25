# Allocation Probability Test: short readme

To reproduce all the results the following path should be considered:

I. Simulate all the data trajectories with Thompson sampling (standard, clipped, BOLS-tuned) and for different experimental scenarios based on: 1) H_0 and H_1 specifications and 2) batch size. This can be performed with file "perform_simulations.R" by specifying the parameters of interest and it is based on the auxiliary file "simulation_functions.R", containing all the functions for simulated data trajectories from Thompson sampling;

II. Perform inference on the simulated data trajectories using the file "perform_simulations.R". This file is based on "inference_reward_functions.R", containing all the functions for performing inference and computing reward and proportion of optimal arm allocation. It requires uploading the simulated datasets in 1. first;

III. To reproduce the plots, the file "make_plots.R" could be used.
