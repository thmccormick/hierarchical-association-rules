# hierarchical-association-rules
Codes for McCormick et al 2012 "A hierarchical model for association rule mining of sequential events: an approach to automated medical symptom prediction."

Questions contact Tyler McCormick, tylermc@uw.edu

## Directory

### Main files to run the code (call the subsequent files) 
- main_fcn_recsys2.R
	- This is the wrapper that calls the scripts below to fit and evaluate the model.

- rec_sim_run_checking.R
	- Formats the data and runs the above wrapper.

### MCMC function
- encounter.metro_s_meanonly_sim.R
	- main MCMC function.  This function only outputs mean posterior probabilities but can be modified to output the full posterior probability distribution.


### Evaluating output
- encounter_eval_meanonly.R
- encounter_eval_newpt2.R

