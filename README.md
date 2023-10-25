# Sedation-Time-Course-Data-Evaluation
R-based script based for automated sedation time course data evaluation and visualisation.

Allows automated evaluation of sedation time course data from drug sensitivity assays, e.g. ethanol sedation sensitivity assays with Drosophila flies.
The standard script provided is only suitable for quantification of 2 groups (Control vs Experimental = Treatment Group), but can be customised with simple adjustments.
Excel files containing the fractional sedation values (e.g. number of sedated = inactive flies/total number of flies) for each time point can be used as input data.


Before running the script, certain parameters need to be set:
1) foldername <- Name of folder containing input data (Excel files)
2) Contname <- Input data file name of the Control Group (full name with .xlsx)
3) Expname <- Input data file name of the Experimental Group (full name with .xlsx)
4) Control <- Name of Control Group (shown in Plot)
5) Experimental <- Name of Experimental Group (shown in Plot)
6) end_time <- Observation time (maximum sedation time recorded, e.g. 20 minutes).
7) stat_method <- Name of the statistical test (T-Test as standard)
   
The parameters set in the script refer to the example data for better comprehensibility, but must be individually adjusted with regard to your own data.


Sedation time course data are fitted to a log-logistic function using the "drc" package, allowing the calculation of ST50 values (sedation time 50).
Successful execution of the script will result in the generation of an evaluation folder containing an Excel file with the summary of the model, 
the calculated ST50 value and the results of the statistical test, as well as a visualisation of the data (Total data fitted with sigmoidal curves, Barplot with St50 values) in pdf, svg or tif format.

