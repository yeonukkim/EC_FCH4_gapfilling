# EC_FCH4_gapfilling

This repository contains examples code related to eddy covariance FCH4 gap-filling. 
For more information about the approach, see the article:

Kim, Y., Johnson, M. S., Knox, S., Black, A. T., Dalmagro, H. J., Kang, M., . . . Baldocchi, D. (resubmitted). Gap-filling approaches for eddy covariance methane fluxes: a comparison of three machine learning algorithms and a traditional method with principal component analysis. Global Change Biology. 

## Explanation on files
rf_run_for_public.R: 
A gap-filling script using Random Forest algorithm written in R. 
This script is based on Caret R package, and so one can easily modify this script to another machine learning algorithm like SVM, ANN.

exampleWETBB.csv:
An example dataset (Ameriflux site CA-DBB).

art_gap_for_public.R:
An R function generating artificial gap.

artificialgap_example.R:
An R script using the function above.
