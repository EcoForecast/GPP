#!/bin/bash -l

Rscript 01.load.input.data.R

# Arguments: nchain n.iter burnin
Rscript 02.runmodel.R 5 15000 100000
Rscript 03.plotoutput.R
