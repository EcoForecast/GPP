#!/bin/bash

# OCO data
Rscript oco-download/01.oco.download.R
Rscript oco-download/02.oco.process.R

# MODIS data
Rscript modis-download/01-modis.get.site.info.R
Rscript modis-download/02-modis.download.R
Rscript modis-download/03-modis.process.R

# Flux tower data
Rscript flux-download/01.download.flux.R
Rscript flux-download/02.process.flux.R
Rscript flux-download/03.plot.flux.R

