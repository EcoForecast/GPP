#!/bin/bash
Rscript './modis-download/01-modis.get.site.info.R'
Rscript './modis-download/02-modis.download.R'
Rscript './modis-download/03-modis.process.R'
