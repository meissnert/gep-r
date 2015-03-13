# Installation of GEP-R #

## Required ##
  * Linux Operating System (e.g. [openSUSE](http://www.opensuse.org) - GEP-R is developed within openSUSE 11.2 64-Bit), Windows or MacOS.
  * [R](http://www.r-project.org)/[Bioconductor](http://www.bioconductor.org) Version 2.10/2.5 or higher
  * with the following packages and their dependencies installed
    * _docval_ (get the gcrma modified version from the [Downloads](http://code.google.com/p/gep-r/downloads/list) section)
    * _affy_
    * _vsn_
    * _Biobase_
    * _gcrma_
    * _affyQCReport_
    * _simpleaffy_
    * _MAQCsubsetAFX_
    * _affydata_
    * _hgu133plus2probe_
    * _hgu133plus2cdf_
    * _gdata_
    * _panp_
    * _AnnotationDbi_
    * _pamr_
    * _affyPLM_
    * _gWidgetsRGtk2_
    * _gWidgets_
    * _preprocessCore_
    * _survival_
    * _genefilter_
    * _limma_
    * _pgUtils_ (for Linux/PSQL support)
    * _Rdbi_ (for Linux/PSQL support)
    * _RdbiPgSQL_ (for Linux/PSQL support)
    * _maDB_ (for Linux/PSQL support)
    * _multicore_ (for multicore support, still experimental and by default turned off and only linux)
  * pdftk http://www.accesspdf.com/pdftk/index.html, a RPM for openSUSE 11.2 64-Bit you will find here http://download.opensuse.org/repositories/home:/aljex/openSUSE_11.2/x86_64/
  * Evince PDF Viewer (or any other PDF Vierwer, this can be changed within the source code)


## Getting GEP-R ##
Checkout the source via subversion. See the [Source-Tab](http://code.google.com/p/gep-r/source/checkout) for more details.

## Running GEP-R ##
To run GEP-R change to the folder containing your local copy of the GEP-R code and start a R session within this folder. Within R execute

`source("gBefund.R")`

to get GEP-R running.


## Notes on Windows Installation ##
(so far tested on Windows XP 32-Bit and Vista 32-Bit)

Please install first:
  * pdftk for Windows http://www.accesspdf.com/pdftk/index.html
  * ImageMagick for Windows http://www.imagemagick.org/script/binary-releases.php?ImageMagick=r4fc8getv5t0elkfloki7skmp0#windows
  * MiKTeX http://miktex.org/ or fptex (prefered) http://tug.ctan.org/cgi-bin/ctanPackageInformation.py?id=fptex
  * Acroread PDF Viewver


## Notes on MacOS Installation ##
(so far tested on MacOS 10.5.8)

Please install first:
  * pdftk for MacOS http://www.pdflabs.com/docs/install-pdftk/
  * ImageMagick http://www.imagemagick.org/script/binary-releases.php#macosx
  * MacTeX http://www.tug.org/mactex/
  * PostgreSQL (for database functionality needed) http://www.postgresql.org/download/macosx