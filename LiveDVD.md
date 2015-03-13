#GEP-R LiveDVD Short Manual

# Get started #

  1. Download the Live-DVD .iso-file and burn it on DVD
  1. Boot the System from  the Live-DVD (note: english keyboard layout is used) or  alternatively use a VirtualMachine (e.g Virtualbox, VMWare to mount the iso-file)
  1. Follow the instructions from the README file on the Desktop to start GEP-R GUI:
    * To start the GEP-R GUI please open the Terminal and enter
> > > `cd gep-r-read-only` 

&lt;Enter&gt;


    * to change to the gep-r source directory
> > > `R` 

&lt;Enter&gt;


    * to start the R session and within R enter
> > > `source("gBefund.R")` 

&lt;Enter&gt;


  1. To load a .CEL-File, click “open” and select a .CEL-File (testfiles are in Desktop/CEL-Files)
  1. On the tab “GEP Analysis” hit “Run Analysis”; wait ~20 min.
  1. After this is finished, you can fill out the fields in the GEP-R
  1. Before creating the PDF you need to check for integrity (i.e. whether all fields are filled out and patient data are validated). Eventual Warnings can be examined in the “Warnings tab” (for testing you can use the “Ignore” Button, e.g. if you  not wish to fill out all the fields)
  1. Create the PDF by hitting the “Create PDF” button

## File-Menu from GEP-R: ##

**File - Open Report**: lets you open a previously saved .report file

**File - Save Report**: lets you save a patient report (note: after a sample file is processed, there will be created a subfolder within the "save“ folder named after the .CEL-file containing the .report file)


---


**Note**: Due to the concept of a live-DVD, it is not possible to store the result of the analysis (above). If you want to do so, however, this “workaround” allows saving the GEP-R (or importing a saved report) on/from an USB-stick (or other USB-device):

Copy  to an USB-Stick

Within the Ubuntu menu go to Places → Home Folder → gep-r-read-only and copy the folders /save and /reports to USB-stick

Copy from an USB-Stick

To use previously saved patient reports copy the folders /save and /report from the USB-Stick to the gep-r-read-only folder. After that you can load the reports within the GUI by File → Open Report


---


**Known Problem**: MacBook -> "No bootable device -- insert boot disk and press any key"
Solution: Hold the option key (alt) as you turn on the machine, then select Macintosh HD and it will boot you back into OS X.