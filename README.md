# Welcome to the Solemon survey GitHub Page!!

This repo hosts the utilities used to collect and process data on board.
The explanation of the procedure used to collect data is detailed in the
[handbook](https://cnrfisheries.github.io/SoleMon_project/).

## Instructions for the use of this folder

### Set up the data collection folder on a new laptop

1. Get a unique identifier for yourself. It must be three letters
   (i.e.: FRA, PIE, CAC)
2. Download the entire folder on your laptop. This can be done by
   clicking the green button “Code” on the top right corner of this
   page, and then on “Dowload Zip”.
3. The file you have downloaded is called “SoleMon\_project-main.zip”.
   Unzip it.
4. Inside the “SoleMon\_project-main” there is a subfolder called
   “OnBoard”. Copy this file and paste it on your desktop
5. Rename the “OnBoard” file on your desktop as
   “OnBoard\_2024\_YOURIDENTIFIER”. For example, “OnBoard\_2024\_FRA”
6. Now you are all set up. Always use your new folder to collect data
   and follow the rest of the instructions

### How to use the data collection folder

The procedure reported here is also explained in more detail in the
[handbook](https://cnrfisheries.github.io/SoleMon_project/).

*Carefully check the version history section to make sure to use the
proper files in 2024*

1. To collect the data, please use the access files in the “access”
   subfolder. Follow the handbook for the terminology to be used when
   starting a new access and a new haul
2. The “data” subfolder contains information that is needed for the
   data processing. The only case you need to modify any file here is
   when you want to run data processing in loop. Check the handbook!
3. The R subfolder contains the code to process the data collected. The
   file you need to open is “workflow\_access\_v0.R”. **IMPORTANT**: when
   you use the script for the first time, make sure to add your working
   directory

## Version history

The access file and R code were modified over the years. In 2024, the
reference versions are:

* access file for data collection: bio\_data\_v2024\_SOLEMON\_template
* R workflow: workflow\_access\_v2024
* R supplementary functions: functions\_access\_v2024.R

A detailed history of past versions is coming

