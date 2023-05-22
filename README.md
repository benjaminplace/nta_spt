# Non-Targeted Analysis - Study Planning Tool

# Currently under development!!! There are no guarantees of the stability or utility of this application.

## About
The Non-Targeted Analysis - Study Planning Tool (NTA-SPT) is a R/Shiny application that enables NTA researchers to develop study plans, or standard operating procedures, for an intended NTA study.

## Getting Started

To use this application: 

1. Ensure 4 v4.1+ is installed ([download](https://www.r-project.org/))
2. Download the project by cloning this repository or downloading the [zip file](https://github.com/benjaminplace/nta_spt/archive/refs/heads/main.zip). Unzip the file into a separate folder.
3. Run the compliance script, which will install everything needed for the application (this only needs to be done once).
	- Open R (or R Studio)
	- Set the working directory to the unzipped folder containing the project (easiest way is to type the command `setwd(choose.dir())`, press enter, and navigate to the folder)
	- Run the compliance file by typing `source('compliance.R')` and press enter
4. With the working directory in the zipped folder containing the project (see above), run the Shiny app via the command `shiny::runApp('current_version')` and the Shiny application should automatically launch in your preferred browser.

## Disclaimer

> Certain commercial equipment, instruments, software, or materials are identified in this documentation in order to specify the experimental procedure adequately. Such identification is not intended to imply recommendation or endorsement by the National Institute of Standards and Technology, nor is it intended to imply that the materials or equipment identified are necessarily the best available for the purpose.

> This work is provided by NIST as a public service and is expressly provided "AS IS." Please see the [license statement](LICENSE.md) for details.