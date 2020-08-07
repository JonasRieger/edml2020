# Assessing the Uncertainty of the Text Generating Process using Topic Models
This repository provides data and scripts related to the paper: 

Rieger, J., Jentsch, C. &  Rahnenführer, J. (2020). Assessing the Uncertainty of the Text Generating Process using Topic Models. To appear in [EDML 2020.](https://imada.sdu.dk/Research/EDML/2020/)

For bug reports, comments and questions please use the [issue tracker](https://github.com/JonasRieger/edml2020/issues).

## Related Software
* [tmT](https://github.com/Docma-TU/tmT) is used to read the XML files and convert them to objects processable by ``tosca``.
* [tm](https://CRAN.R-project.org/package=tm) is used for preprocessing the text data.
* [tosca](https://github.com/Docma-TU/tosca) is used for managing and manipulating the text data to a structure requested by ``ldaPrototype``.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) is used to determine a prototype from a number of runs of Latent Dirichlet Allocation.
* [lubridate](https://lubridate.tidyverse.org/) is used as it makes dealing with dates little easier.
* [arrangements](https://randy3k.github.io/arrangements/) is used for faster computation of combinations.
* [zoo](http://zoo.r-forge.r-project.org/) is used for computation of rolling windows.

## Usage
All scripts are prefixed by numbers suggesting the order of execution. The script for data creation `1data.R` is the only script that unfortunately cannot be executed completely because we are not allowed to provide the raw data. The `2est...` scripts contain the bootstrap studies for the corresponding scenarios BS, BW and BSW. The calculation takes a long time and requires a lot of disk space (about 75GB). Please have a look at `2estBS.R` for a rough documentation of the code.  The other two scripts are similar in structure and uncommented. The script `3topics.R` computes 100 000 different scenarios of combinations of the bootstrapped texts and calculates the corresponding topic matrices, i.e. the frequency of each of the 50 topics per day, per week, per month or per year, respectively. Only the files on a daily basis are needed for further calculations. These values are then smoothed in `4computationFigure4and5.R` with the help of rolling windows. The 2.5 and 97.5% quantile, as well as the mean are stored in the file `roll.rds`. The calculation of all topic matrices and their rolling windows needs again plenty of time and nearly 50 GB disk space. Finally, scripts that start with a `5` provide code for generating the figures from the paper. The code is generic enough that the creation of further graphics can be done quickly by adjusting only a few statements. Figures 4, 5 and 6 can be generated without the time consuming calculations of `2est...`, `3topics.R` and `4computationFigure4and5.R`, because they only rely on `roll.rds` and the files in `coefficientvariation`.
