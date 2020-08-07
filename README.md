# Assessing the Uncertainty of the Text Generating Process using Topic Models
This repository provides data and scripts related to the paper: 

Rieger, J., Jentsch, C. &  Rahnenführer, J. (2020). Assessing the Uncertainty of the Text Generating Process using Topic Models. To appear in [EDML 2020.](https://imada.sdu.dk/Research/EDML/2020/)

For bug reports and comments please use the [issue tracker](https://github.com/JonasRieger/edml2020/issues).

## Related Software
* [tmT](https://github.com/Docma-TU/tmT) is used to read the XML files and convert them to objects processable by ``tosca``.
* [tm](https://CRAN.R-project.org/package=tm) is used for preprocessing the text data.
* [tosca](https://github.com/Docma-TU/tosca) is used for managing and manipulating the text data to a structure requested by ``ldaPrototype``.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) is used to determine a prototype from a number of runs of Latent Dirichlet Allocation.
* [lubridate](https://lubridate.tidyverse.org/) is used as it makes dealing with dates little easier.
* [arrangements](https://randy3k.github.io/arrangements/) is used for faster computation of combinations.
* [zoo](http://zoo.r-forge.r-project.org/) is used for computation of rolling windows.


## Usage
