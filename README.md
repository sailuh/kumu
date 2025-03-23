# Kumu <a href="https://github.com/sailuh/kaiaulu"><img src="man/figures/logo.png" align="right" height="140" /></a>

> Kumu
> n.
> 1. Reason, cause, goal, justification, motive, grounds, purpose, object, why. 

## Overview

Kumu is an R package to facilitate data transformations and causal modeling. This project heavily relies on [Tetrad-CMD](https://github.com/cmu-phil/tetrad).

## Installation 

Kumu has been tested on OS X and Ubuntu. For Windows and other OS users, try [Virtualbox](https://www.virtualbox.org/),
[VMware](https://www.vmware.com/), or any other software to run virtual machines for Ubuntu. 


 1. Clone this repo 
 2. Open `kumu.Rproj` using RStudio
 4. Build the documentation `devtools::document(roclets = c('rd', 'collate', 'namespace'))`.
 5. Build Kumu (Top right pane in RStudio -> Build tab -> Install and Restart)


## Stay up-to-date

 * Read the [NEWS file](https://github.com/sailuh/kumu/blob/master/NEWS.md).
 
## How to cite Kumu 

If you are using Kumu in your research, please cite the following work: 

```
@article{Paradis:2024,
title = {A socio-technical perspective on software vulnerabilities: A causal analysis},
journal = {Information and Software Technology},
volume = {176},
pages = {107553},
year = {2024},
issn = {0950-5849},
doi = {https://doi.org/10.1016/j.infsof.2024.107553},
url = {https://www.sciencedirect.com/science/article/pii/S0950584924001587},
author = {Carlos Paradis and Rick Kazman and Mike Konrad},
keywords = {Software vulnerabilities, Socio-technical analysis, Causal modeling}
}
```
