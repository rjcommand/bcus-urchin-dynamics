# bcus-urchin-dynamics

All code and data required to reproduce the analysis and figures in the paper "Temporal dynamics of the deep-sea pink sea urchin *Stronylocentrotus fragilis* on the Northeast Pacific continental margin" by Command et al. 2022. The manuscript has been published in *Deep-sea Research I* and is available  Open Access [here](https://doi.org/10.1016/j.dsr.2022.103958). 

Raw data files were obtained from:
- Oceans 3.0 (https://data.oceannetworks.ca/DataSearch)
- NASA Ocean Colour (https://oceancolor.gsfc.nasa.gov/l3/order/)
- DFO West Coast Vancouver Island Synoptic Bottom Trawl Survey (https://open.canada.ca/data/en/dataset/557e42ae-06fe-426d-8242-c3107670b1de)

```
bcus-urchin-dynamics/
|
|__ _targets/  # Contains all the `targets` objects
|__ _targets.R # Script that contains the `targets` pipeline
|__ bcus-urchin-dynamics.Rproj  # R Project file
|__ data/  # Contains all the data
|__ doc/  # Contains an .Rmd file with the reproducability report
|__ figures/  # Contains the figures produced by the pipeline
|__ packages.R  # List of packages used in the analysis
|__ R/  # Contains all the functions used in the pipeline
|__ README.md  # This file
|__ scripts/  # Contains experimental scripts etc.
```
The analysis pipeline was built using the `targets` package. A reproducibility report detailing package versions, etc. can be found in the docs/analysis.pdf file.

## To run
1. Clone the repository.
```bash
git clone "https://github.com/rjcommand/bcus-urchin-dynamics"
```

2. Ensure `targets` is installed on your machine. In RStudio, run:
```r
install.packages("targets")
```

3. Open the .Rproj file and run `tar_make` in the console.
