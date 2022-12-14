# bcus-urchin-dynamics

All code and data required to reproduce the analysis and figures in the paper "Temporal dynamics of the deep-sea pink sea urchin *Stronylocentrotus fragilis* on the Northeast Pacific continental margin" by Command et al. 2022. The manuscript is currently undergoing peer-review in *Deep-sea Research I*. 

Raw data files were obtained from:
- Oceans 3.0 (https://data.oceannetworks.ca/DataSearch)
- NASA ()
- DFO ()

The analysis pipeline was built using the `targets` package.

## To run
1. Clone the repository.
```bash
git clone "https://github.com/rjcommand/bcus-urchin-dynamics"
```

2. Ensure `targets` is installed on your machine.

3. Open the .Rproj file and run `tar_make` in the console.
