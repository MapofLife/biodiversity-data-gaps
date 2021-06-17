## Global and national trends, gaps, and opportunities in documenting and monitoring species distributions

### Data and scripts to support Oliver et al. 2021 
#### Species Status Information Index (SSII) and Species Sampling Effectiveness Index (SSEI)

> Oliver, R. Y., Meyer, C., Ranipeta, A., Winner, K., & Jetz, W. (2021). _Global and national trends, gaps, and opportunities in documenting and monitoring species distributions_ [Data set]. PLOS Biology. https://doi.org/10.48600/MOL-3Y3Z-DW77

### Description:
Trends in national biodiversity data coverage and sampling effectiveness for terrestrial vertebrates (1950-2019) as indexed by the Species Status Information Index (SSII) and the Species Sampling Effectiveness Index (SSEI). The National SSII measures how well, on average, species are documented in a given nation over a given timespane, in this case per year. The Steward’s SSII adjusts the National SSII based on nations’ stewardship of species, upweighting the documentation of species for which a nation has particularly high stewardship. For a given species, SSII quantifies the proportion of the range with data, but not how effectively this data is distributed across the proportion of the range it covers. We characterize sampling effectiveness by relating the realized spatial distribution of records to the ideal uniform distribution based on Shannon’s entropy normalized to vary between 0 and 1, a metric we call the Species Sampling Effectiveness Index (SSEI). As for the SSII, we provide unweighted (National SSEI) and weighted (Steward’s SSEI) formulations of the SSEI to account for national stewardship of species. 

Both indices rely on species expectations based on expert-based range maps and digitally available biodiversity observations from the Global Biodiversity Information Facility (GBIF). Species expectations and observations are linked based on custom-generated taxonomic synonym lists. 

For full details see Oliver et al. 2021.

Explore at mol.org/indicators/coverage

### Attribution:
Ruth Y. Oliver, Carsten Meyer, Ajay Ranipeta, Kevin Winner, Walter Jetz. (2021) Global and national trends, gaps, and opportunities in documenting and monitoring species distributions, *PLOS Biology*

**ADD MS DOI**

Oliver, R. Y., Meyer, C., Ranipeta, A., Winner, K., & Jetz, W. (2021). Global and national trends, gaps, and opportunities in documenting and monitoring species distributions [Data set]. PLOS Biology. https://doi.org/10.48600/MOL-3Y3Z-DW77


### Contact:
Ruth Oliver, ruth.oliver@yale.edu

Walter Jetz, walter.jetz@yale.edu

### Overview:
All data and scripts to support figures in Oliver et al. 2021.

#### Data
Download and unzip the "data" folder, which includes the following files:

* supplementary-table-1.csv
* supplementary-table-2.csv
* supplementary-table-3.csv
* supporting-data-figure3.csv
* supporting-data-figure3-replicates.csv
* supporting-data-national-values.csv
* supporting-data-supp-figure3c.sv
* supporting-data-supp-figure3d-i.csv

#### Scripts
The "scripts" folder includes the following files to reproduce figures:

* plot-figure2.R
* plot-figure3.R
* plot-figure4.R
* plot-figure4.R
* plot-supp-figure1.R
* plot-supp-figure2.R
* plot-supp-figure3.R

## Data sources
#### Species occurrence data:

* Global Biodiversity Information Facility (GBIF) 
  + GBIF.org (11 April 2020) GBIF occurrence download, https://doi.org/10.15468/dl.5gzpc3

#### Species distribution data:

* Birds - Jetz et al. 2012, https://mol.org/datasets/d542e050-2ae5-457e-8476-027741538965
* Mammals - IUCN (2017), https://mol.org/datasets/a7d5a735-22f9-4260-aa31-a4a4e7bf3029
* Amphibians - IUCN (2016), https://mol.org/datasets/83cfa8fb-dd6e-4031-8215-1079abddb8a7
* Reptiles - Roll et al. 2017, https://mol.org/datasets/f00b03ed-8345-4497-b194-3fced6ee155c 

#### Taxonomic information

* Birds
  + Master taxonomy: Clemenets et al. 2019
  + Additional sources: ITIS, IUCN, AviBase
* Mammals
  + Master taxonomy: Upham et al. 2019
  + Additional sources: Wilson and Reeder 2005, Mammal Diversity Database V2, IUCN, Wikipedia
* Amphibians
  + Master taxonomy: AmphibiaWeb 2019
  + Additional sources: ITIS, IUCN, Frost
* Reptiles
  + Master taxonomy: Uetz et al. 2019
  + Additional sources: ITIS, IUCN, Wikipedia
  
### References
AmphibiaWeb: Information on ampbhibian biology and conservation (2019). http://amphibiaweb.org

Clements, J.F., Schulenberg, T.S. Iliff, M.J., Billerman, S.M., Fredericks, T.A., Sullivan, B.L., & Wood, C.L. (2019). The eBird/Clements Checklist of Birds of the World: V2019

IUCN. (2016). International Union for Conservation of Nature--Red List of Threatened Species.

IUCN. (2017). International Union for Conservation of Nature--Red List of Threatened Species.

Jetz, W., Thomas, G.H., Joy, J.B., Hartmann, K., & Mooers, A.O. (2012) The global diversity of birds in space and time. *Nature*, 491(7424), 444-448.

Uetz, P., Freed, P., Hosek, J. (2019). The Reptile Database. http://www.reptile-database.org

Upham, N.S., Esselstyn, J.A., & Jetz, W. (2019). Inferring the mammal tree: Species-level sets of phylogenices for questions in ecology, evolution, and conservation. *PLOS Biology*, 17(12), e3000494. https://doi.org/10.1371/journal.pbio.3000494

