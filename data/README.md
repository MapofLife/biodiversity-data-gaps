## Data to support Oliver et al. 2021
#### Species Status Information Index (SSII) and Species Sampling Effectiveness Index (SSEI)
**ADD DATA DOI**

For full details see Oliver et al. 2021.

Explore at mol.org/indicators/coverage

### Attribution:
Ruth Y. Oliver, Carsten Meyer, Ajay Ranipeta, Kevin Winner, Walter Jetz. (2021) Global and national trends, gaps, and opportunities in documenting and monitoring species distributions, *PLOS Biology*

**ADD DOI**

### Overview:
All data and scripts to support figures in Oliver et al. 2021.

#### Data
Download and unzip the "data" folder, which includes the following files:


* **supplementary-table-1.csv** 
  + data to support Supplementary Table 1 and Figure 2
  + empirical species examples for the collared-peccary and jaguar
  + field names:
    + Species - scientific name of species
    + Year - year of data collection
    + Records (n) - total number of records collected
    + SSII - Species Status Information Index
    + SSEI - Species Sampling Effectiveness Index
  
  
* **supplementary-table-2.csv**
  + data to support Supplementary Table 2 and Figure 2
  + empirical national examples for the collared-peccary and jaguar
  + field names:
    + Country - country name as defined by GADM 3.6
    + Year - year of data collection
    + Records (n) - total number of records collected
    + National SSII
    + Steward's SSII
    + National SSEI
    + Steward's SSEI
    
* **supplementary-table-3.csv**
  + data to support Supplementary Table 3
  + national data coverage and sampling effectiveness
  + SSII and SSEI mean values for 2010-2019
  + field names:
    + Country - country name as defined by GADM 3.6
    + ISO3 - International Standard 3 character country code
    + National SSII
    + Steward's SSII
    + National SSEI
    + Steward's SSEI
    
* **supporting-data-figure3.csv**
  + data to support Figure 3
  + global data coverage and sampling effectiveness
  + SSII and SSEI annual values for 1950-2019 by taxa
  + field names:
    + year - year of data collection
    + ssii - mean SSII across species in taxa
    + ssei - mean SSEI across species in taxa
    + n_records - total records collected by taxa
    + prop_species - proportion of expected species with records by taxa
    + taxa - class of terrestrial vertebrates
    
* **supporting-data-figure3-replicates.csv**
  + data to support Figure 3
  + global data coverage and sampling effectiveness
  + SSII and SSEI annual values for 1950-2019
  + species replicates for generating mean values
  + field names:
    + year - year of data collection
    + ssii - mean SSII across species in taxa
    + ssei - mean SSEI across species in taxa
    + n_records - total records collected by taxa
    + taxa - class of terrestrial vertebrates
    
* **supporting-data-national-values.csv**
  + data to support Figures 4 & 5 and Supplementary Figures 1 & 2
  + national data coverage and sampling effectiveness
  + SSII and SSEI annual values for 1950-2019
  + field names:
    + country - country name as defined by GADM 3.6
    + ISO3 - International Standard 3 character country code
    + year - year of data collection
    + national_ssii - National SSII,  mean across terrestrial vertebrate classes
    + stewards_ssii - Steaward's SSII,  mean across terrestrial vertebrate classes
    + national_ssei - National SSEI,  mean across terrestrial vertebrate classes
    + stewards_ssei - Steaward's SSEI,  mean across terrestrial vertebrate classes
    + n_records - total number of GBIF records collected within country, year
    + prop_species_total - proportion of expected species with records across all terrestrial vertebrate species
    + national_ssii_amphibians, national_ssii_birds, national_ssii_mammals, national_ssii_reptiles - National SSII, mean across species within class
    + stewards_ssii_amphibians, stewards_ssii_birds, stewards_ssii_mammals, stewards_ssii_reptiles  - Steward’s SSII, mean across species within class
    + national_ssei_amphibians, national_ssei_birds, national_ssei_mammals, national_ssei_reptiles - National SSEI, mean across species within class
    + stewards_ssei_amphibians, stewards_ssei_birds, stewards_ssei_mammals, stewards_ssei_reptiles - Steward’s SSEI, mean across species within class

    
* **supporting-data-supp-figure3.csv**
  + data to support Supplementary Figure 3c
  + empirical demonstration of the effects of spatial resolution on SSII and SSEI
  + SSII and SSEI annual values for the glowing puffleg and white-sided hillstar
  + annual values for 2010-2019
  + computed at 3 spatial resolutions: 27.5, 55, and 110 km
  + based on Ellis-Soto et al. 2021
  + field names:
    + scientificname - scientificname of species
    + year - year of data collection
    + grid - spatial resolution of grid
    + ssii - SSII
    + ssei - SSEI
    
* **supporting-data-supp-figure3d-i.csv**
  + data to support Supplementary Figure 3c
  + empirical demonstration of the effects of spatial resolution on SSII and SSEI
  + SSII and SSEI annual values for the glowing puffleg and white-sided hillstar
  annual values for 2010-2019
  + computed at 3 spatial resolutions: 27.5, 55, and 110 km
  + based on Ellis-Soto et al. 2021
  + field names:
    + scientificname - scientificname of species
    + year - year of data collection
    + ssii_360 - SSII at 110 km
    + ssei_360 - SSEI at 110 km
    + ssii_720 - SSII at 55 km
    + ssei_720 - SSEI at 55 km
    + ssii_1440 - SSII at 27.5 km
    + ssei_1440 - SSEI at 27.5 km

