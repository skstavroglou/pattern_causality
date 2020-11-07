# Pattern Causality Method
##### for the replication of results found in:
## Unveiling Causal Interactions in Complex Systems
###### DOI: https://doi.org/10.1073/pnas.1918269117
##### AUTHORS: 
Stavros K. Stavroglou (University of Liverpool) <br />
Athanasios A. Pantelous (Monash University) <br />
H. Eugene Stanley (Boston University) <br />
Konstantin M. Zuev (Caltech) <br />

#### USAGE:

For all calculations simply run the patternCausality function for it
to be available in your current **R** environment.
<br />
<br />
Function Parameters:
<br />
**_X_**: time series corresponding to the causal variable.
<br />
**_Y_**: time series corresponding to the affected variable.
<br />
**_E_**: the embedding dimension for the attractors.
<br />
**_tau_**: the time-delay for the attractor reconstruction.
<br />
**metric**: the distance metric for the calculation of nearest neighbors.
<br />
**_h_**: the prediction horizon. 
<br />
<br />
For the parameters used in the applications read the main paper and the supplementary information.
<br />
<br />
Unless treated otherwise any missing value is replaced with 0 for the specific applications.
<br />
<br />
The algorithm does not handle missing values.

#### 1] Guidelines for the Arizona Ecosystem application

STEP 1: Go to https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/15-2115.1 and download "ecy1360-sup-0001-DataS1.zip" from the supporting information section.
<br />
<br />
STEP 2: Keep the files relevant for Rodents, Plants, Ants and the Weather.
<br />
<br />
STEP 3: Find the first and last common years for the four aforementioned categories. Filter the data accordingly for the four categories to be in the same time period.
<br />
<br />
STEP 4: Fill in the missing values with linear regression approximations.
<br />
<br />
STEP 5: Calculate the causality network by taking all possible pairs using the patternCausality function.

#### 2] Guidelines for the Alcoholic and Control EEG application

STEP 1: You can either use the dataset from http://archive.ics.uci.edu/ml/machine-learning-databases/eeg-mld/ or for more convenience use the "data(eegdata)" from the "eegkitdata" **R** package.
<br />
<br />
STEP 2: For each subject and for each trial, calculate the underlying EEG pattern causality network using the 64 electrodes as nodes. The result here is the three aspects (positive, negative, dark) of the EEG network for each trial of each subject.
<br />
<br />
STEP 3: Average over the 5 trials for each subject and for each aspect separately.
<br />
<br />
STEP 4: To compare the alcoholic versus control network structures, average over the 10×3 networks (for each of the 3 aspects separately) for each type of subject (alcoholic and control).
<br />
<br />
STEP 5: You should have at this point 1 positive, 1 negative and 1 dark average alcoholic EEG network and 1 positive, 1 negative and 1 dark average control EEG network.

#### 3] Guidelines for the Banking CDS application

STEP 1: Using the Thomson Reuters **Datastream** download time series for the CDS found in Table S4 in the Supporting Information (https://www.pnas.org/content/117/14/7599/tab-figures-data).
<br />
<br />
STEP 2: Calculate the causality network by taking all possible pairs using the patternCausality function.
<br />
<br />
STEP 3: Calculate the out-strength centrality of each CDS for each aspect (positive/negative/dark) separately.
<br />
<br />
STEP 4: Calculate the in-strength centrality of each CDS for each aspect (positive/negative/dark) separately.