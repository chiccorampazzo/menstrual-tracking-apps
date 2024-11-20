# "UPDATE: I'm pregnant!": Inferring global downloads and reasons for using Menstrual Tracking Apps

This repository contains the data and code accompanying the following publication:

> Rampazzo F, Raybould A, Rampazzo P, Barker R, Leasure DR. 2024. "UPDATE: I'm pregnant!": Inferring global downloads and reasons for using Menstrual Tracking Apps. *Digital Health*. https://doi.org/10.1177/20552076241298315

A SocArXiv pre-print of the manuscript is available from: https://doi.org/10.31235/osf.io/2va3n

## Project Description

This project investigates the global usage of fertility tracking apps based on download, review, and ratings data from the Google Play Store and Apple App Store. It is the first comprehensive study to quantify fertility tracking app use globally, extending beyond the Global North, and includes analysis of macro- and micro-level predictors of app usage.

Key findings include:
- Dominance of three major apps (Clue, Flo, and Period Tracker) in the market.
- Strong associations between modern contraceptive technology, internet access, and app downloads.
- Variability in app usage in low-income countries, linked to unmet family planning needs and total fertility rates.
- Topic analysis revealing primary app uses: period tracking, conception efforts, community engagement, and pregnancy avoidance.

The data and Bayesian analysis code in this repository allow for the replication of the analyses presented in the paper.

## Table of Contents
- Usage
- Contributing
- License
- Contact
- Acknowledgments

## Usage

### Bayesian analysis
The data and code to reproduce the Bayesian analysis are provided in the `./analysis/` folder. To run the analysis, follow these steps:

1. Ensure you have installed all required dependencies, including the [JAGS software](https://mcmc-jags.sourceforge.io/).
2. Using R, run the analysis script:
   `./analysis/1_mcmc.R`
   This script will run variations of the Bayesian model defined in `./analysis/model.jags.R` using the data provided in `./data/`. 
4. You can run out-of-sample cross-validation and other evaluation metrics using the scripts `./analysis/2_xval.R` and `./analysis/3_eval.R`. 
5. Output results, including figures and summary tables, will be saved in the `./analysis/out/` folder.

### Topic modelling
The data and code to reproduce the topic modelling are provided in the `./topic_model/` folder. The run the topic modelling analysis, follow these steps:

1. Ensure you have installed all of the libraries and packages requires for the analysis (listed in the first lines of each script).
2. Using R, clean the web-scraped data (`01_language_filtering.R`).
     This requires the web-scraped reviews to be stored locally.
3. In Jupyer Python, Once complete, run the python files sequentially - first English (the primary script for the results), Portuguese, and Spanish.
4. Return to R (`04_figures.R`) to interpret the results of the python-generated topic model. Here, you will also need to create 'broader groups' of topics, or alternatively, run the analysis with each topic being separate. We did this manually in a csv file once interpreting the individual categories and grouping them according to their similaraties. Scripts also exist for the Portuguese Spanish-trained text models. 

## Contributing

We welcome contributions to improve or extend the analyses. Feel free to fork this repository, make your modifications, and submit a pull request.

1. Fork the repository.
2. Create a new branch (`git checkout -b feature-branch`).
3. Commit your changes (`git commit -am 'Add new feature'`).
4. Push to the branch (`git push origin feature-branch`).
5. Submit a pull request.

## License

This repository is licensed under the GNU General Public License v3.0 (GPLv3). See the LICENSE file for details.

## Contact

For questions or further information, please contact: Dr. Francesco Rampazzo (@chiccorampazzo), [francesco.rampazzo@demography.ox.ac.uk](mailto:Francesco.rampazzo@demography.ox.ac.uk)

For inquiries related to the publication, you may consult the peer-reviewed article in _Digital Health_: https://doi.org/10.1177/20552076241298315. 

The paper is also available as a SocArXiv pre-print: https://doi.org/10.31235/osf.io/2va3n.

## Acknowledgments

This work was funded by the Leverhulme Trust through the Leverhulme Centre for Demographic Science. We would like to thank Tommaso Rigon, Jakub Bijak, Jason Hilton, and Claire Dooley for valuable feedback that improved initial versions of the Bayesian model, and the Evolutionary Demography Group at LSHTM for their feedback on the paper. Special thanks to the reviewers and editorial team at *Digital Health* for their feedback.
