# Ozone extreme downscaling using Latent Gaussian Models
## Overview
This repository contains the code, technical note and summary slides of the results of the project "Downscaling Ozone extremes using Latent Gaussian Models".
In this project, satellite data at 10 $\text{km}^2$ is used to fit an extreme value model with latent gaussian models that allow computing return levels for risk assesment and downscaling to 1 $\text{km}^2$ to allow risk assesment at high resolution.
The modeling strategy is two step:
- Remove seasonal effect from the extreme observations using a regression for extremes model with two pair of sine and cosine terms to account for cyclic variation.
- Model the *corrected* data using a Latent Gaussian Process with Matern covariance function to account for spatial dependence and downscaling of results to 1 $\text{km}^2$ resolution (return levels).

This project employs Bayesian inference for estimation of the parameters of the model, and their uncertainties. The package chosen was R-INLA due to is fast implementation of Latent Gaussian Models for spatial modeling. The technical details can be found in the [corresponding file](technical-report/00_technical-details-report.pdf) and a short presentation (slides) can be found [here](technical-report/ozone-thresholds-spanish.pdf)
