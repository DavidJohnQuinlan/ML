# ML - Repository containing all things Data Science.

This repository contains a number of different statistical modelling and machine learning projects. All of the projects were implemented using either **R** or **Python**. 

The repository is split up into folders where each folder represents a certain statistical or machine learning topic. Each folder subsequently contains number of further subfolders, where each subfolder contains an example of that particular statistical or machine learning algorithm applied to a real world dataset. Each subfolder is self contained, with the majority of subfolders containing the associated dataset of interest. In some cases the dataset was too large to store on github, thus, it will be required to download it from the web, details of the necessary steps to download such datasets are documented within each subfolder. 

Below is a brief summary of the types of models applied within this repository:

Table of Contents
=================

<!--ts-->
* [Clustering](https://github.com/DavidJohnQuinlan/ML/tree/master/Clustering/city_of_chicago_dataset)
  * The clustering folder contains an example of the application of the k-means and hierarchical clustering algorithms to the Chicago crime dataset. Some interesting visualisations where also created of the crime rates in the City of Chicago based on these clustering results. 
* [Neural Networks](https://github.com/DavidJohnQuinlan/ML/tree/master/Neural_Networks)
  * The nerual networks folder contains a small notebook which delves into the inner workings of a neural network model. The main aim of the notebook is to provide an easy understanding of how a neural network model operates. The neural network is applied to the MNIST dataset. 
* [Convolutional Neural Networks](https://github.com/DavidJohnQuinlan/ML/tree/master/Convolutional_Neural_Networks)
  * The CNN folder contains a notebook detailing the implementation of the DecovNet (Deconvolutional Network) visualisation technique as defined initially by Zeiler and Fergus. This technique is also applied to the MNIST dataset. 
* [Linear Regression](https://github.com/DavidJohnQuinlan/ML/tree/master/Linear_Regression)
  * The linear regression folder contains two projects:
    1. Applied a linear regression model to the feul consumption dataset. 
    2. Applied a linear regression model to the KBB Automobile dataset.
* [Poisson Regression](https://github.com/DavidJohnQuinlan/ML/tree/master/Poisson_Regression)
  * The Poisson regression folder contains two projects: 
    1. Applied a Poisson regression, Linear regression and Poisson generalised additive model (GAM) to the bike sharing dataset. 
    2. Applied a Poisson offset regression model to the smoking and lung cancer dataset.  
* [RShiny](https://github.com/DavidJohnQuinlan/ML/tree/master/Rshiny_Application)
  * The RShiny folder contains a project where an RShiny application was created which allows easy user interaction with the Left ventricular hypertrophy Dataset. The application allows a user to examine summary statistics, marginal histograms, boxplots and density plots of the dataset. The user can also apply a linear, logistic or Poisson regression model to the dataset based on their exact requirements. 
<!--te-->
