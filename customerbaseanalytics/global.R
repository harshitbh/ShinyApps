## Clear workspace (important for debugging in interactive session)
rm(list=ls())

## Load packages
library(shiny) #web application framework for R.
library(BTYDplus)#Provides advanced statistical methods to describe and predict customers
#purchase behavior in a non-contractual setting
library(ggplot2)#Create Elegant Data Visualisations Using the Grammar of Graphics
library(plyr)#split data apart, do stuff to it, and mash it back together
library(scatterD3)#D3 JavaScript Scatterplot from R
library(plotly)#Easily translate 'ggplot2' graphs to an interactive web-based version and/or create custom
#web-based visualizations directly from R. 

#define variable datGlobal
datGlobal <- NULL