# Analyzing external review comments of the Nexus assessment 
# output: tidy data for further use in text mining
# plots of the frequency of words countes

# libraries
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)


# directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/RevComms/"
setwd(paste0(wdmain, "data/raw/"))


# sentiment analysis ----
# The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of 
# positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
# bing lexicon categorizes words in a binary fashion into positive and negative categories
# AFINN lexicon assigns words with a score that runs between -5 and 5, 
# with negative scores indicating negative sentiment and positive scores indicating positive sentiment.
