# Analyzing external review comments of the Nexus assessment 
#  1) Counting the frequency of words used in the comments 

# output: 1) tidy data for further use in text mining 2) plots of the frequency of words

# libraries
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)

# directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/RevComms/"
setwd(paste0(wdmain, "data/raw/"))



# get the comments from the FOD and clean ----
l <- list.files()
names <- gsub(".csv", "", l[grep(".csv", l)])
names <- gsub("nexus_first draft_external_review_","", names)

ch_comments <- lapply(l[grep(".csv", l)], read.csv)

lapply(ch_comments, colnames)
# 2-4 have issues with an empty "column 1" column
for(i in 2:4)
{
  ch_comments[[i]] <- ch_comments[[i]][,1:12] # remove empty column
}
lapply(ch_comments, colnames)
# get rid of extraneous column only present in ch 7 df
ch_comments[[11]] <- head(ch_comments[[11]][,-2]) 
# need to add column indicating chapter bc the chapter.reviewed column doesnt actually do this
for(i in 1:length(ch_comments))
{
  ch_comments[[i]]$chapter <- names[i]
}
all.comments <- do.call(rbind, ch_comments)
summary(all.comments)

# clean ----
# get common stop words
data("stop_words") 
stop_words
# add my own stopwords (after having seen these in the plots below)
myStops <- c("https", "al", "e.g", "i", "key", "section", "assessment", "assessments",
             "reference", "ipbes", "chapter", "chapters", "report", "add", "figure", "1")
myStops <- data.frame(word = myStops,
                         lexicon = c(rep("nonsense", length(myStops))))
stop_words <- rbind(stop_words, myStops)

# Count words used across all the chapters' comments ----
# (note - maybe instead of these loops i could improve this by making one data set and then facet wrapping. to be improved)

# make data tidy and get word counts


# CONTINUE HERE: MAKE NOT LISTS.


wcounts <- list()
for(i in 1:length(ch_comments))
{
  # make exception for the word climate change
  ch_comments[[i]]$comment <- gsub("climate change", "climate_change", ch_comments[[i]]$comment)
  ch_comments[[i]] <- ch_comments[[i]] %>% 
    unnest_tokens(word, comment) %>% 
    anti_join(stop_words) 
  # keep the word count as a separate list, so it's possible to see all the nonsense
  wcounts[[i]] <- ch_comments[[i]] %>% count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n))
}

plots <- list()
# plot word counts for each of the chapters
# make one plot for 1-4
for(i in 1:length(wcounts))
{
  plots[[i]] <- wcounts[[i]] %>%
    filter(n > 50) %>% # note: i tested other filters (40, 35, 25, 10) and 50 worked best
    ggplot(aes(n, word)) +
    geom_col() +
    theme(axis.text=element_text(size=8))+
    labs(y = NULL, x = NULL)
}

# make and save plots for all chapters 
setwd(paste0(wdmain, "output/"))
png("wordCounts_chapters.png", units = "px", width = 3000, height = 1700, res = 300)
cowplot::plot_grid(plots[[1]], 
                   plots[[2]], 
                   plots[[3]], 
                   plots[[4]], 
                   plots[[5]], 
                   plots[[6]], 
                   plots[[7]], 
                   plots[[8]], 
                   plots[[9]], 
                   plots[[10]], 
                   # plots[[11]], # excluded ch 7 and the glossary for cleaner
                   # plots[[12]], 
                   scale = 0.9,
                   nrow = 2, ncol = 5,
                   labels = names,
                   label_size = 8,
                   axis = "rlbt",
                   label_y = 1)
dev.off()


# save tidy data
dir.create(paste0(wdmain, "data/processed/nexus_FOD-review"))
setwd(paste0(wdmain, "data/processed/nexus_FOD-review/"))

names(ch_comments) <- names
for(i in 1: length(ch_comments))
{
  write.csv(ch_comments[[i]], paste0("unnestedRevComms_", names[i], ".csv"), row.names = FALSE)
}
















