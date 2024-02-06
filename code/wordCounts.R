# Analyzing external review comments of the Nexus assessment 
#  1) Counting the frequency of words used in the comments 

# output: 
# 1) tidy data for further use in text mining 
# 2) plots of the frequency of words

# libraries
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)

# directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/RevComms/"
setwd(paste0(wdmain, "data/raw/nexus_fod"))


# get the comments from the FOD and clean ----
l <- list.files()
names <- gsub(".csv", "", l[grep(".csv", l)])
names <- gsub("nexus_first draft_external_review_","", names)

ch_comments <- lapply(l[grep(".csv", l)], read.csv)

lapply(ch_comments, colnames) # 2-4 have issues with an empty "column 1" column
for(i in 2:4)
{
  ch_comments[[i]] <- ch_comments[[i]][,1:12] # remove empty column
}
lapply(ch_comments, colnames) # fixed
# get rid of extraneous column only present in ch 7 df
ch_comments[[11]] <- head(ch_comments[[11]][,-2]) 
# need to add column indicating chapter bc the "chapter.reviewed" column doesnt actually do this
for(i in 1:length(ch_comments))
{
  ch_comments[[i]]$chapter <- names[i]
}
all.comments <- do.call(rbind, ch_comments)
summary(all.comments)

# save clean data
# dir.create(paste0(wdmain, "data/processed/nexus_FOD-review"))
setwd(paste0(wdmain, "data/processed/nexus_FOD-review/"))
write.csv(all.comments, "FOD_revComments.csv", row.names = FALSE)

# SOD ----
setwd(paste0(wdmain, "data/raw/nexus_sod"))
ch_comments <- read.csv("NXS_2nd_ext_rev_comments.csv")
unique(ch_comments$X5.X.chapter.if.applicable)

# need to recategorize chapter 5
ch_comments$chapter <- gsub("nexus_chapter", "Ch", ch_comments$chapter.reviewed)
ch_comments[grep("5.1_BIODIV", ch_comments$X5.X.chapter.if.applicable),]$chapter <- "Ch5_biodiversity"
ch_comments[grep("5.2_WATER", ch_comments$X5.X.chapter.if.applicable),]$chapter <- "Ch5_water"
ch_comments[grep("5.3_FOOD", ch_comments$X5.X.chapter.if.applicable),]$chapter <- "Ch5_food"
ch_comments[grep("5.4_HEALTH", ch_comments$X5.X.chapter.if.applicable),]$chapter <- "Ch5_health"
ch_comments[grep("5.5_CLIMATE", ch_comments$X5.X.chapter.if.applicable),]$chapter <- "Ch5_climate"

ch_comments <- ch_comments[,-c(10,11)]


# save clean data
# dir.create(paste0(wdmain, "data/processed/nexus_FOD-review"))
setwd(paste0(wdmain, "data/processed/nexus_SOD-review/"))
write.csv(ch_comments, "SOD_revComments.csv", row.names = FALSE)




# get common stop words ----

data("stop_words") 
stop_words
# add my own stopwords (after having seen these come up in the plots below)
myStops <- c("https", "al", "e.g", "i", "key", "section", "assessment", "assessments",
             "reference", "ipbes", "chapter", "chapters", "report", "add", "figure", "1")
myStops <- data.frame(word = myStops,
                         lexicon = c(rep("nonsense", length(myStops))))
stop_words <- rbind(stop_words, myStops)

# Count words used across all the chapters' comments ----
# read back in clean data
setwd(paste0(wdmain, "data/processed/"))
all.comments <- read.csv("nexus_FOD-review/FOD_revComments.csv")
colnames(all.comments)
# make exception for the word climate change
all.comments$comment <- gsub("climate change", "climate_change", all.comments$comment)
# unnest comments as word tokens
all.comments <- all.comments %>% 
  unnest_tokens(word, comment) %>% 
  anti_join(stop_words)
# get the count of most often used words
wcounts <- all.comments %>% 
  group_by(chapter) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))


plot <- wcounts %>%
  filter(n > 50) %>%
  filter(chapter != "glossary") %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#696B5F") +
  theme(axis.text=element_text(size=8))+
  facet_wrap(~chapter, ncol = 5, scales = "free") +
  theme_light() +
  labs(y = NULL, x = NULL)
plot


# save plot for FOD rev comms
setwd(paste0(wdmain, "output/"))
png("wordCounts_chapters.png", units = "px", width = 3000, height = 1700, res = 300)
plot
dev.off()

# save tidy data
setwd(paste0(wdmain, "data/processed/nexus_FOD-review/"))
write.csv(all.comments, "FOD_revComments_unnested.csv", row.names = FALSE)


# repeat for SOD ----
setwd(paste0(wdmain, "data/processed/"))
all.comments <- read.csv("nexus_SOD-review/SOD_revComments.csv")
colnames(all.comments)
# make exception for the word climate change
all.comments$comment <- gsub("climate change", "climate_change", all.comments$comment)
# unnest comments as word tokens
all.comments <- all.comments %>% 
  unnest_tokens(word, comment) %>% 
  anti_join(stop_words)
# get the count of most often used words
wcounts <- all.comments %>% 
  group_by(chapter) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))


plot <- wcounts %>%
  filter(n > 30) %>%
  filter(chapter != "nexus_glossary") %>%
  filter(chapter != "nexus_spm") %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#696B5F") +
  theme(axis.text=element_text(size=8))+
  facet_wrap(~chapter, ncol = 6, scales = "free") +
  theme_light() +
  labs(y = NULL, x = NULL)
plot

plot_spm <- wcounts %>%
  filter(n > 100) %>%
  filter(chapter == "nexus_spm") %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#696B5F") +
  theme(axis.text=element_text(size=8))+
  theme_light() +
  labs(y = NULL, x = NULL)
plot_spm

cowplot::plot_grid(plot, plot_spm, nrow =  2)



# save plot for FOD rev comms
setwd(paste0(wdmain, "output/"))
png("wordCounts_chapters_SOD.png", units = "px", width = 3000, height = 2000, res = 300)
cowplot::plot_grid(plot, plot_spm, nrow =  2)
dev.off()









