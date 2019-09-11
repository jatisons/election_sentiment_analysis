library(stringr)
library(tidytext)
library(tidyr)
library(dplyr)
library(broom)
library(jpeg)

#load the dataset
all_trump_tweets <- read.csv("all_trump_tweets.csv")


#subset the data to before, during, and after the campaign

#Campaign announcement June 16, 2015
all_trump_tweets[22183,]
#Election
all_trump_tweets$text[29964]

# Create a vector for labeling the dataset
trump_groups <- rep("NA",30795)
trump_groups[1:22182] <- "before"
trump_groups[22183:29963] <- "during"
trump_groups[29964:30795] <- "after"

#Add the vector to the dataset
all_trump_tweets$campaign_groups <- trump_groups

#Regular Expresion pattern to pull word tokens from tweets
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

#create a set of tokenized words, removing stop words and weblinks
trump_words <- all_trump_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#Pull the needed word lists fromt the NRC Lexicon
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

#Take the word tokens, and group them according to the campaign stage
c_groups <- trump_words %>%
  group_by(campaign_groups) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id_str, campaign_groups, total_words)

#Join the tokenized words with the NRC list by matching
#the words to the emotion/sentiment and then count them by
#the campaign group/sentiment tag
sentiment_by_cgroup <- trump_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment,id_str) %>%
  ungroup() %>%
  complete(sentiment, fill = list(n = 0)) %>%
  inner_join(c_groups) %>%
  group_by(campaign_groups, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

#Create a new variable to standardize the counts as a
#proportion of the tweets in each group
sentiment_by_cgroup$portion_of_group <- (sentiment_by_cgroup$words/sentiment_by_cgroup$total_words) * 100


#create datafile of results
write.csv(sentiment_by_cgroup,file = "results",quote = FALSE)



##Create visualizations of each emotion/sentiment group

#Angry
back_img <- readJPEG("angry.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(11,21,1)]
main_title <- "Anger"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")


#Anticipation
back_img <- readJPEG("anticipate.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(12,22,2)]
main_title <- "Anticipation"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")


#Disgust
back_img <- readJPEG("disgust.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(13,23,3)]
main_title <- "Disgust"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Disgust
back_img <- readJPEG("disgust.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(13,23,3)]
main_title <- "Disgust"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Fear
back_img <- readJPEG("fear.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(14,24,4)]
main_title <- "Fear"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Joy
back_img <- readJPEG("joy.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(15,25,5)]
main_title <- "Joy"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Negative
back_img <- readJPEG("negative.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(16,26,6)]
main_title <- "Negative"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Positive
back_img <- readJPEG("positive.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(17,27,7)]
main_title <- "Positive"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Sadness
back_img <- readJPEG("sadness.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(18,28,8)]
main_title <- "Sadness"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Surprise
back_img <- readJPEG("surprise.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(19,29,9)]
main_title <- "Surprise"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")

#Trust
back_img <- readJPEG("trust.jpg")
plt_data <- sentiment_by_cgroup$portion_of_group[c(20,30,10)]
main_title <- "Trust"

plot(plt_data, type='n', main=main_title, ylab="Percentage of Total Words",xlab = "", xaxt = "n")
v1 <- c(1,2,3)
v2 <- c("Before","During","After")
axis(side = 1,at = v1,labels = v2)

lim <- par()
rasterImage(back_img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#grid()

lines(plt_data, type="b", lwd=5, col="#BD3B0C")


















