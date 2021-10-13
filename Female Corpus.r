##### K E Y
### SECTION HEADER
## INDIVIDUAL QUESTION
# NOTES

### LOADING LIBRARIES
library(gutenbergr)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
data("stop_words")

### FORMATION OF THE CORPUS
# done manually to ensure no filler / copyright text in final product

# The Voyage Out by Virginia Woolf
WC01 <- gutenberg_download(144, meta_fields = c("title", "author"))
WC01 <- WC01[42:14948, ]
# Middlemarch by George Eliot (Mary Ann Evans)
WC02 <- gutenberg_download(145, meta_fields = c("title", "author"))
WC02 <- WC02[195:33280, ]
# Emma by Jane Austen
WC03 <- gutenberg_download(158, meta_fields = c("title", "author"))
WC03 <- WC03[80:16486, ]
# Sense and Sensibility by Jane Austen
WC04 <- gutenberg_download(161, meta_fields = c("title", "author"))
WC04 <- WC04[72:12671, ]
# Summer by Edith Wharton
WC05 <- gutenberg_download(166, meta_fields = c("title", "author"))
WC05 <- WC05[14:5937, ] 
# The Princess of Cleves by Madame de La Fayette
WC06 <- gutenberg_download(467, meta_fields = c("title", "author"))
WC06 <- WC06[13:5249, ] 
# The Age of Innocence by Edith Wharton
WC07 <- gutenberg_download(541, meta_fields = c("title", "author"))
WC07 <- WC07[20:11557, ] 
# Agnes Grey by Anne Bronte
WC08 <- gutenberg_download(767, meta_fields = c("title", "author"))
WC08 <- WC08[51:6981, ] 
# Wuthering Heights by Emily Bronte
WC09 <- gutenberg_download(768, meta_fields = c("title", "author"))
WC09 <- WC09[11:12314, ] 
# The Tenant of Wildfell Hall by Anne Bronte
WC10 <- gutenberg_download(969, meta_fields = c("title", "author"))
WC10 <- WC10[480:18458, ] 
# Anthem by Ayn Rand
WC11 <- gutenberg_download(1249, meta_fields = c("title", "author"))
WC11 <- WC11[319:2518, ] 
# Jane Eyre by Charlotte Bronte
WC12 <- gutenberg_download(1260, meta_fields = c("title", "author"))
WC12 <- WC12[147:21001, ] 
# Pride and Prejudice by Jane Austen
WC13 <- gutenberg_download(1342, meta_fields = c("title", "author"))
WC13 <- WC13[142:14199, ] 
# Wives and Daughters by Elizabeth Gaskell
WC14 <- gutenberg_download(4274, meta_fields = c("title", "author"))
WC14 <- WC14[145:28840, ] 
# Evelina by Fanny Burney
WC15 <- gutenberg_download(6053, meta_fields = c("title", "author"))
WC15 <- WC15[345:19732, ] 
# The Enchanted April by Elizabeth Von Arnim
WC16 <- gutenberg_download(16389, meta_fields = c("title", "author"))
WC16 <- WC16[13:8833, ] 
# The Young Visiters by Daisy Ashford
WC17 <- gutenberg_download(21415, meta_fields = c("title", "author"))
WC17 <- WC17[279:1859, ] 
# Ruth Hall by Fanny Fern
WC18 <- gutenberg_download(40814, meta_fields = c("title", "author"))
WC18 <- WC18[371:9011, ] 
# Memoirs of Emma Courtney by Mary Hays
WC19 <- gutenberg_download(41256, meta_fields = c("title", "author"))
WC19 <- WC19[180:7545, ] 
# The Female Quixote by Charlotte Lennox
WC20 <- gutenberg_download(50054, meta_fields = c("title", "author"))
WC20 <- WC20[93:16515, ] 
# The Black Moth by Georgette Hayer
WC21 <- gutenberg_download(38703, meta_fields = c("title", "author"))
WC21 <- WC21[37:13797, ] 
# IN CASE THE MIRROR PORTION NEEDS TO BE REINSERTED:
# , [hit_enter_here] mirror = "http://mirrors.xmission.com/gutenberg/"
# PLACE BETWEEN ))
WomensCorpus <- rbind(WC01, WC02, WC03, WC04, WC05, WC06, WC07, WC08, WC09, WC10,
                      WC11, WC12, WC13, WC14, WC15, WC16, WC17, WC18, WC19, WC20,
                      WC21) %>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)

### PROJECT QUESTIONS

## A: TOP 20 NON-STOP WORDS
WomensCorpusCount <- WomensCorpus %>%
  count(word, sort=TRUE)
WomensCorpusCount%>%
  mutate(word = reorder(word, n))%>%
  head(20)%>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab(NULL)+
  ylab("Number of Occurrences")+
  ggtitle("Most Common Words, Women's Corpus")
# ANSWER: (descending order) time, miss, sir, lady, day,
# eyes, mind, house, love, molly, dear, life, hand, looked,
# heart, told, people, home, heard, father

### BING LEXICON - QUESTIONS B-D
BING <- get_sentiments("bing")
WomensCorpusBING <- WomensCorpus %>%
  inner_join(BING)%>%
  count(title, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentimentratio = negative/(positive+negative)*100)

## B: 10 MOST NEGATIVE NOVELS
WomensCorpusBING%>%
  mutate(title = reorder(title, sentimentratio))%>%
  arrange(sentimentratio)%>%
  head(10)%>%
  ggplot()+
  aes(sentimentratio, title)+
  geom_col()+
  xlab("Percent Negative")+
  ylab(NULL)+
  ggtitle("Most Negative Love Stories by Women")
# ANSWER: (descending order) ruth hall, memoirs of emma courtney,
# wives and daughters, the female quixote, the enchanted april,
# the princess of cleves, sense and sensibility, pride and prejudice, emma,
# the young visiters

## C: OVERALL SENTIMENT
sum(WomensCorpusBING$sentiment)
# ANSWER: -18349

## D: TOP 15 POSITIVE / 15 NEGATIVE WORDS OVERALL
# positive
WCBpos <- WomensCorpusCount%>%
  inner_join(BING)%>%
  arrange(sentiment)%>%
  filter(sentiment=="positive")%>%
  head(15)
WCBpos%>%
  mutate(word = reorder(word, n)) %>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab(NULL)+
  ylab("Occurrences")+
  ggtitle("Most Used Positive Words, Women's Corpus")
# ANSWER: (descending order) love, happy, pleasure, glad, pretty, fine, ready,
# smile, happiness, strong, master, silent, affection, pleased, quiet

# negative
# COMPLICATION: the word 'miss' is likely used more frequently in these novels
# to address an unmarried woman, as opposed to longing for an absent person.
# to that end, this code reflects a version where 'miss' has been removed.
BINGmiss <- BING%>%
  filter(word!="miss")
WCBneg <- WomensCorpusCount%>%
  inner_join(BINGmiss)%>%
  arrange(sentiment)%>%
  filter(sentiment=="negative")%>%
  head(15)
WCBneg%>%
  mutate(word = reorder(word, n)) %>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab(NULL)+
  ylab("Occurrences")+
  ggtitle("Most Used Negative Words, Women's Corpus")
# ANSWER: (descending order) poor, doubt, fear, afraid, death, cold, bad,
# trouble, strange, dark, hard, lost, fell, wrong, impossible

### NRC LEXICON - QUESTIONS E&F
NRC <- lexicon_nrc()

## E: TOP 15 JOY & ANGER WORDS
# joy
NRCjoy <- NRC %>%
  filter(sentiment == "joy")
WCjoy <- WomensCorpus%>%
  inner_join(NRCjoy)%>%
  count(word, sort = TRUE)%>%
  top_n(15)
WCjoy%>%
  mutate(word = reorder(word, n)) %>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab(NULL)+
  ylab("Occurrences")+
  ggtitle("Top Joyful Words, Women's Corpus")
# ANSWER: (descending order) love, mother, found, hope, friend, happy,
# child, feeling, money, daughter, glad, pretty, true, deal, marriage

# anger
NRCanger <- NRC %>%  
  filter(sentiment == "anger")
WCanger <- WomensCorpus%>%
  inner_join(NRCanger)%>%
  count(word, sort = TRUE)%>%
  top_n(15)
WCanger%>%
  mutate(word = reorder(word, n)) %>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab(NULL)+
  ylab("Occurrences")+
  ggtitle("Top Angry Words, Women's Corpus")
# ANSWER: (descending order) words, feeling, ill, money, fear, death,
# bear, bad, spite, court, angry, difficulty, unhappy, mistress, broken

## F: WHICH NOVEL IN CORPUS HAS HIGHEST DISGUST
NRCdisgust <- NRC %>%
  filter(sentiment == "disgust")
WCdisgust <- WomensCorpus%>%
  inner_join(NRCdisgust)%>%
  count(title, sort=TRUE)
# ANSWER: Middlemarch

### AFINN LEXICON - QUESTIONS G&H
AFINN <- lexicon_afinn()
WomensCorpusAFINN <- WomensCorpus%>%
  inner_join(AFINN)%>%
  group_by(title)%>%
  summarise(sentiment = sum(value))
# ANSWER:
# positive: Emma (2543) Pride and Prejudice (2135) Sense and Sensibility (1695),
#           The Enchanted April (1331) Middlemarch (795)
# negative: Wuthering Heights (-3105), The Female Quixote (-1270),
#           Evelina (-1118), Summer (-893) Jane Eyre (-682)
# NOTE: these answers are only for the women's corpus, they need to be
# tallied against the other corpus for the final answer.
