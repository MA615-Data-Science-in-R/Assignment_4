#Install the relevant packages
# devtools::install_github("Truenumbers/tnum/tnum")

# Load relevant packages
library(gutenbergr)
library(tidyverse)
library(tidytext)
library(stringi)
library(textdata)
library("magrittr")
library(ggplot2)
library(wordcloud)
library(tnum)

data(stop_words)

# read in your book

hod_txt <- readLines("hod.txt")
heartofdarkness <- as.data.frame(hod_txt)

# OR hod <- gutenberg_download(219)

# Divide each word in a sentence into seperate rows, remove stop words
# Create a chapter and linenumber column

tidy_hod <- heartofdarkness %>% mutate(linenumber = row_number(),
                                        chapter = cumsum(str_detect(hod_txt, 
                                                                    regex("Chapter [\\divxlc]",  ignore_case = TRUE))))

tidy_hod <- tidy_hod %>%
  unnest_tokens(word, hod_txt) %>%
  anti_join(stop_words) 


# Look at the most common words in the book
common1 <- tidy_hod %>%
  count(word, sort = TRUE)

common <- common1[-3,]

# Read in the sentiments NRC, BING and AFINN and create a dataframe for each

afinn <- tidy_hod %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = chapter) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing <- tidy_hod %>% 
  inner_join(get_sentiments("bing")) %>%
  mutate(method = "BING") %>% 
  count(method, index = chapter, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

nrc <- tidy_hod %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(method = "NRC") %>% 
  count(method, index = chapter, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Bind the dataframes together
             
alllex <- bind_rows(afinn, bing, nrc)

# Words that contributed

afinn_words <- tidy_hod %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(word) %>% 
  summarise(sentiment = sum(value))

afinn_words <-afinn_words[order(afinn_words$sentiment),]

afinn_word_neg <- ggplot(data = head(afinn_words,10)) + geom_bar(mapping = aes(x = word, y = sentiment), stat = "identity", fill= "#9e2c2c") +  labs(title = "AFINN - Most negative words")
afinn_word_pos <- ggplot(data = tail(afinn_words,10)) + geom_bar(mapping = aes(x = word, y = sentiment), stat = "identity", fill= "#34ad3a") +  labs(title = "AFINN - Most positive words")

# Create a wordcloud for the popular words

#wordcloud <- tidy_hod %>%
  #anti_join(stop_words) %>%
  #count(word) %>%
  #with(wordcloud(word, n, max.words = 100))


###########################################

# Task 3

## ingest text into the mssp server

tnum.authorize('mssp1.bu.edu') # access to the server
tnum.setSpace("test3")
#hod_txt <- readLines("hod.txt")
#tnBooksFromLines(hod_txt, 'zara/submission4') # use the Book2Tn to ingest (started 12:48)
#tnum.getDBPathList(taxonomy = 'subject', level = 2)


### Extract all the text of the book 
text <- tnum.query("zara/submission4/section:# has text", max=3000)
df_text<- tnum.objectsToDf(text)

### Extract word counts of each sentence
count <- tnum.query("zara/submission4/section:# has count:#", max=3000)
df_count <- tnum.objectsToDf(count)

### Combine the two and create a workable data frame
df_full <- left_join(select(df_text, subject, string.value, tags),
                            select(df_count, subject, numeric.value))

# Create columns for chapter, paragraph and sentence

## 1. create the chapter column

hod_tnum <- df_full %>%
  separate(col = subject, sep = "/section", into = c("discard", "chapter"))

## 2. create the paragraph column
hod_tnum <- hod_tnum %>%
  separate(col = chapter, sep = "/paragraph", into = c("chapter","paragraph"))

## 3. create the sentence within the column
hod_tnum <- hod_tnum %>%
  separate(col = paragraph, sep = "/sentence", into = c("paragraph","sentence"))


## Make the dataframe tidy

hod_tnum$sentence <- 1:nrow(hod_tnum)
hod_tnum <- rename(hod_tnum, numwords=numeric.value)

regexp <- "[[:digit:]]+" # prepare regular expression
hod_tnum$chapter <- as.numeric(str_extract(hod_tnum$chapter, regexp))
hod_tnum$paragraph <- as.numeric(str_extract(hod_tnum$paragraph, regexp))
hod_tnum$sentence <- as.numeric(str_extract(hod_tnum$sentence, regexp))


## Create a column for paragraph number

### find opposite of separate function (unite)
hod_tnum <- hod_tnum %>% unite(col = "chapara", chapter, paragraph, remove= FALSE)

test <- data.frame(chapara = unique(hod_tnum$chapara), numpara=c(1:nrow(distinct(hod_tnum, chapara))))

hod_tnum <- inner_join(hod_tnum,test, by="chapara")


### separate words and clean up the dataframe

tidy_hod_tnum <- hod_tnum %>%
  unnest_tokens(word, string.value)

### select relevant columns
tidy_hod_tnum <- tidy_hod_tnum %>% select("word", "numpara", "sentence")




# Paragraph analysis

tnum_afinn <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(numpara) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

tnum_bing <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("bing")) %>%
  count(numpara, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(method = "BING") %>% 
  mutate(sentiment = positive - negative)

tnum_nrc <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(numpara, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(method = "NRC") %>% 
  mutate(sentiment = positive - negative)

### Bind the dataframes together

tnum_alllex <- bind_rows(tnum_afinn, tnum_bing, tnum_nrc)

### Compare paragraphs

afinn_paras <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(numpara) %>% 
  summarise(sentiment = sum(value))

afinn_paras <-afinn_paras[order(afinn_paras$sentiment),]

afinn_para_neg <- ggplot(data = head(afinn_paras,10)) + geom_bar(mapping = aes(x = numpara, y = sentiment), stat = "identity", fill= "#9e2c2c") +  labs(title = "AFINN - Most negative paragraphs", x = "Paragraph number")
afinn_para_pos <- ggplot(data = tail(afinn_paras,10)) + geom_bar(mapping = aes(x = numpara, y = sentiment), stat = "identity", fill= "#34ad3a") +  labs(title = "AFINN - Most positive paragraphs", x = "Paragraph number")



# Sentence analysis

tnum_afinn2 <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(sentence) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

tnum_bing2 <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("bing")) %>%
  count(sentence, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(method = "Bing") %>% 
  mutate(sentiment = positive - negative)

tnum_nrc2 <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentence, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(method = "NRC") %>% 
  mutate(sentiment = positive - negative)

### Bind the dataframes together

tnum_alllex2 <- bind_rows(tnum_afinn2, tnum_bing2, tnum_nrc2)


### Compare sentences

afinn_sentences <- tidy_hod_tnum %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(sentence) %>% 
  summarise(sentiment = sum(value))

afinn_sentences <-afinn_sentences[order(afinn_sentences$sentiment),]

afinn_sentence_neg <- ggplot(data = head(afinn_sentences,10)) + geom_bar(mapping = aes(x = sentence, y = sentiment), stat = "identity", fill= "#9e2c2c") +  labs(title = "AFINN - Most negative sentences")
afinn_sentence_pos <- ggplot(data = tail(afinn_sentences,10)) + geom_bar(mapping = aes(x = sentence, y = sentiment), stat = "identity", fill= "#34ad3a") +  labs(title = "AFINN - Most positive sentences")

