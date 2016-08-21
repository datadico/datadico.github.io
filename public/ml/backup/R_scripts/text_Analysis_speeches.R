#tier3-landing-content a

years <- c("2009a", "2010", "2011", "2012", "2013")
years

library(rvest)

url <- paste("http://www.state.gov/secretary/20092013clinton/rm/",years,"/index.htm",sep="")

## Scraper:


HeaderURLs <- c("http://www.state.gov/secretary/20092013clinton/rm/2013/index.htm", "http://www.state.gov/secretary/20092013clinton/rm/2012/index.htm", "http://www.state.gov/secretary/20092013clinton/rm/2011/index.htm")
as.data.frame(HeaderURLs[2])


# generate all URLs
emptyVec <- NULL
for (n in 1:length(years)) {
  varb = ("http://www.state.gov/secretary/20092013clinton/rm/",years,"/index.htm",sep="")
  emptyVec = (rbind(emptyVec,as.data.frame(varb)))
}
View(emptyVec$varb)


funcURL <- function (urlMain) {
  dat <- read_html(urlMain)
  urltext <- dat %>%
    html_node("#tier3-landing-content a") %>%
    html_attrs("href")
  urltext <- as.data.frame(text)
  return(urltext)
}
funcURL("http://www.state.gov/secretary/20092013clinton/rm/2013/index.htm")




# collection of data: 
data <- NULL
for (num in 1:length(emptyVec$varb)) {
  var = paste(funcURL[num])
  data = rbind(data, data.frame(funcURL(URLs[num])))
}

# test
em.all <- NULL
for (n in 0:10) {
  dat <- read_html("http://www.state.gov/secretary/20092013clinton/rm/2013/index.htm")
  urltext <- paste(dat %>%
                     html_node("#tier3-landing-content a") %>%
                     html_attrs("href"))
  data.frame(urltext)
  em.all <- rbind(em.all, urltext)
}
em.all


em2 <- data.frame(title, number_of_views, rebutal, url)
em.all <- rbind(em.all, em2)





##############################################  FINALE SCRAPER START: ####################### 

# this extracts all the URLs from the head links:
# get URL link function: 
parseLink <- function (url) {
  link <- read_html(url)
  urltext <- link %>%
    html_nodes("#tier3-landing-content a") %>%
    html_attr("href")
  #    html_text()
  #    html_text()
  urltext <- as.data.frame(urltext)
  return(urltext)
}
class(parseLink("http://www.state.gov/secretary/20092013clinton/rm/2011/index.htm"))

dataPutputLink <- NULL
for (num2 in 1:5) {
  varLink = paste(emptyVec$varb[num2])
  dataPutputLink = rbind(dataPutputLink, data.frame((parseLink(varLink))))
  print(varLink)
}
View(dataPutputLink)  # has now all the URLs for each "speech"
glimpse(dataPutputLink)

dataPutputLinkFull <-dataPutputLink %>%
  mutate(urltext = ifelse(grepl("^https?://", urltext), urltext, paste("http://www.state.gov",urltext, sep = ""))) %>%
  filter(grepl("^https?://", urltext))

View(dataPutputLinkFull)

#Example ifelse statemnt with mutate
#Pass.Fail = ifelse(grade > 60, "Pass", "Fail")
#   urltext= paste("http://www.state.gov",urltext, sep = ""))
#    mutate(g = ifelse(a == 2 | a == 5 | a == 7 | (a == 1 & b == 4), 2,
#                  ifelse(a == 0 | a == 1 | a == 4 | a == 3 |  c == 4, 3, NA)))
# regex text for http: ^https?://

#Check length
nrow(dataPutputLinkFull)
train <- sample_n(dataPutputLinkFull, 10)
nrow(train)


# This extracts each Hillary Clinton text for each single link
# get data function: 
parse <- function (url) {
  state.gov <- read_html(url)
  text <- state.gov %>%
    html_node("#centerblock") %>%
    html_text()
  dates <- state.gov %>%
    html_node("#date_long") %>%
    html_text()
  title <- state.gov %>%
    html_node("#doctitle span") %>%
    html_text() %>%
    as.character()
  kind <- state.gov %>%
    html_node(".document_type_-_speaker_writer" )%>%
    html_text() %>%
    as.character()
  
  as.data.frame(title)
  as.data.frame(kind)
  text <- as.data.frame(text, dates)
  text <- cbind(text, title, kind)
  #return(class(text))
  return(text)
}

# test
View(parse("http://www.state.gov/secretary/20092013clinton/rm/2009a/12/134300.htm"))
URLs <- c("http://www.state.gov/secretary/20092013clinton/rm/2013/02/203684.htm", "http://www.state.gov/secretary/20092013clinton/rm/2013/01/203579.htm", "http://www.state.gov/secretary/20092013clinton/rm/2013/01/203581.htm")
as.data.frame(URLs)
print(train[5, 1])

# test train: 
#dataPutput <- NULL
#for (num in 1:nrow(train)) {
#  var = paste(train[num, 1])
#  dataPutput = rbind(dataPutput, data.frame(parse(train[num, 1])))
#  #(parse(URLs[num]))
#  #print(var)
#}

dataPutput <- NULL
for (num in 1:nrow(dataPutputLinkFull)) {
  var = paste(dataPutputLinkFull[num, 1])
  dataPutput = rbind(dataPutput, data.frame(parse(dataPutputLinkFull[num, 1])))
  #(parse(URLs[num]))
  print(var)
}

# Done scraping:
nrow(dataPutput)
View(dataPutput)

dataPutput.raw <- dataPutput

# add the rownames as a proper column:
dataPutput.raw <- cbind(Dates = rownames(dataPutput.raw), dataPutput.raw)
glimpse(dataPutput.raw)

# filter for all remarks (speeches):
dataPutput.speeches <- dataPutput.raw %>%
  filter(kind == "Remarks")

nrow(dataPutput.speeches)  # now we gained 630 speeches
# end of scraper

#Analysis: 


### Basic Data Analysis: 
# intention: Model over time H. Clinton's 





















# notes:




############# Use project gutenberg with R:
#Docs: 
https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html
install.packages("gutenbergr")
library(gutenbergr)
md <- gutenberg_metadata
View(md)

md_obama <- filter(md, grepl("Obama",author))
View(md_obama)

library(stringr)
md_clinton <- gutenberg_works(str_detect(author, "Clinton, Bill"))
View(md_clinton)

# Test: download Obama's speeches and analyse them: 
ob <- gutenberg_download(50950)
as.data.frame(ob)

library(xlsx)
write.xlsx(ob, "/Users/BH/BLOG/GUTENBERG.xlsx")
write.table(ob, "/Users/BH/BLOG/GUTENBERG.xlsx", sep="\t")


#################### DOS: Perform a tf-idf
# look at inverse document frequency (idf): 
# ... idf:  decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents. 

###### read in Obama: 
ob_clean <- read.csv("/Users/BH/BLOG/GUTENBERG/ob_clean.csv", header = T)
View(ob_clean)

# test Jane Ausin
library(dplyr)
library(janeaustenr)
library(tidytext)
View(austen_books())
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words

library(ggplot2)
library(viridis)
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  xlim(NA, 0.0009) +
  labs(title = "Term Frequency Distribution in Jane Austen's Novels",
       y = "Count") +
  facet_wrap(~book, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic"))

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words





# OBAMA Speeches: 
ob_clean$Year <- as.character(ob_clean$Year)
ob_clean$text <- as.character(ob_clean$text)
ob_clean$gutenberg_id <- as.character(ob_clean$gutenberg_id)

glimpse(ob_clean)

speech_words <- ob_clean %>%
  unnest_tokens(word, text) %>%
  count(Year, word, sort = TRUE) %>%
  ungroup()

total_words <- speech_words %>% group_by(Year) %>% summarize(total = sum(n))
speech_words <- left_join(speech_words, total_words)
speech_words

View(speech_words)

library(ggplot2)
library(viridis)
ggplot(speech_words, aes(n/total, fill = Year)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  xlim(NA, 0.002) +
  labs(title = "Term Frequency Distribution in Obama's State of the Union Addresses (2009-2016)",
       y = "Count") +
  facet_wrap(~Year, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic"))


speech_words <- speech_words %>%
  bind_tf_idf(word, Year, n)
speech_words

# sort tf_idf terms
speech_words %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 


devtools::install_github("lionel-/ggstance")
library(ggstance)
library(ggthemes)
plot_OB <- speech_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

ggplot(plot_OB[1:20,], aes(tf_idf, word, fill = Year, alpha = tf_idf)) +
  geom_barh(stat = "identity") +
  labs(title = "Highest tf-idf words in Obama's State of the Union Addresses (2009-2016)",
       y = NULL, x = "tf-idf") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1), guide = FALSE) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(legend.title=element_blank()) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))


plot_OB <- plot_OB %>% group_by(Year) %>% top_n(5) %>% ungroup

ggplot(plot_OB, aes(tf_idf, word, fill = Year, alpha = tf_idf)) +
  geom_barh(stat = "identity", show.legend = T) +
  labs(title = "Highest tf-idf words in Obama's State of the Union Addresses (2009-2016)",
       y = NULL, x = "tf-idf") +
  facet_wrap(~Year, ncol = 3, scales = "free") +
  # theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  scale_x_continuous(expand=c(0,0)) +
  #scale_fill_viridis(end = 0.85, discrete=TRUE) 
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic"))





###### sentiment General:

library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)

View(austen_books())

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data("stop_words")
tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

library(tidyr)
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

bing

janeaustensentiment <- tidy_books %>%
  inner_join(bing) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

janeaustensentiment

library(ggplot2)
library(viridis)
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment in Jane Austen's Novels",
       y = "Sentiment") +
  scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())



# Sentiment analysis: Obama speeches: 

library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)

View(ob_clean)

original_speech <- ob_clean %>%
  group_by(Year) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

View(original_speech)

################## Use original_speech from here:

tidy_speech <- original_speech %>%
  unnest_tokens(word, text)

tidy_speech

data("stop_words")
tidy_speech <- tidy_speech %>%
  anti_join(stop_words)

tidy_speech %>%
  count(word, sort = TRUE)

library(tidyr)
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

bing

ObamaSentiment <- tidy_speech %>%
  inner_join(bing) %>% 
  count(Year, index = linenumber %/% 10, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
View(ObamaSentiment)



library(ggplot2)
library(viridis)
?geom_smooth
ggplot(ObamaSentiment, aes(index, sentiment, fill = Year)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_smooth(method = "loess") +
  
  facet_wrap(~Year, ncol = 2, scales = "free_x") +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment in Obama's annual State of the Union Addresses (2009-2016)",
       y = "Sentiment") +
  scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())


ObamaSentimentYear <- tidy_speech %>%
  inner_join(bing) %>% 
  count(Year, index = linenumber %/% 1000, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ObamaSentimentYear
ggplot(ObamaSentimentYear, aes(Year, sentiment, group= index)) +
  geom_line(position = "stack") +
  geom_area(stat = "identity") +
  theme(strip.text = element_text(face = "italic")) +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment in Obama's annual State of the Union Addresses (2009-2016)",
       y = "Sentiment")



### sentiment across speechDate: 
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
get_nrc_sentiment("frightful") # example:

mySentiment <- get_nrc_sentiment(original_speech$text)

# clean text function: 
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}


original_speech_clean <- ob_clean

original_speech_clean <- original_speech_clean %>%
  mutate(text = gsub( "(?:#|@)[a-zA-Z0-9_]+ ?", "", text)) %>%
  mutate(text = gsub( "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", text)) %>%
  mutate(text = gsub( "[[:punct:]]", "", text)) %>%
  mutate(text = gsub( "[\r\n]", "", text)) %>%
  mutate(text = gsub( " {2,}", "", text)) %>%
  filter(text != "")

# after this example: gsub( "[^[:alnum:],]", "", original_speech_clean$text)
D <- get_nrc_sentiment(original_speech_clean$text)

View(original_speech_clean)
View(Detailed)

library(plyr)
DataFinal <- original_speech_clean %>%
  mutate(anger = D$anger, anticipation = D$anticipation, disgust = D$disgust, fear = D$fear, joy = D$joy, sadness = D$sadness, surprise = D$surprise, trust = D$trust)

View(DataFinal)

DataFinal2 <- DataFinal %>%
  group_by(Year)  %>%
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust))

View(DataFinal2.tight)

DataFinal2.tight <- gather(DataFinal2, "sentiment", "value", 2:9)

ggplot(DataFinal2.tight, aes(x = Year, y = value, group = sentiment, col = sentiment)) +
  geom_line() +
  # geom_area(stat = "identity") +
  theme(strip.text = element_text(face = "italic")) +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment in Obama's annual State of the Union Addresses (2009-2016)",
       y = "Sentiment")  



