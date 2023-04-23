# Knowledge Mining: Text mining
# File: Lab_textmining01.R
# Theme: Download text data from web and create wordcloud
# Data: MLK speech

# Install the easypackages package 
install.packages(c("easypackages","XML","wordcloud","RColorBrewer","NLP","tm","quanteda","quanteda.textstats"))
library(easypackages)
libraries("XML","wordcloud","RColorBrewer","NLP","tm","quanteda","quanteda.textstats")

# Download text data from website
mlk_speech <-URLencode("http://www.analytictech.com/mb021/mlk.htm")

# use htmlTreeParse function to read and parse paragraphs

doc.html<- htmlTreeParse(mlk_speech, useInternal=TRUE)
mlk <- unlist(xpathApply(doc.html, '//p', xmlValue))

head(mlk, 3)

words.vec <- VectorSource(mlk)

# Check the class of words.vec

class(words.vec)

# Create Corpus object for preprocessing
words.corpus <- Corpus(words.vec)
inspect(words.corpus)
# Turn all words to lower case

words.corpus <- tm_map(words.corpus, content_transformer(tolower))

# Remove punctuations, numbers
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)

# How about stopwords, then uniform bag of words created

words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

# Create Term Document Matric

tdm <- TermDocumentMatrix(words.corpus)
inspect(tdm)

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

# Create Wordcloud
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)

set.seed(1234)
wordcloud(cloudFrame$word,cloudFrame$freq)
wordcloud(names(wordCounts),wordCounts, min.freq=1,random.order=FALSE, max.words=200,scale=c(4,.5), rot.per=0.35,colors=brewer.pal(8,"Dark2"))


#  N-gram with two to three words
textstat_collocations(mlk, size = 2:3) 

# Run the program on Winston Churchill's Finest Hour speech?
# http://www.historyplace.com/speeches/churchill-hour.htm

