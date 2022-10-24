## Part 1: Basic Text Mining using tm package:

## ============================================================================
## BASIC TEXT MINING
## ============================================================================

## binary packages:
Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)

## Update all/some/none? [a/s/n]:
## enter "a" and press return

## One can downloaded source and binary packages will probably be somewhere in 
## C:\Users\<you>\AppData\Local\Temp\RtmpYXWUHd\downloaded_packages

## one source:
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

## Data Wrangling = is Data Cleaning, 
## Data Remediation (reverse process if its damaging), 
## or Data Munging (manipulating) - refers to a variety of 
## processes designed to transform raw data into more readily 
## used formats.

## Loading Texts

## 1. Create a folder and kept your data.
## 2. That folder should be:
## PC: C: drive
## 3. The files "iliad.txt" and "odyssey.txt" to that folder. 
## You can copy any number of media files in that folder, 
## such as PDF and HTML to do your text mining.

## On a PC, save the folder to your C: drive and use the following code chunk:
cname <- file.path("D:","NEU","Data Engg NEU","W2L1","WordCloudLab")   
cname   
## Use this to check to see that your texts have loaded. 
dir(cname)

## Load the R package for text mining and then load your texts into R.
library(tm)   
docs <- Corpus(DirSource(cname))   
summary(docs) 

## If you so desire, you can read your documents in the R terminal 
## using inspect(docs). Or, if you prefer to look at only one of 
## the documents you loaded, then you can specify which one using something like:
inspect(docs[1])

##
## PART I = Preprocessing
##

## Once sure that all documents loaded properly, I preprocess my texts.
## This step allows me to remove numbers, capitalization, common words, punctuation, and otherwise prepare the texts for analysis.
## This is Time Consuming and picky, but it pays off in the end in terms of high quality analyses.

## Removing punctuation:
## Punctuation and other special characters only look like more words to your computer and R. 
## Using the following to methods to remove them from the text
docs <- tm_map(docs, removePunctuation)

## If necessary, working with emails, one can removed special characters.
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
} 

## Removing numbers:
docs <- tm_map(docs, removeNumbers) 

## Converting to lowercase:
docs <- tm_map(docs, tolower) 

## Removing "stopwords" (common words) that usually have no analytic value.
## In every text, there are a lot of common, and uninteresting words (a, and, also, the, etc.). Such words are frequent by their nature, and will confound your analysis if they remain in the text.
# For a list of the stopwords, see:   
# length(stopwords("english"))   
# stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english"))   
# inspect(docs[3]) # Checked to see if it worked ( will soon reach the max.print limit, so it might do text overlfow..)

## Removing particular words:
## Some works not of value to your particular analysis. You can remove them, specifically, from the text.
docs <- tm_map(docs, removeWords, c("department", "email"))   

## Combining words that should stay together
for (j in seq(docs))
{
  docs[[j]] <- gsub("Peloponnesian War", "PW", docs[[j]])
  docs[[j]] <- gsub("World War I", "WWI", docs[[j]])
  docs[[j]] <- gsub("World War II", "WWII", docs[[j]])
}

## Removing common word endings (e.g., "ing", "es", "s")
## This is referred to as "stemming" documents. 
## I have stemed the documents so that a word will be recognizable to the computer, 
## whether or not it may have a variety of possible endings in the original text.
library(SnowballC)   
docs <- tm_map(docs, stemDocument)   
# inspect(docs[1]) # Checked to see if it worked.

## Stripping unnecesary whitespace from your documents:
## The above preprocessing will leave the documents with a lot of "white space". 
## White space is the result of all the left over spaces that were not removed along with the words that were deleted. 
## The white space is removed.
docs <- tm_map(docs, stripWhitespace)   
# inspect(docs[1]) # Check to see if it worked.  

## Read and Understood:
## https://www.rdocumentation.org/packages/tm/versions/0.7-8/topics/PlainTextDocument
## This tells R to treat your preprocessed documents as text documents.
#  docs <- tm_map(docs, PlainTextDocument)   

## This is the end of the preprocessing stage that is Data Cleansing!

##
## PART II:
## Staging the Data
##

## create a document term matrix.
dtm <- DocumentTermMatrix(docs)   
dtm  

## To inspect, i have used: inspect(dtm)
## This will, however, fill up the terminal quickly. So I preferred to view a subset:
inspect(dtm[1:2, 1:20]) #view first 2 docs & first 20 terms
dim(dtm) #This will display the number of documents & terms (in that order)

## Creating a transpose of this matrix as required.
tdm <- TermDocumentMatrix(docs)   
tdm   

##
## PART III
## Explore your data!
##

## Organize terms by their frequency:
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)

##Export the matrix to Excel:   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv")   

## That will print in the working directory = getwd() 

## Session - Set Working Directory - Choose Directory... can be used.

## An Excel file has many columns, so instead I used
mt <- as.matrix(tdm)   
dim(mt)   
write.csv(mt, file="tdm.csv") 

## Transposed the first matrix in Excel is done.

#  Removed sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)

## Word Frequency

## There are a lot of terms, so for now, I am checking out some of the most and least frequently occurring words.
freq[head(ord)] 
freq[tail(ord)] 

## Checking out the frequency of frequencies.
head(table(freq), 20)

## The resulting output is two rows of numbers. 
## The top number is the frequency with which words appear 
## and the bottom number reflects how many words appear that frequently. 
## Here, considering only the 20 lowest word frequencies, I can see that 
## 7407 terms appear only once and 2562 terms appear twice. 
## There are also a lot of others that appear very infrequently:
tail(table(freq), 20) 

## Considering only the 20 greatest frequencies, I can see that there is a huge disparity in how frequently some terms appear.

## For a less, fine-grained look at term freqency I can view a table of the terms we selected when I removed sparse terms, above. 
## (Look just under the word "Focus".)
freq <- colSums(as.matrix(dtms))   
freq   

## Or:
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)   

## An alternate view of term frequency:
##  This will identify all terms that appear frequently (in this case, 50 or more times).
findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for the text data.

## another way to do this:
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

## Plot Word Frequencies

## Plot words that appear at least 250 times.
library(ggplot2)   
p <- ggplot(subset(wf, freq>250), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 

##
## PART IV
## Statistics
##

## Relationships Between Terms

## Term Correlations

## Identify the words that most highly correlate with that term.
## If words always appear together, then correlation=1.0.
findAssocs(dtms, "vessel", corlimit=0.98) # specifying a correlation limit of 0.999   
## In this case, "vessel" was highly correlated with numerous other terms

## to do it for many terms:
#findAssocs(dtms, c("vessel", "neleus"), corlimit=0.98)

## Word Clouds!

## Loading the package that makes word clouds in R.
library(wordcloud)   

##Plot words that occur at least 250 times.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=250)  

## Note: The "set.seed() function just makes the configuration of the layout of the clouds consistent each time you plot them. 
## I have omitted the part that are not concerned with preserving a particular layout.

## Plotting the 100 most frequently used words.
set.seed(142)   
wordcloud(names(freq), freq, max.words=100) 

##Adding some color and plotting words occurring at least 250 times.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=250, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

##Plottin the 100 most frequently occurring words.
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 

## Clustering by Term Similarity

## First always remove a lot of the uninteresting or infrequent words. 
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss) 

## Hierarchal Clustering

## First calculated distance between words beginning with "a..." & then clustered them according to similarity.
library(cluster)   
d <- dist(t(dtmss[,1:100]), method="euclidian")   
fit <- hclust(d=d, method="ward")   
fit 
plot(fit, hang=-1)  
## Click on the zoom button on the plot..

## Helping to Read a Dendrogram
## To get a better idea of where the groups are in the dendrogram, 
## I have asked R to help identify the clusters. 
## Here, I have arbitrarily chosen to look at five clusters, as indicated by the red boxes. 
## A highlight of a different number of groups can be done by changing the code accordingly
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters
## Achilles is thew root of 5 different word groupings, 
## one of which is agamemnon, so I surmised that achiles and agamemnon have a big relationship
## in the story.. I could begin to infer meaning through context

##K-means clustering

## The k-means clustering method will attempt to cluster words 
## into a specified number of groups (in this case 2), such that 
## the sum of squared distances between individual words and one 
## of the group centers. 
## I can change the number of groups 
## seek by changing the number specified within the kmeans() command.
## This helps creating meaningful clusters like these
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
