# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

libraries <- c("dplyr", "tidytext", "tm", "lda", "LDAvis")
lapply(libraries, require, character.only = TRUE)

path = "../input"
files <- list.files(path, pattern = "test.csv", full.names = TRUE)
data <- read.csv(files, stringsAsFactors = FALSE)



# Remove HTML tags:
remove_html_tags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

bind_tf_idf_ = function (tbl, term_col, document_col, n_col) {
  terms <- tbl[[term_col]]
  documents <- tbl[[document_col]]
  n <- tbl[[n_col]]
  doc_totals <- tapply(n, documents, sum)
  idf <- log(length(doc_totals)/table(terms))
  tbl$tf <- tbl[[n_col]] / (doc_totals[as.character(documents)])
  tbl$idf <- as.numeric(idf[terms])
  tbl$tf_idf <- tbl$tf * tbl$idf
  return(tbl)
}

bind_tf_idf_modify = function (tbl, term_col, document_col, n_col) {
  bind_tf_idf_(tbl, col_name(substitute(term_col)), col_name(substitute(document_col)), 
               col_name(substitute(n_col)))
}

col_name <- function (x, default = stop("Please supply column name", call. = FALSE))
{
  if (is.character(x))
    return(x)
  if (identical(x, quote(expr = )))
    return(default)
  if (is.name(x))
    return(as.character(x))
  if (is.null(x))
    return(x)
  stop("Invalid column specification", call. = FALSE)
}


data$cont_and_title = apply(data[,c("title","content")],1,function(x){paste(x,collapse = " ")})
data$cont_and_title <- remove_html_tags(data$cont_and_title)


# Cleaning content, including removing punctuation, numbers, URLs, nonASCII characters, etc:

# write.csv(data, file = paste0(path, 'test-1.csv'))


## I noticed that Stack Exchange Tags don't contain verbs: http://stackoverflow.com/help/tagging
## To remove verbs, NLTK library in Python seems to be the best solution:
# import pandas as pd
# import nltk
# from nltk import pos_tag
# from nltk.corpus import stopwords
# from nltk.corpus import wordnet as wn
# 
# data = pd.read_csv("input/test-1.csv", encoding="utf-8") 
# stop = stopwords.words('english')
# 
# data['cont_and_title'] = df['cont_and_title'].apply(lambda x: [item for item in x if item not in stop])
# 
# text = nltk.word_tokenize(df['cont_and_title'])
# tags = pos_tag(text)
# filter(lambda (word, tag): tag != 'VRD', tags)



Data_corpus <- Corpus(VectorSource(data$cont_and_title))
Data_corpus <- tm_map(Data_corpus, tolower)
Data_corpus <- tm_map(Data_corpus, removePunctuation)
Data_corpus <- tm_map(Data_corpus, removeNumbers)
Data_corpus <- tm_map(Data_corpus, function(x) gsub("http[[:alnum:]]*","", x))
Data_corpus <- tm_map(Data_corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))
Data_corpus <- tm_map(Data_corpus, removeWords,stopwords("SMART"))

Data_corpus <- tm_map(Data_corpus, function(y) gsub("angular momentum", "angular-momentum", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("magnetic moment", "magnetic-moment", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("quantum", "quantum mechanics", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("qm", "quantum mechanics", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("em", "electromagnetism", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("qft", "quantum field theory", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (dynamics)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (field)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (theory)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (mechanics)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (relativity)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (physics)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (equation)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("([a-z]) (equations)", "\\1-\\2", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("(\\bforce\\b)","forces", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("(\\bgravitational\\b)","gravity", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("(\\borbit\\b)","orbitals", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("(\\b[a-z]{1}\\b)", " ", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("(\\b[a-z]{2}\\b)", " ", y))
Data_corpus <- tm_map(Data_corpus, function(y) gsub("(\\b[a-z]{3}\\b)", " ", y))
Data_corpus <- tm_map(Data_corpus, stripWhitespace)


## Remove stopwords:
stop_words <- c("i.e.", "would", "get", "like", "using", "know", "question", "use", 
                "get", "possible" , 
                "much", "find", "anyone",'able', 'according', 'accordingly', 'across',
                'actually', 'afterwards', "ain't", 'allow', 'allows', 'almost',
                "tell", "know", "another", "various", "also", "etc", "around", "vs",
                "used", "could", "without", "way", "new",
                'alone', 'along', 'already', 'although', 'always', 'among', 
                'amongst', 'anybody', 'anyhow', 'anything', 
                'anyway', 'anyways', 'anywhere', 'apart', 'appear', 'appreciate',
                'appropriate', 'aside', 'ask', 'asking', 
                'associated', 'available', 'away', 'awfully', 
                'b', 'be', 'became', 'become', 'becomes',
                'becoming', 'been', 'before', 'beforehand', 'behind', 'being',
                'believe', 'below', 'beside', 'besides', 'best', 'better',
                'between', 'beyond', 'both', 'brief', 'but', 'by', 'c', "c'mon",
                "c's", 'came', 'can', "can't", 'cannot', 'cant', 'cause',
                'causes', 'certain', 'certainly', 'changes', 'clearly', 'co',
                'com', 'come', 'comes', 'concerning', 'consequently', 'consider',
                'considering', 'contain', 'containing', 'contains',
                'corresponding', 'could', "couldn't", 'course', 'currently', 'd',
                'definitely', 'described', 'despite', 'did', "didn't", 'didnt',
                'different', 'do', 'does', "doesn't", 'doesnt','doing', "don't",
                'dont', 'done',
                'down', 'downwards', 'during', 'e', 'each', 'edu', 'eg', 'eight',
                'either', 'else', 'elsewhere', 'enough', 'entirely', 'especially',
                'et', 'etc', 'even', 'ever', 'every', 'everybody', 'everyone',
                'everything', 'everywhere', 'ex', 'exactly', 'example', 'except',
                'f', 'far', 'few', 'fifth', 'first', 'five', 'followed',
                'following', 'follows', 'for', 'former', 'formerly', 'forth',
                'four', 'from', 'further', 'furthermore', 'g', 'get', 'gets',
                'getting', 'given', 'gives', 'go', 'goes', 'going', 'gone', 'got',
                'gotten', 'greetings', 'h', 'had', "hadn't",'hadnt', 'happens', 'hardly',
                'has', "hasn't", 'hasnt','have', "haven't",'havent', 'having',
                'he', "he's",'hes', 'hello', 'help', 'hence', 'her', 'here', 
                "here's",'heres', 'hereafter',
                'hereby', 'herein', 'hereupon', 'hers', 'herself', 'hi', 'him',
                'himself', 'his', 'hither', 'hopefully', 'how', 'howbeit',
                'however', 'i', "i'd", 'id','ill',"i'll", "i'm", 'im', "i've", 
                'ive','isnt', "isn't", 'ie', 'if',
                'ignored', 'immediate', 'in', 'inasmuch', 'inc', 'indeed',
                'indicate', 'indicated', 'indicates', 'inner', 'insofar',
                'instead', 'into', 'inward', 'is', "isn't",'isnt', 'it', "it'd",
                'itd','itll', "it'll",
                "it's", 'its', 'itself', 'j', 'just', 'k', 'keep', 'keeps',
                'kept', 'know', 'knows', 'known', 'l', 'last', 'lately', 'later',
                'latter', 'latterly', 'least', 'less', 'lest', 'let', "let's",
                'like', 'liked', 'likely', 'little', 'look', 'looking', 'looks',
                'ltd', 'm', 'mainly', 'many', 'may', 'maybe', 'me', 'mean',
                'meanwhile', 'merely', 'might', 'more', 'moreover', 'most',
                'mostly', 'much', 'must', 'my', 'myself', 'n', 'name', 'namely',
                'nd', 'near', 'nearly', 'necessary', 'need', 'needs', 'neither',
                'never', 'nevertheless', 'new', 'next', 'nine', 'no', 'nobody',
                'non', 'none', 'noone', 'nor', 'normally', 'not', 'nothing',
                'novel', 'now', 'nowhere', 'o', 'obviously', 'of', 'off', 'often',
                'oh', 'ok', 'okay', 'old', 'on', 'once', 'one', 'ones', 'only',
                'onto', 'or', 'other', 'others', 'otherwise', 'ought', 'our',
                'ours', 'ourselves', 'out', 'outside', 'over', 'overall', 'own',
                'p', 'particular', 'particularly', 'per', 'perhaps', 'placed',
                'please', 'plus', 'possible', 'presumably', 'probably', 'put',
                'provides', 'q', 'que', 'quite', 'qv', 'r', 'rather', 'rd', 're',
                'really', 'reasonably', 'regarding', 'regardless', 'regards',
                'relatively', 'respectively', 'right', 's', 'said', 'same', 'saw',
                'say', 'saying', 'says', 'second', 'secondly', 'see', 'seeing',
                'seem', 'seemed', 'seeming', 'seems', 'seen', 'self', 'selves',
                'sensible', 'sent', 'serious', 'seriously', 'seven', 'several',
                'shall', 'she', 'should', "shouldn't", 'since', 'six', 'so',
                'some', 'somebody', 'somehow', 'someone', 'something', 'sometime',
                'sometimes', 'somewhat', 'somewhere', 'soon', 'sorry',
                'specified', 'specify', 'specifying', 'still', 'sub', 'such',
                'sup', 'sure', 't', "t's", 'take', 'taken', 'tell', 'tends', 'th',
                'than', 'thank', 'thanks', 'thanx', 'that', "that's", 'thats',
                'the', 'their', 'theirs', 'them', 'themselves', 'then', 'thence',
                'there', "there's", 'theres', 'thereafter', 'thereby', 'therefore',
                'therein', 'theres', 'thereupon', 'these', 'they', "they'd", 'theyd', 
                'theyll', 'theyre',
                "they'll", "they're", "they've", 'think', 'third', 'this', 'theyve',
                'thorough', 'thoroughly', 'those', 'though', 'three', 'through',
                'throughout', 'thru', 'thus', 'to', 'together', 'too', 'took',
                'toward', 'towards', 'tried', 'tries', 'truly', 'try', 'trying',
                'twice', 'two', 'u', 'un', 'under', 'unfortunately', 'unless',
                'unlikely', 'until', 'unto', 'up', 'upon', 'us', 'use', 'used',
                'useful', 'uses', 'using', 'usually', 'uucp', 'v', 'value',
                'various', 'very', 'via', 'viz', 'vs', 'w', 'want', 'wants',
                'was', "wasn't", 'wasnt', 'way', 'we', "we'd", "we'll", "we're", "we've",
                'wed', 'well', 'were', 'weve', 'werent', 'whats', 'wheres', 
                'whos','wont','wouldnt',
                'welcome', 'well', 'went', 'were', "weren't", 'what', "what's",
                'whatever', 'when', 'whence', 'whenever', 'where', "where's",
                'whereafter', 'whereas', 'whereby', 'wherein', 'whereupon',
                'wherever', 'whether', 'which', 'while', 'whither', 'who',
                "who's", 'whoever', 'whole', 'whom', 'whose', 'why', 'will',
                'willing', 'wish', 'with', 'within', 'without', "won't", 'wonder',
                'would', 'would', "wouldn't", 'x', 'y', 'yes', 'yet', 'you',
                'youd', 'youll', 'youre', 'youve', 
                "you'd", "you'll", "you're", "you've", 'your', 'yours',
                'yourself', 'yourselves', 'z')

Data_corpus <- tm_map(Data_corpus, removeWords, unique(stop_words))



split_space = function(x){strsplit(x," ")}
doc.list <- strsplit(unlist(Data_corpus$content), "[[:space:]]+")

term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
# remove terms that are stop words or occur fewer than 3 times:
term.table <- term.table[term.table>3]

vocab <- names(term.table)

# Put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents) # number of documents
W <- length(vocab) # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ])) # number of tokens per document
N <- sum(doc.length) # total number of tokens in the data
term.frequency <- as.integer(term.table) # frequencies of terms in the corpus


### Fit the LDA model
## Ad-hoc parameters - though ideally obtained by CV
#K <- 20
#G <- 1000
#alpha <- 0.1
#eta <- 0.1
#t1 <- print(Sys.time())
#lda_fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
#                                       num.iterations = G, alpha = alpha, eta = eta)
#t2 <- print(Sys.time())
#t2-t1

#top_words <- top.topic.words(lda_fit$topics,30,by.score=TRUE)
#write.csv(top_words,"top_words.csv",quote=FALSE)
#doc_topic <- apply(lda_fit$document_sums, 2, function(x) which(x == max(x))[1])
#original_books <- data_frame(document = 1:length(dat_physic$content), 
#                             text = unlist(Physic_corpus$content),
#                             id = dat_physic$id,
#                             topic = doc_topic)

#stack_words <- original_books %>%
#  unnest_tokens(word, text,token = split_space) %>%
#  filter(word %in% top_words[,topic])%>%
#  count(id, word, sort = TRUE) %>%
#  ungroup()
#stack_words <- stack_words %>%
#  bind_tf_idf_modify(word, id, n)
#df1 <- stack_words %>%
#  group_by(id) %>%
#  arrange(desc(tf)) %>%
#  top_n(5)%>%
#  summarixe(word = paste(word,collapse = " "))%>%
#  ungroup()%>%
#  arrange(id)


#output = data.frame(id=dat_physic$id)%>%arrange(id)
#output = merge(x=output,y=df1,by="id",all.x=TRUE)
#colnames(output) = c("id","tags")

#write.csv(output,"output_lda1.csv",quote=FALSE,row.names = FALSE)