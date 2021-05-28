# ----------------------------------------------------------------------------
# The R code file is for Forecasting and Forensic Analytics course at SMU
# taught by Prof Wang Jiwei (jwwang@smu.edu.sg) in the
# MSc in Accounting (Data & Analytics) program (www.smu.edu.sg/msa).
# You may share the code with people outside of the SMU community, but
# you are prohibited by law from sharing the data with people outside of SMU.
# ----------------------------------------------------------------------------

# Define html_df() function for displaying small tables in html format
library(knitr)
library(kableExtra)
html_df <- function(text, cols = NULL, col1 = FALSE, full = FALSE) {
  if(!length(cols)) {
    cols = colnames(text)
  }
  if(!col1) {
    kable(text, "html", col.names = cols, align = c("l", rep('c', length(cols)-1))) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = full)
  } else {
    kable(text, "html", col.names = cols, align = c("l", rep('c', length(cols) - 1))) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = full) %>%
      column_spec(1, bold = TRUE)
  }
}

library(tidyverse)

#create strings with either double quotes "" (preferred) or single quotes ''
string <- c("string", 'string')
string
#To include quotes in a string, we need to precede it with a backslash `\`
string <- c("\"")
string #the printed output of a string is not the same as string itself
writeLines(string) #to print out the raw content
#To include '\', we need to put '\' before '\'
string <- c("\\")
writeLines(string)

string <- c("What is this? \tIt is a dog.")
writeLines(string)
string <- c("What is this? \nIt is a dog.")
writeLines(string)

# Read text from a .txt file using read_file()
doc <- read_file("../../Data/Session_8-dbs2017.txt")
class(doc)
# str_wrap is from stringr from tidyverse
cat(str_wrap(substring(doc, 1, 160), 80))

cat(str_wrap(str_sub(doc, 16141, 16249), 80))
cat(str_wrap(str_sub(doc, 75315, 75465), 80))

sentence <- str_sub(doc, 15272, 15306)
str_to_lower(sentence)
str_to_upper(sentence)
str_to_title(sentence)

board <- c("Peter Seah", "Piyush Gupta", "Bonghan Cho", "Euleen Goh",
           "Ho Tian Yee", "Punita Lal", "Anthony Lim", "Oliver Lim",
           "Ow Foong Pheng", "Andre Sekulic", "Tham Sai Choy")
titles <- c("Non-Executive Chairman", "CEO",  "Independent Director",
            "Non-Executive Director", "Non-Executive Director",
            "Independent Director", "Independent Director",
            "Lead Independent Director", "Non-Executive Director",
            "Independent Director", "Independent Director")
# board is a list of DBS's director names
# titles is a list of the director's titles
paste(board[1:5], titles[1:5], sep=", ")
cat(str_wrap(paste0("DBS's board consists of: ",
                    paste(board[1:length(board)-1], collapse=", "),
                    ", and ", board[length(board)], "."), 80))

sentence
str_replace_all(sentence, "is", "was")

#doc contains 1 vector of 1 string, ie, DBS's 2017 annual report
paragraphs <- str_split(doc, '\n') #split by new line (paragraph)
str(paragraphs) #list consisting of 1 vector of 423 strings
length(paragraphs) #length of the list
paragraphs <- paragraphs[[1]] #extract first element of the list
length(paragraphs) #length of the vector of strings

# the longest paragraph
nchar <- str_length(paragraphs) #no. of char for each paragraph
ncharmax <- max(nchar) #max number of paragraphs
index <- match(ncharmax, nchar) #index number for the longest paragraphs
cat(str_wrap(paragraphs[index], 80))

# How many paragraphs mention net income in any case?
x <- str_detect(str_to_lower(paragraphs), "net income")
x[1:10]
sum(x)

# What is the most net income mentioned in any paragraph
x <- str_count(str_to_lower(paragraphs), "net income")
x[1:10]
max(x)

# the paragraph mentions "net income" the most
# match() returns an index/position number
cat(str_wrap(paragraphs[match(2, x)], 80))

str_locate(str_to_lower(doc), "net income")

str_extract(str_to_lower(doc), "net income")

str_locate_all(str_to_lower(doc), "net income")

# Extract all emails from the annual report
str_extract_all(doc,'[:graph:]+@[:alnum:]+\\.[.[:alnum:]]+')

text <- c("abcde", 'ABCDE', '12345', '!?!?.', 'ABC123?', "With space",
          "New\nline")
html_df(data.frame(
  text=text,  alpha=str_detect(text, '[:alpha:]'),
  lower=str_detect(text, '[:lower:]'),  upper=str_detect(text, '[:upper:]'),
  digit=str_detect(text, '[:digit:]'),  alnum=str_detect(text, '[:alnum:]')
))

text <- c("abcde", 'ABCDE', '12345', '!?!?.', 'ABC123?', "With space",
          "New\nline")
html_df(data.frame(
  text=text,  punct=str_detect(text, '[:punct:]'),
  graph=str_detect(text, '[:graph:]'),  space=str_detect(text, '[:space:]'),
  blank=str_detect(text, '[:blank:]'),  period=str_detect(text, '.')
))

df_RE <- read.csv("../../Data/Session_8-1.csv")
RE_names <- unique(df_RE$conm)

# Real estate firm names with 3 vowels in a row
str_subset(RE_names, '[AEIOU]{3}')
# Real estate firm names with no vowels
str_subset(RE_names, '^[^AEIOU]+$')
# Real estate firm names with a repeated 4 letter pattern
str_subset(RE_names, '([:upper:]{4}).*\\1')
# Real estate firm names with at least 11 vowels
str_subset(RE_names, '([^AEIOU]*[AEIOU]){11,}')

# Real estate firm names with at least 11 vowels
str_subset(RE_names, '([^AEIOU]*[AEIOU]){11,}')

# Compustat firm names example
df_RE_names <- df_RE %>%
  group_by(isin) %>%
  slice(1) %>%
  mutate(SG_in_name = str_detect(conm, "(SG|SINGAPORE)"),
         SG_firm = ifelse(fic == "SGP", 1, 0)) %>%
  ungroup()

df_RE_names %>%
  group_by(SG_firm) %>%
  mutate(pct_SG = mean(SG_in_name) * 100) %>%
  slice(1) %>%
  ungroup() %>%
  select(SG_firm, pct_SG)

library(quanteda)
textstat_readability(doc, "Flesch")

textstat_readability(doc, "FOG")

textstat_readability(doc, "Coleman.Liau")

# Example of "tokenizing"
library(tidytext)
df_doc <- data.frame(ID=c("DBS"), text = c(doc))
df_doc <- unnest_tokens(df_doc, output = word, input = text, token = "words")
html_df(df_doc[1:7, ])
# word is the name for the new column
# text is the name of the string column in the input data

library(stopwords) # get a list of stopwords
stop_en <- stopwords("english")  # Snowball English
paste0(length(stop_en), " words: ", paste(stop_en[1:10], collapse=", "))
stop_SMART <- stopwords(source = "smart")  # SMART English
paste0(length(stop_SMART), " words: ", paste(stop_SMART[1:8], collapse = ", "))
stop_fr <- stopwords("french")  # Snowball French
paste0(length(stop_fr), " words: ", paste(stop_fr[1:10], collapse = ", "))

df_doc_stop <- df_doc %>% anti_join(data.frame(word = stop_SMART))
nrow(df_doc)
nrow(df_doc_stop)

# to count n = how many times for each token/word in each ID
terms <- df_doc_stop %>%
  count(ID, word, sort = TRUE) %>% 
  ungroup()
# to sum total words per ID and term frequency per word and ID
total_terms <- terms %>% 
  group_by(ID) %>% 
  summarize(total = sum(n)) %>% ungroup()
tf <- left_join(terms, total_terms) %>% mutate(tf = n/total)
tf[1:10, ]

get_sentiments("afinn") %>%
  group_by(value) %>%
  slice(1) %>%
  ungroup()

get_sentiments("bing") %>%
  group_by(sentiment) %>%
  slice(1) %>%
  ungroup()

get_sentiments("nrc") %>%
  group_by(sentiment) %>%
  slice(1) %>%
  ungroup()

get_sentiments("loughran") %>%
  group_by(sentiment) %>%
  slice(1) %>%
  ungroup()

tf_sent <- tf %>% left_join(get_sentiments("loughran"))
tf_sent[1:5, ]
tf_sent[!is.na(tf_sent$sentiment), ][1:5, ]

# spread a key-value pair across multiple columns (long to wide)
tf_sent %>% 
  spread(sentiment, tf, fill = 0) %>% # missing values will be replaced with 0
  select(constraining, litigious, negative, positive, superfluous,
         uncertainty) %>%
  colSums() %>% # Form column sums for numeric arrays (or data frames)
  scales::percent(accuracy = 0.001)

#library(plotly)
tf_sent %>%
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  mutate(row = row_number()) %>%
  filter(row < 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(y = n, x = word)) + geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~sentiment, ncol = 3, scales = "free_x")
#ggplotly(p)

# You may need to add unique_docnames = FALSE if there is an error from corpus()
df_doc_stop %>% filter(!str_detect(word, "([:digit:]|dbs|group)")) %>%
  corpus(docid_field = "ID", text_field = "word", unique_docnames = F) %>%
  dfm() %>% textplot_wordcloud(color = RColorBrewer::brewer.pal(10, "RdBu"))

# You may need to add unique_docnames = FALSE if there is an error from corpus()
corp <- corpus(df_doc, docid_field = "ID", text_field = "word",
               unique_docnames = F)
textplot_wordcloud(dfm(corp), color = RColorBrewer::brewer.pal(10, "RdBu"))