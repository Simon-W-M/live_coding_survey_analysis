

# Specify required packages
my_packages <- c("tidyverse",
                 "janitor", 
                 "googlesheets4", 
                 "tm", 
                 "SnowballC", 
                 "RColorBrewer",
                 "RColorBrewer",
                 "sentimentr",
                 "stringi",
                 "gtExtras",
                 "pacman") 

# Extract not installed packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]  

# Install not installed packages
if(length(not_installed)) install.packages(not_installed)   

# Load all packages
pacman::p_load(char = my_packages) 

###########################

# This is the bit to load in from google sheets
# sadly I can not give you access to it but I have removed email addresses
# and removed any NHS person, to hopefully make it less identifable
# 
# # url for google sheet
# url <- "https://docs.google.com/spreadsheets/d/1KsqNrAVg4OcFzNIdtRE_BAUGvyJSkyj0uSv_XMWxQGM/edit?resourcekey#gid=2011877191"
# 
# data<- read_sheet (url)
# 
# data <- clean_names(data)
# 
# data <- data |>
#   select (-please_enter_your_email_address) |>
#   filter(grepl("NHS", where_do_you_work))
# 
# saveRDS(data,"data.rds")

##############

df <- readRDS("data.rds")

str(df)

summary(df)

colnames(df)

short_cols <- substr(colnames(df), start = 1, stop = 15)

colnames(df) <- c(short_cols)

colnames(df)[8] <- 'more'
colnames(df)[9] <- 'less'

#####

df |> ggplot(aes(x= timestamp, y =1)) +
  geom_jitter() + theme_minimal()

##

df_where <- df |>
  count(where_do_you_wo) |>
  mutate(where_do_you_wo = if_else(is.na(word(where_do_you_wo,1,4)),
                                   where_do_you_wo,
                                   word(where_do_you_wo,1,4)))

df_where |> ggplot(aes(x = n, y= reorder(where_do_you_wo, n), fill = -n)) +
  geom_col()

unique(df_where$where_do_you_wo)

###

list_grades <- unlist(df$if_you_are_will)

grade_df <- data.frame(table(list_grades))

grade_df |> ggplot(aes(x = Freq, y= list_grades, fill = -Freq)) +
  geom_col()

##

data_fact <- df |>
  mutate(how_would_you_d = factor(how_would_you_d,
                                  levels = c(
    "None",
    "Total noob",
    "A beginner with the basics",
    "A reasonably confident beginner",
    "Intermediate, I can do some interesting stuff, maybe some functions",
    "Intermediate, pretty confident, building functions, basic RAP workflows",
    "Advanced, fully RAP workflows",  
    "Advanced, perhaps object orientated R and advanced methods" 
  ))) |>
  count(how_would_you_d) |> 
  complete(how_would_you_d)



data_fact  |> ggplot(aes(x = n, y = how_would_you_d)) +
  geom_col()

how_long <- df |>
  count(how_long_have_y)

###

# read in text
txt <- df$are_there_any_t

docs <- Corpus(VectorSource(txt))

# Remove punctuation
docs <- tm_map(docs, removePunctuation)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))
# specify your stop words as a character vector - in this instance it was picking up some of the copyright notice
docs <- tm_map(docs, removeWords, c("simon")) 
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#it was still bringing back some quotation marks and so this finally removes what is left
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
docs <- tm_map(docs, content_transformer(removeSpecialChars))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

data_f <- d |>
  top_n(10, freq)

data_f |> ggplot(aes(x= freq, y = reorder(word,freq))) + 
  geom_col()




# very quick use of sentimentr to caluclate sentiments

more <- df$more[!is.na(df$more)]
less <- df$less[!is.na(df$less)]

sentiment_more=sentiment_by(more)
sentiment_less=sentiment_by(less)

# create a sentiment density plot with average sentiments
sent <- ggplot(data = sentiment_more, 
               aes(x = ave_sentiment), 
               color = "blue") + 
  geom_density(color = "blue", 
               fill="lightblue", 
               alpha = 0.2) + 
  geom_vline(aes(xintercept=mean(ave_sentiment)), 
             color="blue", 
             linetype="dashed", 
             linewidth=1) + 
  geom_density(data = sentiment_less, 
               aes(x = ave_sentiment), 
               color = "red", 
               fill="orange", 
               alpha = 0.2) + 
  geom_vline(data = sentiment_less,
             aes(xintercept=mean(ave_sentiment)),
             color="red", 
             linetype="dashed", 
             linewidth=1) + 
  theme_minimal() +
  ggtitle("Sentiment analysis:  Blue more -/- Red less ") 



