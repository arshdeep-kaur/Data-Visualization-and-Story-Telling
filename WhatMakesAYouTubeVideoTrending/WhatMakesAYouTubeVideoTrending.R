
# Author: Arshdeep Kaur

# Title: What Makes a YouTube Video Trending? A Data Visualisation Perspective.

# 1. load packages

# Load package to read csv files
library('readr')
# Load package for date and time manipulations
library('lubridate')
# Load package to perform string operations
library('stringr')
# Load the package required to read JSON files.
library("rjson")
# Load package to perform data manipulation
library('dplyr')
# Load package for creating plots
library('ggplot2')
# Load library to perform sql function on dataframe
library('sqldf')
install.packages('gsubfn')
library('gsubfn')
library('proto')
library('RSQLite')
# Load package to plot multiple graphs
library('gridExtra')
library('wordcloud')
library('RColorBrewer')
# load packages for text mining
library('tm')
library('NLP')
library('xtable')

# 2. Read Youtube trending videos data for 4 countries
CA_data<-read_csv('CAvideos.csv')
US_data<-read_csv('USvideos.csv')
DE_data<-read_csv('DEvideos.csv')
IN_data<-read_csv('INvideos.csv')

# 3. Data wrangling

# add new column to dataframes
CA_data$country<-'Canada'
US_data$country<-'USA'
DE_data$country<-'Germany'
IN_data$country<-'India'

# Map category information in JSON file to csv file for differnt Countries
category<-fromJSON(file = "US_category_id.json")
print(category)
df<-as.data.frame(NULL)
for(i in 1:32){
  df[i,1]<-category$items[[i]]$id
  df[i,2]<-category$items[[i]]$snippet$title
}
colnames(df)<-c('category_id','category')

US_data$category_id<-as.character(US_data$category_id)
US_data<-left_join(US_data,df,by='category_id')
apply(is.na(US_data), 2, which)

CA_data$category_id<-as.character(CA_data$category_id)
CA_data<-left_join(CA_data,df,by='category_id')
apply(is.na(CA_data), 2, which)

DE_data$category_id<-as.character(DE_data$category_id)
DE_data<-left_join(DE_data,df,by='category_id')
apply(is.na(DE_data), 2, which)

IN_data$category_id<-as.character(IN_data$category_id)
IN_data<-left_join(IN_data,df,by='category_id')
apply(is.na(IN_data), 2, which)

# combine all files in 1 dataframe
all_country_data<-rbind(CA_data,US_data,DE_data,IN_data)

# extract data only for Music category
music_data<-filter(all_country_data,all_country_data$category=='Music')

# convert date and time in correct formats
music_data$trending_date<-str_replace_all(music_data$trending_date,'\\.','-')
music_data$publish_date<-as.Date(music_data$publish_time,format='%Y-%m-%d')
music_data$publish_time<-strftime(music_data$publish_time,format='%H:%M:%S')
music_data$publish_hour<-hms(music_data$publish_time)$hour

# 4.Data cleaning

# find missing values in data frame
table(is.na(music_data))
apply(is.na(music_data), 2, which)

# replace missing values in description
music_data$description<-str_replace_na(music_data$description)
music_data$description<-str_replace_all(music_data$description,'NA','')

# Visualizations

# Visualization 1: 5 Most Popular Music Channels in different Countries by Views

# Iteration 1

all_channels<-sqldf("select channel_title, count(views) as views, country from music_data  group by channel_title")
all_channels <- all_channels[with(all_channels, order(-views)), ]

ggplot(data=all_channels,aes(x=channel_title,y=views))+
  labs(title='Most Popular Music Channels by Views')+
  geom_bar(stat = 'identity',aes(fill=country))+
  facet_grid(country~.)+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.x = element_text(angle = 90))

# Iteration 2

US_channels<-sqldf("select channel_title, count(views) as views, country from music_data where country='USA' group by channel_title")
US_channels <- US_channels[with(US_channels, order(-views)), ]

CA_channels<-sqldf("select channel_title, count(views) as views, country from music_data where country='Canada' group by channel_title")
CA_channels <- CA_channels[with(CA_channels, order(-views)), ]

DE_channels<-sqldf("select channel_title, count(views) as views, country from music_data where country='Germany' group by channel_title")
DE_channels <- DE_channels[with(DE_channels, order(-views)), ]

IN_channels<-sqldf("select channel_title, count(views) as views, country from music_data where country='India' group by channel_title")
IN_channels <- IN_channels[with(IN_channels, order(-views)), ]

top5_channels<-rbind(US_channels[1:5,],CA_channels[1:5,],DE_channels[1:5,],IN_channels[1:5,])

ggplot(data=top5_channels,aes(x=channel_title,y=views))+
  labs(title='5 Most Popular Music Channels by Views')+
  geom_bar(stat = 'identity',aes(fill=country))+
  facet_grid(country~.)+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.x = element_text(angle = 30))

# final visualization

z =filter(top5_channels,top5_channels$country=='USA')
p1<-ggplot(data =z )
p1<-p1+
  labs(title='5 Most Popular Music Channels in USA by Views')+
  geom_bar(stat = 'identity',fill='firebrick',width=.5,aes(x=channel_title,y=views))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.x = element_text(angle = 30))+
  scale_x_discrete(limits= z$channel_title)
p1

z =filter(top5_channels,top5_channels$country=='Canada')
p2<-ggplot(data =z )
p2<-p2+
  labs(title='5 Most Popular Music Channels in Canada by Views')+
  geom_bar(stat = 'identity',fill='steelblue',width=.5,aes(x=channel_title,y=views))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.x = element_text(angle = 30))+
  scale_x_discrete(limits= z$channel_title)
p2

z =filter(top5_channels,top5_channels$country=='Germany')
p3<-ggplot(data =z)
p3<-p3+
  labs(title='5 Most Popular Music Channels in Germany by Views')+
  geom_bar(stat = 'identity',fill='darkgreen',width=.5,aes(x=channel_title,y=views))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.x = element_text(angle = 30))+
  scale_x_discrete(limits= z$channel_title)
p3

z =filter(top5_channels,top5_channels$country=='India')
p4<-ggplot(data =z )
p4<-p4+
  labs(title='5 Most Popular Music Channels in India by Views')+
  geom_bar(stat = 'identity',fill='tomato1',width=.5,aes(x=channel_title,y=views))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.x = element_text(angle = 30))+
  scale_x_discrete(limits= z$channel_title)
p4

grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

# Visualization 2: Best time to upload Videos in different Countries

publish_data<-sqldf("select publish_hour,count(views)as views, country from music_data group by publish_hour,country order by country")

# Iteration 1
# without x-axis labels

ggplot(data=publish_data,aes(x=publish_hour,y=views))+
  labs(y='Views',title='Best time to upload Videos')+
  geom_line(aes(colour=country),size=1)

# final visualization
ggplot(data=publish_data,aes(x=publish_hour,y=views))+
  labs(y='Views',title='Best time to upload Videos')+
  geom_line(aes(colour=country),size=1)+geom_point(aes(colour=country))+
  scale_x_continuous("Upload Hour", labels = as.character(publish_data$publish_hour), breaks = publish_data$publish_hour)

# Visualization 3: Most frequently used tags in Popular Videos across different Countries

# Word Cloud for USA

# Load the data as a corpus
data1<-subset(music_data,music_data$country=='USA')$tags
docs1 <- Corpus(VectorSource(data1))

# Text Transformation
# Replacing "/", "@" and "|" with space
toSpace1 <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs1 <- tm_map(docs1, toSpace1, "/")
docs1 <- tm_map(docs1, toSpace1, "@")
docs1 <- tm_map(docs1, toSpace1, "\\|")

# Cleaning text

# Convert the text to lower case
docs1 <- tm_map(docs1, content_transformer(tolower))
# Remove numbers
docs1 <- tm_map(docs1, removeNumbers)
# Remove english common stopwords
docs1 <- tm_map(docs1, removeWords, stopwords("english"))
# Remove punctuations
docs1 <- tm_map(docs1, removePunctuation)
# Eliminate extra white spaces
docs1 <- tm_map(docs1, stripWhitespace)

inspect(docs1)

# Build a term-document matrix

dtm1 <- TermDocumentMatrix(docs1)
m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)

# Generate word cloud

set.seed(1234)
wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

png("USACloud.png", width=12, height=8, units="in", res=300)
wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

# Word Cloud for Canada

# Load the data as a corpus
data2<-subset(music_data,music_data$country=='Canada')$tags
docs2 <- Corpus(VectorSource(data2))

# Text Transformation
# Replacing "/", "@" and "|" with space
toSpace2 <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs2 <- tm_map(docs2, toSpace2, "/")
docs2 <- tm_map(docs2, toSpace2, "@")
docs2 <- tm_map(docs2, toSpace2, "\\|")

# Cleaning text

# Convert the text to lower case
docs2 <- tm_map(docs2, content_transformer(tolower))
# Remove numbers
docs2 <- tm_map(docs2, removeNumbers)
# Remove english common stopwords
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
# Remove punctuations
docs2 <- tm_map(docs2, removePunctuation)
# Eliminate extra white spaces
docs2 <- tm_map(docs2, stripWhitespace)

# Build a term-document matrix

dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)

# Generate word cloud

set.seed(1234)
wordcloud(words = d2$word, freq = d2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

png("CanadaCloud.png", width=12, height=8, units="in", res=300)
wordcloud(words = d2$word, freq = d2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

# Word Cloud for Germany

# Load the data as a corpus
data3<-subset(music_data,music_data$country=='Germany')$tags
docs3 <- Corpus(VectorSource(data3))

# Text Transformation
# Replacing "/", "@" and "|" with space
toSpace3 <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs3 <- tm_map(docs3, toSpace3, "/")
docs3 <- tm_map(docs3, toSpace3, "@")
docs3 <- tm_map(docs3, toSpace3, "\\|")

# Cleaning text

# Convert the text to lower case
docs3 <- tm_map(docs3, content_transformer(tolower))
# Remove numbers
docs3 <- tm_map(docs3, removeNumbers)
# Remove english common stopwords
docs3 <- tm_map(docs3, removeWords, stopwords("english"))
# Remove punctuations
docs3 <- tm_map(docs3, removePunctuation)
# Eliminate extra white spaces
docs3 <- tm_map(docs3, stripWhitespace)

# Build a term-document matrix

dtm3 <- TermDocumentMatrix(docs3)
m3 <- as.matrix(dtm3)
v3 <- sort(rowSums(m3),decreasing=TRUE)
d3 <- data.frame(word = names(v3),freq=v3)

# Generate word cloud
set.seed(1234)
wordcloud(words = d3$word, freq = d3$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

png("GermanyCloud.png", width=12, height=8, units="in", res=300)
wordcloud(words = d3$word, freq = d3$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

# Word Cloud for India

# Load the data as a corpus
data4<-subset(music_data,music_data$country=='India')$tags
docs4 <- Corpus(VectorSource(data4))

# Text Transformation
# Replacing "/", "@" and "|" with space
toSpace4 <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs4 <- tm_map(docs4, toSpace4, "/")
docs4 <- tm_map(docs4, toSpace4, "@")
docs4 <- tm_map(docs4, toSpace4, "\\|")

# Cleaning text

# Convert the text to lower case
docs4 <- tm_map(docs4, content_transformer(tolower))
# Remove numbers
docs4 <- tm_map(docs4, removeNumbers)
# Remove english common stopwords
docs4 <- tm_map(docs4, removeWords, stopwords("english"))
# Remove punctuations
docs4 <- tm_map(docs4, removePunctuation)
# Eliminate extra white spaces
docs4 <- tm_map(docs4, stripWhitespace)

# Build a term-document matrix

dtm4 <- TermDocumentMatrix(docs4)
m4 <- as.matrix(dtm4)
v4 <- sort(rowSums(m4),decreasing=TRUE)
d4 <- data.frame(word = names(v4),freq=v4)

# Generate word cloud
set.seed(1234)
wordcloud(words = d4$word, freq = d4$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

png("IndiaCloud.png", width=12, height=8, units="in", res=300)
wordcloud(words = d4$word, freq = d4$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

# Iteration 1

par(mfrow=c(2,2))
wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = d2$word, freq = d2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = d3$word, freq = d3$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = d4$word, freq = d4$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

# Iteration 2

rl <- lapply(list("USACloud.png","CanadaCloud.png","GermanyCloud.png","IndiaCloud.png"), png::readPNG)
gl <- lapply(rl, grid::rasterGrob)
plot.new()
do.call(gridExtra::grid.arrange, gl)
title(paste0("Most frequently used tags in Popular Videos for USA, Canada, Germany and India (clockwise)"), font.main= 1)

# Final visualization

img1 <- readPNG( "USACloud.png")
img2 <- readPNG( "CanadaCloud.png")
img3 <- readPNG( "GermanyCloud.png")
img4 <- readPNG( "IndiaCloud.png")
g1 <- rasterGrob(img1, interpolate=TRUE, width = unit(11,"in"), height=unit(5,"in"))
g2 <- rasterGrob(img2, interpolate=TRUE,width = unit(11,"in"), height=unit(5,"in"))
g3 <- rasterGrob(img3, interpolate=TRUE, width = unit(11,"in"), height=unit(5,"in"))
g4 <- rasterGrob(img4, interpolate=TRUE,width = unit(11,"in"), height=unit(5,"in"))
plot.new()
grid.arrange(g1, g2,g3,g4, nrow=2,widths=c(7,7))
title(paste0("Most frequently used tags in Popular Videos for USA, Canada, Germany and India (clockwise)"), font.main= 1)
dev.off()



