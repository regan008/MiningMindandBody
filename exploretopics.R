topicwordsweightresult <- read.table("mallet_output_files/topic_words_weight.txt", header=F, sep="\t")

exploretopicwords <- function(topicnum, numwords) {
    require(magrittr)
    require(wordcloud2)
    require(dplyr)
    topicwords <- topicwordsweightresult %>% filter(V1 == topicnum) %>% arrange(desc(V3))
    topicwords$V1 <- NULL
    topicwords <- topicwords[1:numwords,]
    wc <- wordcloud2(topicwords)
    return(wc)
}

topicwords <- topicwordsweightresult %>% filter(V1 == 42) %>% arrange(desc(V3))
topicwords$V1 <- NULL
topicwords[1:30,]
#outputdoctopicsresult
exploretopicwords(42, 30)
exploretopics_byyear <- function(topicnum) {
  require(magrittr)
  require(ggplot2)
  require(plyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  byyear <- ddply(tm_values, .(topic, Year), summarise, mean_value = mean(value))
  filterbytopicnum <- byyear %>% filter(topic == topicnum)
  ggplot(data=filterbytopicnum, aes(x=Year, y=mean_value)) + geom_line() + geom_point()  + scale_x_continuous(name="Year", breaks=seq(1895, 1936, 2)) #+ scale_y_continuous(name="Average Topic Weight", labels = "percent")
}
exploretopics_byyear(57)
for(i in 0:59) {
  exploretopics_byyear(i)
  path = paste("files/topics_by_year/topic_",i,".png", sep="")
  ggsave(path, width=10, height=4)
}

for(i in 1:60) {
  
  path = paste("files/wordclouds/topic_",i,".png", sep="")
  png(path,width=500,height=300)
  exploretopicwords(i, 40)
  dev.off()
}


explorealltopicsbyyear <- function() {
    require(magrittr)
    require(ggplot2)
    require(plyr)
    tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
    #byyear <- tm_values %>% group_by(topic, Year) %>% arrange() %>% summarize(total = sum(value) / 100)
    byyear <- ddply(tm_values, .(topic, Year), summarise, mean_value = mean(value))
    sp <- ggplot(data=byyear, aes(x=Year, y=mean_value)) + geom_line() + geom_point()
    sp + facet_wrap(~ topic, ncol=6)
}
explorealltopicsbyyear()

explorealltopic_edchanges <- function() {
  require(magrittr)
  require(ggplot2)
  require(plyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  #byyear <- tm_values %>% group_by(topic, Year) %>% arrange() %>% summarize(total = sum(value) / 100)
  byyear <- ddply(tm_values, .(topic, Year), summarise, mean_value = mean(value))
  sp <- ggplot(data=byyear, aes(x=Year, y=mean_value)) + geom_line() + geom_point()
  years_with_ed_change <- c(1896,1899,1900,1901,1903,1904,1907,1909,1911,1912,1927,1932,1934)
  sp <- sp %>% + geom_vline(xintercept = years_with_ed_change, color="blue")
  sp + facet_wrap(~ topic, ncol=6)
}
explorealltopic_edchanges()




#find the docs heavily associated with a topic
doc_topic_assoc <- function(topicnum, numberofrows) {
  require(magrittr)
  require(ggplot2)
  require(dplyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  bytopic <- tm_values %>% filter(topic == topicnum) %>% arrange(desc(value)) 
  head(bytopic, n=numberofrows)
}
doc_topic_assoc(57, 10)

exploretopicbyyear_editorialcomms <- function(topicnum) {
  require(magrittr)
  require(ggplot2)
  require(plyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  byyear <- ddply(tm_values, .(topic, Year), summarise, mean_value = mean(value))
  filterbytopicnum <- byyear %>% filter(topic == topicnum)
  plot <- ggplot(data=filterbytopicnum, aes(x=Year, y=mean_value)) + geom_line() + geom_point() + scale_x_continuous(name="Year", breaks=seq(1895, 1936, 2)) 
  years_with_ed_change <- c(1896,1899,1900,1901,1903,1904,1907,1909,1911,1912,1927,1932,1934)
  plot <- plot %>% + geom_vline(xintercept = years_with_ed_change, color="blue")
  plot
}
exploretopicbyyear_editorialcomms(23)


word_count<-function(txt_doc){
  con<-file(txt_doc, "r", blocking=FALSE)
  x<-readLines(con)
  #Remove YAML front matter on Rmd
  if(length(grep("---",x))>0){x<-x[-seq(1,max(grep("---",x)))]}
  wrds<-0
  for(line in x){
    #Removes non character and splits
    split_line<-strsplit(gsub("[^[:alnum:] ]", "", line), " +")[[1]]
    #Removes empty string
    split_line<-split_line[split_line!=""]
    wrds<-wrds+length(split_line)
  }
  return(wrds)
}
files <- list.files(path="txt/", pattern="*.txt")
wc <-  function(countwords) {
  for(file in files){
    word_count(file)
  }
}

word_count("txt/1930_April.txt")
#read.table("mallet_output_files/word_topic_counts.txt")
