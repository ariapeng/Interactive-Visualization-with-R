# Yahui Peng
# STA 6233
# How has Chewy.com performed on dog food products?
####Bring In Libraries####
library(rvest)
library(installr)
library(tidyverse)
library(plyr)
library(data.table)
library(RColorBrewer)
library(ggthemes)
library(wordcloud)
library(tm)
library(SnowballC)
library(stopwords)

####Create functions####
#Create functions to clean white space
trim <- function( x ) {
  gsub('(^[[:space:]]+|[[:space:]]+$)', '', x)
}

#Create functions to output errors and null,empty values to NA
try.NA <- function(x){
  if(inherits(try(x), 'try-error') || is.null(x) || is.empty(x)) {
    return(NA)
  } 
  else {
    return(x)
  }
}

#Create a function for later string extraction
my.split <- function(x,y){
  trim(sapply(strsplit(sapply(
    strsplit(as.character(x),y),'[[',2),'\n'),'[[',2)) 
}

#Create a function for later string extraction
get.n <- function(x,y){
  parse_number(sapply(str_extract_all(
    sapply(strsplit(as.character(x),y),'[[',2),'\\d+\\.?\\d?'),'[[',1))
}

#Create a function for later string extraction with output format as percentage or 'ppm'
try.NA.pasteXY <- function(x,y) {
  if(inherits(try(x), 'try-error') || is.null(x) || is.empty(x)) {
    return(NA)
  } else{
    return(paste0(as.character(x),y))
  }
}

#Create a function for later review scrapping
to.LINK <- function(x,y,z) {
  if( is.na(x)) {
    return(y)
  } else {
    return(paste0('https://www.chewy.com',x,z))
  }
}

#### Find the html to scrape####
home <- read_html('https://www.chewy.com/b/food-332')
number <- home %>% 
  html_nodes('.results-pagination ul li:nth-child(9) a') %>% html_text()

#Build Container for all html,brand,name,price,size
all_links <- data.frame()
# Grab the html for every product
for (i in 1:as.numeric(number)) {
  url <- read_html(paste0('https://www.chewy.com/b/food_c332_p',i))
  for (j in 1:41) { #The biggest [j] in the CSS selector is 41 in page 1
    tryCatch({
      nodes <- url %>% 
        html_nodes(paste0('article:nth-child(',j,') a,',
          'article:nth-child(',j,') a section div.ga-eec__brand,',
          'article:nth-child(',j,') a section div.ga-eec__name,',
          'article:nth-child(',j,') a section div.ga-eec__price,',
          'article:nth-child(',j,') a section div.ga-eec__variant'))
      html <- nodes[1] %>% 
        html_attr('href')%>% nth(1)%>%
        {paste0('https://www.chewy.com',.)} %>% try.NA()
      brand <- nodes[3] %>%
        html_text() %>% try.NA()
      name <- nodes[2] %>%
        html_text() %>% {gsub('[[:alpha:]]/d','',.)} %>%#Clean titles with irregular prefix
        str_remove(brand)%>% trim()%>% try.NA()
      price <- nodes[4] %>%
        html_text() %>% as.numeric()%>% try.NA()
      size <- nodes[5] %>% 
        html_text()%>% try.NA()
      links <- data.frame(brand,name,size,price,html)
      
      print(paste0('Finished page ',i,', item ',j))
      all_links <- rbind(all_links,links)
    }, error=function(e){cat(conditionMessage(e))})  
  }
}

# Clean irregular html
irregulars <- c('https://www.chewy.comNA',
                'https://www.chewy.comhttps://www.chewy.com/b/premium-food-11728',	
                'https://www.chewy.com/acana-singles-limited-ingredient-diet/dp/285976')
all_links <- irregulars %>%
  {all_links[!grepl(paste(.,collapse = '|'),all_links$html),]} %>%
  .[!duplicated(.$html),] %>% arrange(brand,name)

####Scrape general stats####
#Create file paths below#
brand <- all_links$brand
name <- all_links$name
price <- all_links$price
size <- all_links$size
path<- all_links$html
#Create a blank dataframe as a container
stats <- data.frame()
#Extract stats from each previous scrapped html
for (i in 1:nrow(all_links)) { 
  tryCatch({
    link <- read_html(path[i])
    #Data to scrape for each cleaned html
    content <- link %>%
      html_nodes('.cw-tabs__content--right') %>% 
      html_text() %>% nth(1) %>%
      {tolower(gsub('\n[[:space:]]+', '\n', .))} %>% try.NA()
    form <- my.split(content,'food form')%>% try.NA()
    breed <- my.split(content,'breed size') %>% try.NA()
    weight <- my.split(content,'weight') %>% try.NA()
    life_stage <- my.split(content,'lifestage') %>% try.NA()
    special_diet <- my.split(content,'special diet') %>% try.NA()
    nutro <- link %>%
      html_nodes('#Nutritional-Info section.cw-tabs__content--right') %>% 
      html_text() %>% {(gsub('[*]|[\n]|[(]|[)]',' ',.))} %>%
      trim() %>% tolower() %>% try.NA()
    protein <- get.n(nutro,'protein') %>%
      try.NA.pasteXY(y='%')
    fat <- get.n(nutro,'fat') %>% try.NA.pasteXY(y='%')
    omega6 <- get.n(nutro,'omega-6 fatty acids') %>% 
      try.NA.pasteXY(y='%')
    calcium <- get.n(nutro,'calcium') %>% 
      try.NA.pasteXY(y='%')
    phosphorus <- get.n(nutro,'phosphorus') %>% 
      try.NA.pasteXY(y='%')
    glucosamine <- get.n(nutro,'glucosamine') %>%
      try.NA.pasteXY(y=' ppm')
    recommend <- link %>%
      html_nodes('.ugc-list__recap__recommend p:nth-child(1) span') %>%
      html_text() %>% parse_number() %>% try.NA.pasteXY(y='%')
    review_content<- link %>%
      html_nodes('.ugc-list_stars') %>% html_text() %>%
      trim()%>% parse_number()%>% try.NA()
    review_num <- review_content[1]%>% try.NA()
    rating <- review_content[2]%>% try.NA()
    #Create a table for the data
    info <- data.frame(Brand=brand[i], Name=name[i], Form=form, Weight=weight, Size=size[i],
                       Price=price[i], Review_Number=review_num,Rating=rating, 
                       Recommendation_Percentage=recommend, Life_Stage=life_stage,
                       Special_diet=special_diet, Breed=breed, Protein=protein, Fat=fat,
                       Omega_6s=omega6, Calcium=calcium, Phosphorus=phosphorus,
                       Glucosamine=glucosamine, html=path[i])
    #Bind the two datasets
    stats <- rbind(stats,info)
    print(paste0('Finished with: link ',i))
   }, error=function(e){cat(conditionMessage(e))})
}
stats <- stats%>% arrange(Brand,Name) 
stats$Name <- ifelse(grepl('Magical Dinner Dust Duck Duck, Goose Recipe Freeze-Dried Raw Dog Food Topper',stats$Name),
                     "Marie's Magical Dinner Dust Duck Duck, Goose Recipe Freeze-Dried Raw Dog Food Topper, 7-oz bag",stats$Name)

####Scrape review details####
rev_num <- stats$Review_Number
review <- data.frame()
for (i in 1:nrow(all_links)) { 
  tryCatch({
    page_num <- ceiling(0.1*rev_num[i])
    rev_url <- gsub('/dp/','/product-reviews/',path[i])
    rev_html <- rev_url %>% read_html() %>%
      html_nodes('#ugc-list-content footer section ul li:nth-child(2) a')
    first_link <- rev_html %>% html_attr('data-target') 
    general_link <- gsub('ALL_STARS.*','ALL_STARS&pageNumber=',first_link)%>% try.NA
      for (j in 1:as.numeric(page_num)) {
        pagelink <- to.LINK(general_link,rev_url,j) %>% read_html()
        for (k in 1:10) {
          rev_nodes <- pagelink %>% 
            html_nodes(paste0('li:nth-child(',k,') header, ',
                              'li:nth-child(',k,') header picture img, ',
                              'li:nth-child(',k,') header p span:nth-child(2), ',
                              'li:nth-child(',k,') header div h3, ',
                              'li:nth-child(',k,') section p span.ugc-list__review__display'))
          rev <- rev_nodes[1] %>% html_text()
          per_rating <- rev_nodes[2] %>% html_attr('src')%>%
            {str_sub(parse_number(.),start = 2,end = 2)} %>% as.numeric()
          title <- rev_nodes[3] %>%
            html_text()%>% {gsub('\n','',.)}%>% trim()
          year <- rev_nodes[4] %>%
            html_attr('content')%>% as.Date() %>% format(format = '%Y') %>% as.numeric()
          full <- rev_nodes[5] %>%
            html_text()
          rating_date <- data.frame( Brand=brand[i], Name=name[i], Rating=per_rating,
                                     Year = year, Review_Title=title, Full_Review=full)
          print(paste('Finished product',i,'page',j,'review',k))
          review <- rbind(rating_date,review)
      }
    }
  }, error=function(e){cat(conditionMessage(e))})
}


####Clean data ####
#clean Weight when Weight and Size doesn't give enough information
weight.patch <- data.frame()
for (i in 1:nrow(stats)) {
  if (!is.na(stats[i,"Weight"])) {
    weight1 <- stats[i,"Weight"]
  } else if (nchar(stats[i,"Size"])==0) {
    weight1 <-trim(str_remove(stats[i,"Name"],
                              paste0(sapply(strsplit(stats[i,"Name"],","),'[[',1),',')))
  } else { 
    weight1 <- stats[i,"Size"]       
  }
  neweight <- data.frame(stats$Name[i],weight1)#
  weight.patch <- rbind(weight.patch,neweight)
}
names(weight.patch)[1] <- "Name"
weight.patch$weight1 <- gsub('-',' ',weight.patch$weight1)

weight.clean <- data.frame()
for (i in 1:nrow(weight.patch)) {
 weight2 <- as.numeric(unlist(regmatches(weight.patch[i,'weight1'], 
                                          gregexpr("[[:digit:]]+|[0-9]+\\.[0-9]+",weight.patch[i,'weight1'])))) 
  if (grepl(' oz',weight.patch[i,'weight1'])& grepl('of',weight.patch[i,'weight1'])){
      weight3 <- nth(weight2,1)*nth(weight2,2)/16
  } else if (grepl('lbs',weight.patch[i,'weight1'])| grepl('pounds',weight.patch[i,'weight1'])|
             grepl(' lb',weight.patch[i,'weight1'])) {
    weight3 <- nth(weight2,1)
  } else if (grepl('ounces',weight.patch[i,'weight1'])| grepl('oz',weight.patch[i,'weight1'])){
    weight3 <- weight2/16
  } else if (i %in% c(147,2175,2177,2254,2529,3057,3512,3513,3539,3856)) {
    weight3 <- weight2/453.592
  }
    else {
    weight3 <- weight.patch[i,'weight1']
    }
  weight3 <- as.character(weight3)
  weight3 <- data.frame(weight.patch$Name[i],weight3) # 
  weight.clean <- rbind(weight.clean,weight3)
}

names(weight.clean)[1] <- "Name"
weight.clean[c(117,127,711,1126,1172,1489,1490,1499,2447,2465,2485,2693,2696,
               2697,2771,2790,2849,2851,2973,3455,3862,3879),"weight3"] <- 
  c("33.375","25.125","5.29","0.265","0.463","9.6","9.6","9.75","0.5","30","28.5",
    "5","5","9.75","0.066","41","0.3125","0.3125","15.25","0.265","9.675","0.375")
weight.clean <- as.data.frame(weight.clean[-c(2176,3863),])

#clean Name,Breed,and Special_diet
stats.clean <- stats
stats.clean$Form <- stats.clean$Form %>% gsub('frozen','freeze-dried',.) %>% gsub('air-dried','dehydrated',.)
stats.clean$Breed <- stats.clean$Breed %>% gsub(', n/a','',.) %>% gsub('n/a',NA,.)
stats.clean$Special_diet <- stats.clean$Special_diet %>% gsub(', n/a','',.) %>% gsub('n/a',NA,.)

#create a new var Weight.lb and unit price var Price.per.lb for correct weight info
stats.clean$Weight.lb <- weight.clean$weight3 %>% parse_number()%>% round(digits=2)
names(stats.clean$Weight) <- "Weight.lb"
stats.clean$Price.per.lb <- round(stats.clean$Price/stats.clean$Weight.lb,digits=2)

#remove miscategorized obs in stats and review dfs where Form not included in dog food category
stats.clean <- stats.clean %>%
  {.[.$Form!='treats'& !(grepl('supplement',.$Form)&!grepl('Top',.$Name)),]}%>% .[,c(1:3,6,20,21,7:19,4,5)]
review <- setDT(review)[Name %chin% stats.clean$Name]%>%
  arrange(desc(Year),desc(Rating),desc(Brand))

####Analyze data####
#create frequency table for general stats
t.spcldiet <- table(trim(unlist(strsplit(as.character(stats.clean$Special_diet), ","))))%>%
  as.data.frame()%>% arrange(desc(Freq))
t.spcldiet[15,2] <-  sum(t.spcldiet[c(15:22),2])
t.spcldiet <- t.spcldiet[c(1:15),]
t.breed <- table(trim(unlist(strsplit(as.character(stats.clean$Breed), ","))))%>%
  as.data.frame()%>% arrange(desc(Freq))
t.lifestg <- table(trim(unlist(strsplit(as.character(stats.clean$Life_Stage), ","))))%>%
  as.data.frame()%>% arrange(desc(Freq))
t.form <- table(trim(unlist(strsplit(as.character(stats.clean$Form), ",")))) %>%
  as.data.frame()%>% arrange(desc(Freq))%>% .[c(1:4,6),]

#analyze review frequencies, mean, and estimated yearly sales
counts <- ddply(review, .(review$Year, review$Name), nrow) %>% data.frame()
names(counts) <- c("Year", "Name", "Freq")
counts$Price <- stats.clean$Price[match(counts$Name,stats.clean$Name)]
counts$Unit_Price <- stats.clean$Price.per.lb[match(counts$Name,stats.clean$Name)]
counts$Sales <- counts$Price*counts$Freq

#summarise ratings and reviews
cnt.sales <- counts %>%
  group_by(Year) %>%
  dplyr::summarise(Est_Sales = round(sum(Sales),digits=2))

cnt.mean.rating <- review %>%
  group_by(Year) %>%
  dplyr::summarise(mean_Rating = round(mean(Rating),digits=1),stdReview_Count = n()) 

cnt.summary <- merge(cnt.mean.rating,cnt.sales,by='Year')
cnt.summary[10,3] <- cnt.summary[10,3]*365/74
cnt.summary[10,4] <- cnt.summary[10,4]*365/74
names(cnt.summary) <- c("Year","Rating","Review_Count","Estimated_Sales")

cnt.rating <- review %>%
  arrange(Year) %>% 
  group_by(!!!syms(c('Year','Rating'))) %>%
  dplyr::summarise(Review_Count = n())
cnt.rating$Review_Count <- as.numeric(cnt.rating$Review_Count)
cnt.rating[c(46:50),3] <- cnt.rating[c(46:50),3]*365/74

ave.price.brand <- stats.clean %>%
  group_by(Brand) %>%
  dplyr::summarise(mean_Price = round(mean(Price),digits=2),Variety = n()) %>%
  arrange(desc(Variety))
names(ave.price.brand) <- c("Brand","Price","Variety") 

best.rev.brand.price <- na.omit(stats.clean[c(1,4,7)]) %>%
  group_by(Brand) %>%
  dplyr::summarise(Mean_Price = round(mean(Price),digits=2),Review_Count=sum(Review_Number),Product_Variety = n()) %>%
  arrange(desc(Product_Variety))
varity10.rev.brand.price <- best.rev.brand.price[c(1:10),]
rev10.brand.price <- best.rev.brand.price %>% arrange(desc(Review_Count))
rev10.brand.price <- best.rev.brand.price[c(1:10),]

####Plotting####
dev.off()
#create a pie chart of var Breed
ggplot(t.breed, aes(x="", y=Freq, fill=Var1)) +
geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
   coord_polar("y") +
geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100), "%"), x = 1.7),
             size=3,fontface = "bold", colour="black",
            position = position_stack(vjust = 0.5)) +
labs(x = NULL, y = NULL, fill = NULL, 
    title = "Dog Breed Makeup") +
guides(fill = guide_legend(reverse = TRUE)) +
scale_fill_manual(values = c( '#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f')) +
theme_classic() +
theme(axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust=-2,size=13,color = "#666666"))
ggsave("pie.breed.svg")  
#create a pie chart of var Lige_Stage
ggplot(t.lifestg, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100), "%"), x = 1.6),
            size=4, colour="black",
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Life Stage Makeup") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c('#de77ae','#7f3b08',"#ffd700","#bcbcbc", "#ffa500",'#7fbc41','#dfc27d','#542788','#2d004b','#e08214','#ffff99','#386cb0','#4393c3','#beaed4','#fee0b6','#f7f7f7','#d8daeb','#b2abd2')) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=-2,size=13,color = "#666666"))
ggsave("pie.lifestge.svg")  
#create a pie chart of var Form
ggplot(t.form, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100), "%"), x = 1.6),
            size=3,fontface = "bold", colour="black",
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Dog Food Form Makeup") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0')) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=-2,size=13,color = "#666666"))
ggsave("pie.form.svg")  

#create a pie chart of var Special_Diet
ggplot(t.spcldiet, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100), "%"), x = 1.6),
            size=4, colour="black",
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Special Diet Makeup") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c('#de77ae','#7f3b08',"#ffd700","#bcbcbc", "#ffa500",'#7fbc41','#dfc27d','#542788','#2d004b','#e08214','#ffff99','#386cb0','#4393c3','#beaed4','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0')) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=-2,size=13,color = "#666666"))
ggsave("pie.spcldiet.svg")  

options(scipen=999)  # turn-off scientific notation like 1e+48
# Scatterplot
ggplot(varity10.rev.brand.price, aes(fill=Brand, y=Mean_Price, x=Product_Variety)) + 
  geom_bar(stat="identity",width=1.5)+theme_minimal() +    
  labs(y="Mean Price, $", 
       x="Product Variety", 
       title="Top 10 in Product Variety"
  )   + theme(panel.background = element_rect(fill = "grey97"),
              panel.border = element_blank(),
              legend.position = "right")
ggsave("Top 10 in Product Variety.svg") 

# create a point plot for 10 Most reviewed Products
ggplot(rev10.brand.price, aes(y=Mean_Price, x=Review_Count)) + 
  geom_point(aes(color=Brand,size=Product_Variety))+theme_minimal() +    
  labs(y="Mean Price, $", 
       x="Review Count", 
       title="10  Most Reviewed Product"
  )   + theme(panel.background = element_rect(fill = "grey97"),
              panel.border = element_blank(),
              legend.position = "right")
ggsave("10 Most reviewed.svg") 
# scatter-line relationship between review counts and recommendation percentage
stats.sub <- stats.clean[,c(4,8,9)]
stats.sub$Recommendation_Percentage <- parse_number(stats.sub$Recommendation_Percentage)
ggplot(na.omit(stats.sub),aes(x=Recommendation_Percentage,y=Rating))+
  geom_point(aes(color=Price))+ 
  geom_smooth(method="loess", se=F) +
  labs(subtitle="", 
       y="Rating", 
       x="Recommendation Percentage, %", 
       title=""
  )
ggsave("relation.svg") 
# create a scatter-line plot for review stats
ggplot(cnt.summary,aes(x=Year,y=Estimated_Sales))+
  geom_point(aes(size=Rating,col=Review_Count))+  
  geom_smooth(method="loess", se=F) +
  labs(subtitle="Year 2021 is Estimated Based on Data by Mar 15", 
       y="Estimated Sales", 
       x="Year", 
       title="Estimated Yearly Sales"
       )
ggsave("cnt.rating.year.svg")  

# Stacked bar chart for rating percentiles
ggplot(cnt.rating, aes(fill=Rating, y=Review_Count, x=Year)) + 
  geom_bar(position="stack", stat="identity") +    
  labs(subtitle="Year 2021 is Estimated Based on Data by Mar 15", 
       y="Estimated Sales", 
       x="Year", 
       title="Rating Percentile"
  )   + theme(panel.background = element_rect(fill = "grey97"),
             panel.border = element_blank(),
             legend.position = "right")
ggsave("Ratio Percentile.svg") 

####Create word clouds ####
#create a wordcloud for complete reviews
#combine review title and body
review_complete <- review %>% unite(Review_Title,Full_Review,sep = '') %>%
  dplyr::rename(Title_and_Full=Review_Title)
text <- Corpus(VectorSource(review_complete$Title_and_Full))
inspect(text)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
text <- text %>% 
  tm_map(toSpace, "/") %>%
  tm_map(toSpace, "'") %>%
  tm_map(toSpace, "@") %>%
  tm_map(toSpace, "\\|") %>%
  tm_map(content_transformer(tolower)) %>% # Convert the text to lower case
  tm_map(removeWords, stopwords("english")) %>%# Remove english common stopword #
  tm_map(removePunctuation) %>% # Remove punctuations
  tm_map(stripWhitespace) %>% # Eliminate extra white spaces
  tm_map(stemDocument) %>% # Text stemming
  tm_map(removeWords, stopwords("english")) %>%# Remove english common stopword #
  tm_map(removeWords, c("dog","pet","food","product","can","seem","also","just",
                        "one","two","three","any","year","month","day","now","will","look",
                        "get","got","make","do","use","dont","very","even","because","since","ive")) 
# inspect frequent words
tdm <- TermDocumentMatrix(text)
#tdm <- Term Document Matrix
tdm2 <- removeSparseTerms(tdm,sparse = 0.99)

#Create Word Clouds for product name#
m <- as.matrix(tdm2)
v <- sort(rowSums(m),decreasing=TRUE)
c <- data.frame(word = names(v),freq=v)
#Generate the Word cloud
set.seed(1234)
par(mar = c(5.1, 4.1, 4.1, 2.1))
wordcloud(words = c$word, freq = c$freq, min.freq=200,
          random.order = FALSE,rot.per=.35, max.words=100,
          colors=brewer.pal(12,name="Paired"))
####Create word clouds for product name####
product <- Corpus(VectorSource(stats.clean$Name))
inspect(product)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
myStopwords <- c("dog","cat","recipe","formula","case","breed","pet","food","treat","product","can","oz","jar","bag","pouch")
product <- product %>% 
  tm_map(toSpace, "/") %>%
  tm_map(toSpace, "'") %>%
  tm_map(toSpace, "@") %>%
  tm_map(toSpace, "\\|") %>%
  tm_map(content_transformer(tolower)) %>% # Convert the text to lower case
  tm_map(removeNumbers) %>%# Remove numbers #
  tm_map(removePunctuation) %>% # Remove punctuations
  tm_map(stripWhitespace) %>% # Eliminate extra white spaces
  tm_map(stemDocument) %>% # Text stemming
  tm_map(removeWords, c(myStopwords,stopwords("english")))# Remove english common stopword #
# inspect frequent words
tdm_p <- TermDocumentMatrix(product)
#tdm <- Term Document Matrix
tdm2_p <- removeSparseTerms(tdm_p,sparse = 0.996)
str(tdm2_p)
#Create Word Clouds for overall review#
m_p <- as.matrix(tdm2_p)
v_p <- sort(rowSums(m_p),decreasing=TRUE)
c_p <- data.frame(word = names(v_p),freq=v_p)
#Generate the Word cloud
set.seed(1234)
par(mar = c(2.1, 2.1, 2.1, 1.1))
wordcloud(words = c_p$word, freq = c_p$freq, min.freq=1,
          random.order = FALSE,rot.per=.25, max.words=150,
          colors=brewer.pal(8,name="Dark2"))