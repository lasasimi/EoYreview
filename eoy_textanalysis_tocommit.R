library(dplyr)
library(tidytext) #for unnest_tokens
library(stringr)
library(googlesheets4) #for importing data from Gsheets
library(ggplot2)
library(tidyr)
library(forcats)

### Weekly Checkup Analysis --------------------------------------------------
# importing data from GSheets
df_weekly <- read_sheet("https://docs.google.com/spreadsheets/d/1RQWmA88TT02Zn0XCN_I7rF_s6efkHn6XlXltqAs29iY/edit?resourcekey#gid=1059060710")

# rename columns
colnames(df_weekly) <- c("time","happy", "work", "fri_fam", "self", "learned", "to_learn", "barrier", "solution", "gratitude", "notes")

## Visualizing weekly rating per aspect
#wrangling the weekly rating data
df_rating <- df_weekly %>% 
        select(time, happy, work, fri_fam, self) %>% 
        pivot_longer(!time, names_to = "rating", values_to = "score") %>% 
        mutate(score = recode(score, "Fulfilling!" = 5, 
                               "Baik" = 4,
                               "Lumayan" = 3,
                               "Buruk" = 2,
                               "Jelek banget" = 1,
                              "Ya" = 1,
                              "Mungkin" = 0,
                              "Tidak" = -1)) #recode likert scale label to number

# create a number of week column
df_rating <- transform(df_rating, week=match(time, unique(time)))

# exclude general happiness
plot <- df_rating[!grepl("happy", df_rating$rating),]

# add additional dataviz library
library(viridis)
library(hrbrthemes)
library(plotly)

# creating the plot
plot$time <- as.Date(plot$time)
p <- ggplot(plot, aes(x=time, y=score, fill=rating))+
        geom_area(alpha=0.6, size=0.5, colour="white")+
        scale_fill_viridis(discrete=TRUE)+
        ggtitle("Rating per aspect")+
        coord_cartesian(ylim= c(1, 5))+
        scale_x_date(limits= as.Date(c("2021-01-01", "2021-12-30")))+
        facet_wrap(~rating, scale="free_y")+
        theme_ipsum()

#plotly to be fancy (but cannot upload this since I don't have server)
ggplotly(p)

#add library for pairwise correlations
library(GGally)
plot_wide <- df_rating %>% 
                select(!week) %>% 
                pivot_wider(names_from = "rating", values_from = "score") 

ggcorr(plot_wide, method = c("everything", "pearson"),
       label = TRUE)+
        ggtitle("Work is the least correlated with general happiness")+
        labs(subtitle = "...maybe I have separated work from life(?)",
        caption="...or maybe my workplace this year are too varied to impact consistently with general happiness")

## Visualizing things that I'm grateful of weekly
# wrangling the gratitude data
df_gratitude <- df_weekly %>% 
        select(time, gratitude) %>% 
        mutate(gratitude = strsplit(as.character(gratitude), "- " )) %>% 
        unnest(gratitude) 

# remove blank rows
df_gratitude <- df_gratitude[!(is.na(df_gratitude$gratitude) | df_gratitude$gratitude==""), ]

df_gratitude <- df_gratitude %>% 
        transform(week=match(time, unique(time))) %>% 
        group_by(week) %>% 
        mutate(number = row_number())

##Curiosity kicks in
# looking for the week where I can mention things I'm grateful the most (it's eight)
f_gratitude <- as.data.frame(xtabs(~number, data=df_gratitude))
df_gratitude[df_gratitude$number == 8, "week"] #it's the 15th week (a week in April)

# list the stop words that I created
stopwords <- read_sheet("https://docs.google.com/spreadsheets/d/1RQWmA88TT02Zn0XCN_I7rF_s6efkHn6XlXltqAs29iY/edit?resourcekey#gid=1059060710", sheet="Sheet2")

## 1-gram
# removing the stopwords
tidy_gratitude <- df_gratitude %>% 
        ungroup("week") %>% 
        unnest_tokens(word, gratitude) %>% 
        anti_join(stopwords) 
# counting the words
tidy_gratitude %>% 
        count(word, sort=TRUE)

## 2-grams
# create Bigram
tidy_gratitude_2 <- df_gratitude %>% 
        ungroup("week") %>% 
        unnest_tokens(bigram, gratitude, token = "ngrams", n=2)

# remove stopwords separately for the bigrams
bigrams_separated <- tidy_gratitude_2 %>% 
        separate(bigram, c("word1", "word2"), sep=" ")

bigrams_filtered <- bigrams_separated %>% 
        filter(!word1 %in% stopwords$word) %>% 
        filter(!word2 %in% stopwords$word)

# count the filtered bigrams        
bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort=TRUE) %>% 
        mutate(word_c = paste(word1, word2, sep=" "))

bigram_counts

# select only meaningful words (operationally, occured more than 5)
plot_bigram <- bigram_counts[(bigram_counts[,3]>=5),]

# create the Bigram barchart
plot_bigram %>% 
        mutate(word_c = fct_reorder(word_c, n)) %>% 
        ggplot(aes(x=word_c, y=n, fill=word_c))+
                geom_bar(stat="identity")+
                scale_fill_viridis(discrete=TRUE)+
                coord_flip()+
                theme_ipsum()+
        theme(legend.position="none")+
        xlab("")+
        ylab("")+
        ggtitle("Two words that contribute to what I'm grateful of each week")
      
## 2-gram network visualization of Bigrams with ggraph
# add library
library(igraph)
library(ggraph)

bigram_graph <- bigram_counts %>% 
        filter(n >=2) %>% 
        graph_from_data_frame()
set.seed(2021) #set seed so the visual is consistent

# Design arrow
a <- grid::arrow(type="closed", length=unit(.15, "inches"))

ggraph(bigram_graph, layout="fr") +
        geom_edge_link(aes(edge_alpha=n), show.legend=FALSE,
                       arrow=a, end_cap=circle(.07, 'inches')) +
        geom_node_point(color="lightblue", size=5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
        theme_void()

## Visualizing things I learned
# wrangling the things I learned data
df_learned <- df_weekly %>% 
        select(time, learned) %>% 
        mutate(learned = strsplit(as.character(learned), "- " )) %>% 
        unnest(learned) %>% 
        filter(learned!="") # remove blank rows

df_learned <- df_learned %>% 
        transform(week=match(time, unique(time))) %>% 
        group_by(week) %>% 
        mutate(number = row_number())

##Curiosity kicks in
#finding the week where I learn things the most (it's 7 things)
f_learned <- as.data.frame(xtabs(~number, data=df_learned)) 

#it's in the week 5 and 19
df_learned[df_learned$number == 7, "week"]
df_learned[df_learned$week==5, "time"] #week 5 is in Feb
df_learned[df_learned$week==19, "time"] #week 19 is in May


# list stop words for things I learned
stopwords_learned <- read_sheet("https://docs.google.com/spreadsheets/d/1RQWmA88TT02Zn0XCN_I7rF_s6efkHn6XlXltqAs29iY/edit?resourcekey#gid=1059060710", sheet="Sheet3")


tidy_learned <- df_learned %>% 
        ungroup("week") %>% 
        unnest_tokens(word, learned) %>% 
        anti_join(stopwords_learned) 

tidy_learned %>% 
        count(word, sort=TRUE)

# 2-gram things I learned
tidy_learned_2 <- df_learned %>% 
        ungroup("week") %>% 
        unnest_tokens(bigram, learned, token = "ngrams", n=2)

# remove stop words separately for the bigrams, counting
learned_counts <- tidy_learned_2 %>% 
        separate(bigram, c("word1", "word2"), sep=" ") %>% 
        filter(!word1 %in% stopwords_learned$word) %>% 
        filter(!word2 %in% stopwords_learned$word) %>% 
        count(word1, word2, sort=TRUE) %>% 
        mutate(word_c = paste(word1, word2, sep=" "))

learned_counts

# filter meaningful relations (operationally, those that occur >=2 times)
bigram_learned <- learned_counts %>% 
        filter(n >=2) 

# create vertices to count occurences for separate words   
vert_give <- bigram_learned %>% 
        select(!word_c) %>% 
        pivot_longer(cols=c(word1, word2), names_to="item", values_to="word") %>% 
        group_by(word) %>% summarise(n = sum(n))
set.seed(2021)

# visualize the network
bigram_learned %>% 
        graph_from_data_frame(vertices=vert_give) %>% 
        ggraph(layout="fr")+
        geom_edge_link(aes(edge_alpha=n),
                       show.legend=FALSE,
                       arrow=a, end_cap=circle(.03, 'inches'),
                       edge_colour = "purple")+
        geom_node_point(aes(size=n), show.legend=FALSE, 
                        color="grey")+ 
        scale_size(range=c(1,5))+
        geom_node_text(aes(label=name, alpha=n), vjust=1, hjust=1,size=3)+
        theme_void()+
        theme(legend.position="none")+
        ggtitle("Connection of things that I learned weekly")+
        labs(subtitle="Revisiting, creating, enjoying, advancing more")




#### Daily Checkup Analysis ---------------------------------
## Data wrangling
# import data from GSheets
df_daily  <- read_sheet("https://docs.google.com/spreadsheets/d/1IAIA0JmGM3t5Q5bXuMAfuRmRzf0uFWXOzYWLg1CjDHI/edit?resourcekey#gid=2067616189")

# rename columns
colnames(df_daily) <- c("time","feelings", "rating", "give_en", "drain_en", "values", "reasoning", "notes")

# select rating column to be analyzed
df_d_rating <- df_daily %>% 
        select(time, rating) %>% 
        mutate(rating = recode(rating, "Sangat positif" = 5,
                               "Positif" = 4, 
                               "Netral" = 3,
                               "Negatif" = 2, 
                               "Sangat negatif" = 1))

df_d_rating$time <- as.Date(df_d_rating$time)
fit <- loess(rating ~ as.numeric(time), data = df_d_rating, span = 0.3)
new.x <- seq(as.Date(min(df_daily$time)), as.Date(max(df_daily$time)), by="1 day")
new.y <- predict(fit, newdata=data.frame(time=as.numeric(new.x)))

df_seg <- data.frame(x1=head(new.x, -1), x2=tail(new.x, -1),
                     y1=head(new.y, -1), y2=tail(new.y, -1))

df_seg$col <- cut(df_seg$y1, c(1, 3, 5))

# visualize the plot with smooth line
p_d <- ggplot(df_d_rating, aes(x=time, y=rating))+
        geom_line(alpha=0.6)+
        ggtitle("Daily rating of the day in 2021")+
        scale_x_date(date_breaks = '1 month', date_labels = "%b") +
        coord_cartesian(ylim= c(1, 5))+
        geom_smooth(span = 0.3, method='loess', color="blue", size=0.4)+
        theme_ipsum()
p_d

# visualize only the smooth line with Good/Bad indicators
p_d_smooth <- ggplot(data=df_seg, aes(x=x1, y=y1, xend=x2, yend=y2, colour=col))+
        geom_segment(size=1)+
        ylim(1,5)+
        scale_x_date(date_breaks='1 month', date_labels="%b")+
        ggtitle("Smoother graph for rating of the day: I'm ok most of the time!")+
        labs(subtitle="October was something, huh?")+
        ylab("rating")+
        xlab("month")+
        labs(colour="Bad days vs Good days")+
        scale_colour_hue(labels = c("Bad", "Good"))+
        theme_ipsum()
p_d_smooth

# plotly for fancier use
ggplotly(p_d_smooth)


### Correlation between feelings and daily rating

# selecting columns and recoding rating into ordinal scale
df_d_feelings <- df_daily %>% 
        select(time, feelings) %>% 
        mutate(feelings = strsplit(as.character(feelings), ", " )) %>% 
        unnest(feelings)

library(reshape2)
si <- unique(df_d_feelings)
df_d_feelings <- dcast(si, formula=time~feelings, fun.aggregate=length)

# join feelings and rating
df_d_feelrat <- merge(df_d_feelings, df_d_rating)

corr_feelrat <- ggcorr(df_d_feelrat, method = c("everything", "pearson"),
        nbreaks=7,
        palette = "PiYG",
        label =T,
        label_alpha = T,
        label_size=2,
        label_round=2)+
        ggtitle("Confident and Joy contribute mostly for daily rating")+
        labs(subtitle="While being uneasy and lonely negatively affects daily rating")
corr_feelrat

### Bigram daily

## Data wrangling

# create a number of week column
df_rating <- transform(df_rating, week=match(time, unique(time)))

# select relevant columns
df_give_en <- df_daily %>% 
        select(time, give_en) %>% 
        mutate(give_en = strsplit(as.character(give_en), "- " )) %>% 
        unnest(give_en) %>% 
        filter(give_en!="") %>% #remove blank rows 
        transform(day=match(time, unique(time))) %>%
        group_by(day) %>% #create day number
        mutate(number=row_number()) #create row number

df_drain_en <- df_daily %>% 
        select(time, drain_en) %>% 
        mutate(drain_en = strsplit(as.character(drain_en), "- ")) %>% 
        unnest(drain_en) %>% 
        filter(drain_en!="") %>%  #remove blank rows
        transform(day=match(time, unique(time))) %>% #create day number
        group_by(day) %>% 
        mutate(number=row_number()) #create row number

# create tidy words for things that give energy
tidy_give_en <- df_give_en %>% 
        ungroup("day") %>% 
        unnest_tokens(word, give_en) %>% 
        anti_join(stopwords) %>% 
        select (-"time")
# count
tidy_give_en %>% 
        count(word, sort=TRUE)

# create tidy words for things that drain energy
tidy_drain_en <- df_drain_en %>% 
        ungroup("day") %>% 
        unnest_tokens(word, drain_en) %>% 
        anti_join(stopwords) %>% 
        select (-"time")
# count
tidy_drain_en %>% 
        count(word, sort=TRUE)

## Bigram analysis (Give energy) -----------------

tidy_give_2 <- df_give_en %>% 
        ungroup("day") %>% 
        unnest_tokens(bigram, give_en, token = "ngrams", n=2)

# removing stop words separately for the bigrams
give_separated <- tidy_give_2 %>% 
        separate(bigram, c("word1", "word2"), sep=" ")

give_filtered <- give_separated %>% 
        filter(!word1 %in% stopwords$word) %>% 
        filter(!word2 %in% stopwords$word)

# count the filtered bigrams        
give_2_counts <- give_filtered %>% 
        filter(!is.na(word1), !is.na(word2)) %>% 
        count(word1, word2, sort=TRUE) %>% 
        mutate(word_c = paste(word1, word2, sep=" "))

give_2_counts

# select only meaningful words (operationally, occured more than 10)
plot_give <- give_2_counts[(give_2_counts[,3]>=10),]

# create plot for Things that give me energy
plot_give %>% 
        mutate(word_c = fct_reorder(word_c, n)) %>% 
        ggplot(aes(x=word_c, y=n, fill=word_c))+
        geom_bar(stat="identity")+
        scale_fill_hue(h=c(180,300))+
        coord_flip()+
        theme_ipsum()+
        theme(legend.position="none")+
        xlab("")+
        ylab("")+
        ggtitle("Things that give me energy daily")+
        labs(subtitle = "Note: I am still a F2P Genshin Player and sometimes read pirated books lmao")

## Bigram analysis (Drain energy) -----------------

tidy_drain_2 <- df_drain_en %>% 
        ungroup("day") %>% 
        unnest_tokens(bigram, drain_en, token = "ngrams", n=2)

# removing stop words separately for the bigrams
drain_separated <- tidy_drain_2 %>% 
        separate(bigram, c("word1", "word2"), sep=" ")

drain_filtered <- drain_separated %>% 
        filter(!word1 %in% stopwords$word) %>% 
        filter(!word2 %in% stopwords$word)

# count the filtered bigrams        
drain_2_counts <- drain_filtered %>% 
        filter(!is.na(word1), !is.na(word2)) %>% 
        count(word1, word2, sort=TRUE) %>% 
        mutate(word_c = paste(word1, word2, sep=" "))

drain_2_counts
# select only meaningful words (operationally, occured more than 10)
plot_drain <- drain_2_counts[(drain_2_counts[,3]>=5),]

# plot drain
plot_drain %>% 
        mutate(word_c = fct_reorder(word_c, n)) %>% 
        ggplot(aes(x=word_c, y=n, fill=word_c))+
        geom_bar(stat="identity")+
        scale_fill_hue(h=c(80,5))+
        coord_flip()+
        theme_ipsum()+
        theme(legend.position="none")+
        xlab("")+
        ylab("")+
        ggtitle("Things that drain my energy daily")+
        labs(subtitle = "Love x Hate relationship with books and mostly physical stuffs")

## Visualizing networks of bigrams with ggraph -----------------
# add library for ggraph
library(igraph)
library(ggraph)

# design arrow
a <- grid::arrow(type="closed", length=unit(.12, "inches"))
set.seed(2021)

# graph: Give energy

bigram_give <- give_2_counts %>% 
        filter(n >=2) 

# create vertices to calculate occurences in separate words  
vert_give <- bigram_give %>% 
        select(!word_c) %>% 
        pivot_longer(cols=c(word1, word2), names_to="item", values_to="word") %>% 
        group_by(word) %>% summarise(n = sum(n))
set.seed(2021) #set seed so the viz is consistent

# visualize the network of Things that give me energy
bigram_give %>% 
        graph_from_data_frame(vertices=vert_give) %>% 
        ggraph(layout="fr")+
        geom_edge_link(aes(edge_alpha=n),
                       show.legend=FALSE,
                       arrow=a, end_cap=circle(.03, 'inches'),
                       edge_colour = "royalblue")+
        geom_node_point(aes(size=n), show.legend=FALSE, 
                        color="lightgreen")+ 
        scale_size(range=c(1,5))+
        geom_node_text(aes(label=name, alpha=n), vjust=1, hjust=1,size=3)+
        theme_void()+
        theme(legend.position="none")+
        ggtitle("Connection of things that give my energy daily")+
        labs(subtitle="Play, achieve something, listening to music, read book, coffee, togetherness")

# graph: drain energy
bigram_drain <- drain_2_counts %>% 
        filter(n >=2) 
        
vert_drain <- bigram_drain %>% 
        select(!word_c) %>% 
        pivot_longer(cols=c(word1, word2), names_to="item", values_to="word") %>% 
        group_by(word) %>% summarise(n = sum(n))
set.seed(2023)
bigram_drain %>% 
        graph_from_data_frame(vertices=vert_drain) %>% 
        ggraph(layout="fr")+
        geom_edge_link(aes(edge_alpha=n),
                       show.legend=FALSE,
                       arrow=a, end_cap=circle(.03, 'inches'),
                       edge_colour = "darkred")+
        geom_node_point(aes(size=n), show.legend=FALSE, 
                        color="orange")+ 
        scale_size(range=c(1,5))+
        geom_node_text(aes(label=name, alpha=n), vjust=1, hjust=1,size=3)+
        theme_void()+
        theme(legend.position="none")+
        ggtitle("Connection of things that drain my energy daily")+
        labs(subtitle="Lack and excess of things, sleeping and eating habit, and read books that feel like an obligation, context switching")


        
 