## Libraries
library(pdftools)
library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud)
library(ggplot2)

##---------Read the data------------
bjp_pdf = 'data/bjp_manifesto_2019.pdf';
cong_pdf = 'data/congress_manifesto_2019.pdf';
cpm_pdf = 'cpm_manifesto_2019.pdf'


## A big function encompassing cleaning, tokenizing and plotting
pdf_to_tidy <- function(pdf_input){
  # This function takes the pdf, converts the pages into list, cleans the list by removing spaces
  pdf_to_CleanList <- function(pdf_input){
    pdf_text(pdf_input) %>% 
      sapply(list) %>% 
      str_split("\\r\\n", simplify = F ) %>% ## \\r\\n is the separator in windows
      unlist()
  }
  tidy1 <- pdf_to_CleanList(pdf_input)
  # function to convert the list data into tidy data format 
  list_to_tidy <- function(input_data){
    # t1 <- pdf_to_CleanList(input_data) %>% 
    #   unlist();
    t1 <- input_data;
    t2 <- tibble(line=1:length(t1),text=t1) %>% 
      unnest_tokens(word, text) %>% 
      as.data.frame(table_df) %>% 
      filter(nchar(word)>2) %>% 
      anti_join(stop_words, by='word')
    return(t2);
  }
  
  # Most popular words
  tidy2 <- list_to_tidy(tidy1)
  return(tidy2)

}
tidy_data <- pdf_to_tidy(pdf_input = bjp_pdf)
#------------------Plotting---------------
# bar plot of the most popular words in BJP's manifesto
barplotPopularWords <- function(tidy_data, pdf_input, plot_type){
  if plot_type ='p'
  p <- tidy_data %>%
    count(word, sort = TRUE) %>%
    filter(n > 30) %>%
    mutate(word = reorder(word, n))%>% ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
  
}
savePlots_to_pdf <- function(){
  temp1 <- paste(pdf_input,'_barplot.jpg')
  jpeg(filename = temp1)
  barplotPopularWords(tidy2)
  dev.off();
  
}

# wordcloud
wordcloudTidyData <- function(tidy_data){
  tidy_data %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))
}

savePlots_to_pdf <- function(){
  temp2 <- paste(pdf_input,'_wordcloud.pdf')
  pdf(temp2)
  wordcloudTidyData(tidy2)
  dev.off()  ;
}



pdf_input = bjp_pdf;
pdf_to_plot(pdf_input)
