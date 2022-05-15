# Dependencies -----------------------------------------------------------------
print("LOGGING ROW 002: Loading dependencies.")
source("libraries.R")
source("functions.R")
source("variables.R")

wd <- toString(getwd())
print(glue("LOGGING ROW 008 WORKDIR = {wd}"))
y <- toString(list.files(R.home()))
print(glue("LOGGING ROW 010 LIST OF ROOT FILES: {y}"))

if (run_id=="lalala"){
  inputF <- glue("/input/schatsi_terms.csv")
  path <- glue("/output/")
} else {
  inputF <- glue("{path_prefix}input/schatsi_terms.csv")
  path <- glue("{path_prefix}output/")
}
print(path)

if (file.exists(inputF)) {
  
  print("LOGGING ROW 029: The file exists")
} else {
  
  print("LOGGING ROW 032: The file does not exist")
}

# Cleanse parameters------------------------------------------------------------
paramk <- 6 # parameter for topics to be created
paramkmin <- 2 # parameter for optimizing paramk mininum k range
paramkmax <- 15 # parameter for optimizing paramk maximum k range
##### TO-DO: Should be fixed or leave room for interaction?
paramFreq <- 50 # parameter to depict minimum frequency of a word required
paramLen <- 1 # parameter for cleaning words out which are too short
paramn <- 10 # parameter for top n words for each topic k
remWords <- c("et","al","et al") ## !!!!! enhance words interactively
addWords <- c("av","avs","p 0","vr","hr","c1","c2","c3","c4","r e","ra","may","dmd")
remWords <- c(remWords, addWords)

# Load data and apply cleanse parameters----------------------------------------

# retrieve table
print(glue("LOGGING ROW 50 {Sys.time()}: Loading input file. -> {inputF}"))
dataTerm <- read.csv(inputF, header = TRUE, sep=";")
print(glue("LOGGING ROW 53 {Sys.time()}: First row of loaded file. -> {toString(dataTerm[1,])}"))

names(dataTerm)[names(dataTerm) == "term.count"] <- "termCount" #TO-DO: Needs to be changed in predecessor docker

#dataterm <- dataTerm %>% rowwise() %>% mutate(singular = singularize(term))
dataSet <- subset(dataTerm, dataTerm[3] >= paramFreq)
dataSet$Length <- stringr::str_count(dataSet$term)
dataSet.ParamLen <- subset(dataSet, dataSet$Length <= paramLen) # for checking what has been excluded by paramLen
dataSet <- subset(dataSet, dataSet$Length > paramLen)
dataSet <- dataSet[!grepl(paste(remWords, collapse="|"), dataSet$term),]

dataSet %>% filter(!grepl("[[:digit:]]", dataSet$term)) #!!! Critical if digits are important?

# Analyses ---------------------------------------------------------------------
# Wordcloud --------------------------------------------------------------------
# Make messy and uncleansed wordcloud
dataSet0WC <- dataTerm[2:3]
dataSet0WC <- aggregate(dataSet0WC$termCount, by=list(Category=dataSet0WC$term), FUN=sum)
file <- glue("{path}schatsi_ML_dataSet0WC.csv")
write.table(dataSet0WC, file=file,sep = ";", col.names = T, row.names = F)
# Generate wordcloud !!local only!!
wordCloudPlain0 <- wordcloud2::wordcloud2(data=dataSet0WC, size=1.6, color='random-light')
wordCloudPlainFile0 <- paste(path, "wordCloudPlain0", ".html", sep ="", collpase=NULL)
htmlwidgets::saveWidget(wordCloudPlain0,wordCloudPlainFile0,selfcontained = F)
rm(dataSet0WC)

# Make cleansed wordcloud
dataSet1WC <- dataSet[2:3]
dataSet1WC <- aggregate(dataSet1WC$termCount, by=list(Category=dataSet1WC$term), FUN=sum)
file <- glue("{path}schatsi_ML_dataSet1WC.csv")
write.table(dataSet1WC, file=file,sep = ";", col.names = T, row.names = F)
# Generate wordcloud !!local only!!
wordCloudPlain1 <- wordcloud2::wordcloud2(data=dataSet1WC, size=1.6, color='random-light')
wordCloudPlainFile1 <- paste(path, "wordCloudPlain1", ".html", sep ="", collpase=NULL)
htmlwidgets::saveWidget(wordCloudPlain1,wordCloudPlainFile1,selfcontained = F)
rm(dataSet1WC)

# Topicmodeling-----------------------------------------------------------------
# Create document term matrix 
data_frame <- setNames(dataSet[1:3], c("doc_id","term","freq")) # udpipe pckg requires this column naming
dtm <- udpipe::document_term_matrix(data_frame)

# Derive optimized paramk
paramkdf <- data.frame(c(paramk))
paramkdf[,2] <- paramkdf
tryCatch({
result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = paramkmin, to = paramkmax, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
) # "Arun2010", "Deveaud2014" 

file <- glue("{path}SCHATSI_ML_ldaTuning.csv")
write.table(result, file=file,sep = ";", col.names = T, row.names = F)
ldatuning::FindTopicsNumber_plot(result)
#file <- glue("{path}SCHATSI_ML_ldaTuning.png")
ggsave(file, width = 16, height = 10, units = "cm")

paramCaoJuan2009 <- result[which.min(result[,3]),1]
#paramArun2010 <- result[which.min(result[,4]),1]
paramGriffiths2004 <- result[which.max(result[,2]),1]	
#paramDeveaud2014 <- result[which.max(result[,5]),1]	

#paramkList <- c(paramk,paramCaoJuan2009,paramArun2010,paramGriffiths2004,paramDeveaud2014)
paramkList <- c(paramk,paramCaoJuan2009,paramGriffiths2004)
#paramkdf <- data.frame(c("paramk","paramCaoJuan2009","paramArun2010","paramGriffiths2004","paramDeveaud2014"))
paramkdf <- data.frame(c("paramk","paramCaoJuan2009","paramGriffiths2004"))
paramkdf[,2] <- paramkList
}, 
error=function(e) {
})
# Conduct Latent Dirichlet Allocation for text clustering

paramkdf <- setNames(paramkdf, c("Metric","Value"))
n <- nrow(paramkdf)

for (i in 1:n){
  r <- i
  param <- paramkdf[r,2]
  paramName <- paramkdf[r,1]
  
  ap_lda <- topicmodels::LDA(dtm, param, control = list(seed = 1234))
  ap_topics <- tidytext::tidy(ap_lda, matrix = "beta")
  
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = paramn) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
  
  file <- glue("{path}schatsi_ML_topTerms_{paramName}.png")
  ggsave(file, width = 16, height = 10, units = "cm")
  
  ap_top_terms[3] <- ap_top_terms[3] %>% dplyr::mutate_each(funs(myfun))
  file <- glue("{path}schatsi_ML_topTerms_{paramName}.csv")
  write.table(ap_top_terms, file=file,sep = ";", col.names = T, row.names = F)
  
  ap_documents <- tidytext::tidy(ap_lda, matrix = "gamma")
  ap_documents[3] <- ap_documents[3] %>% mutate_each(funs(myfun))
  file <- glue("{path}schatsi_ML_topicAllocation_{paramName}.csv")
  write.table(ap_documents, file=file,sep = ";", col.names = T, row.names = F)
}

#dev.off()

