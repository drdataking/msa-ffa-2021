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
library(readtext)
library(quanteda)

head(readRDS("../../Data/Session_9-corp_summary.rds"), n = 10)

corp_FOG <- readRDS("../../Data/Session_9-corp_readability_FOG.rds")
corp_FOG %>%  head() %>%  html_df()

summary(corp_FOG$FOG)
ggplot(corp_FOG, aes(x=FOG)) + geom_density()

# the csv file contains filing info to SEC Edgar, from WRDS
# accession: file name used for SEC filing; regsic: SIC industry code
# case_when(): multiple if_else, same as SQL CASE WHEN
df_SIC <- read.csv('../../Data/Session_9-Filings2014.csv') %>%
  select(accession, regsic) %>%
  mutate(accession = paste0(accession, ".txt")) %>%
  rename(document = accession) %>%
  mutate(industry = case_when(
    regsic >=0100 & regsic <= 0999 ~ "Agriculture",
    regsic >=1000 & regsic <= 1499 ~ "Mining",
    regsic >=1500 & regsic <= 1799 ~ "Construction",
    regsic >=2000 & regsic <= 3999 ~ "Manufacturing",
    regsic >=4000 & regsic <= 4999 ~ "Utilities",
    regsic >=5000 & regsic <= 5199 ~ "Wholesale Trade",
    regsic >=5200 & regsic <= 5999 ~ "Retail Trade",
    regsic >=6000 & regsic <= 6799 ~ "Finance",
    regsic >=7000 & regsic <= 8999 ~ "Services",
    regsic >=9100 & regsic <= 9999 ~ "Public Admin")) %>%
  group_by(document) %>% slice(1) %>% ungroup()
corp_FOG <- corp_FOG %>% left_join(df_SIC)

corp_FOG %>%  head() %>%  html_df()

ggplot(corp_FOG[!is.na(corp_FOG$industry), ], aes(x = factor(industry),
                                                  y = FOG)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = "blue") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

library(ggthemes)
ggplot(corp_FOG[!is.na(corp_FOG$industry), ], aes(x = FOG)) +
  geom_density(color = "blue") + facet_wrap(~industry) + theme_solarized()

library(lattice)
densityplot(~FOG | industry,
            data = corp_FOG,
            plot.points = F,
            main = "Fog index distibution by industry (SIC)",
            xlab = "Fog index",
            layout = c(3, 3))

library(DT)
readRDS('../../Data/Session_9-corp_kwic.rds') %>%
  mutate(text = paste(pre, keyword, post)) %>%
  select(docname, text) %>%
  datatable(options = list(pageLength = 3), rownames = F)

gw_count <- readRDS('../../Data/Session_9-corp_kwic.rds') %>%
  left_join(select(df_SIC, document, industry), by = c("docname" = "document")) %>%
  group_by(industry) %>%
  mutate(docs_gw = n()) %>%
  slice(1) %>%
  ungroup() %>%
  select(industry, docs_gw)

corp_FOG %>% group_by(industry) %>%
  mutate(docs = n()) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(gw_count) %>%
  mutate(docs_gw = ifelse(is.na(docs_gw), 0, docs_gw)) %>%
  mutate(`Global warming` = docs_gw / docs,
         `Does not mention` = (docs - docs_gw) / docs) %>%
  gather(mention, percent, `Global warming`, `Does not mention`) %>%
  ggplot() + 
  geom_col(aes(x = industry, y = percent, fill = mention)) + 
  ylab("Percent of annual reports") +
  xlab("Industry (SIC code)") +
  ggtitle("Industries discussing global warming in 2014") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("#CCCCCCAA", "#88CC88")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

readRDS('../../Data/Session_9-corp_dfm_feat.rds')

readRDS('../../Data/Session_9-corp_tfidf_feat.rds')

readRDS('../../Data/Session_9-corp_tfidf_bank.rds')

out <- readRDS('../../Data/Session_9-corp_out_stm.rds')
#out <- readRDS('../../Data/Session_9-corp_out_lda.rds')
#out <- readRDS('../../Data/Session_9-corp_out_topicmodels.rds')

out$documents[[1]][ , 126:130]
out$vocab[c(out$documents[[1]][ , 126:130][1, ])]

library(stm)
topics <- readRDS('../../Data/Session_9-corp_stm_topics.rds')

labelTopics(topics, c(4, 6, 10))

# Convert into a document-topic dataframe (5991*10 obs)
# The theta object is the posterior proportion of a topic
out$meta$industry <- factor(out$meta$industry)
out$meta$industry <- addNA(out$meta$industry) # turning NA into an extra level
doc_topics = data.frame(document = names(out$documents),
                        industry = out$meta$industry,
                        topic = 1,
                        weight = topics$theta[ , 1]) # topic proportion
for (i in 2:10) {
  temp = data.frame(document = names(out$documents),
                    industry = out$meta$industry,
                    topic = i,
                    weight = topics$theta[ , i])
  doc_topics = rbind(doc_topics, temp)}
# Proporitional topics (%)
doc_topics <- doc_topics %>%
  group_by(document) %>%
  mutate(topic_prop = weight / sum(weight)) %>% ungroup()

# Manually label topics
topic_labels = data.frame(topic = 1:10,
                          topic_name = c('Real Estate', 'Management', 'Product',
                                         'Investment', 'Services', 'Financing',
                                         'Service2', 'Insurance', 'Industrial',
                                         'Utility'))
doc_topics <- doc_topics %>% left_join(topic_labels)

doc_topics %>% filter(document=='0001104659-14-015152.txt')

doc_topics %>% filter(document=='0001104659-14-015152.txt' |
                      document=='0000019617-14-000289.txt') %>%
  mutate(Company=ifelse(document=='0001104659-14-015152.txt', 'Citi','JPM')) %>%
  ggplot(aes(x=factor(topic_name), y = topic_prop, fill = factor(topic_name))) + 
  geom_col() + facet_wrap(~Company) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

doc_topics %>%  group_by(industry, topic) %>%
  mutate(topic_prop = mean(topic_prop)) %>% slice(1) %>%  ungroup() %>%
  ggplot(aes(x = factor(topic_name), y = topic_prop, fill=factor(topic_name))) + 
  geom_col() + facet_wrap(~industry) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

library(tidyr)
# refer previous slides for doc_topics data structure
wide_topics <- spread(doc_topics[, c(1,2,5,6)], topic_name, topic_prop)
mat <- wide_topics[, 3:12] # keep topic_name, drop document and industry
# we should scale/standardize the data if they are in different scale units
# our data is fine, so we don't need to scale
# you may use scale() and it does not change the subsequent results
mat[, 1:6] %>% head(n=3) %>% html_df()

set.seed(678)
clusters <- kmeans(mat, 9) # k = 9
# kmeans() function returns a list of several components, including:
# cluster: integers vector (from 1:k) indicating the cluster each point is in
# centers: A matrix of cluster centers (cluster means)
# totss: The total sum of squares.
# withinss: Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.
str(clusters[1:7]) # structure of the first seven components

cbind(as.data.frame(clusters$centers), data.frame(kmean=1:9)) %>% # add kmean
  gather("Topics","weights",-kmean) %>% # convert to long format
  ggplot(aes(x=factor(Topics), y=weights, fill=factor(Topics))) +
  geom_col() + facet_wrap(~kmean) + 
  theme(axis.text.x=element_blank(),axis.ticks.x = element_blank())

# How to plot 10 features in a 2-D plot?
# For a multi-dimensional dataset, we may perform Principal Component Analysis
# and to plot data points according to the first two principal components.
library(cluster)
clusplot(mat, clusters$cluster, color=T, shade=T, labels=4)

library(Rtsne)
dups <- which(duplicated(mat))
wide_nodup <- wide_topics[-dups,]
wide_nodup$kmean <- clusters$cluster[-dups]
#slow O(n log(n)).  Original model was O(n^2) though
tsne_data <- readRDS('../../Data/Session_9-corp_tsne.rds')
wide_nodup <- wide_nodup %>%
  mutate(tsne1 = tsne_data$Y[, 1], tsne2 = tsne_data$Y[, 2])

ggplot(wide_nodup, aes(x = tsne1, y = tsne2, colour = industry)) + 
    geom_point(alpha = 0.3) + theme_bw()

ggplot(wide_nodup, aes(x = tsne1, y = tsne2, colour = factor(kmean))) + 
    geom_point(alpha = 0.3) + theme_bw()

# one bar for each cluster if they match
ggplot(wide_nodup, aes(x=kmean)) + geom_bar() + facet_wrap(~factor(industry))

# same color if match
ggplot(wide_nodup, aes(x=tsne1, y=tsne2, color=factor(kmean))) +
  geom_point() +  facet_wrap(~factor(industry))

# same color and in one group for each kmean
ggplot(wide_nodup, aes(x=tsne1, y=tsne2, color=factor(industry))) + geom_point() +
  facet_wrap(~factor(kmean))

wide_topics$dist <- sqrt(rowSums((mat - fitted(clusters))^2)) #Euclidean dist
# wide_topics$dist <- sqrt(rowSums(abs(mat - fitted(clusters))))
wide_topics[,c(1,2,3,5,13)] %>% arrange(desc(dist)) %>% slice(1:3) %>% html_df()

## # It takes very long to compute the gap statistic
## library(factoextra)
## set.seed(123)
## fviz_nbclust(mat, kmeans, method = "wss", k.max = 15) # default k.max is 10
## fviz_nbclust(mat, kmeans, method = "silhouette", k.max = 15)
## fviz_nbclust(mat, kmeans, method = "gap_stat", k.max = 15)

wide_topics$industry <- factor(wide_topics$industry)
train <- wide_topics[!is.na(wide_topics$industry), ]
label <- wide_topics[is.na(wide_topics$industry), ]
head(train)

## library(caret)
## set.seed(123)
## trControl <- trainControl(method='cv', number=10) # 10-fold cross validation
## tout <- train(industry ~ ., # including all as independent varaibles
##       method = 'knn', # there are 237 methods in the package
##       tuneGrid = expand.grid(k = 1:20), # tune the parameter k from 1 to 20
##       trControl = trControl,
##       metric = "Accuracy", # overall agreement rate averaged over cv iterations
##       data = select(train, -c(document, dist))) # remove "document" and "dist"
## #saveRDS(tout, '../../Data/Session_9-corp_knn.rds') # save the R object

library(caret)
tout <- readRDS('../../Data/Session_9-corp_knn.rds')

tout

ggplot(tout$results, aes(x=k, y=Accuracy)) +
  geom_line() + 
  geom_ribbon(aes(ymin=Accuracy - AccuracySD*1.96,
                  ymax=Accuracy + AccuracySD*1.96), alpha=0.2) + 
  geom_vline(xintercept=11, color="blue") + 
  xlab("k, optimal = 11")

label$industry_pred <- predict(tout, label)
label[ , c("document", "industry", "industry_pred")] %>% head %>% html_df

ts_wt <- wide_nodup %>% left_join(label[,c("document","industry_pred")])

ts_wt <- ts_wt %>%
  mutate(tsne1 = tsne_data$Y[, 1], tsne2 = tsne_data$Y[, 2])

# Force consistent factor values
inds <- unique(ts_wt$industry)
ts_wt$industry <- factor(ts_wt$industry, inds)
ts_wt$industry_pred <- factor(ts_wt$industry_pred, inds)

# Replicate default ggplot colors
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

ggplot() +
  scale_shape_identity() + # Allow for more plot point options
  geom_point(data=ts_wt[!is.na(ts_wt$industry),],
             aes(x=tsne1, y=tsne2, color=industry, shape=1), size=1) + 
  geom_point(data=ts_wt[!is.na(ts_wt$industry_pred),], aes(x=tsne1, y=tsne2,
             fill=industry_pred, shape=23, stroke=0.5), size=2) +
  guides(fill = "none") + 
  scale_color_manual(values=ggplotColours(n = 9), labels=inds, drop=FALSE) + 
  scale_fill_manual(values=ggplotColours(n = 9), labels=inds, drop=FALSE)