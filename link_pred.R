library(ggplot2)
library(igraph)
library(randomForest)
library(adabag)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(rpart)

setwd("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/")

createTrainData <- function(g1, g2, tags) {
  if(is.null(g1) || is.null(g2))
    return(NULL);
  adj_mat = as_adjacency_matrix(g1, type = "both", attr = "weight", sparse = FALSE)
  adj_mat_2 = as_adjacency_matrix(g2, type = "both", attr = "weight", sparse = FALSE)
  
  degrees = rowSums(adj_mat != 0)
  #shortest_paths = distances(g, v = V(g), to = V(g), weights = NA, algorithm = "dijkstra")
  jaccard = similarity(g1, method = "jaccard")
  dice = similarity(g1, method = "dice")
  adamic_adar = similarity(g1, method = "invlogweighted")
  #eg_cent = eigen_centrality(g, weights = NA)
  #eg_cent_wtd = eigen_centrality(g, weights = NULL)
  page_rank = page_rank(g1)
  local_cc = transitivity(g1, type = "local", vids = NULL, isolates = "zero")
  
  diff = difference(g2, g1, byname = "auto")
  edgeList = get.edgelist(diff)
  num_rows = min(50000, 2 * length(edgeList[,1]))
  
  data = data.frame(shortest_paths = numeric(num_rows), 
                    common_neighbors = numeric(num_rows), 
                    jaccard = numeric(num_rows), 
                    dice = numeric(num_rows), 
                    adamic_adar = numeric(num_rows), 
                    preferential_attach = numeric(num_rows), 
                    min_page_rank = numeric(num_rows), 
                    max_page_rank = numeric(num_rows),
                    min_local_cc = numeric(num_rows),
                    max_local_cc = numeric(num_rows),
                    cos_sim = numeric(num_rows),
                    label = numeric(num_rows))
  
  n = 1
  while(n <= (num_rows/2)) {
    #ends(g1, E(g1)[1])
    e = sample(1:length(edgeList[,1]), 1)
    i = as.numeric(edgeList[e, 1])
    j = as.numeric(edgeList[e, 2])
    data$shortest_paths[n] = all_shortest_paths(g1, from = i, to = j, weights = NA)$nrgeo[j]
    data$common_neighbors[n] = cocitation(g1, i)[j]
    data$jaccard[n] = jaccard[i,j]
    data$dice[n] = dice[i,j]
    data$adamic_adar[n] = adamic_adar[i,j]
    data$preferential_attach[n] = degrees[i] * degrees[j]
    data$min_page_rank[n] = min(page_rank$vector[i], page_rank$vector[j])
    data$max_page_rank[n] = max(page_rank$vector[i], page_rank$vector[j])
    data$min_local_cc[n] = min(local_cc[i], local_cc[j])
    data$max_local_cc[n] = max(local_cc[i], local_cc[j])
    data$cos_sim[n] = cos_text(tags[i,1], tags[j,1])
    data$label[n] = 1
    n = n + 1
  }
  
  while(n <= num_rows) {
    i = sample(1:vcount(g1), 1)
    j = sample(1:vcount(g1), 1)
    if(adj_mat_2[i, j] == 0) {
      data$shortest_paths[n] = all_shortest_paths(g1, from = i, to = j, weights = NA)$nrgeo[j]
      data$common_neighbors[n] = cocitation(g1, i)[j]
      data$jaccard[n] = jaccard[i,j]
      data$dice[n] = dice[i,j]
      data$adamic_adar[n] = adamic_adar[i,j]
      data$preferential_attach[n] = degrees[i] * degrees[j]
      data$min_page_rank[n] = min(page_rank$vector[i], page_rank$vector[j])
      data$max_page_rank[n] = max(page_rank$vector[i], page_rank$vector[j])
      data$min_local_cc[n] = min(local_cc[i], local_cc[j])
      data$max_local_cc[n] = max(local_cc[i], local_cc[j])
      data$cos_sim[n] = cos_text(tags[i,1], tags[j,1])
      data$label[n] = 0
      n = n + 1
    }
  }
  data$label = as.factor(data$label)
  return(data)
}

library(stringr)
cos_text <- function(x,y)
{
  x <- unlist(str_extract_all(x, "[^,]+"))
  y <- unlist(str_extract_all(y, "[^,]+"))
  #x <- x[x %in% GradyAugmented]
  #y <- y[y %in% GradyAugmented]
  if(length(x) == 0 || length(y) == 0) return(0.0)
  #if(length(y) == 0) return(0.0)
  table_x <- as.data.frame(table(x))
  table_y <- as.data.frame(table(y))
  
  data_frame <- NULL
  data_frame$vocab <- unique(sort(c(x,y)))
  data_frame <- as.data.frame(data_frame)
  match <- match(data_frame$vocab, table_x$x)
  data_frame$x <- table_x$Freq[match]
  data_frame$x[is.na(match)] <- 0
  
  match <- match(data_frame$vocab, table_y$y)
  data_frame$y <- table_y$Freq[match]
  data_frame$y[is.na(match)] <- 0
  
  norm <- function(v)
  {
    return(sqrt(sum(v^2)))
  }
  cos <- sum(data_frame$x*data_frame$y)/norm(data_frame$x)/norm(data_frame$y)
  return(cos)
}

splitTrain <- function(data) {
  size <- nrow(data) * 0.8
  validation_index <- sample(1:nrow(data), size = size)
  validation <- data[-validation_index,]
  train <- data[validation_index,]
  return(list(train, validation))
}

### Loading the two networks (3-month and 6-month)
g1 = read_graph("./project/data/NAJan_April_Network.ncol", format = "ncol")
g1 = set_vertex_attr(g1, "name", value = 1:vcount(g1))
g2 = read_graph("./project/data/NAFullNetwork.ncol", format = "ncol")
g2 = set_vertex_attr(g2, "name", value = 1:vcount(g2))
g2 = induced_subgraph(g2, vids = 1:vcount(g1), impl = "auto")

### Reading tags of users
tags = read.csv("./project/data/year-tags-dict.csv", sep = ";", header = FALSE)
View(tags)
rownames(tags) <- tags$V1
tags$V1 = NULL
tags$V2 = as.character(tags$V2)

### Creating training data for the machine learning models
ptm <- proc.time()
d = createTrainData(g1, g2, tags)
proc.time() - ptm
write.csv(d, "./project/data/ml-dataset.csv", row.names = FALSE)
d = read.csv("./project/data/ml-dataset.csv")
d$label = as.factor(d$label)
d$shortest_paths = as.numeric(d$shortest_path)
d$common_neighbors = as.numeric(d$common_neighbors)
d$jaccard = as.numeric(d$jaccard)
d$dice = as.numeric(d$dice)
d$adamic_adar = as.numeric(d$adamic_adar)
d$preferential_attach = as.numeric(d$preferential_attach)
d$min_page_rank = as.numeric(d$min_page_rank)
d$max_page_rank = as.numeric(d$max_page_rank)
d$min_local_cc = as.numeric(d$min_local_cc)
d$max_local_cc = as.numeric(d$max_local_cc)
d$cos_sim = as.numeric(d$cos_sim)

### split dataset as training set and validation/test 
td = splitTrain(d)
train = data.frame(td[1])
test = data.frame(td[2])

### random forest
set.seed(50)
rf = randomForest(label~., data = train, importance = TRUE, ntree = 1500)
#rf = trainRFModel(subset(train, select = -cos_sim), 500)
predictions = predict(rf, subset(test, select = -label))
#predictions = predict(rf, subset(test, select = -c(cos_sim, label)))
confusion.matrix = prop.table(table(predictions, test$label))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
accuracy
#[1] 0.8752 - 50 trees with tags
#[1] 0.8753 - 500 trees with tags
#[1] 0.8758 - 1500 trees with tags
#[1] 0.6838 - 500 trees without tags
plot(predictions)
plot(test$label)
#to find importance features - VARIABLE IMPORTANCE
par(mfrow=c(1,2))
varImpPlot(rf)


# decision tree
fit <- rpart(label ~ ., data=train, method="class")
predictions <- predict(fit, test, type = "class")
confusion.matrix = prop.table(table(predictions, test$label))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
accuracy
#[1] 0.8611

# logistic regression
fit = glm(label~., family=binomial(link="logit"), data=train)
predictions = predict(fit, test, type="response")
predictions = ifelse(predictions > 0.5,1,0)
confusion.matrix = prop.table(table(predictions, test$label))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
accuracy
#[1] 0.8413

# multi layer perceptron
library(nnet)
neuralnets = nnet(label~., data=train, size = 20, maxit = 1000, contasts = NULL)
predictions = predict(neuralnets, subset(test, select = -label))
predictions = ifelse(predictions > 0.5,1,0)
confusion.matrix = prop.table(table(predictions, test$label))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
accuracy
#[1] 0.8713

# gradient boost - xgboost
xgb = xgboost(data = data.matrix(subset(train, select = -label)), label = as.numeric(levels(train$label))[train$label], max_depth = 20, eta = 0.1, nthread = 4, nrounds = 100, objective = "binary:logistic")
predictions = predict(xgb, data.matrix(subset(test, select = -label)))
predictions = ifelse(predictions > 0.5,1,0)
predictions = as.factor(predictions)
plot(predictions)
plot(test$label)
confusion.matrix = prop.table(table(predictions, test$label))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
accuracy
#[1] 0.8728

### RANDOM FORESTS - Accuracy Vs Training Examples
accVsEx = data.frame(examples = numeric(7),
                     accuracy = numeric(7),
                     runtime = numeric(7))
set.seed(50)
accVsEx$examples = c(00, 500, 1000, 2500, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000)
for(i in 1:length(accVsEx$examples)){
  #print(i)
  #print(accVsEx$examples[i])
  ptm <- proc.time()
  rf = randomForest(label~., data = train[1:accVsEx$examples[i],], importance = TRUE, ntree = 1500)
  accVsEx$runtime[i] = proc.time() - ptm
  predictions = predict(rf, subset(test, select = -label))
  confusion.matrix = prop.table(table(predictions, test$label))
  accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
  accVsEx$accuracy[i] = accuracy * 100
}

### plot for anlysing random forest performance
### number of training examples VS training time and validation accuracy
library(ggplot2)
library(gtable)
library(grid)
library(extrafont)

# Create p1
p1 <- ggplot(accVsEx, aes(as.factor(examples), runtime, group = 1)) + 
  geom_line(colour = "blue4", size = 1) + geom_point(colour = "blue4", size = 2) +
  labs(x="Number of Training Examples",y=NULL) +
  scale_x_discrete(breaks = accVsEx$examples) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,300)) +
  theme(
    # panel.background = element_blank(),
    # panel.grid.minor = element_blank(), 
    # panel.grid.major = element_line(color = "gray50", size = 0.75),
    # panel.grid.major.x = element_blank(),
    axis.text.y = element_text(colour="blue4", size = 10),
    axis.text.x = element_text(size = 10, colour = "black"),
    # axis.ticks = element_line(colour = 'gray50'),
    # axis.ticks.length = unit(.2, "cm"),
    # axis.ticks.x = element_line(colour = "black"),
    # axis.ticks.y = element_blank(),
    axis.title=element_text(size=14,face="bold"),
    plot.title = element_text(hjust = -0.135, vjust=2.12, colour="blue4", size = 10)) 

# Create p2
p2 <- ggplot(accVsEx, aes(as.factor(examples), accuracy, group = 1)) + 
  geom_line(colour = "chartreuse4", size = 1) +  geom_point(colour = "chartreuse4", size = 2) +
  labs(x="Number of Training Examples",y=NULL) +
  scale_x_discrete(breaks = accVsEx$examples) +
  scale_y_continuous(expand = c(0, 0), limits = c(80,90)) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(colour="chartreuse4", size=10),
    axis.text.x = element_text(size=10),
    #axis.ticks.length = unit(.2, "cm"),
    #axis.ticks.y = element_blank(),
    axis.title=element_text(size=14,face="bold"),
    plot.title = element_text(hjust = 0.6, vjust=2.12, colour = "chartreuse4", size = 10))

# Get the plot grobs
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

# Get the locations of the plot panels in g1.
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# Overlap panel for second plot on that of the first plot
g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

# ggplot contains many labels that are themselves complex grob; 
# usually a text grob surrounded by margins.
# When moving the grobs from, say, the left to the right of a plot,
# make sure the margins and the justifications are swapped around.
# The function below does the swapping.
# Taken from the cowplot package:
# https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R 
hinvert_title_grob <- function(grob){
  
  # Swap the widths
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  # Fix the justification
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}

# Get the y axis from g2 (axis line, tick marks, and tick mark labels)
index <- which(g2$layout$name == "axis-l")  # Which grob
yaxis <- g2$grobs[[index]]                  # Extract the grob

# yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
# The relevant grobs are contained in axis$children:
#   axis$children[[1]] contains the axis line;
#   axis$children[[2]] contains the tick marks and tick mark labels.

# Second, swap tick marks and tick mark labels
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)

# Third, move the tick marks
# Tick mark lengths can change. 
# A function to get the original tick mark length
# Taken from the cowplot package:
# https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R 
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}

tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml

# Fourth, swap margins and fix justifications for the tick mark labels
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

# Fifth, put ticks back into yaxis
yaxis$children[[2]] <- ticks

# Put the transformed yaxis on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")

# Labels grob
left = textGrob("Training Time (seconds)", x = 0, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 14, fontface="bold", col =  "blue4"))
right =  textGrob("Validation Accuracy (%)", x = 1, y = 0.9, just = c("right", "top"), gp = gpar(fontsize = 14, fontface="bold", col =  "chartreuse4"))
labs = gTree("Labs", children = gList(left, right))

# New row in the gtable for labels
height = unit(3, "grobheight", left)
g1 <- gtable_add_rows(g1, height, 2)  

# Put the label in the new row
g1 = gtable_add_grob(g1, labs, t=3, l=3, r=5)

# Turn off clipping in the plot panel
g1$layout[which(g1$layout$name == "panel"), ]$clip = "off"
grid.draw(g1)
