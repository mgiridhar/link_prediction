library(reshape2)
library(ggplot2)

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

# Shortest paths
sp = data.frame(links_formed = numeric(25000), links_not_formed = numeric(25000))
sp$links_formed = d$shortest_paths[1:25000]
sp$links_not_formed = d$shortest_paths[25001:50000]
plot (density(sp$links), col="red",xlim=c(0,20)) 
lines (density(sp$no_links), col="blue")
#Plot
ggplot(melt(sp), aes(value, col = variable)) + geom_density() + xlim(0,50)

# preferential attachement
pa = data.frame(links_formed = numeric(25000), links_not_formed = numeric(25000))
pa$links_formed = d$preferential_attach[1:25000]
pa$links_not_formed = d$preferential_attach[25001:50000]
plot (density(pa$links), col="red", xlim=c(0,3000)) 
lines (density(pa$no_links), col="blue")
p2 = ggplot(melt(pa), aes(value, col = variable)) + geom_density() + ggtitle("Preferential Attachment Distribution") + xlim(0,100)

# cosine similarity
cs = data.frame(links_formed = numeric(25000), links_not_formed = numeric(25000))
cs$links_formed = d$cos_sim[1:25000]
cs$links_not_formed = d$cos_sim[25001:50000]
p1 = ggplot(melt(cs), aes(value, col = variable)) + geom_density() + ggtitle("Tag Similarity Distribution") + ylim(0,20) + xlim(0,1)
#ggplot(melt(cs), aes(value, col = variable)) + geom_density() + stat_function(fun = pnorm, args = list(mean = 0, sd = 3)) + ggtitle("Tag Similarity Distribution") + ylim(0,20)

# max page rank values
maxpg = data.frame(links_formed = numeric(25000), links_not_formed = numeric(25000))
maxpg$links_formed = d$max_page_rank[1:25000]
maxpg$links_not_formed = d$max_page_rank[25001:50000]
p3 = ggplot(melt(maxpg), aes(value, col = variable)) + geom_density() + ggtitle("Page Rank (max value) Distribution") + xlim(0,0.0015)

#min page rank values
minpg = data.frame(links_formed = numeric(25000), links_not_formed = numeric(25000))
minpg$links_formed = d$min_page_rank[1:25000]
minpg$links_not_formed = d$min_page_rank[25001:50000]
p4 = ggplot(melt(minpg), aes(value, col = variable)) + geom_density() + ggtitle("Page Rank (min value) Distribution") + xlim(0,0.0001)

#common neighbors
cn = data.frame(links_formed = numeric(25000), links_not_formed = numeric(25000))
cn$links_formed = d$common_neighbors[1:25000]
cn$links_not_formed = d$common_neighbors[25001:50000]
p5 = ggplot(melt(cn), aes(value, col = variable)) + geom_density() + ggtitle("Common Neighbors Distribution") + xlim(0,5)

multiplot(p1, p2, p3, p4, cols=2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}






##############plots for rough work#############
#distributions
plhist = path.length.hist(g1)$res
plDist = data.frame(pathLength = integer(length(plhist)), freq = integer(length(plhist)))
plDist$pathLength = 1:length(plhist)
plDist$freq = plhist
ggplot(plDist, aes(x=pathLength, y=freq)) + geom_bar(stat = "identity", fill = "steelblue") + labs(title="Path length distribution - Political blogs network", x="path length", y="number of paths") 
+ scale_y_continuous(labels = comma)

plot(degree.distribution(g1), pch=19, cex=1.2, col="orange", xlab="node degree (k)", ylab = "probability P(k)", main = "Degree distribution of so-jan-apr network")
plot(degree.distribution(g2), pch=19, cex=1.2, col="orange", xlab="node degree (k)", ylab = "probability P(k)", main = "Degree distribution of so-full network")
lines(degree_distribution(g1))
lines(degree_distribution(g2))

plDist = as.table(path.length.hist(g1)$res)
names(plDist) = 1:length(plDist)
barplot(plDist)