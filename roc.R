library(rminer)
library(igraph)

d = read.csv("./project/data/year2.csv")
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

MDT3=mining(label~.,d,method=c("holdout",2/3), model="dt",Runs=20)
savemining(MDT3, "./project/data/mining_dt.output",ascii=TRUE)
MDT3=loadmining("./project/data/mining_dt.output")

MLR3=mining(label~.,d,method=c("holdout",2/3), model="logistic",Runs=20)
savemining(MLR3, "./project/data/mining_lr.output",ascii=TRUE)
MLR3=loadmining("./project/data/mining_lr.output")

MRF3=mining(label~.,d,method=c("holdout",2/3), model="randomforest",Runs=20)
savemining(MRF3, "./project/data/mining_rf.output",ascii=TRUE)
MRF3=loadmining("./project/data/mining_rf.output")
#> accuracy
#[1] 0.8707333

MXGB3 = mining(label~.,d,method=c("holdout",2/3), model = "xgboost", Runs = 20)
savemining(MXGB3, "./project/data/mining_xgb.output",ascii=TRUE)
MXGB3=loadmining("./project/data/mining_xgb.output")

MNN3 = mining(label~.,d,method=c("holdout",2/3), model = "mlp", Runs = 20)
savemining(MNN3, "./project/data/mining_nn.output", ascii=TRUE)
MNN3 = loadmining("./project/data/mining_nn.ouput")

MBOOST3=mining(label~.,d,method=c("holdout",2/3), model="boosting",Runs=20)
savemining(MBOOST3, "./project/data/mining_boost.output",ascii=TRUE)
MBOOST3=loadmining("./project/data/mining_boost.output")

### ROC Curve - Model Comparison
L=vector("list",5); L[[1]]=MRF3; L[[2]]=MLR3; L[[3]]=MDT3; L[[4]]=MXGB3; L[[5]]=MNN3;
mgraph(L,graph="ROC",TC=2,
       leg=list(pos=c(0.65,0.55), leg=c("RF (Acc: 87.58 %)","LR (Acc: 84.13 %)","DT (Acc: 86.11 %)", "XGB (Acc: 87.28 %)", "NN (Acc: 87.13 %)")), 
       baseline=TRUE, 
       Grid=15, main="ROC curves",
       col = c("blue","green","red","purple","magenta")) # ROC graph

MRF_wo_tags=mining(label~.,subset(d, select = -cos_sim),method=c("holdout",2/3), model="randomforest",Runs=20)
savemining(MRF_wo_tags, "./project/data/mining_rf_wo_tags.output",ascii=TRUE)
MRF_wo_tags=loadmining("./project/data/mining_rf_wo_tags.output")
#> accuracy
#[1] 0.6838   ---- recall very bad performance

### ROC Curve - Model comparison with and without tag similarity
L1=vector("list",2); L1[[1]]=MRF3; L1[[2]]=MRF_wo_tags;
mgraph(L1,graph="ROC",TC=2,
       leg=list(pos=c(0.65,0.55), leg=c("RF_with_tag-similarity\n(Acc: 87.58%)", "RF_without_tag-similarity\n(Acc: 68.38%)")), 
       baseline=TRUE, 
       Grid=15, main="ROC curves",
       col = c("blue","red")) # ROC graph
