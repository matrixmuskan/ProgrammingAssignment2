setwd("C:/Users/mkukreja/Desktop/Machine_Learning_Datasets")
library(data.table)
library(ggplot2)
library(caret)
library(gbm)
library(doParallel)
library(foreach)
library(foreign)
library(corrplot)
library(reshape2)
cl = makeCluster(6)
registerDoParallel(cl)
library(gplots)

data = read.csv('bank/bank.csv',header = T,sep = ';',stringsAsFactors=TRUE)

nums = sapply(data,is.numeric)
numeric_data = data[,nums]
#Analyzing the numerical data to check for correlation
corrplot(cor(numeric_data),method = 'ellipse')
#featurePlot(x = numeric_data, y = data$y, plot = 'pairs')
numeric_label = data[,c(colnames(numeric_data),'y')]

dat = melt(numeric_label,id.vars = 'y')
x2 = theme(axis.text.y = element_text(color = "black",face = 'bold',size = 20),axis.text.x = element_text(color = "black", face ="bold",size = 15, hjust = 1),plot.title = element_text(lineheight = 0.8,face = 'bold',size = 30,color = 'red'),axis.title.y = element_text(color = "black",face = 'bold',size = 20),panel.background = element_rect(fill = 'gray88'),panel.grid = element_line(size = 2, colour = 'red'))  

ggplot(data = dat, aes(value)) + geom_density(aes(fill = y,aes = 0.5)) + facet_wrap(~variable,scales = 'free') + x2
ggplot(data = dat_tab, aes(y = value,x = y)) + geom_boxplot(aes(fill = y)) + facet_wrap(~variable,scales = 'free') + x2 

# Feature Selection Process
dat_tab = as.data.table(dat)
pval = dat_tab[, list(pval = round(t.test(value ~ y)$p.value,4)), by = list(variable)]
textplot(pval,cex = 2)

#Rank features
control = trainControl(method = 'cv',number = 3,repeats = 1,returnData = F)
model = train(y ~., data = data, method = 'rpart',model = FALSE)
importance = varImp(model, scale = TRUE)
plot(importance)
library(rattle)
fancyRpartPlot(model$finalModel)

#Some more methods for feature selection
library(randomForest)

library(mlbench)
results = rfe(data[,1:16],data[,17],rfeControl = control)
print(results)
predictors(results)
plot(results,type = c('o','g'))

set.seed(7)
# Lets do some more modeling 
data = as.data.frame(data)
data$y = as.factor(make.names(data$y))

rf_model = train(class ~., data = data, method = 'rf',allowParallel = FALSE)

fit = randomForest(class ~ ., data = data)
print(fit)
write.csv(data,'bank_data.csv',quote = FALSE)

data$class = data$y
data$y = NULL
nb = train(class ~ ., data = data, method = 'nb')
nb_cv = train(class ~ ., data = data, method = 'nb')



########## CARET TUTORIAL 
library(mlbench)
data(Sonar)
head(Sonar)
dim(Sonar)
set.seed(998)
inTraining = createDataPartition(Sonar$Class,p = 0.75,list = FALSE)
training  = Sonar[inTraining,]
test = Sonar[-inTraining,]

#Basic Parameter Tuning
# By default bootstrap resamlping is used
fitControl = trainControl(method  = 'repeatedcv',number = 10, repeats = 10,classProbs = TRUE,summaryFunction = twoClassSummary)

gbmFit1 = train(Class ~ ., data = training, method = 'gbm', verbose = FALSE,tuneLength = 5)
trellis.par.set(caretTheme())
plot(gbmFit1)
plot(gbmFit1,plotType = "level")
ggplot(gbmFit1) + x2

#Preprocess options
# center, scaling, and imputation 
library(devtools)


highlyCorrelated = findCorrelation(cor(training[,1:60]),cutoff = 0.90)
highlyCorrelated
corrplot(cor(training[,1:60]),method = 'ellipse')

dat = melt(training,id.vars = 'Class')
x2 = theme(axis.text.y = element_text(color = "black",face = 'bold',size = 20),axis.text.x = element_text(color = "black", face ="bold",size = 15, hjust = 1),plot.title = element_text(lineheight = 0.8,face = 'bold',size = 30,color = 'red'),axis.title.y = element_text(color = "black",face = 'bold',size = 20),panel.background = element_rect(fill = 'gray88'),panel.grid = element_line(size = 2, colour = 'red'))  

ggplot(data = dat, aes(value)) + geom_density(aes(fill = Class,aes = 0.5)) + facet_wrap(~variable,scales = 'free') + x2
ggplot(data = dat, aes(y = value,x = Class)) + geom_boxplot(aes(fill = Class)) + facet_wrap(~variable,scales = 'free') + x2 

results = predict(gbmFit1, newdata = test,type = 'prob')
results$Class = ifelse(test$Class == 'R',1,0)
results$Class = test$Class
ggplot(data = results, aes(x = Class, y = R )) + geom_point()

confusionMatrix(test$Class,predict(gbmFit1, newdata = test))
plot(varImp(gbmFit1))

#Feature decoration 
nearZeroVar(training)

svmFit <- train(Class ~ ., data = training,
                method = "svmRadial",
                trControl = fitControl,
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "ROC")

set.seed(825)
rdaFit <- train(Class ~ ., data = training,
                method = "rda",
                trControl = fitControl,
                tuneLength = 4,
                metric = "ROC")

resamps <- resamples(list(
                          SVM = svmFit,
                          RDA = rdaFit))

trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")

difValues <- diff(resamps)

library(gplots)

heatmap.2(as.matrix(training[,1:60]),margin = c(5,5),trace = 'none',RowSideColors = as.character(as.numeric(training$Class)))
heatmap.2(as.matrix(iris[,1:4]),margin = c(5,5),trace = 'none',RowSideColors = as.character(as.numeric(iris$Species)))
heatmap.2(as.matrix(iris[,1:4]),margin = c(5,5),trace = 'none',RowSideColors = as.character(as.numeric(iris$Species)),scale = '')
heatmap.2(as.matrix(mtcars),margin = c(5,15),trace = 'none',RowSideColors = as.character(as.numeric(mtcars$cyl)),scale = 'col')
legend("topright",      
       legend = unique(mtcars$cyl),
       col = unique(as.numeric(mtcars$cyl)), 
       lty= 1,             
       lwd = 5,           
       cex=.7
)

pca_iris = prcomp(iris[,1:4],center = TRUE, scale = TRUE)

require(caret)
trans = preProcess(iris[,1:4], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, iris[,1:4])

library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)

rf_fit = randomForest(x = iris[,1:4],y = iris$Species)

library(caret)
intrain = createDataPartition(iris$Species,p = 0.7,list = FALSE)
training = iris[intrain,]
test = iris[-intrain,]
rf_fit = randomForest(x = training[,1:4],y = training$Species)
confusionMatrix(predict(rf_fit,test),test$Species)

library(caret)
rf_caret  = train(Species ~ . , data = training, method = 'rf')
confusionMatrix(predict(rf_caret,test),test$Species)



