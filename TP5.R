library(caret)

parties <- read.csv("http://lipn.univ-paris13.fr/~buscaldi/parties.csv", sep=";")
summary(parties$winner)
barplot(table(parties$winner))

matrice_corr <- cor(parties[2:5])
findCorrelation(matrice_corr)

params <- preProcess(parties[, 1:5], method = c("range"))
normalised <- predict(params, parties[,1:5])

params <- preProcess(parties[, 1:5], method = c("center", "scale"))
standardised <- predict(params, parties[,1:5])

summary(normalised) ### ou ###
summary(standardised)


plot(x=parties$white_rating, y = parties$black_rating, col=ifelse(standardised$winner=='white','red','blue'))
plot(x=parties$white_rating, y=parties$turns, col=ifelse(standardised$winner=='white','red','blue'))     
plot(x=parties$white_rating, y=parties$opening_ply, col=ifelse(standardised$winner=='white','red','blue'))     


#### PCA ####
params <- preProcess(standardised, method = c("pca"))


pca_red = predict(params, parties[,1:5])
ggplot(data=pca_red,aes(PC3,PC4)) +geom_point(aes(col=parties$winner))


### apprentissage et test ####

set.seed(42)  #pour reproduire les mêmes résultats
trainIndex <- createDataPartition(parties$winner, p=0.7, list=F)
training <- parties[trainIndex, ]
testing <- parties [-trainIndex, ]

table(training$winner)
table(testing$winner)

fitControl <- trainControl(method="none")
model_lr <- train(training[2:5], training$winner, method="glm", trControl=fitControl)
summary(model_lr)

pred = predict(model_lr, newdata=testing)
confusionMatrix(data=pred,reference=as.factor(testing$winner),positive="white") 
# le model donne 62% d'accuracy, juste correct


## PAREIL avec les donnees standardisees ##
trainIndexst <- createDataPartition(standardised$winner, p=0.7, list=F)
trainingst <- standardised[trainIndex, ]
testingst <- standardised [-trainIndex, ]


fitControl2 <- trainControl(method="none")
model_lrst <- train(trainingst[2:5], trainingst$winner, method="glm", trControl=fitControl)
summary(model_lrst)

predst = predict(model_lrst, newdata=testingst)
confusionMatrix(data=pred,reference=as.factor(testingst$winner),positive="white") 

# ne donne rien de mieux, on soupconne que puique l'elo est reparti de la meme manniere cheez
# les joueurs blancs et noir alors on n'a pas mieux

### PAREIL mais avec PCA ###

set.seed(42)  #pour reproduire les mêmes résultats
trainIndex <- createDataPartition(parties$winner, p=0.7, list=F)
trainingpca <- pca_red[trainIndex, ]
testingpca <- pca_red [-trainIndex, ]

fitControlpca <- trainControl(method="none")
model_lrpca <- train(trainingpca[c(3,5)], trainingpca$winner, method="glm", trControl=fitControlpca)
summary(model_lrpca)

predpca = predict(model_lrpca, newdata=testingpca)
confusionMatrix(data=pred,reference=as.factor(testingpca$winner),positive="white") 

### Cross validation ####
crossvalCtl <- trainControl(method="cv", number = 10) # number = folder, convention = 10 ou leave one out
model_cv <- train(standardised[2:5], standardised$winner, method="glm", trControl=crossvalCtl)
model_cv # on n'obtient encore rien de mieux, meme accuracy 
# val croisee = encore plus rapide, pas besoin de split nous meme les tests et train