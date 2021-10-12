library(caret)


#### reg line simple sur un jeu de test #####

data_train <- read.csv("/home/student/905/12004704/Documents/R2eannee/MachLearning/data_train.csv", header = F, col.names = c("weeks", "temp", "humidity", "growth"))


reg = lm(data$growth ~ data$humidity, data = data) # a une seule variable donc 2D
lm(data$growth ~ data$temp + data$humidity, data = data) # a deux variables donc 3D

ggplot(data = data_train, aes(x=humidity, y=growth)) + geom_point() + geom_abline(intercept=reg$coefficients[1], slope=reg$coefficients[2], col="red")

##### classification ######

t3var <- read.csv("https://lipn.univ-paris13.fr/~buscaldi/t3var", sep="\t")
test_t3 <- t3var[61:66,]
train_t3 <- t3var[1:60,]

glm_fit <- mutate(train_t3, y = as.numeric(sexe == "h")) %>% glm(y ~ poi + tai, data= ., family = "binomial")
summary(glm_fit)
##on test sur des valeurs 
sigmoid = function(x) {
  1/(1+exp(-x))
}

#on prend ce qu'on a appris via la regression, puis on multiplie par un 
sigmoid( 0.22396*50+ 0.18878*175-45.74686)
#en dessous de 0,5 donc appartient a femme, proba faible d'etre homme
sigmoid( 0.22396*65+ 0.18878*175-45.74686)
#probabilite d'etre un homme

y_hat_logit = ifelse(p_hat_logit > 0.5, "h","f")%>%factor
p_hat_logit <- predict(glm_fit, newdata = test_t3, type = "response")

confusionMatrix(y_hat_logit, factor(test_t3$sexe))

