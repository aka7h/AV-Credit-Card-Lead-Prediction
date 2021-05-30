library(tidyverse)
library(tidymodels)
library(h2o)
library(themis)

train <- read_csv("../input/jobathon-may-2021-credit-card-lead-prediction/train.csv")
test <- read_csv("../input/jobathon-may-2021-credit-card-lead-prediction/test.csv")

train <- train %>% mutate_if(is.character,as.factor) %>% select(-ID) %>% 
  mutate(Is_Lead=factor(Is_Lead))
test <- test %>% mutate_if(is.character,as.factor)%>% select(-ID)

head(train)
sapply(train,function(x)sum(is.na(x)))

splits <- initial_split(train,prop = 0.8,strata = Is_Lead)

pre_proc_1 <- function(x,...){
  x %>% recipe(Is_Lead~.) %>% 
    step_log(Avg_Account_Balance) %>%
    step_impute_bag(Credit_Product,
                    impute_with = imp_vars(Age,Vintage,Occupation,
                                           Avg_Account_Balance),
                    options = list(nbagg = 5, keepX = FALSE)) %>%
    step_dummy(all_nominal()) %>%
    step_nzv(all_predictors()) %>%
    prep()
}


pre_proc_2 <- function(x,...){
  x %>% recipe() %>% 
    step_log(Avg_Account_Balance) %>%
    step_impute_bag(Credit_Product,
                    impute_with = imp_vars(Age,Vintage,Occupation,
                                           Avg_Account_Balance),
                    options = list(nbagg = 5, keepX = FALSE)) %>%
    step_dummy(all_nominal()) %>%
    step_nzv(all_predictors()) %>%
    prep()
}


tr_bag_rec_d <- pre_proc_1(training(splits)) %>% juice(.)
te_bag_rec_d <- pre_proc_1(testing(splits)) %>% juice(.)
test_bag_rec_d <- pre_proc_2(test) %>% juice(.)

tr_bag_rec_d <- tr_bag_rec_d %>% mutate(Is_Lead_X1=factor(Is_Lead_X1))
te_bag_rec_d <- te_bag_rec_d %>% mutate(Is_Lead_X1=factor(Is_Lead_X1))



h2o.init()

tr_dd <- as.h2o(tr_bag_rec_d)
va_dd <- as.h2o(te_bag_rec_d)
te_dd <- as.h2o(test_bag_rec_d)

X <- colnames(te_dd)
Y <- "Is_Lead_X1"


almname <- paste('ak_h2o_automl',format(Sys.time(),"%d%H%M%S"),sep = '_')
autoML <- h2o.automl(X,Y,training_frame = tr_dd,
                     validation_frame = va_dd,seed=223, 
                     max_models=20,stopping_metric=c("AUC"),balance_classes=TRUE)

autoML@leader

leader_name <- as.tibble(autoML@leaderboard) %>% slice(1) %>% pull(model_id)

leader_model <- h2o.getModel(leader_name)

save(autoML, file="automlv1.rda")

yhat <- h2o.predict(leader_model,te_dd) %>% as_tibble()


submission <- data.frame('ID'=sample_sub$ID,'Is_Lead'=yhat$p1)
filename <- paste('ak_h20_automl_bag_imp_nvz_dummy',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(submission,paste0(filename,'.csv',collapse = ''),row.names = FALSE)