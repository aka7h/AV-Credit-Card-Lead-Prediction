library(tidyverse)

#we are going to create an ensemble
#here we are combining all the prediction that used
cat_boost_best <-read_csv("v1/catboost/catboost_baseline_1000_2021052814541622213656.csv")
cat_boost_X2 <- read_csv("v1/catboost/catboost_1000_X2_2021053009051622365527.csv")
cat_boost_X1 <- read_csv("v1/catboost/catboost_1000_2021053008511622364696.csv")
cat_boost_dw <- read_csv("v1/catboost/catboost_downsample_1000_2021052901251622251540.csv")
cat_boost_up <- read_csv("v1/catboost/catboost_upsample_1000_2021052901251622251542.csv")
cat_boost_X2_999 <- read_csv("v1/catboost/catboost_1000_X2_883_2021053010041622369074.csv")
cat_boost_X2_999_C <- read_csv("v1/catboost/catboost_1000_X2_883_clss_2021053010041622369092.csv")
cat_boost_class <- read_csv("v1/catboost/catboost_1000_X2_CLASS_2021053009211622366505.csv")
cat_tidy_v1 <- read_csv("v1/catboost/ak_bag_cat_tidymodels_2021052920351622320542.csv")
automl_v1 <- read_csv("v1/ak_h20_automl_bag_imp_nvz_dummy_2021052816421622200368.csv")
cat_bag_impute <- read_csv("v1/catboost/catboost_1206_bag_impute_2021053012251622377536.csv")
cat_baseline_2k <- read_csv("v1/catboost/catboost_baseline_2957_2021053014051622383516.csv")
cat_baseline_3k <- read_csv("v1/catboost/catboost_baseline_3946_2021053015021622386957.csv")

all_combine <- cbind(cat_boost_best,
                     select(cat_boost_dw,Is_Lead),
                     select(cat_boost_up,Is_Lead),
                    select(cat_boost_X1,Is_Lead),
                    select(cat_boost_X2,Is_Lead),
                    select(cat_boost_class,Is_Lead),
                    select(cat_tidy_v1,Is_Lead),
                    select(automl_v1,Is_Lead),
                    select(cat_boost_X2_999,Is_Lead),
                    select(cat_boost_X2_999_C,Is_Lead),
                    select(cat_bag_impute,Is_Lead),
                    select(cat_baseline_2k,Is_Lead),
                    select(cat_baseline_3k,Is_Lead))

colnames(all_combine) <- c("ID","cat_boost_best",
                           "cat_boost_dw",
                           "cat_boost_up","cat_boost_X1",
                           "cat_boost_X2","cat_boost_class",
                           "cat_tidy_v1","automl_v1","cat_boost_X2_999","cat_boost_X2_999_C","cat_bag_impute",
                           "cat_baseline_2k","cat_baseline_3k")

all_combine <- all_combine %>% mutate(cat_best_class=if_else(cat_boost_best>0.5,1,0))

# cor(all_combine[,-1])

#lets ensemble the prediction using the best 3
end <- all_combine %>% select(ID,cat_boost_best,cat_boost_X2_999,cat_boost_X2,cat_baseline_2k,cat_baseline_3k)


cor(end[,-1])

end <- end %>% mutate(final_ens = (cat_boost_best*0.40)+(cat_baseline_2k*0.40)+(cat_boost_X2*0.20))


submission <- data.frame('ID'=sample_sub$ID,'Is_Lead'=end$final_ens)
filename <- paste('cat_ens_2k_1k',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(submission,paste0(filename,'.csv',collapse = ''),row.names = FALSE)





