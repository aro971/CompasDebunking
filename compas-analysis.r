library(dplyr)
library(ggplot2)
raw_data <- read.csv("./compas-scores-two-years.csv")
nrow(raw_data)


df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')
nrow(df)

df$length_of_stay <- as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in))
cor(df$length_of_stay, df$decile_score)

#count(df$age_cat)
#count(df$race)
#count(df$score_text)
#count(df$decile_score)
xtabs(~ sex + race, data=df)
#count(df$sex)

#nrow(filter(df, two_year_recid == 1))

#Male TP, FP, TN, FN
tp <- nrow(filter(df, two_year_recid == 1 & (score_text == "High" | score_text == "Medium") & sex == "Male"))
fp <- nrow(filter(df, two_year_recid == 0 & (score_text == "High" | score_text == "Medium") & sex == "Male"))
fn <- nrow(filter(df, two_year_recid == 1 & score_text == "Low" & sex == "Male"))
tn <- nrow(filter(df, two_year_recid == 0 & score_text == "Low" & sex == "Male"))

tp
fp
fn
tn
(tp+fp)/(tp+fp+tn+fn)
tp/(tp+fn)
fp/(tn+fp)
tn/(fp+tn)
tp/(tp+fp) #quanti tra quelli danneggiati se lo meritavano
fp/(tp+fp) #quanti tra quelli danneggiati non se lo meritavano

#Female TP, FP, TN, FN
tp <- nrow(filter(df, two_year_recid == 1 & (score_text == "High" | score_text == "Medium") & sex == "Female"))
fp <- nrow(filter(df, two_year_recid == 0 & (score_text == "High" | score_text == "Medium") & sex == "Female"))
fn <- nrow(filter(df, two_year_recid == 1 & score_text == "Low" & sex == "Female"))
tn <- nrow(filter(df, two_year_recid == 0 & score_text == "Low" & sex == "Female"))

tp
fp
fn
tn
(tp+fp)/(tp+fp+tn+fn)
tp/(tp+fn)
fp/(tn+fp)
tn/(fp+tn)
tp/(tp+fp) #quanti tra quelli danneggiati se lo meritavano
fp/(tp+fp) #quanti tra quelli danneggiati non se lo meritavano

#Male TP, FP, TN, FN
tp <- nrow(filter(df, two_year_recid == 1 & decile_score > 5 & sex == "Male"))
fp <- nrow(filter(df, two_year_recid == 0 & decile_score > 5 & sex == "Male"))
fn <- nrow(filter(df, two_year_recid == 1 & decile_score <= 5 & sex == "Male"))
tn <- nrow(filter(df, two_year_recid == 0 & decile_score <= 5 & sex == "Male"))

(tp+fp)/(tp+fp+tn+fn)
tp/(tp+fn) #TPR tra i soggetti realmente pericolosi, quanti hanno avuto uno score alto
fp/(tn+fp)
tn/(fp+tn)
tp/(tp+fp) #quanti tra quelli danneggiati se lo meritavano
fp/(tp+fp) #quanti tra quelli danneggiati non se lo meritavano

#Female TP, FP, TN, FN
tp <- nrow(filter(df, two_year_recid == 1 & decile_score > 5 & sex == "Female"))
fp <- nrow(filter(df, two_year_recid == 0 & decile_score > 5 & sex == "Female"))
fn <- nrow(filter(df, two_year_recid == 1 & decile_score <= 5 & sex == "Female"))
tn <- nrow(filter(df, two_year_recid == 0 & decile_score <= 5 & sex == "Female"))

(tp+fp)/(tp+fp+tn+fn)
tp/(tp+fn)
fp/(tn+fp)
tn/(fp+tn)
tp/(tp+fp) #quanti tra quelli danneggiati se lo meritavano
fp/(tp+fp) #quanti tra quelli danneggiati non se lo meritavano

#White TP, FP, TN, FN
tp <- nrow(filter(df, two_year_recid == 1 & decile_score > 5 & race == "Caucasian"))
fp <- nrow(filter(df, two_year_recid == 0 & decile_score > 5 & race == "Caucasian"))
fn <- nrow(filter(df, two_year_recid == 1 & decile_score <= 5 & race == "Caucasian"))
tn <- nrow(filter(df, two_year_recid == 0 & decile_score <= 5 & race == "Caucasian"))

(tp+fp)/(tp+fp+tn+fn)
tp/(tp+fn)
fp/(tn+fp)
tn/(fp+tn)
tp/(tp+fp) #quanti tra quelli danneggiati se lo meritavano
fp/(tp+fp) #quanti tra quelli danneggiati non se lo meritavano

#Black TP, FP, TN, FN
tp <- nrow(filter(df, two_year_recid == 1 & decile_score > 5 & race == "African-American"))
fp <- nrow(filter(df, two_year_recid == 0 & decile_score > 5 & race == "African-American"))
fn <- nrow(filter(df, two_year_recid == 1 & decile_score <= 5 & race == "African-American"))
tn <- nrow(filter(df, two_year_recid == 0 & decile_score <= 5 & race == "African-American"))

(tp+fp)/(tp+fp+tn+fn)
tp/(tp+fn)
fp/(tn+fp)
tn/(fp+tn)
tp/(tp+fp) #quanti tra quelli danneggiati se lo meritavano
fp/(tp+fp) #quanti tra quelli danneggiati non se lo meritavano






library(grid)
library(gridExtra)
pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 650) + ggtitle("Black Defendant's Decile Scores")
pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 650) + ggtitle("White Defendant's Decile Scores")
grid.arrange(pblack, pwhite,  ncol = 2)

pmale <- ggplot(data=filter(df, sex =="Male"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 1100) + ggtitle("Male Defendant's Decile Scores")
pfemale <- ggplot(data=filter(df, sex =="Female"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 1100) + ggtitle("Female Defendant's Decile Scores")
grid.arrange(pmale, pfemale,  ncol = 2)


