
library(dplyr)
library(ggpubr)
library(ggplot2)

# complete dataset, 2018-2023
enr_files <- paste("data/enrollment/",
                   list.files("data/enrollment/", pattern = ".csv$"), sep = "")

enrol <- do.call(rbind, lapply(enr_files, read.csv))

names(enrol) <- tolower(names(enrol))

enrol <- enrol %>%
  filter(lea.type != "Charter") %>%
  group_by(lea.name, school.year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

# data from 2013-2018, to be imputed
enrol_imp <- enrol[-c(1:nrow(enrol)), ]

enrol_imp <- data.frame(year = c(2013:2018),
                        school.lea = c(enrol$lea.name))

years <- 2013:2018
leas <- unique(enrol$lea.name)

enrol_imp <- expand.grid("lea.name" = leas, "school.year" = years)

for(i in names(enrol)[3:30]) {
  enrol_imp[i] <- NA
}
head(enrol_imp)

data.frame(unique(enrol_imp$lea.name))

imp_enr <- read.csv("data/enrollment/imp.csv")

imp_enr <- imp_enr %>%
  group_by(lea.name, school.ye)

g <- ggplot(imp_enr, aes(x = school.year, y = total.k.12, 
              group = lea.name, color = lea.name)) +
  geom_line() + geom_point(size = 2) + theme_bw()

g + gghighlight(lea.name == "North Summit District")

imp_enr$total.k.12 <- gsub(",", "", imp_enr$total.k.12)
imp_enr$total.k.12 <- as.numeric(imp_enr$total.k.12)

enrol_imp <- merge(imp_enr, enrol_imp, by = c('lea.name', 'school.year')) %>%
  select(-c(total.k.12.y)) %>%
  rename(total.k.12 = total.k.12.x)

# combining complete data and data to be imputed
enrol_all <- rbind(enrol_imp, enrol)

vars <- names(enrol_all)[4:30]
allModelsList <- lapply(paste(vars, " ~", 'total.k.12 + school.year + lea.name'), as.formula)
allModelsResults <- lapply(allModelsList, function(x) lm(x, data = enrol_all[]))  

# regression imputation
enrol_all <- enrol_all %>%
  mutate(k = ifelse(is.na(k), predict(allModelsResults[[1]], newdata = across()), k)) %>%
  mutate(grade_1 = ifelse(is.na(grade_1), predict(allModelsResults[[2]], newdata = across()), grade_1)) %>%
  mutate(grade_2 = ifelse(is.na(grade_2), predict(allModelsResults[[3]], newdata = across()), grade_2)) %>%
  mutate(grade_3 = ifelse(is.na(grade_3), predict(allModelsResults[[4]], newdata = across()), grade_3)) %>%
  mutate(grade_4 = ifelse(is.na(grade_4), predict(allModelsResults[[5]], newdata = across()), grade_4)) %>%
  mutate(grade_5 = ifelse(is.na(grade_5), predict(allModelsResults[[6]], newdata = across()), grade_5)) %>%
  mutate(grade_6 = ifelse(is.na(grade_6), predict(allModelsResults[[7]], newdata = across()), grade_6)) %>%
  mutate(grade_7 = ifelse(is.na(grade_7), predict(allModelsResults[[8]], newdata = across()), grade_7)) %>%
  mutate(grade_8 = ifelse(is.na(grade_8), predict(allModelsResults[[9]], newdata = across()), grade_8)) %>%
  mutate(grade_9 = ifelse(is.na(grade_9), predict(allModelsResults[[10]], newdata = across()), grade_9)) %>%
  mutate(grade_10 = ifelse(is.na(grade_10), predict(allModelsResults[[11]], newdata = across()), grade_10)) %>%
  mutate(grade_11 = ifelse(is.na(grade_11), predict(allModelsResults[[12]], newdata = across()), grade_11)) %>%
  mutate(grade_12 = ifelse(is.na(grade_12), predict(allModelsResults[[13]], newdata = across()), grade_12)) %>%
  mutate(female = ifelse(is.na(female), predict(allModelsResults[[14]], newdata = across()), female)) %>%
  mutate(male = ifelse(is.na(male), predict(allModelsResults[[15]], newdata = across()), male)) %>%
  mutate(american.indian = ifelse(is.na(american.indian), predict(allModelsResults[[16]], newdata = across()), american.indian)) %>%
  mutate(afam.black = ifelse(is.na(afam.black), predict(allModelsResults[[17]], newdata = across()), afam.black)) %>%
  mutate(asian = ifelse(is.na(asian), predict(allModelsResults[[18]], newdata = across()), asian)) %>%
  mutate(hispanic = ifelse(is.na(hispanic), predict(allModelsResults[[19]], newdata = across()), hispanic)) %>%
  mutate(multiple.race = ifelse(is.na(multiple.race), predict(allModelsResults[[20]], newdata = across()), multiple.race)) %>%
  mutate(pacific.islander = ifelse(is.na(pacific.islander), predict(allModelsResults[[21]], newdata = across()), pacific.islander)) %>%
  mutate(white = ifelse(is.na(white), predict(allModelsResults[[22]], newdata = across()), white)) %>%
  mutate(economically.disadvantaged = ifelse(is.na(economically.disadvantaged), predict(allModelsResults[[23]], newdata = across()), economically.disadvantaged)) %>%
  mutate(english.learner = ifelse(is.na(english.learner), predict(allModelsResults[[24]], newdata = across()), english.learner)) %>%
  mutate(student.with.a.disability = ifelse(is.na(student.with.a.disability), predict(allModelsResults[[25]], newdata = across()), student.with.a.disability)) %>%
  mutate(homeless = ifelse(is.na(homeless), predict(allModelsResults[[26]], newdata = across()), homeless)) %>%
  mutate(preschool = ifelse(is.na(preschool), predict(allModelsResults[[27]], newdata = across()), preschool)) %>%
  mutate(across(4:30, round)) # integers only

# bottom coding
enrol_all[enrol_all < 0] <- 0

write.csv(enrol_all, "enrollment_13_23.csv")


