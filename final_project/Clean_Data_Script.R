library(tidyverse)
library(janitor)
library(readxl)
library(gganimate)
library(modelr)
library(easystats)
library(GGally)
library(naniar)
library(modelbased)
library(emmeans)
library(skimr)

theme_set(theme_minimal())
path <- "C:/Users/email/cameronmills0825.github.io/final_project/Data/Salaries_Among_Students_Based_On_Demographics.xlsx"
# get header names
education_names <-
  read_xlsx(path,range = "D4:AB6",col_names = FALSE) %>% 
  c() %>% 
  unlist() %>% 
  unique() %>% 
  str_remove_all("\\r") %>% 
  str_replace_all("\\n"," ") %>% 
  str_remove("\\\\1\\\\")
education_names <- c("Year","Total_nocollege",education_names[!is.na(education_names)])
# remove reduntant merged header
education_names <-
  education_names[education_names != "Bachelor's or higher degree"]

read_special <-
  function(file,sheetrange){
    x <- 
      read_xlsx(file,range = sheetrange,col_names = FALSE) %>% 
      select(all_of(c(1,2,4,6,9,12,15,17,20,23,25,27))) %>% 
      apply(2,as.numeric) %>% 
      as.data.frame()
    colnames(x) <- education_names
    return(x)
  }
male_realdollars <-
  read_special(path,"A10:AB32") %>% # sheet range for this first data area
  mutate(sex="Male",DataType="2020_Dollars")
female_realdollars <-
  read_special(path,"A34:AB56") %>% # sheet range for this second data area
  mutate(sex="Female",DataType="2020_Dollars")
real_dollars <- full_join(male_realdollars,female_realdollars)
real_dollars <-
  real_dollars %>%
  pivot_longer(!c("Year","sex","DataType"),names_to = "Education",values_to = "Median Income (2020 dollars)") %>% 
  filter(Education != "Total",
         Education != "Total_nocollege") 
real_dollars <-
  real_dollars %>%   
  mutate(Education = factor(Education, levels = real_dollars$Education %>% unique))
real_dollars %>%
  ggplot(aes(x=Education,y=`Median Income (2020 dollars)`,fill=sex,group=sex)) +
  geom_col(position = 'dodge') +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = .5)) +
  transition_states(Year)


df <- read_xlsx("C:/Users/email/cameronmills0825.github.io/final_project/Data/Full_Time_Worker_Data.xlsx")

df$`Percent of full time workers` <- as.numeric(as.character(df$`Percent of full time workers`))
df$Year <- as.numeric(as.character(df$Year))
df$`Full time workers` <- as.numeric(as.character(df$`Full time workers`))
df$Education <- as.factor(as.character(df$Education))

df %>% 
  mutate(Education = fct_relevel(Education, "Less than 9th Grade", "Some High School", "High School", 
                            "Some College", "Associates", "Bachelor's", "Master's", "Professional", "Doctor's")) %>% 
  ggplot(aes(x=Education, y=`Percent of full time workers`, fill=Gender, group=Gender)) + 
  geom_col(position = 'dodge') + 
  facet_wrap(~Gender) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  transition_states(Year)

# Models
names(real_dollars)
mod1 <- glm(data = real_dollars, 
            formula = real_dollars$`Median Income (2020 dollars)` ~ real_dollars$Year * real_dollars$Education)

mod2 <- glm(data = real_dollars, 
            formula = real_dollars$`Median Income (2020 dollars)` ~ real_dollars$Education + real_dollars$Year)

mod3 <- glm(data = real_dollars, 
            formula = real_dollars$`Median Income (2020 dollars)` ~ Education * Year * sex)

mod4 <- glm(data = real_dollars, 
            formula = real_dollars$`Median Income (2020 dollars)` ~ Year * Education)

step <- MASS::stepAIC(mod4, trace = 0)
mod5 <- glm(data = real_dollars, 
            formula = step$formula)


comps <- compare_performance(mod1, mod2, mod3, mod4, mod5, rank = TRUE)
comps %>% plot()


real_dollars %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5) %>% 
  ggplot(aes(x=`Median Income (2020 dollars)`, y=pred, color=model)) + 
  geom_segment(aes(x=0, y=0, xend=200000, yend=200000), linetype=2, color="black", alpha=.5) + 
  geom_smooth(method = "lm", se = FALSE)

new_data_m <- data.frame(Education="Master's degree", 
                       Year=2040, sex="Male")
predict(mod3, new_data_m)


new_data_f <- data.frame(Education="Master's degree", 
                       Year=2040, sex="Female")
predict(mod3, new_data_f)
###


?naniar

??modelbased


estimate_response(mod3)
skim(real_dollars)

