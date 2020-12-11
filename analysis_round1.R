library(tidyverse)
library(readxl)
library(openxlsx)

#### Data loading ####

file_data <- 'round1_data.xlsx'

column_names <- c('Nachname', 'Vorname',	'Benutzername',	'Duration', 'Terminated',	'participation_agreement',	'participation_agreement.num',
                  'data_usage',	'data_usage.num',	'background', 'background.num',	'gait_expertise_years_group',	'gait_expertise_years_group.num',
                  'expertise_free_living',	'expertise_free_living.num',	'expertise_patients',	'expertise_patients.num',	
                  'walking_1','walking_1_agree', 'walking_1_agree.num',
                  'walking_2', 'walking_2_agree', 'walking_2_agree.num',	'walking_comments',	
                  'purposeful', 'purposeful_agree', 'purposeful_agree.num',	'purposeful_comments',
                  'RW_1', 'RW_1_agree', 'RW_1_agree.num',
                  'RW_2', 'RW_2_agree','RW_2_agree.num',
                  'RW_3', 'RW_3_agree', 'RW_3_agree.num',	'RW_comments',
                  'WB', 'WB_agree', 'WB_agree.num',	'WB_comments'	,
                  'walking_speed_1', 'walking_speed_1_agree', 'walking_speed_1_agree.num',
                  'walking_speed_2', 'walking_speed_2_agree', 'walking_speed_2_agree.num',
                  'walking_speed_3', 'walking_speed_3_agree', 'walking_speed_3_agree.num','walking_speed_comments',
                  'turning', 'turning_agree', 'turning_agree.num',			'turning_comments')

data <- read_excel(file_data)
colnames(data) <- column_names

#### Data cleaning ####

# Remove entries without participation or data usage agreement (= NA or = No)
dropout_participation <- data$participation_agreement %in% c(NA, 'No')
droput_data <- data$data_usage %in% c(NA, 'No')
dropout <- dropout_participation | droput_data
n_dropout <- sum(dropout)
data_clean<-data[!dropout,]

# Remove descriptive columns
data_clean <- data_clean %>% select(-Nachname, -Vorname, -Benutzername, -Duration, -Terminated, -participation_agreement, -data_usage)

# Remove columns indicating skipped items
data_clean <- data_clean %>% select(-walking_1, -walking_2, -purposeful, -RW_1, -RW_2, -RW_3, -WB, -walking_speed_1, -walking_speed_2, -walking_speed_3, -turning)

# Remove all numeric columns (keep only factor columns)
numeric_columns <- grep('.num', colnames(data_clean))
colnames(data_clean)[numeric_columns]
data_clean <- data_clean %>% select(-numeric_columns)

# Extract comments into a separate text file
cat(paste0(data_clean$walking_comments[!is.na(data_clean$walking_comments)],'0', sep='\n-----\n'), file = 'round1_comments_walking.txt')
cat(paste0(data_clean$purposeful_comments[!is.na(data_clean$purposeful_comments)],'0', sep='\n-----\n'), file = 'round1_comments_purposeful.txt')
cat(paste0(data_clean$RW_comments[!is.na(data_clean$RW_comments)],'0',sep='\n-----\n'), file = 'round1_comments_RW.txt')
cat(paste0(data_clean$WB_comments[!is.na(data_clean$WB_comments)],'0', sep='\n-----\n'), file = 'round1_comments_WB.txt')
cat(paste0(data_clean$walking_speed_comments[!is.na(data_clean$walking_speed_comments)],'0', sep='\n-----\n'), file = 'round1_comments_WS.txt')
cat(paste0(data_clean$turning_comments[!is.na(data_clean$turning_comments)],'0', sep='\n-----\n'), file = 'round1_comments_turning.txt')
data_clean <- data_clean %>% select(-walking_comments, -purposeful_comments, -RW_comments, -WB_comments, -walking_speed_comments, -turning_comments)

# Remove entries without complete answers in the consensus questions
data_results <- data_clean[,5:15]
non_empty_rows <- !rowSums(is.na(data_results))>0 
sum(non_empty_rows)
data_clean <- data_clean[non_empty_rows,]

#### Extract participant information ####

data_participants <- data_clean[1:4]

df_background <- as.data.frame(table(data_participants$background)) %>%
  mutate(Perc = Freq / sum(Freq)*100)
df_gait_expertise <- as.data.frame(table(data_participants$gait_expertise_years_group))%>%
  mutate(Perc = Freq / sum(Freq)*100)
df_free_living_expertise <- as.data.frame(table(data_participants$expertise_free_living))%>%
  mutate(Perc = Freq / sum(Freq)*100)
df_patients_expertise <- as.data.frame(table(data_participants$expertise_patients))%>%
  mutate(Perc = Freq / sum(Freq)*100)

# Write background data to excel tables
l <- list('Background' = df_background,
          'Expertise' = df_gait_expertise,
          'Free Living expertise' = df_free_living_expertise,
          'Patient expertise' = df_patients_expertise)
write.xlsx(l, file = "round1_participants.xlsx")

#### Data analysis ####
# Set correct factor levels
levels <- c("Strongly disagree", "Somewhat disagree", "No opinion", "Somewhat agree", "Strongly agree")
data_clean$walking_1_agree <- factor(data_clean$walking_1_agree, levels = levels, ordered=TRUE)
data_clean$walking_2_agree <- factor(data_clean$walking_2_agree, levels = levels, ordered=TRUE)
data_clean$purposeful_agree <- factor(data_clean$purposeful_agree, levels = levels, ordered=TRUE)
data_clean$RW_1_agree <- factor(data_clean$RW_1_agree, levels = levels, ordered=TRUE)
data_clean$RW_2_agree <- factor(data_clean$RW_2_agree, levels = levels, ordered=TRUE)
data_clean$RW_3_agree <- factor(data_clean$RW_3_agree, levels = levels, ordered=TRUE)
data_clean$WB_agree <- factor(data_clean$WB_agree, levels = levels, ordered=TRUE)
data_clean$walking_speed_1_agree <- factor(data_clean$walking_speed_1_agree, levels = levels, ordered=TRUE)
data_clean$walking_speed_2_agree <- factor(data_clean$walking_speed_2_agree, levels = levels, ordered=TRUE)
data_clean$walking_speed_3_agree <- factor(data_clean$walking_speed_3_agree, levels = levels, ordered=TRUE)
data_clean$turning_agree <- factor(data_clean$turning_agree, levels = levels, ordered=TRUE)

# Absolute frequencies
data_results <- data_clean[5:15]
summary(data_results)
df_summary <- as.data.frame(t(as.data.frame(do.call(cbind,lapply(data_results, summary)))))

# Proportions
df_summary_rel <- sweep(df_summary,MARGIN=1, rowSums(df_summary), FUN="/" )

# Rownames to column
df_summary <- tibble::rownames_to_column(df_summary, "Question")
df_summary_rel <- tibble::rownames_to_column(df_summary_rel, "Question")

# Agreement
df_summary_agreement <- df_summary_rel%>%
  transmute(Question= Question,
            disagreement = `Strongly disagree`+`Somewhat disagree`,         
            neutral = `No opinion`,
            agreement = `Strongly agree`+`Somewhat agree`)

# Write agreement results to excel tables
l <- list('Agreement' = df_summary_agreement,
          'Absolute' = df_summary,
          'Relative' = df_summary_rel)
write.xlsx(l, file = "round1_results.xlsx")
