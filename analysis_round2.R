library(tidyverse)
library(readxl)
library(openxlsx)

#### Data loading ####

file_data <- 'round2_data.xlsx'

column_names <- c('Nachname', 'Vorname',	'Benutzername',	'Duration', 'Terminated',	'participation_agreement',	'participation_agreement.num',
                  'purposeful', 'purposeful_agree', 'purposeful_agree.num',	'purposeful_comments',
                  'WB', 'WB_agree', 'WB_agree.num',	'WB_comments'	,
                  'walking_speed_3', 'walking_speed_3_agree', 'walking_speed_3_agree.num','walking_speed_comments')

data <- read_excel(file_data,1)
colnames(data) <- column_names

#### Data cleaning ####

# Remove entries without participation or data usage agreement (= NA or = No)
dropout_participation <- data$participation_agreement %in% c(NA, 'No')
dropout <- dropout_participation
n_dropout <- sum(dropout)
data_clean<-data[!dropout,]

# Remove descriptive columns
data_clean <- data_clean %>% select(-Nachname, -Vorname, -Benutzername, -Duration, -Terminated, -participation_agreement)

# Remove columns indicating skipped items
data_clean <- data_clean %>% select(-purposeful, -WB, -walking_speed_3)

# Remove all numeric columns (keep only factor columns)
numeric_columns <- grep('.num', colnames(data_clean))
colnames(data_clean)[numeric_columns]
data_clean <- data_clean %>% select(-numeric_columns)

# Extract comments into a separate text file
cat(paste0(data_clean$purposeful_comments[!is.na(data_clean$purposeful_comments)],'0', sep='\n-----\n'), file = 'round2_comments_purposeful.txt')
cat(paste0(data_clean$WB_comments[!is.na(data_clean$WB_comments)],'0', sep='\n-----\n'), file = 'round2_comments_WB.txt')
cat(paste0(data_clean$walking_speed_comments[!is.na(data_clean$walking_speed_comments)],'0', sep='\n-----\n'), file = 'round2_comments_WS.txt')
data_clean <- data_clean %>% select(-purposeful_comments, -WB_comments, -walking_speed_comments)

# Remove entries without complete answers (but where the participation agreement has been signed)
data_results <- data_clean
non_empty_rows <- !rowSums(is.na(data_results))>0 
sum(non_empty_rows)
data_clean <- data_clean[non_empty_rows,]

#### Data analysis ####
# Set correct factor levels
levels <- c("Strongly disagree", "Somewhat disagree", "No opinion", "Somewhat agree", "Strongly agree")
data_clean$purposeful_agree <- factor(data_clean$purposeful_agree, levels = levels, ordered=TRUE)
data_clean$WB_agree <- factor(data_clean$WB_agree, levels = levels, ordered=TRUE)
data_clean$walking_speed_3_agree <- factor(data_clean$walking_speed_3_agree, levels = levels, ordered=TRUE)

# Absolute frequencies
data_results <- data_clean
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
write.xlsx(l, file = "round2_results.xlsx")
