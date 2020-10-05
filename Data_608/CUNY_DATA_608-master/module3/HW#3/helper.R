## Get and prep data for CUNY 608 / Project3

if (!file.exists('data.rds')) {
  data <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')
  saveRDS(data, 'data.rds')
} else {
  data <- readRDS('data.rds')
}

## Subset for Q1
sub <- data %>%
  filter(Year == 2010) %>%
  select(State, Crude.Rate, ICD.Chapter) %>%
  arrange(Crude.Rate)

sub2 <- data %>%
  group_by(ICD.Chapter, Year) %>%
  mutate(Nat.Avg = round((sum(Deaths) / sum(Population)) * 10^5, 1)) %>%
  select(ICD.Chapter, State, Year, Crude.Rate, Nat.Avg) %>%
  rename(State.Avg = Crude.Rate) %>%
  ungroup()

sub2$State <- as.character(sub2$State)
sub2$ICD.Chapter <- as.character(sub2$ICD.Chapter)

