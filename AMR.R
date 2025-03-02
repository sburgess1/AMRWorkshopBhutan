library(AMR)

df_wide <- data.frame(
  microorganism = "Escherichia coli",
  amoxicillin = as.mic(8),
  cipro = as.mic(0.256),
  tobra = as.disk(16),
  genta = as.disk(18),
  ERY = "R"
)
df_wide

#Check if columns contain MIC values using is.mic
#Convert MIC values to senstivie S, intermediate I and resistant R
df_wide %>% mutate_if(is.mic, as.sir)

df <- read_csv("CEisolates_test.csv")
#Note dbl format stands for double - mean double precision floating point. It's more precise than an integer as it can store decimals.

#Join the columns bact_genus and bact_species, separate with a space
df2 <-  df %>%
  unite("microorganism", bact_genus, bact_species, sep = " ")

#To determine the correct abbreviation for streptomycin
as.ab("streptomycin")

#convert tibble to a dataframe
df2 <- data.frame(df2)
#Rename column names
colnames(df2) <- c("sample", "region", "wastemilk", "isolateID", "microorganism", "STR1", "CIP", "CTX", "TCY")

#Use mutate to modify/transform columns. as.disk converts numeric values into disk diffusion zone diameter data. 
df3 <- df2 %>%
  mutate(across(c(STR1, CIP, CTX, TCY), as.disk))
#Some of the entries were incorrect. To check which ones they were.
df2 %>%
  filter(STR1 %in% c("117", "636", "1701"))

df2 %>%
  filter(CIP %in% c("343.5", "348"))

df2 %>%
  filter(CTX %in% c("313"))

df2 %>%
  filter(TCY %in% c("241"))

#is.disk check that columns contain disk diffusion measurements
#as.sir converts the disk diffusion values into S, I or R using the CLSI guideline 
df4 <- df3 %>% mutate_if(is.disk, as.sir, guideline = "CLSI")

#To define my own breakpoints
df3 <- df2 %>%
  mutate(STR_sir = case_when(
    STR >= 15 ~ "S",  # Replace with your own breakpoints
    STR < 12 ~ "R",
    TRUE ~ "I"       # Intermediate
  ))

ggplot(df4, aes(x = CTX)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Frequency of Antimicrobial Interpretations (CTX)",
    x = "CTX Interpretation (S, I, R)",
    y = "Frequency"
  ) +
  theme_minimal()

##filtering clinical breakpoints used by the AMR package
clinical_breakpoints %>%
  filter(mo == "B_ESCHR_COLI" & method == "DISK" & guideline == "CLSI 2023") %>%
  print(n = Inf)



clinical_breakpoints %>%
  filter(ab == "STH" & method == "DISK") %>%
  print(n = Inf)


head(example_isolates_unclean)

example_isolates_unclean %>%
  pivot_wider(id_cols = patient_id,
              names_from = "bacteria",
              values_from = "AMX")

df %>%
  pivot_wider(id_cols = Region2,
              names_from = "WasteMilkFed",
              values_from = "Sdiff")
