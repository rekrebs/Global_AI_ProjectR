#clear house
rm(list = ls())
gc()
##libraries
library(dplyr)
library(randomForest)
library(ggplot2)
library(patchwork)
library(nlme)
library(scales)
library(lme4)
library(performance)
library(tidyr)
##set directory
setwd("C:/Users/Admin/Desktop/AI")

# read in
df <- read.csv("ai_company_adoption.csv", header=T, sep=",")

#set colour palette data bears

databears_pal_9 <- c(
  "blue"        = "#0078D4",  
  "orange"      = "#F25F22",
  "yellow"      = "#FFB900",  
  "green"       = "#107C41",  
  "royal"       = "#1F4E79",  
  "muted_teal"  = "#3B738F",  
  "slate_blue"  = "#1D3E56",  
  "pale_aqua"   = "#8BB3C6",  
  "navy"        = "#081726"   
)



databears_pal_6 <- c(
 
  "orange"      = "#F25F22",
  "yellow"      = "#FFB900",  
  "green"       = "#107C41",  
  "royal"       = "#1F4E79",  
  "slate_blue"  = "#1D3E56",  
  "navy"        = "#081726"   )
  


databears_pal_3 <- c(
  
  "yellow"      = "#FFB900",  
  "green"       = "#107C41",  
  "royal"       = "#1F4E79"   )


#Clean

model_data <- df %>%
  select(
    company_id,
    jobs_displaced, 
    industry, 
    company_size, 
    task_automation_rate, 
    ai_investment_per_employee, 
    productivity_change_percent
  ) %>%
  filter(complete.cases(.)) ###handle missing 


##should be the case already, but to be sure
model_data$industry <- as.factor(model_data$industry)  #handle as factors 
model_data$company_size <- as.factor(model_data$company_size) #handle as factors 

###############################################################################
### I think this is a good idea for a multilinear regression model... 
### (and there is nested data (company)from the companies over time... autocorrelation?)

# make zscore norms 
model_data_scaled <- model_data %>%
  mutate(
    automation_rate_z = scale(task_automation_rate),
    ai_investment_z   = scale(ai_investment_per_employee),
    productivity_z     = scale(productivity_change_percent)
  )

#fit the Linear Mixed-Effects Model
# fixed effects: jobs_displaced ~ industry + company_size + automation_rate_z + ai_investment_z + productivity_z
# random effects: ~ 1 | company_id
mixed_m1 <- lme(
  fixed = jobs_displaced ~ industry + company_size + automation_rate_z + ai_investment_z + productivity_z,
  random = ~ 1 | company_id,
  data = model_data_scaled,
  control = lmeControl(opt = "optim") # stabilizer
)

# statistics

summary(mixed_m1)
summary_meta <- summary(mixed_m1)
coef_df <- as.data.frame(summary_meta$tTable)
colnames(coef_df) <- c("Value", "StdError", "DF", "tValue", "pValue")
coef_df$Variable <- rownames(coef_df)


##wqikc look

#focus on factors
coef_plot_data <- coef_df %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Significance = if_else(pValue < 0.05, "Statistically Significant", "Not Significant"),
    # Clean
    Variable = case_when(
      Variable == "automation_rate_z" ~ "Task Automation Rate",
      Variable == "ai_investment_z"   ~ "AI Investment per Employee",
      Variable == "productivity_z"     ~ "Productivity Change %",
      TRUE ~ Variable
    ),
    Variable = reorder(Variable, Value) # Sorting bars by the scale of their effect,
  )
##wqikc look

coef_plot_data
#Driver Impact Chart
plot_drivers=ggplot(coef_plot_data, aes(x = Variable, y = Value)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7, fill = "#107C41") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
  coord_flip() + 
  labs(
  #  title = "Which Factors Drive Job Displacement Most Over Time?",
  #  subtitle = "lme",
    x = "Faktoren",
    y = "Relative Schlagfestigkeit" #repeated measures
  ) +
  theme_minimal(base_size = 14) +
  theme(

    axis.text.y = element_text(hjust = 1) ,
 
    panel.grid.minor = element_blank()
  )
plot_drivers
##Weird... I think something is wrong...


library(car)
# is the model good though?

##check for correlation 
vif_model <- lm(jobs_displaced ~ industry + company_size + task_automation_rate + ai_investment_per_employee + productivity_change_percent, data = df)
vif(vif_model)



library(performance)  ## check performance

# Check the R2 values
r2(mixed_m1)   ### R2: 0.861
##### after plotting this I have realized that I can´t use the raw numbers for this because enterprises
##are way too different


df_glmm <- df %>%
  mutate(

    Company_Size = as.factor(company_size),
    Company_ID   = as.factor(company_id),     
    Year         = as.factor(survey_year),          
    Automation = task_automation_rate,
    Industry = as.factor(industry),
    Total_Employees = num_employees, 
    Raw_Proportion = jobs_displaced / 100,
    Loss_Proportion   = pmax(0, pmin(1, Raw_Proportion))
  ) %>%
  # Eliminate missing rows or invalid zero-employee structural edge cases
  filter(!is.na(Raw_Proportion), Total_Employees > 0, !is.na(Automation), !is.na(Industry)) ### rm missing


# family = binomial (0 and 1 boundary limits)
# weights = log(Total_Employees)
#(1 + Automation | Company_Size) slopes can vary my company size
#(1 | Company_ID) random effect for company

glmm_model <- glmer(
  Loss_Proportion ~ Automation + Year + Industry + (1 + Automation | Company_Size) + (1 | Company_ID), 
  data = df_glmm, 
  family = binomial, 
  weights = log(Total_Employees),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


summary(glmm_model)
print(glmm_model, correlation=TRUE)

###make corr mat here with heat map. this looks cool
##get r2
if(!require(performance)) install.packages("performance")
library(performance)
r2_results <- r2(glmm_model)
print(r2_results)


df_glmm$Model_Predictions_Pct <- predict(glmm_model, type = "response", allow.new.levels = TRUE) * 100
df_glmm$Actual_Loss_Pct         <- df_glmm$Loss_Proportion * 100
df_glmm$Automation_Rate_Axis    <- df_glmm$Automation


df_dots <- df_glmm %>% 
  group_by(Company_Size) %>% 
  sample_frac(size = 0.1) %>% 
  ungroup()


df_lines <- df_glmm %>% 
  group_by(Company_Size, Automation_Rate_Axis) %>% 
  summarise(Model_Predictions_Pct = mean(Model_Predictions_Pct, na.rm = TRUE), .groups = "drop") %>% 
  arrange(Automation_Rate_Axis) 

glmm_model_plot <- ggplot() + 

  geom_jitter(data = df_dots, aes(x = Automation_Rate_Axis, y = Actual_Loss_Pct, color = Company_Size),
              alpha = 0.2, size = 1.2, width = 0.5, height = 0) + 
   geom_smooth(data = df_lines, aes(x = Automation_Rate_Axis, y = Model_Predictions_Pct, color = Company_Size, group = Company_Size), 
            linewidth = 1.5) + 
   scale_color_manual(
    values = c("Enterprise" = "#FFC107", "SME" = "#198754", "Startup" = "#3B628B"), 
    name = "Company Size"
  ) + 
  coord_cartesian(ylim = c(0, 100)) + 
  
  theme_minimal(base_size = 14) + 
  labs(
    title = "GLMM Model Fits", 
    subtitle = "Calculated using Binomial distribution weights to control for employee count constraints", 
    x = "AI Automation Rate (%)", 
    y = "Workforce Reduction Impact (%)"
  ) + 
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 16), 
    panel.grid.minor = element_blank()
  )



glmm_model_plot


###### run with ceiling cap #####

###assuming a may number of jobs that can actually be lost
CEILING_FACTOR <- 0.45 ## try to cap the bionomial distribution so I don´t have a doomsday scenario

df_glmm_corrected <- df_glmm %>%
  mutate(
    Max_Possible_Displaced = Total_Employees * CEILING_FACTOR,
    Displaced_Count_Ceiling = pmin(Displaced_Count, Max_Possible_Displaced),
    Remaining_Count_Ceiling = Total_Employees - Displaced_Count_Ceiling
  )


model_capped <- glmer(
  cbind(Displaced_Count_Ceiling, Remaining_Count_Ceiling) ~ Automation_Rate_Axis * Company_Size + (1 | Random_Effect),
  data = df_glmm_corrected,
  family = binomial,
  weights = log(Total_Employees) 
)

df_glmm_corrected$Model_Predictions_Pct <- predict(model_capped, type = "response") * 100
###############################################################################
##way too slow and messy

##################################################################################

df_glmm_corrected$Model_Predictions_Pct <- predict(df_glmm_corrected, type = "response", allow.new.levels = TRUE) * 100
df_glmm_corrected$Actual_Loss_Pct         <- df_glmm_corrected$Loss_Proportion * 100
df_glmm_corrected$Automation_Rate_Axis    <- df_glmm_corrected$Automation


df_dots <- df_glmm_corrected %>% 
  group_by(Company_Size) %>% 
  sample_frac(size = 0.1) %>% 
  ungroup()


df_lines <- df_glmm_corrected %>% 
  group_by(Company_Size, Automation_Rate_Axis) %>% 
  summarise(Model_Predictions_Pct = mean(Model_Predictions_Pct, na.rm = TRUE), .groups = "drop") %>% 
  arrange(Automation_Rate_Axis) 

glmm_model_plot <- ggplot() + 
  
  geom_jitter(data = df_dots, aes(x = Automation_Rate_Axis, y = Actual_Loss_Pct, color = Company_Size),
              alpha = 0.2, size = 1.2, width = 0.5, height = 0) + 
  geom_smooth(data = df_lines, aes(x = Automation_Rate_Axis, y = Model_Predictions_Pct, color = Company_Size, group = Company_Size), 
              linewidth = 1.5) + 
  scale_color_manual(
    values = c("Enterprise" = "#FFC107", "SME" = "#198754", "Startup" = "#3B628B"), 
    name = "Company Size"
  ) + 
  coord_cartesian(ylim = c(0, 100)) + 
  
  theme_minimal(base_size = 14) + 
  labs(
    title = "GLMM Model Fits", 
    subtitle = "Binomial distribution weights (employee count)", 
    x = "AI Automation Rate (%)", 
    y = "Workforce Reduction Impact (%)"
  ) + 
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 16), 
    panel.grid.minor = element_blank()
  )


### better to 

######Make graphs ### for story telling  ##

###set bears palette



my_palette <- c(
  "Displacement_Driver" = "#D9534F", 
  "Job_Shield"          = "#228B22", 
  "Control_Layer"       = "#777777"  
)

coef_df <- as.data.frame(summary(mixed_m1)$coefficients)
coef_df$Variable <- rownames(coef_df)
colnames(coef_df)[1] <- "Value" 

coef_plot_data <- coef_df %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Clean_Name = case_when(
      Variable == "automation_rate_z"   ~ "Task Automation Rate",
      Variable == "ai_investment_z"     ~ "AI Investment per Employee",
      Variable == "productivity_z"      ~ "Productivity Change %",
      Variable == "company_sizeStartup" ~ "Company Structure: Startup",
      TRUE ~ Variable
    ),
    Impact_Group = case_when(
      Variable %in% c("automation_rate_z", "company_sizeStartup") ~ "Displacement_Driver",
      Variable == "productivity_z" ~ "Job_Shield",
      TRUE ~ "Control_Layer"
    ),
    Clean_Name = reorder(Clean_Name, Value)
  )

ggplot(coef_plot_data, aes(x = Clean_Name, y = Value, fill = Impact_Group)) +
  geom_bar(stat = "identity", alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "#444444", size = 0.6) +
  scale_fill_manual(values = my_palette, guide = "none") +
  coord_flip() + 
  labs(
    title = "Key Catalysts of Modern Job Displacement Patterns",
    subtitle = "Standardized Model Impact Strength (Explains 86% of Dataset Variance)",
    x = "Organizational & Operational Drivers",
    y = "Relative Impact Multiplier Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = ggplot2::margin(t = 15, r = 20, b = 15, l = 140, unit = "pt"),
    axis.text.y = element_text(hjust = 1, face = "bold")
  )
##############################

#bear col
my_palette <- c(
  "Primary_Bar" = "#3B738F"  
)


industry_summary <- df %>%
  filter(!is.na(industry), !is.na(jobs_displaced)) %>%
  group_by(industry) %>%
  summarize(Total_Displaced = sum(jobs_displaced), .groups = 'drop') %>%
  mutate(industry = reorder(industry, Total_Displaced))

# plot
industry_summary_plot= ggplot(industry_summary, aes(x = industry, y = Total_Displaced)) +
  geom_bar(stat = "identity", fill = databears_pal_9, alpha = 0.85, width = 0.7) +
  scale_y_continuous(labels = scales::comma) + 
  coord_flip() +
  labs(
    x = "Branche",
    y = "Gesamtzahl der verdrängten Arbeitsplätze (2023–2026)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
     plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
#    axis.text.y = element_text(hjust = 1, face = "bold")
  )

industry_summary_plot



###### and just germany

industry_summary <- df%>%

  select(
    country = matches("country", ignore.case = TRUE),
    industry = matches("industry", ignore.case = TRUE),
    jobs_displaced = matches("jobs_displaced", ignore.case = TRUE)
  ) %>%

  filter(grepl("germany", country, ignore.case = TRUE)) %>% 
  filter(!is.na(industry), !is.na(jobs_displaced)) %>%
  group_by(industry) %>%
  summarize(Total_Displaced = sum(jobs_displaced), .groups = 'drop') %>%
  mutate(industry = reorder(industry, Total_Displaced))

df$jobloss_proportion=(df$jobs_displaced/df$num_employees)

corr_DE=cor(df$jobloss_proportion,df$task_automation_rate)

industry_summary_plot_DE <- ggplot(industry_summary, aes(x = industry, y = Total_Displaced)) +
  geom_bar(stat = "identity", fill = databears_pal_9, alpha = 0.85, width = 0.7) +
  scale_y_continuous(labels = scales::comma) + 
  coord_flip() +
  labs(
  ##  title = "Gesamtzahl der verdrängten Arbeitsplätze in Deutschland",
  ##  subtitle = "Branchenübersicht basierend auf den Modellergebnissen (2023–2026)",
    x = "Branche",
    y = "Gesamtzahl der verdrängten Arbeitsplätze (2023-2026)"
  ) +
  theme_minimal() +
  theme(
##    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
 ##   plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
 #   axis.text.y = element_text(hjust = 1, face = "bold"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)) # Pushes axis title down slightly

  
  )

# Display the final German industry graphic panel
industry_summary_plot_DE


######################################


# DE
de_summary <- df %>%
  select(
    country = matches("country", ignore.case = TRUE),
    company_size = matches("company_size", ignore.case = TRUE),
    jobs_displaced = matches("jobs_displaced", ignore.case = TRUE),
    num_employees = matches("num_employees", ignore.case = TRUE)
  ) %>%
  filter(grepl("germany", country, ignore.case = TRUE)) %>%
  filter(!is.na(company_size), !is.na(jobs_displaced), !is.na(num_employees)) %>%
  group_by(company_size) %>%
  summarize(Rate_Displaced = sum(jobs_displaced) / sum(num_employees), .groups = 'drop') %>%
  mutate(company_size = reorder(company_size, Rate_Displaced))


de_plot <- ggplot(de_summary, aes(x = company_size, y = Rate_Displaced)) +
  geom_bar(stat = "identity", fill = databears_pal_3, alpha = 0.85, width = 0.6) +
  geom_text(
    aes(label = scales::percent(Rate_Displaced, accuracy = 0.1)),
    hjust = -0.2, 
    face = "bold", 
    size = 4.5, 
    color = "#333333"
  ) +
  
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.12))) + 
  coord_flip(clip = "off") +
  
  labs(
    title = "Deutschland: Interner Stellenabbau",
    subtitle = "Prozentualer Anteil der verdrängten Belegschaft (2023–2026)",
    x = "Unternehmensgröße",
    y = "Anteil der verdrängten Belegschaft"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    # fix padding
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
    
    axis.text.y = element_text(hjust = 1, face = "bold"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10))
  )


print(de_plot)

#################################################################################

hidden_driver_plot <- ggplot(plot_data, aes(x = task_automation_rate, y = Rate_Displaced)) + 
  geom_point(
    data = plot_data %>% sample_frac(0.10), 
    aes(color = company_size), 
    alpha = 0.5, 
    size = 2
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "#2B2B2B", linewidth = 1.8) +
  
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c("Enterprise" = "#FFC107", "SME" = "#198754", "Startup" = "#3B628B"),
    name = "Unternehmensgröße"
  ) +
  
  labs(
    x = "Wie viel Prozent der Arbeit automatisiert ist",
    y = "Interner Stellenabbau (Prozent)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
    
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Display the plot
print(hidden_driver_plot)

#################################################################################################
raw_jobs<- ggplot(plot_data, aes(x = task_automation_rate, y = jobs_displaced, color = company_size)) +
  geom_point(data = plot_data %>% sample_frac(0.10), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.8) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous() +
  scale_color_manual(
    values = c("Enterprise" = "#FFC107", "SME" = "#198754", "Startup" = "#3B628B"),
    name = "Unternehmensgröße"
  ) +
  
  labs(
    x = "Wie viel Prozent der Arbeit automatisiert ist",
    y = "Interner Stellenabbau"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
    
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )


print(raw_jobs)

##this graph is a mess
##Unternehmen jeder Größe verlieren durch die Automatisierung Arbeitsplätze. Der Anteil ist derselbe, die absoluten Zahlen jedoch nicht. 
##########################################################################

### absolute job loss in just germany
#################################################################################################
germany_absolute_2026 <- df %>%
  filter(grepl("germany", country, ignore.case = TRUE))

raw_jobs <- ggplot(germany_absolute_2026, aes(
  x = forcats::fct_relevel(company_size, "Startup", "SME", "Enterprise"), 
  y = jobs_displaced, 
  fill = company_size
)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_fill_manual(
    values = c("Enterprise" = "#FFC107", "SME" = "#198754", "Startup" = "#3B628B"),
    name = "Unternehmensgröße",
    breaks = c("Enterprise", "SME", "Startup") 
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Deutschland: Interner Stellenabbau",
    subtitle = "Anzahl der verdrängten Belegschaft (2023–2026)",
    x = "Unternehmensgröße",
    y = "Absoluter Stellenabbau"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(raw_jobs)





####################################################################
#COMPARE created vs lost jobs
##############################################################################
germany_balance_2026 <- df %>%
  select(
    country = matches("country", ignore.case = TRUE),
    year = matches("survey_year", ignore.case = TRUE), 
    jobs_displaced = matches("jobs_displaced", ignore.case = TRUE),
    jobs_created = matches("jobs_created", ignore.case = TRUE)
  ) %>%
##filter country year
  filter(grepl("germany", country, ignore.case = TRUE)) %>%
  filter(year == 2026) %>% # Isolates only rows from the year 2026
  
  summarize(
    Verdrängt = sum(jobs_displaced, na.rm = TRUE),
    Neu = sum(jobs_created, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Metrik", values_to = "Anzahl")


ggplot(germany_balance_2026, aes(x = reorder(Metrik, Anzahl), y = Anzahl, fill = Metrik)) +
  geom_bar(stat = "identity", alpha = 0.85, width = 0.6) +
  geom_text(
    aes(label = scales::comma(Anzahl)),
    hjust = -0.2,            
    fontface = "bold",       
    size = 4.5,             
    color = "#333333"
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Verdrängt" = "#D9534F", "Neu" = "#198754"), guide = "none") +
  coord_flip(clip = "off") +
   labs(
    x = "Arbeitsmarkteffekt",
    y = "Anzahl der Arbeitsplätze (2026)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10))
  )


#########################################################
###cost of reskilling
############################################################

#filter germny 2026
reskilling_costs_2026 <- df %>%
  select(
    country = matches("country", ignore.case = TRUE),
    year = matches("survey_year", ignore.case = TRUE),
    industry = matches("industry", ignore.case = TRUE),
    jobs_displaced = matches("reskilled_employees", ignore.case = TRUE)
  ) %>%
  filter(grepl("germany", country, ignore.case = TRUE)) %>%
  filter(year == 2026) %>% # ─── TIME FILTER ───
  filter(!is.na(industry)) %>%
  group_by(industry) %>%
  summarize(Total_Displaced = sum(jobs_displaced, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    Total_Cost_Euro = Total_Displaced * 1300,  ##assumption comes from old WEF report
    industry = reorder(industry, Total_Cost_Euro)
  )
ggplot(reskilling_costs_2026, aes(x = industry, y = Total_Cost_Euro)) +
  geom_bar(stat = "identity", fill = databears_pal_9, alpha = 0.85, width = 0.6) +
  geom_text(
    aes(label = scales::dollar(Total_Cost_Euro, prefix = "€ ", big.mark = ",")),
    vjust = -0.5,            
    fontface = "bold",
    size = 2.5,
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "€ ", big.mark = ","), 
    expand = expansion(mult = c(0, 0.18)) 
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    x = "Branche",
    y = "Gesamte finanzielle Belastung (in Euro)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )

#####################################################################
####
#####################################################################
#satifaction/AI stage
# Germany 2026
correlation_2026 <- df %>%
  select(
    country = matches("country", ignore.case = TRUE),
    year = matches("survey_year", ignore.case = TRUE),
    ai_maturity_score = matches("ai_maturity_score", ignore.case = TRUE),
    satisfaction = matches("employee_satisfaction_score", ignore.case = TRUE)
  ) %>%
  filter(grepl("germany", country, ignore.case = TRUE)) %>%
  filter(year == 2026) %>% 
  filter(!is.na(ai_maturity_score), !is.na(satisfaction))


de_corr_2026 <- cor(correlation_2026$ai_maturity_score, correlation_2026$satisfaction)
de_corr_2026


ggplot(correlation_2026, aes(x = ai_maturity_score, y = satisfaction)) +
  geom_point(data = correlation_2026 %>% sample_frac(0.10), color = "#198754", alpha = 0.4, size = 2) +
   geom_smooth(method = "lm", color = "#444444", size = 1.5, se = FALSE) +
  
  labs(
    x = "KI Reifegrad Score",
    y = "Zufriedenheits Score der Belegschaft"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
     axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10)),
    axis.text = element_text(face = "bold", color = "#555555")
  )
#######################################################################################################

df_netto <- df %>%
  filter(country == "Germany") %>%
  group_by(survey_year) %>%
  summarise(
    Net_Difference = sum(jobs_created, na.rm = TRUE) - sum(jobs_displaced, na.rm = TRUE),
    .groups = "drop"
  ) %>%

  mutate(Tendenz = ifelse(Net_Difference >= 0, "Gewinn", "Verlust"))


ggplot(df_netto, aes(x = factor(survey_year), y = Net_Difference, fill = Tendenz)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", size = 0.2) +
  

  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  

  geom_text(
    aes(
      label = label_number(big.mark = ".", decimal.mark = ",")(Net_Difference),
      vjust = ifelse(Net_Difference >= 0, -0.5, 1.3)
    ),
    fontface = "bold", size = 4.5
  ) +

  scale_fill_manual(values = c("Gewinn" = "#198754", "Verlust" = "#D9534F")) +
  

  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  
  theme_minimal(base_size = 14) +

scale_y_continuous(
  labels = label_number(big.mark = ".", decimal.mark = ","),
  expand = expansion(mult = c(0.15, 0.15)) 
) +
  labs(
 #   title = "Netto-Veränderung des deutschen Arbeitsmarktes",
 #   subtitle = "Gesamtsaldo im Zeitverlauf (Neu geschaffene minus verlorene Stellen)",
    x = "Jahr",
    y = "Netto-Arbeitsplatzentwicklung",
    fill = "Arbeitsmarkttendenz"
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() 
  )

##################################################################################################
##lost the original plot with proper colours and labels. this is the reconstruction 

#################################################################################
de_correlation_data <- df %>% 
  select(
    country              = matches("country", ignore.case = TRUE), 
    company_size         = matches("company_size", ignore.case = TRUE), 
    jobs_displaced       = matches("jobs_displaced", ignore.case = TRUE), 
    num_employees        = matches("num_employees", ignore.case = TRUE), 
    task_automation_rate = matches("task_automation_rate", ignore.case = TRUE)
  ) %>% 
  filter(grepl("germany", country, ignore.case = TRUE)) %>% 

  filter(!is.na(jobs_displaced), !is.na(num_employees), !is.na(task_automation_rate)) %>% 

  mutate(prop_jobs_lost = jobs_displaced / num_employees)

print(de_correlation_data)


correlation_table <- de_correlation_data %>% 
  select(prop_jobs_lost, task_automation_rate) %>% 
  cor(use = "complete.obs")

print(correlation_table)




ggplot(de_correlation_data, aes(x = task_automation_rate, y = prop_jobs_lost, color = company_size)) +

  geom_point(alpha = 0.6, size = 2.5) +
  

  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  
  scale_color_manual(
    values = c("Enterprise" = "#FFC107", "SME" = "#198754", "Startup" = "#3B628B"),
    name = "Unternehmensgröße"
  ) +
  

  labs(

    x = "Automatisierung Rate ",
    y = "% Abbau",
    color = "Firma Größe"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

### not "pruned" for points and no percent scale but I was running out of time here. 
## oh yeah, and the three overlapping lines should be one. 





