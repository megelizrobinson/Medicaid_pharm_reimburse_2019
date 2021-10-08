library(readr)
#import pharmacy drug reimbursement data 2019
drug_reimburse_2019 <- read_csv("State_Drug_Utilization_Data_2019.csv")

library(dplyr)
glimpse(drug_reimburse_2019) #quick look to view data types for correction, if needed

# keeping necessary columns for analysis
drug_reimburse_2019 <- drug_reimburse_2019 %>%  
  select(`Medicaid Amount Reimbursed`,
         `Utilization Type`, State) %>% 
  rename(Medicaid_Amount_Reimbursed = `Medicaid Amount Reimbursed`,
          Payment_model = `Utilization Type`) #renaming columns

#Correcting data type: State
drug_reimburse_2019 <- drug_reimburse_2019 %>% 
  mutate(State = as.factor(State))

#Correcting data type: Payment_model
drug_reimburse_2019 <- drug_reimburse_2019 %>% 
  mutate(Payment_model = as.factor(Payment_model))

drug_reimburse_2019 %>% 
  distinct(Payment_model) #view different payment models 

# modify values within variable: Payment_model
drug_reimburse_2019 <- drug_reimburse_2019 %>% 
  mutate(Payment_model = recode(Payment_model, "FFSU" = "FFS",
                                "MCOU" = "MCP"))
drug_reimburse_2019

#Checking completeness 
sum(is.na(drug_reimburse_2019)) #total missing values in entire data set

library(naniar)
gg_miss_var(drug_reimburse_2019, #visualizing percent of missing values
            show_pct = TRUE)

# Investigating missingness, by State and Payment model
drug_reimburse_2019 %>% 
  arrange(State) %>% 
  vis_miss(warn_large_data = FALSE)

drug_reimburse_2019 %>% 
  arrange(Payment_model) %>% 
  vis_miss(warn_large_data = FALSE)

# Determining if variable containing missing values follows a normal distribution
mean(drug_reimburse_2019$Medicaid_Amount_Reimbursed, na.rm = TRUE)
median(drug_reimburse_2019$Medicaid_Amount_Reimbursed, na.rm = TRUE)
# Mean is larger than median indicating the distribution is skewed to the right
# Because of this, should replace missing values with median rather than mean


#Replacing missing values with median: Medicaid_Amount_Reimbursed 

drug_reimburse_2019 <- drug_reimburse_2019 %>% 
  mutate(Medicaid_Amount_Reimbursed = ifelse(
    is.na(Medicaid_Amount_Reimbursed),
    median(Medicaid_Amount_Reimbursed, na.rm = TRUE),
    Medicaid_Amount_Reimbursed))


#Verify missing values replaced within column Medicaid_Amount_Reimbursed

sum(is.na(drug_reimburse_2019$Medicaid_Amount_Reimbursed)) 

#view how many distinct States
drug_reimburse_2019 %>% 
  distinct(State) %>%
  arrange(State) %>% 
  print(n = Inf)

#Identify States represented in data and any error needing to be removed 
#52 States displayed: all 50 States and includes nation's capital: DC
#Remove Unknown State/territory: "XX"
drug_reimburse_2019 <- drug_reimburse_2019 %>% 
  filter(State != "XX")

#count number of observations for each Payment model
drug_reimburse_2019 %>% 
  count(Payment_model)

#comparing FFS & MCP pharmacy reimbursement totals
compare_model_totals <- drug_reimburse_2019 %>% 
  group_by(Payment_model) %>% 
  summarise(total_phar_reimburse_USD = sum(Medicaid_Amount_Reimbursed)) 

#Create bar plot of FFS & MCP pharmacy reimbursement totals
library(scales)
bar_model_totals <- ggplot(compare_model_totals, aes(x = Payment_model, y = total_phar_reimburse_USD,
  fill = Payment_model)) +
  geom_col(width = 0.5) +
  labs (
    title = "Amount reimbursed to pharmacies from Medicaid by payment model 2019",
    x = "Payment model",
    y = "Amount reimbursed to pharmacies (billions)"
  ) +
  geom_text(aes(label = comma(round(total_phar_reimburse_USD, 0)), vjust = 1.5)) +
  scale_y_continuous(limits = c(0,50000000000),
                  breaks = c(0,10000000000, 20000000000, 30000000000,
                             40000000000, 50000000000),
                     labels = c("0","10B", "20B","30B", "40B","50B"),
                     expand = c(0,0)
  ) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")
  )

# Save bar plot
ggsave("bar_model_totals_medianNA.png", plot = bar_model_totals)

#importing US_regions file
US_regions <- read_csv("US_regions.csv")
glimpse(US_regions)

#Correcting data type 
US_regions <- US_regions %>% 
  mutate(Region = as.factor(Region))

US_regions <- US_regions %>% 
  mutate(State = as.factor(State))

#Join tables US_regions & drug_reimburse_2019
Medicaid_data_2019 <- drug_reimburse_2019 %>% 
  full_join(US_regions, by = "State")# includes new Region column


#Table to use for reimbursement facet plot: State, Payment model, 
#Region, Phar reimburse
region_drug_reimburse_2019 <- Medicaid_data_2019 %>% 
  group_by(State, Payment_model, Region) %>% 
  summarise(total_phar_reimburse = sum(Medicaid_Amount_Reimbursed)) %>% 
  ungroup() %>% 
  print(n = Inf)

# Reimbursement Facet plot

library(ggplot2)
library(tidyverse) #needed for fct_relevel()
library(scales) # needed for unit_format

facet_plot_reimburse <- region_drug_reimburse_2019 %>% 
  mutate(Region = fct_relevel(Region, "West", "Northeast", "Midwest", "Southeast", "Southwest"),
    State = fct_reorder(State, total_phar_reimburse)) %>% 
  ggplot(aes(x = State, y = total_phar_reimburse, fill = Payment_model)) +
  geom_col(width = 0.85) +
  scale_fill_brewer(palette = "Accent") +
  coord_flip()+
  facet_grid (rows = vars(Region), scales = "free", switch = "y",
              space = "free_x") +
  labs (
    title = "Amount reimbursed to pharmacies from Medicaid by payment model,\nstate, and region, 2019",
    caption = "Source: Centers for Medicare and Medicaid\nData.Medicaid.gov",
    y = "Annual amount reimbursed to pharmacies (billions)",
    fill = "Payment Model"
    ) +
 theme(plot.margin = margin(0.5, 0.5, 0.5, unit = "cm"),
       plot.title = element_text(size = 22, face = "bold"),
       plot.caption = element_text(margin = margin(0,0,0,0), size = 14),
       strip.text.y = element_text(angle = 270, face = "bold", size = 16),
       strip.placement = "outside",
       axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm"), 
                                   size = 18),
       axis.title.y = element_blank(),
       axis.text.x = element_text(size = 16.5),
       axis.text.y = element_text(size = 14),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
       legend.position= c(0.5,0.5),
       legend.background = element_rect(size = 0.5, linetype="solid", 
                                        color ="black"),
       legend.text = element_text(size = 20),
       legend.title = element_text(size = 20),
       axis.line = element_line(color = "grey50")) +
       scale_y_continuous(limits = c(0,8000000000),
                         breaks = c(0, 2000000000, 4000000000, 6000000000,
                                    8000000000),
                         labels = c("0","2B", "4B", "6B", "8B"),
                         expand = c(0,0)) 
facet_plot_reimburse
# For best visualization, see ggsave plot
ggsave("facet_plot_reimburse_medianNA.png", plot = facet_plot_reimburse, 
       width = 17, height = 12, units = "in")


                      



                      
