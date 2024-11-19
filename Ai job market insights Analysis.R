install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)


data <- read.csv("C:/Users/7haze/Desktop/ai_job_market_insights.csv")


head(data)

head(data)        # عرض أول 6 صفوف

str(data)         # عرض الهيكل

summary(data)     # ملخص عن القيم

sum(is.na(data))  # عدد القيم المفقودة

# حذف الصفوف التي تحتوي على قيم مفقودة
data <- na.omit(data)



data$Job_Title <- as.factor(data$Job_Title) # Convert the "Job_Title" column to a factor
data$Industry <- as.factor(data$Industry) # Convert the "Industry" column to a factor
data$Company_Size <- as.factor(data$Company_Size) # Convert the "Company_Size" column to a factor
data$Location <- as.factor(data$Location) # Convert the "Location" column to a factor
data$AI_Adoption_Level <- as.factor(data$AI_Adoption_Level) # Convert the "AI_Adoption_Level" column to a factor
data$Automation_Risk <- as.factor(data$Automation_Risk) # Convert the "Automation_Risk" column to a factor
data$Remote_Friendly <- as.factor(data$Remote_Friendly) # Convert the "Remote_Friendly" to factor
data$Job_Growth_Projection <- as.factor(data$Job_Growth_Projection) # Convert column "Job_Growth_Projection" to factor



boxplot(data$Salary_USD, main="Boxplot of Salary", ylab="Salary (USD)")


mean(data$Salary_USD, na.rm = TRUE)  # حساب المتوسط
sd(data$Salary_USD, na.rm = TRUE)    # حساب الانحراف المعياري






#Density Plot of Salary
ggplot(data, aes(x=Salary_USD)) +
  geom_density(fill="blue", alpha=0.5) +
  labs(title="Density Plot of Salary")




#Table showing industries and average salaries for each.
library(dplyr)
salary_by_industry <- data %>%
  group_by(Industry) %>%
  summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE)) %>%
  arrange(desc(Avg_Salary))




ggplot(salary_by_industry, aes(x = reorder(Industry, Avg_Salary), y = Avg_Salary)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Salary by Industry", x = "Industry", y = "Average Salary (USD)")




#Salary Distribution by AI Adoption Level
ggplot(data, aes(x = AI_Adoption_Level, y = Salary_USD)) +
  geom_boxplot(aes(fill = AI_Adoption_Level), alpha = 0.7) +
  labs(title = "Salary Distribution by AI Adoption Level", y = "Salary (USD)")




#Salary by Company Size and Industry
ggplot(data, aes(x = Company_Size, y = Salary_USD, color = Industry)) +
  geom_jitter(alpha = 0.6) +
  labs(title = "Salary by Company Size and Industry", x = "Company Size", y = "Salary (USD)")





#Average Salary: Remote vs. In-Office
salary_remote <- data %>%
  group_by(Remote_Friendly) %>%
  summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE))

ggplot(salary_remote, aes(x = Remote_Friendly, y = Avg_Salary, fill = Remote_Friendly)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Salary: Remote vs. In-Office", y = "Average Salary (USD)")





#Interactive Salary Plot by Industry and Job Growth
install.packages("plotly")
library(plotly)
p <- ggplot(data, aes(x = Industry, y = Salary_USD, color = Job_Growth_Projection)) +
  geom_point() +
  labs(title = "Interactive Salary Plot by Industry and Job Growth")

ggplotly(p)




ggplot(data, aes(x = AI_Adoption_Level, y = Salary_USD, color = Company_Size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Salary vs. AI Adoption Level")


install.packages("ggmap")
library(ggmap)
# تأكد من أن لديك إحداثيات للمدن
ggmap(get_map(location = "World", zoom = 2)) +
  geom_point(data = data, aes(x = Longitude, y = Latitude, size = Salary_USD, color = Industry), alpha = 0.7)







# تجميع البيانات حسب "Job Title" وحساب متوسط الرواتب
grouped_data <- data %>%
  group_by(Job_Title) %>%
  summarise(
    Avg_Salary = mean(Salary_USD, na.rm = TRUE),
    Count = n()  # عدد الوظائف في كل مجموعة
  )

# عرض النتائج
print(grouped_data)



library(dplyr)

# إضافة عمود جديد "Job_Group" للمجموعات
data <- data %>%
  mutate(Job_Group = as.factor(Job_Title))

# عرض البيانات مع المجموعة الجديدة
head(data)





write.csv(data, "processed_data.csv", row.names = FALSE)
























