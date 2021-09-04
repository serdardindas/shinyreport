#Home Pane; Tables Plots and Functions


Yearly_Applicaton <- 
  Project_Table %>% 
  mutate(Application_Year=year(Application_Date)) %>% 
  group_by(Application_Year) %>% 
  summarise(Project_Application=n_distinct(Project_No)) %>% 
  mutate(Cumulative_Application=cumsum(Project_Application))

Yearly_Approved <- 
  Project_Table %>% 
  filter(Project_Status=="Completed" | Project_Status=="Ongoing") %>% 
  mutate(Evaluation_Year=year(Evaluation_Date)) %>% 
  group_by(Evaluation_Year) %>% 
  summarise(Project_Approved=n_distinct(Project_No)) %>% 
  mutate(Cumulative_Approved=cumsum(Project_Approved))

Yearly_Application_Approved <- 
  left_join(Yearly_Applicaton, Yearly_Approved, 
            by=c("Application_Year"="Evaluation_Year")) %>% 
  rename("Year"="Application_Year")

#Values for ValueBoxes Top of Homepage
Total_Applications <- sum(Yearly_Application_Approved$Project_Application) %>% number(big.mark = ".")

Total_Approved <- sum(Yearly_Application_Approved$Project_Approved) %>% number(big.mark = ".")

Total_Evaluation_Ongoing <- 
  Project_Table %>% 
  filter(Project_Status=="Evaluating") %>% 
  nrow() %>% 
  number(big.mark = ".")

Total_Rejected <- 
  Project_Table %>% 
  filter(Project_Status=="Rejected") %>% 
  nrow()  %>% 
  number(big.mark = ".")


# Model Data for Future Estimates
Model_Data <- 
  Yearly_Application_Approved %>% 
  select(Year,Project_Application, Project_Approved) %>% 
  filter(Year != 2021)

#Linear Model Coefficients for Project Applications
Lin_Model_Application <- lm(Project_Application~Year, data=Model_Data)
Summary_Lin_Model_Application <- summary(Lin_Model_Application)
Model_Intercept_App <- Summary_Lin_Model_Application$coefficients[1]
Model_Slope_App <- Summary_Lin_Model_Application$coefficients[2]

#Linear Model Coefficients for Approved Projects
Lin_Model_Approved <- lm(Project_Approved~Year, data=Model_Data)
Summary_Lin_Model_Approved <- summary(Lin_Model_Approved)
Model_Intercept_Acc <- Summary_Lin_Model_Approved$coefficients[1]
Model_Slope_Acc <- Summary_Lin_Model_Approved$coefficients[2]

#Estimations for both Application and Approved between 2021-2024
Estimated_Years <- 
  data.frame(Year=c(2021,2022,2023,2024)) %>% 
  mutate(Project_Application=round(Year*Model_Slope_App+Model_Intercept_App,0),
         Project_Approved=round(Year*Model_Slope_Acc+Model_Intercept_Acc,0),
         Cumulative_Application=cumsum(Project_Application),
         Cumulative_Approved=cumsum(Project_Approved))

#Yearly Data (2010-2021) and Estimated Years (2021-2024)
Yearly_Data_And_Estimations <- 
  bind_rows(Model_Data,Estimated_Years) %>% 
  mutate(Cumulative_Application=cumsum(Project_Application),
         Cumulative_Approved=cumsum(Project_Approved)) %>% 
  filter(Year >= 2021)


#Yearly Cumulative Application And Approved Counts
Plot_Yearly_Application_Approved <- 
ggplot() +
  geom_line(data=Yearly_Application_Approved, aes(y=Cumulative_Application, x=Year), 
            color="#ffcb77", size=1.5) +
  geom_point(data=Yearly_Application_Approved, aes(y=Cumulative_Application, x=Year), 
             color="#ffcb77", size=3, shape=21, fill="#ffcb77", stroke=2)+
  geom_line(data=Yearly_Application_Approved ,aes(y=Cumulative_Approved, x=Year), 
            color="#FFDFAD", size=1.5) +
  geom_point(data=Yearly_Application_Approved ,aes(y=Cumulative_Approved, x=Year), 
             color="#ffcb77", size=3, shape=21, fill="#FFDFAD", stroke=2) +
  geom_line(data=Yearly_Data_And_Estimations, aes(y=Cumulative_Application, x=Year), 
            color="#ffcb77", size=1.5, linetype="dashed")+
  geom_point(data=Yearly_Data_And_Estimations, aes(y=Cumulative_Application, x=Year),
             color="#ffcb77", size=3, shape=21, fill="white", stroke=2) +
  geom_line(data=Yearly_Data_And_Estimations ,aes(y=Cumulative_Approved, x=Year), 
            color="#FFDFAD", size=1.5, linetype="dashed") +
  geom_point(data=Yearly_Data_And_Estimations ,aes(y=Cumulative_Approved, x=Year), 
             color="#ffcb77", size=3, shape=21, fill="white", stroke=2) +
  expand_limits(y=c(0,6000), x=c(2010, 2024)) +
  scale_x_continuous(breaks = c(2010:2024))+
  scale_y_continuous(breaks = c(0,1500, 3000,4500, 6000,7500)) +
  labs(
    title = "<span style= 'font-size:20pt;
                           color: black;
                           text-align: center;'> 
               **Cumulative Projects**
               </span><br>
             <span style= 'font-size:16pt;
                           color: #ffcb77;
                           text-align: center;'> 
               **Project Applications**</span>
             <span style= 'font-size:16pt;
                           color: black;
                           text-align: center;'> 
               **/**</span>
             <span style= 'font-size:16pt;
                           color: #FFDFAD;
                           text-align: center;'> 
               **Project Approved**</span>"
  )+
  theme(
    plot.background = element_rect(fill = NA, color =NA),
    panel.background = element_rect(fill =NA, color =NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "black", size=0.05),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(face="bold", size = 10),
    plot.title = element_markdown(hjust=0.5),
  )


#Yearly Grant Bar Plot Data

Yearly_Grant <- 
  Grant_Table %>% 
  mutate(Grant_Year=year(Grant_Date)) %>% 
  group_by(Grant_Year) %>% 
  summarise(Total_Yearly_Grant=round(sum(Grant_Amount)/1000000,1))


#Yearly Grant Bar Plot
Plot_Yearly_Grant <- 
  ggplot(Yearly_Grant, 
         aes(x=Grant_Year, y=Total_Yearly_Grant))+
  geom_col(fill="#ffcb77", colour="black",width=0.5) +
  geom_text(aes(label=paste0(Total_Yearly_Grant," M")), 
            vjust=-0.5, size=5)+
  expand_limits(y=c(0,500), x=c(2010, 2021)) +
  scale_x_continuous(breaks = c(2010:2021))+
  scale_y_continuous(breaks = c(0,100,200,300,400,500), 
                     labels = label_number(suffix = " M")) +
  labs(
    title = "<span style= 'font-size:20pt;
                             color: black;
                            text-align: center;'> 
               **Yearly Total Grant Amount**
               </span> "
  )+
  theme(
    plot.background = element_rect(fill = NA, color =NA),
    panel.background = element_rect(fill =NA, color =NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "black", size=0.05),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(face="bold", size = 10),
    plot.title = element_markdown(hjust=0.5),
  )