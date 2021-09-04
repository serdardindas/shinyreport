#Sector Pane; Tables Plots and Functions

Project_Application_Sector <- 
  Project_Table %>% 
  group_by(Project_Sector) %>% 
  summarise(Application=n_distinct(Project_No)) 

Project_Approved_Sector <- 
  Project_Table %>% 
  filter(Project_Status=="Completed" | Project_Status=="Ongoing") %>% 
  group_by(Project_Sector) %>% 
  summarise(Acceptance=n_distinct(Project_No))

Project_Evaluating_Sector <- 
  Project_Table %>% 
  filter(Project_Status=="Evaluating") %>% 
  group_by(Project_Sector) %>% 
  summarise(Evaluating=n_distinct(Project_No))

Grant_Sector <- 
  Grant_Table %>% 
  left_join(Project_Table %>% 
              select(Project_No,Project_Sector), 
                     by="Project_No") %>% 
  group_by(Project_Sector) %>% 
  summarise(Total_Grant=sum(Grant_Amount))

Table_Data <- 
  list(Project_Application_Sector, 
       Project_Approved_Sector, 
       Project_Evaluating_Sector,
       Grant_Sector) %>% 
  reduce(left_join, 
         by="Project_Sector")

# Metrics By All Sector Table
Sector_Table_gt <- 
  Table_Data %>% 
  setNames(c("Sector", "Application", "Approved","Evaluating", "Total Grant")) %>% 
  gt() %>% 
  tab_header(
    title = gt::html("<span style= 'font-weight: bold;
                                    font-size: 24pt;'> Metrics </span>
                      <span style= 'font-weight: bold;
                                    color: #ffcb77;
                                    font-size: 24pt;'> By All Sectors </span>")) %>% 
  fmt_number(
    columns = 5,
    decimals = 2,
    suffixing = TRUE) %>% 
  tab_options(table.border.top.style = "hidden") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(TRUE)
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )),
    locations = list(
      cells_column_labels(
        columns = gt::everything()))) %>% 
  cols_align(align="center",
             columns=c("Application", "Approved","Evaluating", "Total Grant"))

#Sector Pane

#Sector Choice List
Choices_List = c(Project_Table %>% pull(Project_Sector) %>% unique())

# Function for Sector Info 
sector_info_function <- function(x) {

Application_Company <- 
  Project_Table %>% 
  filter(Project_Sector==x) %>% 
  pull(Company_ID) %>% n_distinct()

Approved_Company <- 
  Project_Table %>% 
  filter(Project_Sector==x) %>% 
  filter(Project_Status=="Completed" | Project_Status=="Ongoing") %>% 
  pull(Company_ID) %>% n_distinct()

Application_Project <- 
  Table_Data %>% 
  filter(Project_Sector==x) %>% 
  pull(Application)

Approved_Project <- 
  Table_Data %>% 
  filter(Project_Sector==x) %>% 
  pull(Acceptance)

Sector_Grant <- 
  Table_Data %>% 
  filter(Project_Sector==x) %>% 
  pull(Total_Grant)

Sector_Name <- as.character(x)  

Sector_Text <- paste0(Application_Project," project applications were made by ",
       Application_Company," companies from the ", Sector_Name,
       " field. Among the projects, it was decided to support ", Approved_Project,
       " projects belonging to ", Approved_Company," companies. To date, ",
       round(Sector_Grant/1000000,2), "M grants have been given to the projects supported.")

return(Sector_Text)
}



#Function for Yearly Cumulative Application And Approved Counts

sector_plot1_function <- function(x) {

Yearly_Application_Sector <- 
  Project_Table %>% 
  filter(Project_Sector==x) %>% 
  mutate(Application_Year=year(Application_Date)) %>% 
  group_by(Application_Year) %>% 
  summarise(Project_Application=n_distinct(Project_No)) %>% 
  mutate(Cumulative_Application=cumsum(Project_Application))

Yearly_Approved_Sector <- 
  Project_Table %>%
  filter(Project_Sector==x) %>%
  filter(Project_Status=="Completed" | Project_Status=="Ongoing") %>% 
  mutate(Evaluation_Year=year(Evaluation_Date)) %>% 
  group_by(Evaluation_Year) %>% 
  summarise(Project_Evaluation=n_distinct(Project_No)) %>% 
  mutate(Cumulative_Evaluation=cumsum(Project_Evaluation))

Yearly_Application_Approved_Sector <- 
  left_join(Yearly_Application_Sector, Yearly_Approved_Sector, 
            by=c("Application_Year"="Evaluation_Year")) %>% 
  rename("Year"="Application_Year")

Yearly_Application_Approved_Sector_Plot <- 
  ggplot() +
  geom_line(data=Yearly_Application_Approved_Sector, 
            aes(y=Cumulative_Application, x=Year), 
            color="#ffcb77", size=1.5) +
  geom_point(data=Yearly_Application_Approved_Sector, 
             aes(y=Cumulative_Application, x=Year), 
             color="#ffcb77", size=3, shape=21, fill="#ffcb77", stroke=2)+
  geom_line(data=Yearly_Application_Approved_Sector, 
            aes(y=Cumulative_Evaluation, x=Year), 
            color="#FFDFAD", size=1.5) +
  geom_point(data=Yearly_Application_Approved_Sector, 
             aes(y=Cumulative_Evaluation, x=Year), 
             color="#ffcb77", size=3, shape=21, fill="#FFDFAD", stroke=2)+
  expand_limits(y=c(0,700), x=c(2010, 2021)) +
  scale_x_continuous(breaks = c(2010:2021))+
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700)) +
  labs(
    title = "<span style= 'font-size:20pt;
                           color: black;
                           text-align: center;'> 
               **CUMULATIVE PROJECTS**
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

return(Yearly_Application_Approved_Sector_Plot)

}



#Function for Yearly Grant Bar Plot Data
sector_plot2_function <- function(x) {

Sector_Yearly_Grant <- 
  Grant_Table %>% 
  left_join(Project_Table %>% select(Project_No, Project_Sector), 
            by="Project_No") %>% 
  filter(Project_Sector==x) %>% 
  mutate(Grant_Year=year(Grant_Date)) %>% 
  group_by(Grant_Year) %>% 
  summarise(Total_Yearly_Grant=round(sum(Grant_Amount)/1000000,1))

Sector_Yearly_Grant_Plot <- 
  ggplot(Sector_Yearly_Grant, 
         aes(x=Grant_Year, y=Total_Yearly_Grant))+
  geom_col(fill="#ffcb77", colour="black",width=0.5) +
  geom_text(aes(label=paste0(Total_Yearly_Grant," M")), 
            vjust=-0.5, size=5)+
  
  expand_limits(y=c(0,60), x=c(2010, 2021)) +
  scale_x_continuous(breaks = c(2010:2021))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60), 
                     labels = label_number(suffix = " M")) +
  labs(
    title = "<span style= 'font-size:20pt;
                             color: black;
                            text-align: center;'> 
               **YEARLY TOTAL GRANT AMOUNT**
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
return(Sector_Yearly_Grant_Plot)
}



### Top 5 Companies Table by Sector

Grant_Table_Sectors <- 
  Grant_Table %>% 
  left_join(Project_Table %>% 
            select(Project_No,
                   Company_ID,
                   Project_Sector), 
            by="Project_No") %>% 
  left_join(Company_Table %>%  
            select(Company_ID,
                   Company_Name,
                   Company_Size),
            by="Company_ID") %>% 
   group_by(Company_Name, Company_Size, Project_Sector) %>% 
   summarise(Project_Granted = n_distinct(Project_No),
             Total_Grant=sum(Grant_Amount)) %>% 
   arrange(desc(Total_Grant)) %>% 
   ungroup()
 
#Table               
top5_company_sector_function <- function(x) {
  Grant_Table_Sectors %>% 
    filter(Project_Sector==x) %>% 
    mutate(Percantage=(Total_Grant/sum(Total_Grant))) %>% 
    head(5) %>% 
    select(-Percantage) %>% 
    setNames(c("Company","Size","Sector","Project","Total Grant")) %>% 
    gt() %>% 
    tab_header(
      title = gt::html("<span style= 'font-weight: bold;
                                    font-size: 24pt;'> Top 5 Company</span>
                      <span style= 'font-weight: bold;
                                    color: #ffcb77;
                                    font-size: 24pt;'> By The Sector </span>"),
      subtitle = gt::html("<span style= 'font-weight: bold;
                                    font-size: 12pt;'> (Grant-Based)</span>")) %>% 
  fmt_number(
    columns = 5,
    decimals = 2,
    suffixing = TRUE
  ) %>% 
  tab_options(
    table.border.top.style = "hidden")
}


#Pie Chart
sector_piechart <- function(x) {  
  Grant_Table_Sectors %>% 
  filter(Project_Sector==x) %>% 
  mutate(Percantage=(Total_Grant/sum(Total_Grant)),
         Order= row_number(),
         Category=ifelse(Order<=5,"Top 5 Company","Others")) %>% 
  group_by(Category) %>% summarise(Pie_Percantage= sum(Percantage)) %>% 
  ungroup() %>% 
  #ggplot part
  ggplot(aes(x="", y=Pie_Percantage, fill=Category)) +
  geom_bar(stat="identity", width=0.5, color="white") +
  coord_polar("y", start=0) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values = c("#f6f6f6","#ffcb77"))+
  labs(fill="Grant Share")+
  theme_void()
}