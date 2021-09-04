#City Pane; Tables Plots and Functions


Map_Data <- 
  Company_Table %>% 
  select(Company_Name, Company_City, Company_Size) %>% 
  unique() %>% 
  group_by(Company_City,Company_Size) %>% 
  summarise(n=n_distinct(Company_Name)) %>%
  ungroup() %>% 
  pivot_wider(names_from = "Company_Size", values_from = "n", values_fill = 0) %>% 
  mutate(onclick=paste0("set_search_val(\"",Company_City,"\");")) %>% 
  left_join(Mapdata %>% select(long, lat, Company_City), by="Company_City")  

#Selectable City Map Plot
Map_Plot <- 
  ggplot(data=Map_Data,
            aes(x = long, y = lat,
                fill="#FF0000",
                tooltip = Company_City,
                data_id = Company_City,
                onclick = onclick)) +
  geom_polygon_interactive(size=0.5, colour="black") +
  labs(title= "<span style='font-size:32px;
                            text-decoration:underline;'> **Region IV. Map** </span>")+
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size=20,
                                      hjust=0.5))

#Selectable City Map Plot (interactive part)

#Distribution of Institutions/Enterprises
Ggiraph_Map_Plot <- 
girafe(code=print(Map_Plot),
       options= list(
         opts_hover(css= "fill:#FFDFAD;"),
         opts_selection(type="single", css="fill:#ffcb77;")
       )) 

#Table After Selected City
ggiraph_table <- function(x) {
    Map_Data %>% 
    select(-c(long,lat, onclick)) %>% 
    unique() %>% 
    setNames(c("City", "Large Companies", "SME", "Startup","Univesity","Entrepreneurship")) %>% 
    filter(City==x) %>% 
    gt() %>% 
    tab_header(
      title = gt::html("<span style= 'font-weight: bold;
                                    font-size: 20pt;'> Distribution of Institutions/Enterprises </span>
                      <span style= 'font-weight: bold;
                                    color: #ffcb77;
                                    font-size: 20pt;'> By Types </span>")) %>% 
    cols_align(align="center")
}




#First Time Applications 
Companies_Before_2021 <- Project_Table %>% 
                         filter(year(Application_Date) <=2020) %>% 
                         select(Company_ID) %>% 
                         unique()

Companies_2021 <- Project_Table %>% 
                  filter(year(Application_Date)==2021) %>% 
                  select(Company_ID) %>% 
                  unique() %>% 
                  anti_join(
                      Project_Table %>% 
                          filter(year(Application_Date) <=2020) %>% 
                          select(Company_ID) %>% 
                          unique()
                  )

First_Inyear <- Companies_2021 %>%
      left_join(Project_Table %>% select(Company_ID,Project_Sector, Grant_Programme), by="Company_ID") %>% 
      left_join(Company_Table %>% select(-City_ID, ), by="Company_ID") %>% 
      select(Company_City, Company_Name, Company_Size, Grant_Programme, Project_Sector)


first_inyear <- function(x) {
  First_Inyear %>% 
    filter(Company_City==x) 
}

first_inyear_gt <- function(x) {
  First_Inyear %>% 
    filter(Company_City==x) %>% 
    select(-Company_City) %>% 
    setNames(c("Company Name", "Company Size", "Grant Programme", "Project Sector")) %>% 
      gt() %>% 
      tab_header(
        title = gt::html("<span style= 'font-weight: bold;
                                    font-size: 20pt;'> First Time Applied </span>
                      <span style= 'font-weight: bold;
                                    color: #ffcb77;
                                    font-size: 20pt;'> In 2021 </span>")) %>% 
      cols_align(align="center") %>% 
      tab_style(
        style=(cell_borders(weight=px(2), color="white")),
               locations = cells_body()) %>% 
      tab_style(
        style=(cell_text(weight="bold")),
        locations = cells_column_labels())
}