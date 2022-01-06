library(shiny)
library(dplyr)
library(tidyverse)
library(scales)
library(plotly)
library(RColorBrewer)

cancer_incidence_data = load("Data/Cancer_incidence.RData")
cancer_mortality_data = load("Data/Cancer_mortality.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Cancer Incidence and Mortality Rate"),
    
    sidebarLayout(
        sidebarPanel(
            # Select the Area
            selectInput(inputId = "State",
                        label = "Select the State",
                        choices = c("United States",
                                    "Alabama",
                                    "Arizona",
                                    "Arkansas",
                                    "California",
                                    "Colorado",
                                    "Connecticut",
                                    "Delaware",
                                    "District Of Columbia",
                                    "Florida",
                                    "Georgia",
                                    "Hawaii",
                                    "Idaho",
                                    "Illinois",
                                    "Indiana",
                                    "Iowa",
                                    "Kansas",
                                    "Kentucky",
                                    "Lousiana",
                                    "Maine",
                                    "Maryland",
                                    "Massachusetts",
                                    "Michigan",
                                    "Minnesota",
                                    "Missisippi",
                                    "Missouri",
                                    "Montana",
                                    "Nebraska",
                                    "Nevada",
                                    "New Hampshire",
                                    "New Jersey",
                                    "New Mexico",
                                    "Newyork",
                                    "North Carolina",
                                    "North Dakota",
                                    "Ohio",
                                    "Oklahama",
                                    "Oregon",
                                    "Pennsylvania",
                                    "Rhode Island",
                                    "South Carolina",
                                    "South Dakota",
                                    "Tennessee",
                                    "Texas",
                                    "Utah",
                                    "Vermont",
                                    "Virginia",
                                    "Washington",
                                    "West Virginia",
                                    "Wisconsin",
                                    "Wyoming"),
                        selected = "United States"),
            
            # Select the Cancer Types
            selectInput(inputId = "Cancersites",
                        label = "Select the Cancer Types",
                        choices=c("All types of Cancer",
                                  "Brain and Other Nervous System",
                                  "Cervix Uteri",
                                  "Colon and Rectum",
                                  "Corpus Uteri",
                                  "Esophagus",
                                  "Female Breast",
                                  "Hodgkin Lymphoma",
                                  "Kaposi Sarcoma",
                                  "Kidney and Renal Pelvis",
                                  "Larynx",
                                  "Leukemias",
                                  "Liver and Intrahepatic Bile Duct",
                                  "Lung and Bronchus",
                                  "Melanoma of the Skin",
                                  "Mesothelioma",
                                  "Myeloma",
                                  "Non-Hodgkin Lymphoma",
                                  "Oral Cavity and Pharynx",
                                  "Ovary",
                                  "Pancreas",
                                  "Prostate",
                                  "Stomach",
                                  "Testis",
                                  "Thyroid",
                                  "Urinary System"),
                        selected = "All types of Cancer"),
            
            # Select the Gender
            radioButtons(inputId = "gender",
                         label = "Select the gender",
                         choices = c("Male","Female","Both"),
                         selected = "Both"),
            
            # Set min/max of Year
            selectInput(inputId = "min", 
                        label = "Choose Year Range (Min):", 
                        choices = c(2005, 2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017),
                        selected= 2005),
            
            selectInput(inputId = "max",
                        label = "Choose Year Range (Max):", 
                        choices = c(2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                        selected= 2018)
            
        ),
        
        # Show a plot of the cancer and mortality details
        mainPanel(
            tabsetPanel(
                tabPanel("New Cancer: Total Cases", plotlyOutput(outputId = "scatterplot_1")),
                tabPanel("Cancer Mortality: Total Cases", plotlyOutput(outputId = "scatterplot_3")),
                tabPanel("New Cancer: Demographic", plotlyOutput(outputId = "map_1")),
                tabPanel("Cancer Mortality: Demographic", plotlyOutput(outputId = "map_2")),
                tabPanel("New Cancer Data Summary",  DT::dataTableOutput(outputId="datasheet_1")),
                tabPanel("Cancer Mortality Data Summary",  DT::dataTableOutput(outputId="datasheet_2")),
                tabPanel("New Cancer Data",  DT::dataTableOutput(outputId="datasheet_3")),
                tabPanel("Cancer Mortality Data",  DT::dataTableOutput(outputId="datasheet_4"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    CI_df  = reactive({
        df1=Cancer_incidence_Wstates_df %>%
            filter((Year>=input$min & Year<=input$max),
                   (if(input$State == "United States") {States > " "}else {States == input$State}),
                   (if(input$Cancersites == "All types of Cancer") {Cancer.Sites > " "} else {Cancer.Sites==input$Cancersites}),
                   (if(input$gender=="Both"){Sex > " "}else {Sex == input$gender}))
        if (input$Cancersites == "All types of Cancer"){
            CI_SUM_1 = df1 %>% 
                select(Year,Cancer.Sites,Count) 
            
            CI_SUM_1 = aggregate(CI_SUM_1$Count, by=list(CI_SUM_1$Year,CI_SUM_1$Cancer.Sites),FUN=sum)
            CI_SUM_1 = plyr::rename(CI_SUM_1, c(Group.1="Year", Group.2="CancerSites",x="Total_Cases"))
            
            CI_SUM_1 = CI_SUM_1 %>% 
                arrange(.,desc(Total_Cases)) %>% 
                group_by(Year) %>% 
                slice(1:8)
        }
        else{
            CI_SUM_1 = df1 %>% 
                select(Year,Count)
            CI_SUM_1 = aggregate(CI_SUM_1$Count, by=list(CI_SUM_1$Year),FUN=sum)
            CI_SUM_1 = plyr::rename(CI_SUM_1, c(Group.1="Year", x="Total_Cases"))
        }
        return(CI_SUM_1)
    })
    output$scatterplot_1 <- renderPlotly({
        if (input$Cancersites == "All types of Cancer"){
            
            fig = plot_ly(data=CI_df(),x=~CI_df()$Year,y=~CI_df()$Total_Cases,type='bar',color=~CI_df()$CancerSites,
                          colors = brewer.pal(length(unique(CI_df()$CancerSites)),"Paired")) %>% 
                layout(title=paste0("Top 8 Cancer's new cases from ",input$min, "-", input$max),
                    yaxis=list(title='Count'),barmode='stack')
            fig}
        else{
            fig = plot_ly(data=CI_df(),x=~CI_df()$Year,y=~CI_df()$Total_Cases,type='bar') %>% 
                layout(title = paste0("Total ",input$Cancersites," new cancer cases from",input$min, "-", input$max),
                    yaxis=list(title='Count'))
            fig
        }
        
    })
    
    CI_SUM_2  = reactive({
        df2= Cancer_incidence_Wstates_df %>%
            filter((Year>=input$min & Year<=input$max),
                   (if(input$State == "United States") {States > " "}else {States == input$State}),
                   (if(input$Cancersites == "All types of Cancer") {Cancer.Sites > " "} else {Cancer.Sites==input$Cancersites}),
                   (if(input$gender=="Both"){Sex > " "}else {Sex == input$gender})) %>% 
            select(States,Year,state_abb,Count)
        
        df3= aggregate(df2$Count, by=list(df2$States, df2$Year,df2$state_abb), FUN=sum)
        df3= plyr::rename(df3,c(Group.1="States", Group.2="Year",Group.3="state_abb", x="Total_Cases"))
        return(df3) 
    })
    
    output$map_1 = renderPlotly({
        p = plot_geo(data = CI_SUM_2(),
                     locationmode='USA-states',
                     frame= ~Year) %>% 
            add_trace(locations = ~CI_SUM_2()$state_abb,
                      type='choropleth',
                      z=~Total_Cases,
                      zmin=0,
                      zmax=max(CI_SUM_2()$Total_Cases),
                      color=~Total_Cases,
                      mode = "markers",
                      marker = list(size = 10,
                                    color = colorRampPalette(brewer.pal(10,"YlOrBr"))(41))) %>% 
            layout(geo=list(scope='usa'),
                   title = paste0("Total ",input$Cancersites ," Incidence in the USA"))
        colorbar(p,nticks=7)
    })
    
    CM_df  = reactive({
        df4=Cancer_mortality_Wstates_df %>%
            filter((Year>=input$min & Year<=input$max),
                   (if(input$State == "United States") {States > " "}else {States == input$State}),
                   (if(input$Cancersites == "All types of Cancer") {Cancer.Sites > " "} else {Cancer.Sites==input$Cancersites}),
                   (if(input$gender=="Both"){Sex > " "}else {Sex == input$gender}))
        if (input$Cancersites == "All types of Cancer"){
            CM_SUM_1 = df4 %>% 
                select(Year,Cancer.Sites,Count) 
            
            CM_SUM_1 = aggregate(CM_SUM_1$Count, by=list(CM_SUM_1$Year,CM_SUM_1$Cancer.Sites),FUN=sum)
            CM_SUM_1 = plyr::rename(CM_SUM_1, c(Group.1="Year", Group.2="CancerSites",x="Total_Cases"))
            
            CM_SUM_1 = CM_SUM_1 %>% 
                arrange(.,desc(Total_Cases)) %>% 
                group_by(Year) %>% 
                slice(1:8)
        }
        else{
            CM_SUM_1 = df4 %>% 
                select(Year,Count)
            CM_SUM_1 = aggregate(CM_SUM_1$Count, by=list(CM_SUM_1$Year),FUN=sum)
            CM_SUM_1 = plyr::rename(CM_SUM_1, c(Group.1="Year", x="Total_Cases"))
        }
        return(CM_SUM_1)
        
    })
    
    output$scatterplot_3 <- renderPlotly({
        if (input$Cancersites == "All types of Cancer"){ 
            fig = plot_ly(data=CM_df(),x=~Year,y=~Total_Cases,type='bar',color=~CancerSites,
                          colors = brewer.pal(length(unique(CM_df()$CancerSites)),"Paired")) %>% 
                layout(title=paste0("Top 8 Cancer's Death cases from ",input$min, "-", input$max),
                    yaxis=list(title='Count'),barmode='stack')
            fig}
        else{
            fig = plot_ly(data=CM_df(),x=~Year,y=~Total_Cases,type='bar') %>% 
                layout(title = paste0("Total ",input$Cancersites,"cancer death cases from",input$min, "-", input$max),
                    yaxis=list(title='Count'))
            fig
        }
    })
    
    CM_SUM_2  = reactive({
        df2_CM= Cancer_mortality_Wstates_df %>%
            filter((Year>=input$min & Year<=input$max),
                   (if(input$State == "United States") {States > " "}else {States == input$State}),
                   (if(input$Cancersites == "All types of Cancer") {Cancer.Sites > " "} else {Cancer.Sites==input$Cancersites}),
                   (if(input$gender=="Both"){Sex > " "}else {Sex == input$gender})) %>% 
            select(States,Year,state_abb,Count)
        
        df3_CM= aggregate(df2_CM$Count, by=list(df2_CM$States, df2_CM$Year,df2_CM$state_abb), FUN=sum)
        df3_CM= plyr::rename(df3_CM,c(Group.1="States", Group.2="Year",Group.3="state_abb", x="Total_Cases"))
        return(df3_CM) 
    })
    output$map_2 = renderPlotly({
        p = plot_geo(data = CM_SUM_2(),
                     locationmode='USA-states',
                     frame= ~Year) %>% 
            add_trace(locations = ~CM_SUM_2()$state_abb,
                      type='choropleth',
                      z=~CM_SUM_2()$Total_Cases,
                      zmin=0,
                      zmax=max(CM_SUM_2()$Total_Cases),
                      color=~Total_Cases,
                      mode = "markers",
                      marker = list(size = 10,
                                    color = colorRampPalette(brewer.pal(10,"BrBG"))(41))) %>% 
            layout(geo=list(scope='usa'),
                   title = paste0("Total ",input$Cancersites ," Mortality in the USA"))
        colorbar(p,nticks=7)
    })
    
    output$datasheet_1<-DT::renderDataTable({
        DT::datatable(data=CI_df(),
                      options=list(pageLength= 20),
                      rownames=FALSE)
    })
    
    output$datasheet_2<-DT::renderDataTable({
        DT::datatable(data=CM_df(),
                      options=list(pageLength= 20),
                      rownames=FALSE)
    })
    
    CI_DATA_DF= reactive({
        df6=Cancer_incidence_Wstates_df %>%
            filter((Year>=input$min & Year<=input$max),
                   (if(input$State == "United States") {States > " "}else {States == input$State}),
                   (if(input$Cancersites == "All types of Cancer") {Cancer.Sites > " "} else {Cancer.Sites==input$Cancersites}),
                   (if(input$gender=="Both"){Sex > " "}else {Sex == input$gender}))
        return(df6)
    })
    output$datasheet_3<-DT::renderDataTable({
        DT::datatable(data=CI_DATA_DF()[,1:6],
                      options=list(pageLength= 20),
                      rownames=FALSE)
    })
    
    CM_DATA_DF=reactive({
        df6_CM= Cancer_mortality_Wstates_df %>%
            filter((Year>=input$min & Year<=input$max),
                   (if(input$State == "United States") {States > " "}else {States == input$State}),
                   (if(input$Cancersites == "All types of Cancer") {Cancer.Sites > " "} else {Cancer.Sites==input$Cancersites}),
                   (if(input$gender=="Both"){Sex > " "}else {Sex == input$gender}))
        return(df6_CM)
    })
    output$datasheet_4<-DT::renderDataTable({
        DT::datatable(data=CM_DATA_DF()[,1:6],
                      options=list(pageLength= 20),
                      rownames=FALSE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
