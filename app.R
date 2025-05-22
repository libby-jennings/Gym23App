
### GYM23 ###

library(shiny)
library(tidyr)
library(tidyverse)
library(googlesheets4)
library(openxlsx)
library(emoGG)
library(gridExtra)
library(plotly)
library(ggridges)



gs4_deauth()
# import and process data
Libby <- read_sheet("https://docs.google.com/spreadsheets/d/1-WZD8w1-b5rN0yrAzVvRgA_b3GQUHxBaJrDOx7_wUfE/edit#gid=1302068595", sheet = 3, .name_repair = "universal")
JB <- read_sheet("https://docs.google.com/spreadsheets/d/1-WZD8w1-b5rN0yrAzVvRgA_b3GQUHxBaJrDOx7_wUfE/edit#gid=1302068595", sheet = 4, .name_repair = "universal")

Libby$Name <- "Libby"
JB$Name <- "JB"


everyone <- rbind(Libby, JB)

#tidy dataframe
data <- pivot_longer(everyone, cols = c(2:(ncol(everyone)-1)), names_to = "Lift", values_to = "value")
data <- data[complete.cases(data),]

data2 <- cbind(data[seq(1, nrow(data), 2),], data[seq(2, nrow(data), 2), "value"])
names(data2) <- c("Date", "Name", "Lift", "kg", "reps")
data2$Lift <- gsub(".weight", "", data2$Lift)
data2$Lift <- factor(data2$Lift, levels = c("Back.squat", "Front.Squats", "Bench", "Deadlift", "Overhead.Press"))

# for competitive plots
errybody <- filter(data2, year(Date) == year(Sys.Date()) & week(Date) >= week(Sys.Date())-52)
errybody <- filter(errybody, Lift %in% c("Bench", "Back.squat", "Deadlift")) 
errybody_agg <- aggregate(errybody$kg, by = list(errybody$Lift, errybody$Name), FUN = max)
names(errybody_agg) <- c("Lift", "Name", "kg")

# open powerlifting data
opl_data <- read.csv("./openpowerlifting-2025-03_data.csv")
opl_long <- pivot_longer(opl_data, cols = 6:ncol(opl_data), names_to = "Lift", values_to = "kg")


# weightlifting standards
standards <- read.csv("./standards.csv")
legend_df <- data.frame("class" = factor(names(standards)[4:8], levels = c("Beginner", "Novice", "Intermediate", "Advanced", "Elite")), "arb" = 1:5)

#define colour scheme
quins_pal <- colorRampPalette(c( "#Fabb11", "#77103c","#252c66"))
people_pal <- c("Becky" = "olivedrab3",
                "Geary" = "deeppink",
                "Libby" = "orange", 
                "Rachel" = "grey50", 
                "Alex" = "#00aaff",
                "Ashleigh" = "#f0f3bd",
                "Fae" = "#745296",
                "JB" = "#2a9d8f")
emoji_pal <- c("Becky" = "1f438", "Ashleigh" = "1f331",
               "Geary" = "1f991", "Libby" = "1f990",
               "Rachel" = "1f42f", "Alex" = "1f42c", "Fae" = "1f98a", "JB" = "1f950")


### write app
ui <- fluidPage(

    # Application title
    titlePanel("GYM23"),
    
tabsetPanel(
    tabPanel("Progress",
        sidebarLayout(
            sidebarPanel(
                #h4("SORRY - this is a bit broken at the moment, it's not my fault so I can't fix it, and I'm pretty sure noone is using this anyway. Other tabs seems to be working ok though :) "),
                selectInput("daterange", "Show me", c("Last 6 weeks", "Last 3 months", "Last 6 months", "1 Year", "2 Years"), selected = "Last 3 months"),
                #sliderInput("months", "Show me months", min = 1, max = 12, value = 1)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("mainPlot")
            )
        )
    ),
    
    tabPanel("Libby",
             mainPanel(
               plotlyOutput("tempPlot")

             )
             
             ),
    
    tabPanel("I'm competitive",
             sidebarLayout(
                 sidebarPanel(
                     "Side-by-side last 3 months"
                 ),
            mainPanel(
                #plotOutput("podiums"),
                plotOutput("compPlotSquat"),
                plotOutput("compPlotBench"),
                plotOutput("compPlotDead"),
                plotOutput("compPlotFrSquat"),
                plotOutput("compPlotOHP"),
                plotOutput("compPlotGB")
            )
             )
        
    ),
    
    tabPanel("I love pie",
             sidebarLayout(
                 sidebarPanel(
                     "Total of best lifts from last year",
                     selectInput("name", "Show me", c("Alex", "Ashleigh", "Becky", "Geary", "Libby", "Rachel", "JB"), selected = "Libby"),
                     checkboxInput("lift.selection", label = "All lifts")
                 ),
                 
                 mainPanel(
                     plotOutput("PiePlot"),
                     plotlyOutput("TotalPlot")
                 )
             )
             
    ) ,
    
    tabPanel("Standards & meets data",
             sidebarLayout(
               sidebarPanel(
                 "Enter your data below, see how you compare to powerlifting standards and lifters of similar weight (+/- 2kg) in unequipped powerlifting meets across the world",
                 # input body weight, lifts
                 radioButtons(inputId = "inputSex", label = NULL, choices = c("Male", "Female"), selected = "Female"),  
                 numericInput(inputId = "bw", label ="Bodyweight", value = 60),
                   numericInput(inputId = "inputSquat", label = "Squat", value = 10),
                   numericInput(inputId = "inputBench", label = "Bench", value = 10),
                   numericInput(inputId = "inputDeadlift", label = "Deadlift", value = 10),
                 "This page uses data from the OpenPowerlifting project, https://www.openpowerlifting.org",
               ),
               
               mainPanel(
                 "Standards",
                plotOutput("StandardsPlot"),
                "\n Open powerlifting meets data",
                plotOutput("OplPlot")
               )
             )
             
    )
    

)
    # Sidebar with a slider input for number of bins 

)

server <- function(input, output) {

  output$tempPlot <- renderPlotly({

    libs <- filter(data2, Name == "Libby")
    libs <- filter(libs, Lift != "<NA>")
    
    lib_plot <- ggplot(libs, aes(x = Date, y = kg)) +
      geom_line(aes(group = Lift), alpha = 0.6) +
      geom_point(aes(col = Lift, size = reps)) +
      theme_light() +
      theme(panel.grid.major.x = element_blank()) +
      scale_color_manual(values = quins_pal(6))
    
    ggplotly(lib_plot) %>% 
                plotly::layout(legend=list(x=0, 
                               xanchor='left',
                               yanchor='bottom',
                               orientation='h')) 
  })  
  
  output$mainPlot <- renderPlot({
        
        if(input$daterange == "Last 6 weeks") {
            xlimit <- as.Date(Sys.Date() -42)
        } else if(input$daterange == "Last 3 months") {
            xlimit <- as.Date(Sys.Date() -92)
        } else if(input$daterange == "Last 6 months") {
            xlimit <- as.Date(Sys.Date() -182)
        } else if(input$daterange == "1 Year"){
            xlimit <- as.Date(Sys.Date() -365)
        } else {
          xlimit <- as.Date(Sys.Date() -730)
        }
             
        
        xlimit <- as.POSIXct(xlimit)
        
        mainplot_data <- filter(data2, Lift != "Front.Squats")

        
        mainplot <- ggplot(mainplot_data, aes(x = Date, y = kg)) +
            geom_line(aes(group = Lift, color = Lift), alpha = 0.5, lwd = 1) +
            geom_point(aes(size = reps, color = Lift), alpha = 0.8) +
            geom_emoji(data = filter(mainplot_data, Lift == "Bench"), emoji = "1f4aa", cex = 0.04) +
            geom_emoji(data = filter(mainplot_data, Lift == "Back.squat"), emoji = "1f351", cex = 0.04) +
            geom_emoji(data = filter(mainplot_data, Lift == "Deadlift"), emoji = "1f525", cex = 0.04) +
            geom_emoji(data = filter(mainplot_data, Lift == "Overhead.Press"), emoji = "1f64c", cex = 0.04) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey80"),
                  legend.position = "top",
                  strip.background =element_rect(fill="#77103c", color = "white"),
                  strip.text = element_text(color = "#f8edeb", size = 10, face = "bold")#,
                  #panel.background = element_rect(fill = "#f8edeb", color = "white")
                  )+
            scale_color_manual(values = quins_pal(length(unique(data2$Lift)))) +
            scale_size_continuous(range = c(1,11)) +
            scale_y_continuous(position = "right") +
            scale_x_datetime(position = "top", limits = c(xlimit, as.POSIXct(Sys.Date()))) +
            #scale_x_datetime(position = "top", limits = c(as.POSIXct(Sys.Date() - (input$months * 30.5)), as.POSIXct(Sys.Date()))) +
            facet_grid(rows = vars(Name), switch = "both", scales = "free_y")
        
        print(mainplot)
    }, width = 400, height = 700) # height 100px + 300px per person
    
    
    
    output$compPlotSquat <- renderPlot({
        
        squat_data <- filter(data2, Lift == "Back.squat")
        
        print(ggplot(squat_data, aes(x = Date, y = kg)) +
            geom_line(aes(group = Name, color = Name), alpha = 0.5, lwd = 1) +
            geom_point(aes(color = Name), size = 5, alpha = 0.5) +
            geom_emoji(data = filter(squat_data, Name == "Libby"), emoji = "1f990", cex = 0.03) +
            geom_emoji(data = filter(squat_data, Name == "Becky"), emoji = "1f438", cex = 0.03) +
            geom_emoji(data = filter(squat_data, Name == "Geary"), emoji = "1f991", cex = 0.03) +
            geom_emoji(data = filter(squat_data, Name == "Rachel"), emoji = "1f42f", cex = 0.03) +
            geom_emoji(data = filter(squat_data, Name == "Alex"), emoji = "1f42c", cex = 0.03) +
            geom_emoji(data = filter(squat_data, Name == "Ashleigh"), emoji = "1f331", cex = 0.03) +
            geom_emoji(data = filter(squat_data, Name == "JB"), emoji = "1f950", cex = 0.03) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white"),
                  legend.position = "top",
                  legend.key.size = unit(0.1, 'cm'),
                  strip.background =element_rect(fill="#252c66", color = "white"),
                  strip.text = element_text(color = "white"),
                  panel.background = element_rect(fill = "#eaedf8", color = "white"))+
            scale_color_manual(values = people_pal) +
            scale_x_datetime(position = "top", limits = c( as.POSIXct(Sys.Date() -42), as.POSIXct(Sys.Date()))) +
            ylab("Back Squat (kg)")+
            ggtitle(""))
        
        
        
        
    })
    
    output$compPlotFrSquat <- renderPlot({
        
        Frsquat_data <- filter(data2, Lift == "Front.Squats")
        
        ggplot(Frsquat_data, aes(x = Date, y = kg)) +
            geom_line(aes(group = Name, color = Name), alpha = 0.5, lwd = 1) +
            geom_point(aes(color = Name), size = 5, alpha = 0.5) +
            geom_emoji(data = filter(Frsquat_data, Name == "Libby"), emoji = "1f990", cex = 0.03) +
            geom_emoji(data = filter(Frsquat_data, Name == "Becky"), emoji = "1f438", cex = 0.03) +
            geom_emoji(data = filter(Frsquat_data, Name == "Geary"), emoji = "1f991", cex = 0.03) +
            geom_emoji(data = filter(Frsquat_data, Name == "Rachel"), emoji = "1f42f", cex = 0.03) +
            geom_emoji(data = filter(Frsquat_data, Name == "Alex"), emoji = "1f42c", cex = 0.03) +
            geom_emoji(data = filter(Frsquat_data, Name == "Ashleigh"), emoji = "1f331", cex = 0.03) +
            geom_emoji(data = filter(Frsquat_data,  Name == "JB"), emoji = "1f950", cex = 0.03) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white"),
                  legend.position = "top",
                  legend.key.size = unit(0.1, 'cm'),
                  strip.background =element_rect(fill="#252c66", color = "white"),
                  strip.text = element_text(color = "white"),
                  panel.background = element_rect(fill = "#eaedf8", color = "white"))+
            scale_color_manual(values = people_pal) +
            scale_x_datetime(position = "top", limits = c( as.POSIXct(Sys.Date() -42), as.POSIXct(Sys.Date()))) +
            ylab("Front Squat (kg)")+
            ggtitle("")
        
        
        
        
    })
    
    output$compPlotBench <- renderPlot({
        
        bench_data <- filter(data2, Lift == "Bench")
        
        ggplot(bench_data, aes(x = Date, y = kg)) +
            geom_line(aes(group = Name, color = Name), alpha = 0.5, lwd = 1) +
            geom_point(aes(color = Name), size = 5, alpha = 0.5) +
            geom_emoji(data = filter(bench_data, Name == "Libby"), emoji = "1f990", cex = 0.03) +
            geom_emoji(data = filter(bench_data, Name == "Becky"), emoji = "1f438", cex = 0.03) +
            geom_emoji(data = filter(bench_data, Name == "Geary"), emoji = "1f991", cex = 0.03) +
            geom_emoji(data = filter(bench_data, Name == "Rachel"), emoji = "1f42f", cex = 0.03) +
            geom_emoji(data = filter(bench_data, Name == "Alex"), emoji = "1f42c", cex = 0.03) +
            geom_emoji(data = filter(bench_data, Name == "Ashleigh"), emoji = "1f331", cex = 0.03) +
            geom_emoji(data = filter(bench_data,  Name == "JB"), emoji = "1f950", cex = 0.03) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white"),
                  legend.position = "top",
                  legend.key.size = unit(0.1, 'cm'),
                  strip.background =element_rect(fill="#252c66", color = "white"),
                  strip.text = element_text(color = "white"),
                  panel.background = element_rect(fill = "#eaedf8", color = "white"))+
            scale_color_manual(values = people_pal) +
            scale_x_datetime(position = "top", limits = c( as.POSIXct(Sys.Date() -42), as.POSIXct(Sys.Date()))) +
            ylab("Bench (kg)")+
            ggtitle("")
        
    })
    
    output$compPlotDead <- renderPlot({
        
        dead_data <- filter(data2, Lift == "Deadlift")
        
        ggplot(dead_data, aes(x = Date, y = kg)) +
            geom_line(aes(group = Name, color = Name), alpha = 0.5, lwd = 1) +
            geom_point(aes(color = Name), size = 5, alpha = 0.5) +
            geom_emoji(data = filter(dead_data, Name == "Libby"), emoji = "1f990", cex = 0.03) +
            geom_emoji(data = filter(dead_data, Name == "Becky"), emoji = "1f438", cex = 0.03) +
            geom_emoji(data = filter(dead_data, Name == "Geary"), emoji = "1f991", cex = 0.03) +
            geom_emoji(data = filter(dead_data, Name == "Rachel"), emoji = "1f42f", cex = 0.03) +
            geom_emoji(data = filter(dead_data, Name == "Alex"), emoji = "1f42c", cex = 0.03) +
            geom_emoji(data = filter(dead_data, Name == "Ashleigh"), emoji = "1f331", cex = 0.03) +
            geom_emoji(data = filter(dead_data,  Name == "JB"), emoji = "1f950", cex = 0.03) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white"),
                  legend.position = "top",
                  legend.key.size = unit(0.1, 'cm'),
                  strip.background =element_rect(fill="#252c66", color = "white"),
                  strip.text = element_text(color = "white"),
                  panel.background = element_rect(fill = "#eaedf8", color = "white"))+
            scale_color_manual(values = people_pal) +
            scale_x_datetime(position = "top", limits = c( as.POSIXct(Sys.Date() -42), as.POSIXct(Sys.Date()))) +
            ylab("Deadlift (kg)")+
            ggtitle("")
        
    })
    
    output$compPlotOHP <- renderPlot({
        
        OHP_data <- filter(data2, Lift == "Overhead.Press")
        
        ggplot(OHP_data, aes(x = Date, y = kg)) +
            geom_line(aes(group = Name, color = Name), alpha = 0.5, lwd = 1) +
            geom_point(aes(color = Name), size = 5, alpha = 0.7) +
            geom_emoji(data = filter(OHP_data, Name == "Libby"), emoji = "1f990", cex = 0.03) +
            geom_emoji(data = filter(OHP_data, Name == "Becky"), emoji = "1f438", cex = 0.03) +
            geom_emoji(data = filter(OHP_data, Name == "Geary"), emoji = "1f991", cex = 0.03) +
            geom_emoji(data = filter(OHP_data, Name == "Rachel"), emoji = "1f42f", cex = 0.03) +
            geom_emoji(data = filter(OHP_data, Name == "Alex"), emoji = "1f42c", cex = 0.03) +
            geom_emoji(data = filter(OHP_data, Name == "Ashleigh"), emoji = "1f331", cex = 0.03) +
            geom_emoji(data = filter(OHP_data, Name == "Fae"), emoji = "1f98a", cex = 0.03) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white"),
                  legend.position = "top",
                  legend.key.size = unit(0.1, 'cm'),
                  strip.background =element_rect(fill="#252c66", color = "white"),
                  strip.text = element_text(color = "white"),
                  panel.background = element_rect(fill = "#eaedf8", color = "white"))+
            scale_color_manual(values = people_pal) +
            scale_x_datetime(position = "top", limits = c( as.POSIXct(Sys.Date() -42), as.POSIXct(Sys.Date()))) +
            ylab("Overhead Press (kg)")+
            ggtitle("")
        
    })
    
    
    
    
    output$PiePlot <- renderPlot({
        
        last12weeks <- filter(errybody_agg, Name == input$name)
        
        total <- sum(last12weeks$kg)
        
        emoji.name <- emoji_pal[names(emoji_pal) == input$name]
        
        ggplot(last12weeks, aes(x = "", y = kg)) +
            geom_bar(aes(fill = Lift), stat = "identity", alpha = 0.9) +
            geom_emoji(aes(x = 0, y = 0), emoji = emoji.name ,cex = 0.2) +
            geom_label(aes(x = 1.8, y = 0), label = paste(total), size = 6, alpha = 0.4) +
            coord_polar("y", start =0) +
            scale_fill_manual(values = quins_pal(length(unique(data2$Lift)))) +
            theme_void()
    })
    
    output$TotalPlot <- renderPlotly({
        
        
        errybody <- filter(data2, year(Date) == year(Sys.Date()) & week(Date) >= week(Sys.Date())-11)
        
        if(input$lift.selection == F) {
            errybody <- filter(errybody, Lift %in% c("Back.squat", "Bench", "Deadlift"))
        }
        
        errybody_agg <- aggregate(errybody$kg, by = list(errybody$Lift, errybody$Name), FUN = max)
        
        names(errybody_agg) <- c("Lift", "Name", "kg")
        
        errybody_agg2 <- aggregate(errybody_agg$kg, by = list(errybody_agg$Lift), FUN = sum)
        names(errybody_agg2) <- c("Lift", "kg")
        
        errybody_agg2$Lift <- factor(errybody_agg2$Lift,
                                     levels = c("Back.squat", "Front.Squats", "Bench", "Deadlift", "Overhead.Press"))
        
        a <- ggplot(errybody_agg, aes(y = Name, x = kg, text = paste(kg, "kg", Lift))) +
            geom_bar(aes(fill = Lift), stat = "identity") +
            scale_fill_manual(values = quins_pal(length(unique(data2$Lift)))) +
            theme_minimal() +
            scale_x_continuous(breaks = seq(0, 350, by = 50)) +
            xlab("kg") +
            ylab("") 
        
        ggplotly(a, tooltip = c("text"))
    })
    
    output$podiums <- renderPlot({
        
        BS_order <- arrange(filter(errybody_agg, Lift == "Back.squat"), desc(kg))
        deadlift_order <- arrange(filter(errybody_agg, Lift == "Deadlift"), desc(kg))
        bench_order <- arrange(filter(errybody_agg, Lift == "Bench"), desc(kg))
        
        
        
        podium_df <- data.frame("a" = factor(c("2nd", "1st", "3rd"), levels = c("2nd", "1st", "3rd")), b = c(2,3,1.2))
        
        podium_plot <- ggplot(podium_df, aes(x = a, y = b)) +
            geom_col(aes(fill = a), alpha = 0.6) +
            geom_text(aes(label = a), vjust = 1.8, color = "white", size = 6) +
            scale_fill_manual(values = quins_pal(3)) +
            theme_void() +
            theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 20))
        
        
        
        
        BS_podium <- podium_plot +
            geom_text(aes(x = "1st", y = 3), label = BS_order[1, "kg"], vjust =-5.5) +
            geom_text(aes(x = "2nd", y = 2), label = BS_order[2, "kg"], vjust =-5.5) +
            geom_text(aes(x = "3rd", y = 1), label = BS_order[3, "kg"], vjust =-5.5) +
            geom_emoji(aes(x = "1st", y = 3.5), emoji = emoji_pal[BS_order[1, "Name"]], size = 0.25)+
            geom_emoji(aes(x = "2nd", y = 2.5), emoji = emoji_pal[BS_order[2, "Name"]], size = 0.25)+
            geom_emoji(aes(x = "3rd", y = 1.45), emoji = emoji_pal[BS_order[3, "Name"]], size = 0.25)+
            ylim(0,5) +
            ggtitle("Squat") 
        
        dead_podium  <- podium_plot +
            geom_text(aes(x = "1st", y = 3), label = deadlift_order[1, "kg"], vjust =-5.5) +
            geom_text(aes(x = "2nd", y = 2), label = deadlift_order[2, "kg"], vjust =-5.5) +
            geom_text(aes(x = "3rd", y = 1), label = deadlift_order[3, "kg"], vjust =-5.5) +
            geom_emoji(aes(x = "1st", y = 3.5), emoji = emoji_pal[deadlift_order[1, "Name"]], size = 0.25)+
            geom_emoji(aes(x = "2nd", y = 2.5), emoji = emoji_pal[deadlift_order[2, "Name"]], size = 0.25)+
            geom_emoji(aes(x = "3rd", y = 1.45), emoji = emoji_pal[deadlift_order[3, "Name"]], size = 0.25)+
            ylim(0,5) +
            ggtitle("Deadlift") 
        
        bench_podium  <- podium_plot +
            geom_text(aes(x = "1st", y = 3), label = bench_order[1, "kg"], vjust =-5.5) +
            geom_text(aes(x = "2nd", y = 2), label = bench_order[2, "kg"], vjust =-5.5) +
            geom_text(aes(x = "3rd", y = 1), label = bench_order[3, "kg"], vjust =-5.5) +
            geom_emoji(aes(x = "1st", y = 3.5), emoji = emoji_pal[bench_order[1, "Name"]], size = 0.25)+
            geom_emoji(aes(x = "2nd", y = 2.5), emoji = emoji_pal[bench_order[2, "Name"]], size = 0.25)+
            geom_emoji(aes(x = "3rd", y = 1.45), emoji = emoji_pal[bench_order[3, "Name"]], size = 0.25)+
            ylim(0,5) +
            ggtitle("Bench") 
        
        
        grid.arrange(BS_podium, dead_podium, bench_podium, ncol = 3)
    }, width = 400, height = 200)
    
    
    output$StandardsPlot <- renderPlot({
      
      totalKg <- sum(input$inputSquat, input$inputBench, input$inputDeadlift)
      
      standardLegend <- ggplot(legend_df, aes(y = 1, x = arb)) +
        geom_tile(aes(fill = class), alpha = 0.6, color = "white", lwd = 1.5, height = 1)+
        geom_text(aes(label = class), color = "white", size = 4) +
        scale_fill_manual(values = quins_pal(5)) +
        theme_void() + 
        theme(legend.position = "none")
      
      # filter by sex
      if(input$inputSex == "Female") {
        standards_sex <- filter(standards, Sex == "Fe")
      } else {
        standards_sex <- filter(standards, Sex == "M")
      }
      
      # separate by lift
      bench_standards <- filter(standards_sex, lift == "bench")
      squat_standards <- filter(standards_sex, lift == "squat")
      dl_standards <- filter(standards_sex, lift == "deadlift")
      total_standards <- filter(standards_sex, lift == "total")
      
      
      benchStan <- ggplot(bench_standards, aes(x = bw)) +
        geom_ridgeline(aes(y = Beginner, height = Novice - Beginner), fill = "#FABB11", alpha = 0.6) +
        geom_ridgeline(aes(y = Novice, height = Intermediate - Novice), fill = "#B86526", alpha = 0.6) +
        geom_ridgeline(aes(y = Intermediate, height = Advanced - Intermediate), fill = "#77103C", alpha = 0.6) +
        geom_ridgeline(aes(y = Advanced, height = Elite - Advanced), fill = "#4E1E51", alpha = 0.6) +
        geom_ridgeline(aes(y = Elite, height = 40), fill = "#252C66", alpha = 0.6,  color = "white") +
        geom_point(aes(x = input$bw, y = input$inputBench), color = "white", size = 2) +
        geom_text(aes(x = input$bw, y = input$inputBench), label = paste(input$inputBench, "kg"), color = "white", size = 4, hjust = -0.3) +
        theme_minimal() +
        xlab("Bodyweight (kg)") +
        ylab("Bench (kg)")
      
      squatStan <- ggplot(squat_standards, aes(x = bw)) +
        geom_ridgeline(aes(y = Beginner, height = Novice - Beginner), fill = "#FABB11", alpha = 0.6) +
        geom_ridgeline(aes(y = Novice, height = Intermediate - Novice), fill = "#B86526", alpha = 0.6) +
        geom_ridgeline(aes(y = Intermediate, height = Advanced - Intermediate), fill = "#77103C", alpha = 0.6) +
        geom_ridgeline(aes(y = Advanced, height = Elite - Advanced), fill = "#4E1E51", alpha = 0.6) +
        geom_ridgeline(aes(y = Elite, height = 40), fill = "#252C66", alpha = 0.6,  color = "white") +
        geom_point(aes(x = input$bw, y = input$inputSquat), color = "white", size = 2) +
        geom_text(aes(x = input$bw, y = input$inputSquat), label = paste(input$inputSquat, "kg"), color = "white", size = 4, hjust = -0.3) +
        theme_minimal() +
        xlab("Bodyweight (kg)") +
        ylab("Squat (kg)")
      
      deadliftStan <- ggplot(dl_standards, aes(x = bw)) +
        geom_ridgeline(aes(y = Beginner, height = Novice - Beginner), fill = "#FABB11", alpha = 0.6) +
        geom_ridgeline(aes(y = Novice, height = Intermediate - Novice), fill = "#B86526", alpha = 0.6) +
        geom_ridgeline(aes(y = Intermediate, height = Advanced - Intermediate), fill = "#77103C", alpha = 0.6) +
        geom_ridgeline(aes(y = Advanced, height = Elite - Advanced), fill = "#4E1E51", alpha = 0.6) +
        geom_ridgeline(aes(y = Elite, height = 40), fill = "#252C66", alpha = 0.6,  color = "white") +
        geom_point(aes(x = input$bw, y = input$inputDeadlift), color = "white", size = 2) +
        geom_text(aes(x = input$bw, y = input$inputDeadlift), label = paste(input$inputDeadlift, "kg"), color = "white", size = 4, hjust = -0.3) +
        theme_minimal() +
        xlab("Bodyweight (kg)") +
        ylab("Deadlift (kg)")
      
      totalStan <- ggplot(total_standards, aes(x = bw)) +
        geom_ridgeline(aes(y = Beginner, height = Novice - Beginner), fill = "#FABB11", alpha = 0.6) +
        geom_ridgeline(aes(y = Novice, height = Intermediate - Novice), fill = "#B86526", alpha = 0.6) +
        geom_ridgeline(aes(y = Intermediate, height = Advanced - Intermediate), fill = "#77103C", alpha = 0.6) +
        geom_ridgeline(aes(y = Advanced, height = Elite - Advanced), fill = "#4E1E51", alpha = 0.6) +
        geom_ridgeline(aes(y = Elite, height = 40), fill = "#252C66", alpha = 0.6,  color = "white") +
        geom_point(aes(x = input$bw, y = totalKg), color = "white", size = 2) +
        geom_text(aes(x = input$bw, y = totalKg), label = paste(totalKg, "kg"), color = "white", size = 4, hjust = -0.3) +
        theme_minimal() +
        xlab("Bodyweight (kg)") +
        ylab("Total (kg)")
      
      #grid.arrange(standardLegened, benchStan, squatStan, deadliftStan, totalStan, ncol = 2)
      
      grid.arrange(
        grobs = list(standardLegend, benchStan, squatStan, deadliftStan, totalStan),
        widths = c(1, 1, 1, 1),
        layout_matrix = rbind(c(1,1,1,1),
                              c(2,2,3,3),
                              c(2,2,3,3),
                              c(2,2,3,3),
                              c(2,2,3,3),
                              c(2,2,3,3),
                              c(2,2,3,3),
                              c(4,4,5,5),
                              c(4,4,5,5),
                              c(4,4,5,5),
                              c(4,4,5,5),
                              c(4,4,5,5),
                              c(4,4,5,5))
      )
      
    })
    
    output$OplPlot <- renderPlot({
      
      if(input$inputSex == "Female") {
        opl_sex <- filter(opl_data, Sex == "F")
      } else {
        opl_sex <- filter(opl_data, Sex == "M")
      }
      
      opl_select_df <- filter(opl_sex, BodyweightKg >= (input$bw - 2) & BodyweightKg <= (input$bw + 2))
      
      opl_squat <- ggplot(opl_select_df, aes(x = Best3SquatKg)) +
                      geom_histogram(binwidth = 5) +
                      geom_vline(xintercept = input$inputSquat, color = "#FABB11") +
                      xlim(0, NA) +
                      theme_light() +
                      xlab("kg") +
                      ggtitle("Squat")
      
      opl_bench <- ggplot(opl_select_df, aes(x = Best3BenchKg)) +
        geom_histogram(binwidth = 5) +
        geom_vline(xintercept = input$inputBench, color = "#FABB11") +
        xlim(0, NA) +
        theme_light() +
        xlab("kg") +
        ggtitle("Bench")
      
      opl_deadlift <- ggplot(opl_select_df, aes(x = Best3DeadliftKg)) +
        geom_histogram(binwidth = 5) +
        geom_vline(xintercept = input$inputDeadlift, color = "#FABB11") +
        xlim(0, NA) +
        theme_light() +
        xlab("kg") +
        ggtitle("Deadlift")
      
      opl_total <- ggplot(opl_select_df, aes(x = TotalKg)) +
        geom_histogram(binwidth = 5) +
        geom_vline(xintercept = sum(input$inputSquat, input$inputBench, input$inputDeadlift), color = "#FABB11") +
        xlim(0, NA) +
        theme_light() +
        xlab("kg") +
        ggtitle("Total")
      
      grid.arrange(opl_bench, opl_squat, opl_deadlift, opl_total, ncol = 2)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
