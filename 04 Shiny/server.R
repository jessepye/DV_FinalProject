# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  KPI_value <- reactive({input$KPI1})
  rv <- reactiveValues(alpha = 0.50)
  
  df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select GovType, TotalCost, SchoolSize, GradRate,
CASE
when GradRate < "p1" then \\\'Fail\\\'
else \\\'Pass\\\'
end kpi
from (select TotalCost, SchoolSize, GradRate,
CASE CONTROL
WHEN 1 THEN \\\'Public\\\'
WHEN 2 THEN \\\'Private Nonprofit\\\'
WHEN 3 THEN \\\'Private For-profit\\\'
END GovType
from (select CONTROL, TotalCost, SchoolSize, AVG(C150_4) as GradRate
from (select CONTROL, SchoolSize, C150_4,
CASE
WHEN COSTT4_A > 30000 THEN \\\'3 HIGH\\\'
WHEN COSTT4_A > 15000 THEN \\\'2 MEDIUM\\\'
ELSE \\\'1 LOW\\\'
END TotalCost
from (SELECT C150_4, COSTT4_A, CONTROL,
CASE
WHEN Ugds <= 5000 THEN \\\'1 Small\\\'
WHEN Ugds <= 10000 THEN \\\'2 Medium\\\'
WHEN Ugds <= 25000 THEN \\\'3 Large\\\'
ELSE \\\'4 VeryLarge\\\'
END SchoolSize
from COLLEGE_DATA_2013
where (Costt4_A is NOT NULL
AND C150_4 is NOT NULL
AND Ugds is NOT NULL)))
Group By CONTROL, SchoolSize, TotalCost
Order By CONTROL, TotalCost, SchoolSize))"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1 = KPI_value()), verbose = TRUE)))
  })
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() +
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      facet_grid(GOVTYPE~.) +
      labs(title='Performance of College Types') +
      labs(x="School Size", y="Cost of School / Governance Type") +
      layer(data=df1(), 
            mapping=aes(x=SCHOOLSIZE, y=TOTALCOST, label=round(GRADRATE, 4)), 
            stat="identity",
            stat_params=list(),
            geom="text",
            geom_params=list(),
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=SCHOOLSIZE, y=TOTALCOST, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) + theme_minimal()
    plot
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  
  df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select INSTNM, Costt4_A, Ugds, Preddeg
from COLLEGE_DATA_2013
where Costt4_A is NOT NULL
AND Ugds > 30000"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot2 <- renderPlot(height=500, width=1000, {
    plot2 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~PREDDEG) +
      labs(title='Cost of College') +
      labs(x="Institution", y="Cost") +
      layer(data=df2(), 
            mapping=aes(x=INSTNM, y=COSTT4_A), 
            stat="identity",
            stat_params=list(),
            geom="bar",
            geom_params=list(),
            position=position_identity()
      ) + coord_flip() + theme_bw()
    plot2
  })
  
  # Begin code for Third Tab:
  
  df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select INSTNM, ADM_RATE, SAT_AVG
from COLLEGE_DATA_2013
where NOT(
ADM_RATE is NULL OR
SAT_AVG is NULL
)"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot3 <- renderPlot(height=500, width=1000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='SAT Scores and College Admission') +
      labs(x="Admission Rate", y="Average SAT Score") +
      layer(data=df3(), 
            mapping=aes(x=ADM_RATE, y=SAT_AVG), 
            stat="identity",
            stat_params=list(),
            geom="point",
            geom_params=list(),
            position=position_identity()
      ) + theme_classic()
    plot3
  })
})
