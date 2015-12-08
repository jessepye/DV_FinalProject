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

df0 <- eventReactive(input$clicks0, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select RET_FT4, CURROPER, UGDS,
CASE CONTROL
WHEN 1 THEN \\\'Public\\\'
WHEN 2 THEN \\\'Private Nonprofit\\\'
WHEN 3 THEN \\\'Private For-profit\\\'
END GovType
from COLLEGE_DATA_2013_FINAL
where (CURROPER = 1
AND Ugds is NOT NULL
AND RET_FT4 is NOT NULL
AND RET_FT4 > 0
AND RET_FT4 < 1)"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })

  output$distPlot0 <- renderPlot(height=500, width=1000, {
    plot0 <- ggplot() +
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      labs(title='Student Retention') +
      labs(x=NULL) +
      labs(y="Retention Rate") +
      layer(data=df0(), 
            mapping=aes(x=GOVTYPE, y=RET_FT4), 
            stat="boxplot",
            stat_params=list(),
            geom="boxplot",
            geom_params=list(fill="grey", alpha=.5),
            position=position_identity()
      ) + theme_bw()
    plot0
  })
  
# Second Tab
  
  df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select CURROPER, UGDS
from COLLEGE_DATA_2013_FINAL
where (CURROPER = 1
AND UGDS is NOT NULL
AND UGDS > 0
AND UGDS < 60000)"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })

  output$distPlot1 <- renderPlot(height=500, width=1000, {
    plot1 <- ggplot(df1(), aes(x=UGDS)) +
      geom_histogram(fill="blue") +
      labs(x ="Number of Undergraduates") +
      labs(y ="Count") + labs(title = "School Sizes")
    plot1
  })

  
  # Begin code for Third Tab:
  
  df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select PAR_ED_PCT_1STGEN, RET_FT4, CURROPER, UGDS
from COLLEGE_DATA_2013_FINAL
where CURROPER = 1
and RET_FT4>0
and RET_FT4<1
and PAR_ED_PCT_1STGEN is not null"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot3 <- renderPlot(height=500, width=1000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Percentage of 1st Generation College Students vs Retention Rate') +
      labs(x="Percentage of 1st Generation College Students", y="Retention Rate") +
      layer(data=df3(), 
            mapping=aes(x=PAR_ED_PCT_1STGEN, y=RET_FT4, size=UGDS), 
            stat="identity",
            stat_params=list(),
            geom="point",
            geom_params=list(),
            position=position_identity()
      ) + theme_classic()
    plot3
  })
  

# Fourth Tab
  
  KPI_value <- reactive({input$KPI4})
  rv <- reactiveValues(alpha = 0.50)
  
  df4 <- eventReactive(input$clicks4, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select GovType, TotalCost, SchoolSize, DefaultRate,
CASE
when DefaultRate > "p1" then \\\'Fail\\\'
else \\\'Pass\\\'
end kpi
from (select TotalCost, SchoolSize, DefaultRate,
CASE CONTROL
WHEN 1 THEN \\\'Public\\\'
WHEN 2 THEN \\\'Private Nonprofit\\\'
WHEN 3 THEN \\\'Private For-profit\\\'
END GovType
from (select CONTROL, TotalCost, SchoolSize, AVG(Cdr3) as DefaultRate
from (select CONTROL, SchoolSize, Cdr3,
CASE
WHEN COSTT4_A > 30000 THEN \\\'3 HIGH\\\'
WHEN COSTT4_A > 15000 THEN \\\'2 MEDIUM\\\'
ELSE \\\'1 LOW\\\'
END TotalCost
from (SELECT Cdr3, COSTT4_A, CONTROL,
CASE
WHEN Ugds <= 5000 THEN \\\'1 Small\\\'
WHEN Ugds <= 10000 THEN \\\'2 Medium\\\'
WHEN Ugds <= 25000 THEN \\\'3 Large\\\'
ELSE \\\'4 VeryLarge\\\'
END SchoolSize
from COLLEGE_DATA_2013_FINAL
where (Costt4_A is NOT NULL
AND Cdr3 is NOT NULL
AND Ugds is NOT NULL)))
Group By CONTROL, SchoolSize, TotalCost
Order By CONTROL, TotalCost, SchoolSize))"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1 = KPI_value()), verbose = TRUE)))
  })
  
  output$distPlot4 <- renderPlot({             
    plot4 <- ggplot() +
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      facet_grid(GOVTYPE~.) +
      labs(title='Default Rate of Students') +
      labs(x="School Size", y="Cost of School / Governance Type") +
      layer(data=df4(), 
            mapping=aes(x=SCHOOLSIZE, y=TOTALCOST, label=round(DEFAULTRATE, 4)), 
            stat="identity",
            stat_params=list(),
            geom="text",
            geom_params=list(),
            position=position_identity()
      ) +
      layer(data=df4(), 
            mapping=aes(x=SCHOOLSIZE, y=TOTALCOST, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) + theme_minimal()
    plot4
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for fifth Tab:
  
  df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select INSTNM, UGDS, CONTROL, CURROPER, GOVTYPE, SCHOOLSIZE
from (
    select INSTNM, UGDS, CONTROL, CURROPER, GOVTYPE,
    CASE
    WHEN Ugds <= 4000 THEN \\\'1 Small\\\'
    WHEN Ugds <= 14000 THEN \\\'2 Medium\\\'
    ELSE \\\'3 Large\\\'
    END SCHOOLSIZE
    from (
        select INSTNM, UGDS, CONTROL, CURROPER,
        CASE
        WHEN CONTROL = 1 THEN \\\'1 Public\\\'
        WHEN CONTROL = 2 THEN \\\'2 Private Non-Profit\\\'
        WHEN CONTROL = 3 THEN \\\'3 Private For-Profit\\\'
        END GOVTYPE
        from COLLEGE_DATA_2013_FINAL
     )
)
where CURROPER = 1
and UGDS is not null"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot2 <- renderPlot(height=500, width=1000, {
    plot2 <- qplot(SCHOOLSIZE, data=df2(), geom="bar", weight=UGDS, facets = ~ GOVTYPE, fill=factor(SCHOOLSIZE))
    plot2
  })
})
