require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select CONTROL, TotalCost, SchoolSize, AVG(Cdr3) as DefaultRate
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
Order By CONTROL, TotalCost, SchoolSize"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

df$CONTROL[df$CONTROL == 1] <- "Public"
df$CONTROL[df$CONTROL == 2] <- "Private Nonprofit"
df$CONTROL[df$CONTROL == 3] <- "Private For-Profit"
df$DEFAULTRATE <- round(df$DEFAULTRATE, 4)
df$KPI[df$DEFAULTRATE < .1] <- "PASS"
df$KPI[df$DEFAULTRATE >= .1] <- "FAIL"


ggplot() +
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  facet_grid(CONTROL~.) +
  labs(title='Default Rates') +
  labs(x="School Size", y="Cost of School / Governance Type") +
  layer(data=df, 
        mapping=aes(x=SCHOOLSIZE, y=TOTALCOST, label=DEFAULTRATE), 
        stat="identity",
        stat_params=list(),
        geom="text",
        geom_params=list(),
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=SCHOOLSIZE, y=TOTALCOST, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.50), 
        position=position_identity()
  ) + theme_minimal()