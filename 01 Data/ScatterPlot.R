require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select INSTNM, ADM_RATE, SAT_AVG
from COLLEGE_DATA_2013
where NOT(
ADM_RATE is NULL OR
SAT_AVG is NULL
)"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='SAT Scores and College Admission') +
  labs(x="Admission Rate", y="Average SAT Score") +
  layer(data=df, 
        mapping=aes(x=ADM_RATE, y=SAT_AVG), 
        stat="identity",
        stat_params=list(),
        geom="point",
        geom_params=list(),
        position=position_identity()
  ) + theme_classic()