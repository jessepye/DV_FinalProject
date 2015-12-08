require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select CONTROL, RET_FT4, CURROPER, UGDS
from COLLEGE_DATA_2013_FINAL
where (CURROPER = 1
AND Ugds is NOT NULL
AND RET_FT4 is NOT NULL
AND RET_FT4 > 0
AND RET_FT4 < 1)"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

df$CONTROL[df$CONTROL == 1] <- "Public"
df$CONTROL[df$CONTROL == 2] <- "Private Nonprofit"
df$CONTROL[df$CONTROL == 3] <- "Private For-Profit"

ggplot() +
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  labs(title='Student Retention') +
  labs(x=NULL) +
  labs(y="Retention Rate") +
  layer(data=df, 
        mapping=aes(x=CONTROL, y=RET_FT4), 
        stat="boxplot",
        stat_params=list(),
        geom="boxplot",
        geom_params=list(fill="grey", alpha=.5),
        position=position_identity()
  ) + theme_bw()