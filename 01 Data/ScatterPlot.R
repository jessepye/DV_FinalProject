require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select PAR_ED_PCT_1STGEN, RET_FT4, CURROPER, UGDS
from COLLEGE_DATA_2013_FINAL
where CURROPER = 1
and RET_FT4>0
and RET_FT4<1
and PAR_ED_PCT_1STGEN is not null"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Percentage of 1st Generation College Students vs Retention Rate') +
  labs(x="Percentage of 1st Generation College Students", y="Retention Rate") +
  layer(data=df, 
        mapping=aes(x=PAR_ED_PCT_1STGEN, y=RET_FT4, size=UGDS), 
        stat="identity",
        stat_params=list(),
        geom="point",
        geom_params=list(),
        position=position_identity()
  ) + theme_classic()
