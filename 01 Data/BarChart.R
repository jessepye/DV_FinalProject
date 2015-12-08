require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select INSTNM, Costt4_A, Ugds, Preddeg
from COLLEGE_DATA_2013
where Costt4_A is NOT NULL
AND Ugds > 30000"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~PREDDEG) +
  labs(title='Cost of College') +
  labs(x="Institution", y="Cost") +
  layer(data=df, 
        mapping=aes(x=INSTNM, y=COSTT4_A), 
        stat="identity",
        stat_params=list(),
        geom="bar",
        geom_params=list(),
        position=position_identity()
  ) + coord_flip() + theme_bw()