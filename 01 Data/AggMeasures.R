require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select CURROPER, UGDS
from COLLEGE_DATA_2013_FINAL
where (CURROPER = 1
AND UGDS is NOT NULL
AND UGDS > 0
AND UGDS < 50000)"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

ggplot(df, aes(x=UGDS)) +
  geom_histogram(fill="blue") +
  labs(x ="Number of Undergraduates") +
  labs(y ="Count") + labs(title = "School Sizes")