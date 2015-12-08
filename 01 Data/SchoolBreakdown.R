require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select CONTROL, SchoolSize, Cdr3, INSTNM,
CASE
WHEN COSTT4_A > 30000 THEN \\\'3 HIGH\\\'
WHEN COSTT4_A > 15000 THEN \\\'2 MEDIUM\\\'
ELSE \\\'1 LOW\\\'
END TotalCost
from (SELECT Cdr3, COSTT4_A, CONTROL, INSTNM,
CASE
WHEN Ugds <= 5000 THEN \\\'1 Small\\\'
WHEN Ugds <= 10000 THEN \\\'2 Medium\\\'
WHEN Ugds <= 25000 THEN \\\'3 Large\\\'
ELSE \\\'4 VeryLarge\\\'
END SchoolSize
from COLLEGE_DATA_2013_FINAL
where (Costt4_A is NOT NULL
AND Cdr3 is NOT NULL
AND Ugds is NOT NULL))
"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

df$CONTROL[df$CONTROL == 1] <- "Public"
df$CONTROL[df$CONTROL == 2] <- "Private Nonprofit"
df$CONTROL[df$CONTROL == 3] <- "Private For-Profit"

LowCostPublicVL <- subset(df, TOTALCOST == "1 LOW" & CONTROL == "Public" & SCHOOLSIZE == "4 VeryLarge")
LowCostPublicS <- subset(df, TOTALCOST == "1 LOW" & CONTROL == "Public" & SCHOOLSIZE == "1 Small")
HighCostPrivateVL <- subset(df, TOTALCOST == "3 HIGH" & CONTROL == "Private Nonprofit" & SCHOOLSIZE == "4 VeryLarge")

write.table(LowCostPublicVL, file="./LowCostPublicColleges.txt", sep=",")
