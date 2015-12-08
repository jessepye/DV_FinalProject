require("jsonlite")
require("RCurl")
require("extrafont")
require("ggplot2")

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
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
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jjp378', PASS='orcl_jjp378', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));

df$UGDS <- as.numeric(as.character(df$UGDS))

qplot(SCHOOLSIZE, data=df, geom="bar", weight=UGDS, facets = ~ GOVTYPE, fill=factor(SCHOOLSIZE))