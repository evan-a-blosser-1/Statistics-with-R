v <- Intro2R::myreadxl()
v$MTBE -> mtbe

table(mtbe$Aquifier, mtbe$`MTBE-Detect`) -> tabb

addmargins(tabb)

