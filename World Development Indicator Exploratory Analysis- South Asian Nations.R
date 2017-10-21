#exploratory data analysis for Visualizing Development Indicators in South Asian Countries

library(ggplot2) # Data visualization library 
library(readr) # CSV file I/O, e.g. the read_csv function

indicators.data  <- read_csv("C:/Users/shiva/Desktop/world-development-indicators/Indicators.csv")

countires.data <- read_csv("C:/Users/shiva/Desktop/world-development-indicators/Country.csv")

#crearing subset data for south asian regions
south.asia.region <- subset(countires.data, countires.data$Region == "South Asia")

#merging data subset with indicators tables
x <- merge(south.asia.region, indicators.data, by.x = "ShortName", by.y = "CountryName")


# Shortlisting the required colums
x <- x[, c("Year", "ShortName", "IndicatorCode", "IndicatorName", "Value")] 


# fixed telephone subscribers per 100 persons
 fixed.telephones.per100.persons <- subset(x, x$IndicatorCode == "IT.MLT.MAIN.P2" )

 ggplot(fixed.telephones.per100.persons) + aes(x = Year, y = Value) + geom_point() + 
  ggtitle("Fixed Telephones per 100 persons")  + facet_wrap(~ShortName)

# hospital beds per 1000 persons
 hospital.beds.per1000 <- subset(x, x$IndicatorCode == "SH.MED.BEDS.ZS")

 ggplot(hospital.beds.per1000) + aes(x = Year, y = Value) + geom_point() +
  ggtitle("Hospital beds per 1000 persons") + facet_wrap(~ShortName)

# life expectancies at birth (years), female: SP.DYN.LE00.FE.IN; male: SP.DYN.LE00.MA.IN; total: SP.DYN.LE00.IN

 life.expectancies <- subset(x, x$IndicatorCode == "SP.DYN.LE00.FE.IN" | 
                                x$IndicatorCode =="SP.DYN.LE00.MA.IN" | 
                                x$IndicatorCode == "SP.DYN.LE00.IN")

 ggplot(life.expectancies) +
  aes(x = Year, y = Value, color = IndicatorName) +  geom_point() +
  ggtitle("Life expectancies at birth (in years)") + facet_wrap(~ShortName)  


# mobile data subscriptions
 mobile.data <- subset(x, x$IndicatorCode == "IT.CEL.SETS.P2")

 ggplot(mobile.data) + aes(x = Year, y = Value) + geom_point(color = "green") + 
  labs(x = "Year", y = "Subscriptions per 100 persons") + ggtitle("Mobile Subscribers") + facet_wrap(~ShortName)


# population in largest city
popln.data <- subset(x, x$IndicatorCode == "EN.URB.LCTY.UR.ZS")

 ggplot(popln.data) + aes(x = Year, y = Value) + geom_point(color = "blue") + 
  labs(x = "Year", y = "% of Urban Population") + ggtitle("Population in largest city") + facet_wrap(~ShortName)

# population distribution by age groups in percents
#demographic deistribution percentage
# SP.POP.65UP.TO.ZS ;SP.POP.0014.TO.ZS ;SP.POP.1564.TO.ZS
 population.by.age <- subset(x, x$IndicatorCode == "SP.POP.65UP.TO.ZS" | x$IndicatorCode == "SP.POP.0014.TO.ZS" |
                              x$IndicatorCode == "SP.POP.1564.TO.ZS")
 ggplot(population.by.age) + aes(x = Year, y = Value, colour = IndicatorName) + 
   geom_point(stat = "identity") + facet_wrap(~ShortName)


 female.popl.percent.total <- subset(x, x$IndicatorCode == "SP.POP.TOTL.FE.ZS")

 ggplot(female.popl.percent.total) + aes(x  = Year, y = Value) + geom_point(color = "red") +
  labs(x = "Year", y = "% population") + ggtitle("Percentage of total female population") + facet_wrap(~ShortName)

# fertility rate data
 fertility.data <- subset(x, x$IndicatorCode == "SP.DYN.TFRT.IN")

 ggplot(fertility.data) + aes(x  = Year, y = Value) + geom_point(color = "red", shape = 10) +
  labs(x = "Year", y = "Total births per woman ") + ggtitle("Fertility Rates") + facet_wrap(~ShortName)


# Electric power consumption (kWh per capita)	EG.USE.ELEC.KH.PC
# Electric power transmission and distribution losses (% of output)	EG.ELC.LOSS.ZS
# Electricity production from coal sources (% of total)	EG.ELC.COAL.ZS
# Electricity production from hydroelectric sources (% of total)	EG.ELC.HYRO.ZS
# Electricity production from natural gas sources (% of total)	EG.ELC.NGAS.ZS
# Electricity production from nuclear sources (% of total)	EG.ELC.NUCL.ZS
# Electricity production from oil sources (% of total)	EG.ELC.PETR.ZS
# Electricity production from oil, gas and coal sources (% of total)	EG.ELC.FOSL.ZS
# Electricity production from renewable sources, excluding hydroelectric (% of total)	EG.ELC.RNWX.ZS
# Electricity production from renewable sources, excluding hydroelectric (kWh)	EG.ELC.RNWX.KH

electricity.production <- subset(x, x$IndicatorCode == "EG.ELC.COAL.ZS" | x$IndicatorCode == "EG.ELC.HYRO.ZS" |
                x$IndicatorCode == "EG.ELC.NGAS.ZS" | x$IndicatorCode == "EG.ELC.NUCL.ZS" |
                x$IndicatorCode == "EG.ELC.PETR.ZS" | x$IndicatorCode == "EG.ELC.FOSL.ZS" |
                x$IndicatorCode == "EG.ELC.RWNX.ZS")
 ggplot(electricity.production) + aes(x = Year, y = Value, color = IndicatorName) + 
  ggtitle("Electricity production") + geom_line(size = 1) + facet_wrap(~ShortName)



# plot the electric power consumption
electric.power.consumption <- subset(x, x$IndicatorCode == "EG.USE.ELEC.KH.PC")

 ggplot(electric.power.consumption) + aes(x = Year, y = Value, color = ShortName) + ggtitle("Electric Power consumption") +
    labs(x = "Year", y = "KWh per capita") + geom_line(size = 1)



 # population growth in annual perctange
 
 population.growth.annual <- subset(x, x$IndicatorCode == "SP.POP.GROW")
 ggplot(population.growth.annual) + aes(x = Year, y = Value) +
   ggtitle("Annual piopulation growth") +labs(x = "Year", y = "Growth percent") + geom_line(size = 1) +
   facet_wrap(~ShortName)
 

# food imports and exports
patt <- "Food*[ ]*im*.*" # look for Food followed by a space followed by im and everything else
patt1 <- "Food*[ ]*ex*.*"
a <- grep(patt, x$IndicatorName, value = TRUE)
a1 <- grep(patt1, x$IndicatorName, value = TRUE)
food.imports.exports <- subset(x, x$IndicatorName == a | x$IndicatorName == a1 )
ggplot(food.imports.exports) + aes(x = Year, y = Value, color = IndicatorName) + 
  labs(x = "Years", y = "% import/export") +
 ggtitle("Food import/export visualization") + geom_line(size = 1) + facet_wrap(~ShortName)
 
 # Unemployment with primary education (% of total unemployment)	SL.UEM.PRIM.ZS
# Unemployment with primary education, female (% of female unemployment)	SL.UEM.PRIM.FE.ZS
# Unemployment with primary education, male (% of male unemployment)	SL.UEM.PRIM.MA.ZS
# Unemployment with secondary education (% of total unemployment)	SL.UEM.SECO.ZS
# Unemployment with secondary education, female (% of female unemployment)	SL.UEM.SECO.FE.ZS
# Unemployment with secondary education, male (% of male unemployment)	SL.UEM.SECO.MA.ZS
# Unemployment with tertiary education (% of total unemployment)	SL.UEM.TERT.ZS
# Unemployment with tertiary education, female (% of female unemployment)	SL.UEM.TERT.FE.ZS
# Unemployment with tertiary education, male (% of male unemployment)	SL.UEM.TERT.MA.ZS

unemployment.data.by.eduction.level <- subset(x, x$IndicatorCode == "SL.UEM.PRIM.ZS" | 
          x$IndicatorCode == "SL.UEM.PRIM.FE.ZS" | x$IndicatorCode == "SL.UEM.PRIM.MA.ZS" |
          x$IndicatorCode == "SL.UEM.SECO.ZS" | x$IndicatorCode =="SL.UEM.SECO.FE.ZS" |
          x$IndicatorCode == "SL.UEM.SECO.MA.ZS" | x$IndicatorCode =="SL.UEM.TERT.ZS" |
          x$IndicatorCode == "SL.UEM.TERT.FE.ZS" | x$IndicatorCode == "SL.UEM.TERT.MA.ZS") 

# ggplot(unemployment.data.by.eduction.level) + aes(x = Year, y = Value) + ggtitle("Unemployment by education") +
# labs(x = "Years", y = "Percentage unemployed") + geom_line(size = 1) + facet_wrap(ShortName~IndicatorName)   

 ggplot(unemployment.data.by.eduction.level) + aes(x = Year, y = Value, color = ShortName) + 
  ggtitle("Unemployment by education") +
   labs(x = "Years", y = "Percentage unemployed") + geom_line(size = 1) + facet_wrap(~IndicatorName) 


# improved water source
 water.sources.access <- subset(x, x$IndicatorName == "Improved water source (% of population with access)" |
  x$IndicatorName == "Improved water source, rural (% of rural population with access)" |
    x$IndicatorName == "Improved water source, urban (% of urban population with access)"  )

 ggplot(water.sources.access) + aes(x = Year, y = Value, color = IndicatorName) + geom_point(size = 1) +
  labs(x = "Year", y = "Percent of population with access") + ggtitle("Improved water source access to people") +
    facet_wrap(~ShortName)

# fuels pump prices
fuel.prices <- subset(x, x$IndicatorName == "Pump price for diesel fuel (US$ per liter)" |
                      x$IndicatorName ==  "Pump price for gasoline (US$ per liter)" )

 ggplot(fuel.prices) + aes(x = Year, y = Value, color = IndicatorName) + geom_line(size = 1) +
 labs(y = "Pump price of fuel (US dollar/liter)") + ggtitle("Variation of fuel prices at pumps") + 
  facet_wrap(~ShortName)

# women who think being beaten by husband is justified~???
women.reasons <- subset(x, x$IndicatorName == "Women who believe a husband is justified in beating his 
                        wife (any of five reasons) (%)" |
  x$IndicatorName == "Women who believe a husband is justified in beating his wife when she argues with him (%)" |
  x$IndicatorName == "Women who believe a husband is justified in beating his wife when she burns the food (%)" |
  x$IndicatorName == "Women who believe a husband is justified in beating his wife when she goes out without telling him (%)" |
  x$IndicatorName == "Women who believe a husband is justified in beating his wife when she neglects the children (%)" |
  x$IndicatorName == "Women who believe a husband is justified in beating his wife when she refuses sex with him (%)"  
  )
 ggplot(women.reasons) + aes(x = Year, y = Value, fill = IndicatorName) + 
  geom_bar(position = "dodge", stat = "identity", width = 1.5) +
  labs(y = "Percentage of women") + ggtitle("How women think about husband beating them???") + 
  facet_wrap(~ShortName)


