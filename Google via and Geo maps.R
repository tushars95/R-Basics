### Google Vis

library(googleVis)

# The googleVis concept: 
# Charts: 'gvis' + ChartType
# Output of googleVis is a list of list
# Display the chart by simply plotting the output: plot(M)
# Plot will generate a temporary html-file and open it in a new browser window

# Types of charts

# Histogram

datHist = data.frame(A=rpois(100, 20),
                    B=rpois(100, 5),
                    C=rpois(100, 50))

Hist = gvisHistogram(datHist, options=list(
  legend="{ position: 'right', maxLines: 2 }",
  colors="['Blue', 'Orange', 'Green']",
  width=850, height=360))

plot(Hist)

# Line Chart - with single axis

df = data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))

Line = gvisLineChart(df,options=list(width=750, height=500))
plot(Line)

# Line chart with two axis

Line2 = gvisLineChart(df, "country", c("val1","val2"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                         {targetAxisIndex:1}]",
                         vAxes="[{title:'val1'}, {title:'val2'}]",
                         width=750, height=400))
plot(Line2)

# Bar Charts - Horizontal Bar Chart

Bar = gvisBarChart(df,options=list(width=750, height=400))
plot(Bar)

# Bar Chart - Vertical Bar Chart

Column = gvisColumnChart(df,options=list(width=750, height=400))

plot(Column)

# Combo Chart

Combo = gvisComboChart(df, xvar="country",
                        yvar=c("val1", "val2"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}',width=750,height=400))
plot(Combo)

# Scatter chart

Scatter = gvisScatterChart(women, 
                            options=list(
                              legend="none",
                              pointSize=4,lineWidth=.5, 
                              title="Women", vAxis="{title:'weight (lbs)'}",
                              hAxis="{title:'height (in)'}", 
                              width=700, height=350))
plot(Scatter)

# Bubble Chart

Bubble = gvisBubbleChart(Fruits, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}',width=700,height=400))
plot(Bubble)

# Motion chart 

M1=gvisMotionChart(Fruits, "Fruit", "Year", 
                   options=list(width=500, height=350))

plot(M1)

# Pie charts

Pie = gvisPieChart(CityPopularity, options=list(width=700,height=400))

plot(Pie)

# Gauge charts

Gauge =  gvisGauge(CityPopularity, 
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)

# Table

PopTable = gvisTable(Population, 
                      formats=list(Population="#,###",
                                   '% of World Population'='#.#%'),
                      options=list(page='enable'))
plot(PopTable)

# Organization Chart 

Org = gvisOrgChart(Regions, 
                    options=list(width=600, height=250,
                                 size='large', allowCollapse=TRUE))
plot(Org)

# Tree Map

Tree = gvisTreeMap(Regions,  
                    "Region", "Parent", 
                    "Val", "Fac", 
                    options=list(fontSize=16))
plot(Tree)

# Calendar chart

Cal = gvisCalendar(Cairo, 
                    datevar="Date", 
                    numvar="Temp",
                    options=list(
                      title="Daily temperature in Cairo",
                      height=320,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                      fontSize: 32, color: 'blue', bold: true},
                      cellSize: 10,
                      cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                      focusedCellColor: {stroke:'red'}}"))

plot(Cal)


# Geo Chart

Geo = gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii",
                              width=750, height=400))
plot(Geo)

# Geo chart USA

library(datasets)

states = data.frame(state.name, state.x77)

GeoStates = gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", displayMode="regions", resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

# Merging two charts

G = gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=600, height=400))
T = gvisTable(Exports, 
               options=list(width=440, height=400))

GT = gvisMerge(G,T, horizontal=TRUE) 
plot(GT)









