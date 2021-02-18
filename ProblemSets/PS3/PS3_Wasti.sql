#Q5
a.

CREATE TABLE FL_insurance(
  "policyID" INT,
  "statecode" CHAR,
  "county" VARCHAR(255),
  "eq_site_limit" DECIMAL(18,4),
  "hu_site_limit" DECIMAL(18,4),
  "fl_site_limit" DECIMAL(18,4),
  "fr_site_limit" DECIMAL(18,4),
  "tiv_2011" DECIMAL(18,4),
  "tiv_2012" DECIMAL(18,4),
  "eq_site_deductible" DECIMAL(18,4),
  "hu_site_deductible" DECIMAL(18,4),
  "fl_site_deductible" DECIMAL(18,4),
  "fr_site_deductible" DECIMAL(18,4),
  "point_latitude" DECIMAL(18,4),
  "point_longitude" DECIMAL(18,4),
  "line" VARCHAR(255),
  "construction" VARCHAR(255),
  "point_granularity" INT); -- creating an SQL table to hold the data

-- Reading the raw data file into SQL's memory:
.mode csv
.import /home/ouecon052/DScourseS21/ProblemSets/PS3/FL_insurance_sample.csv FL_insurance 


b. 

SELECT * FROM FL_insurance LIMIT 10; -- printing the first 10 rows of the dataset

policyID,statecode,county,eq_site_limit,hu_site_limit,fl_site_limit,fr_site_limit,tiv_2011,tiv_2012,eq_site_deductible,hu_site_deductible,fl_site_deductible,fr_site_deductible,point_latitude,point_longitude,line,construction,point_granularity
119736,FL,"CLAY COUNTY",498960,498960,498960,498960,498960,792148.9,0,9979.2,0,0,30.102261,-81.711777,Residential,Masonry,1
448094,FL,"CLAY COUNTY",1322376.3,1322376.3,1322376.3,1322376.3,1322376.3,1438163.57,0,0,0,0,30.063936,-81.707664,Residential,Masonry,3
206893,FL,"CLAY COUNTY",190724.4,190724.4,190724.4,190724.4,190724.4,192476.78,0,0,0,0,30.089579,-81.700455,Residential,Wood,1
333743,FL,"CLAY COUNTY",0,79520.76,0,0,79520.76,86854.48,0,0,0,0,30.063236,-81.707703,Residential,Wood,3
172534,FL,"CLAY COUNTY",0,254281.5,0,254281.5,254281.5,246144.49,0,0,0,0,30.060614,-81.702675,Residential,Wood,1
785275,FL,"CLAY COUNTY",0,515035.62,0,0,515035.62,884419.17,0,0,0,0,30.063236,-81.707703,Residential,Masonry,3
995932,FL,"CLAY COUNTY",0,19260000,0,0,19260000,20610000,0,0,0,0,30.102226,-81.713882,Commercial,"Reinforced Concrete",1
223488,FL,"CLAY COUNTY",328500,328500,328500,328500,328500,348374.25,0,16425,0,0,30.102217,-81.707146,Residential,Wood,1
433512,FL,"CLAY COUNTY",315000,315000,315000,315000,315000,265821.57,0,15750,0,0,30.118774,-81.704613,Residential,Wood,1


c.

SELECT county, COUNT(*) FROM FL_insurance GROUP BY county; -- to list the unique values of the county variable

"ALACHUA COUNTY",973
"BAKER COUNTY",70
"BAY COUNTY",403
"BRADFORD COUNTY",31
"BREVARD COUNTY",872
"BROWARD COUNTY",3193
"CALHOUN COUNTY",68
"CHARLOTTE COUNTY",414
"CITRUS COUNTY",384
"CLAY COUNTY",363
"COLLIER COUNTY",787
"COLUMBIA COUNTY",125
"DESOTO COUNTY",108
"DIXIE COUNTY",40
"DUVAL COUNTY",1894
"ESCAMBIA COUNTY",494
"FLAGLER COUNTY",204
"FRANKLIN COUNTY",37
"GADSDEN COUNTY",196
"GILCHRIST COUNTY",39
"GLADES COUNTY",22
"GULF COUNTY",72
"HAMILTON COUNTY",35
"HARDEE COUNTY",81
"HENDRY COUNTY",74
"HERNANDO COUNTY",120
"HIGHLANDS COUNTY",369
"HILLSBOROUGH COUNTY",1166
"HOLMES COUNTY",40
"INDIAN RIVER COUNTY",380
"JACKSON COUNTY",208
"JEFFERSON COUNTY",57
"LAFAYETTE COUNTY",68
"LAKE COUNTY",206
"LEE COUNTY",678
"LEON COUNTY",246
"LEVY COUNTY",126
"LIBERTY COUNTY",36
"MADISON COUNTY",81
"MANATEE COUNTY",518
"MARION COUNTY",1138
"MARTIN COUNTY",109
"MIAMI DADE COUNTY",4315
"MONROE COUNTY",152
"NASSAU COUNTY",135
"North Fort Myers",1
"OKALOOSA COUNTY",1115
"ORANGE COUNTY",1811
"OSCEOLA COUNTY",1
Orlando,1
"PALM BEACH COUNTY",2791
"PASCO COUNTY",790
"PINELLAS COUNTY",1774
"POLK COUNTY",1629
"PUTNAM COUNTY",268
"SANTA ROSA COUNTY",856
"SARASOTA COUNTY",417
"SEMINOLE COUNTY",1100
"ST  JOHNS COUNTY",657
"SUMTER COUNTY",158
"SUWANNEE COUNTY",154
"TAYLOR COUNTY",113
"UNION COUNTY",15
"VOLUSIA COUNTY",1367
"WAKULLA COUNTY",85
"WALTON COUNTY",288
"WASHINGTON COUNTY",116
county,1


d. 
SELECT AVG(tiv_2012 - tiv_2011) FROM FL_insurance; -- computing the average property appreciation from 2011 to 2012

398118.229567353 is the average property appreciation

e. 
Select construction, count(*) From   FL_insurance Group By construction; -- Creating a frequency table of the construction variable

Masonry,9257
"Reinforced Concrete",1299
"Reinforced Masonry",4225
"Steel Frame",272
Wood,21581
construction,1

