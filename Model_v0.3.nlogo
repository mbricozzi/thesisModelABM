extensions [ gis palette r ]

;# #################################################### #
;# --- Define globals, entities and state variables --- #
;# #################################################### #


globals [
  elevation-data   ; Necessary
  slope-data       ; for GIS
  aspect-data      ; Extension


  dataFarms       ; Just data on viticultural farms
  qualityStandard ; First defined as the average quality of all farmed plots, then defined by endogenous rule changing process
  giPrestige      ; Can be increased by the gi Board marketing investments
  giWinePrice     ; Linear function of quality, GI label and GI prestige (first fixed to 1.5)
  stdWinePrice    ; 1
  wineYield       ; 50 hl/ha
]

breed [farms farm]         ; N. agents
breed [giBoards giBoard]   ; One agent
breed [ftokens ftoken]     ; Just tokens of Farms' patches to visualise farms


patches-own [
  elevation    ; Random raster uniform distribution smoothed with gaussian kernel
  slope        ; Convolution of elevation
  aspect       ; Convolution of elevation
  soilQ        ; 0-1 Interval Randomly generated
  microclimate ; GS avg Temperature downscaled with elevation data.
  wineQ        ; 0-1 Interval weighted linear function of soilQ and microclimate
  ageVines     ; Important, only after 3 years can produce
  owner        ; Farm number or nobody
  pastOwners   ; Agentset all past owners
  farmStead    ; Bolean
  varCost      ; For productive plots (ageVines >3); Increasing with slope
  fixCost      ; For each plot owned
  giLabel      ; Bolean
  landPrice    ; Function of Quality and GI label
]

farms-own [
  capital
  myPlots      ; Agentset farm's land
  mySlope      ; Avg slope of myPlots, interesting for variable costs
  myQuality    ; Avg quality of myPlots
  pastVintages ; List, Memory on past vintages' quality
  totalCost
]

giBoards-own [
  capital
]

;# ###################### #
;# --- Initialisation --- #
;# ###################### #


to setup
  clear-all
  r:clear
  initiate-globals
  generate-raster
  setup-world
  setup-climate
  setup-quality
  setup-farmers
  setup-giBoard
  setup-giArea
  setup-landPrice
  reset-ticks
end

to initiate-globals
;  set GSavgTemp GSavgTemp_i
  set dataFarms (list 3.284 678.887	1849.086) ; Note: Eurostat data on average farm size, numb of farms and hectares of PDO wine regions in ITA, FRA, SPA, POR, GER
  set wineYield 5000                          ; Max production per hectare 50 hectolitres good approximation for PDO wine regions
  set giWinePrice 1.5
  set stdWinePrice 1
  ask patches [set varCost slope * 4000 set fixCost 2000]    ; is you sell wine at normal price of 1 can barely cover variable costs of one hectare
end                                                          ; Arbitrary variable cost increasing with slope (max cost 6000 for most slopy plots)

;###################
;# --- PATCHES --- #
;###################

to generate-raster
  ; NetLogo-R interaction to generate a randomised Gaussian Kernel Raster for elevation

  ; Set up env variables to interact with R scripts
  let sgk (list sigma_GaussKernel)
  let minelev_i (list MinimumElevation)
  let maxelev_i (list MaximumElevation)
  r:put "s" sgk
  r:put "Min" minelev_i
  r:put "Max" maxelev_i

  ; Three lines create a 107x107 random raster from uniform ditribution with range defined by the globals MinimumElevation and MaximumElevation
  r:eval "require(raster)"
  r:eval "raster <- raster(ncols=107, nrows=107, xmn=-53, xmx=53, ymn=-53, ymx=53 )"
  r:eval "raster[] <- runif(ncell(raster), min = Min, max = Max)"

  r:eval ("source('C:/NetLogo_models/thesisModel/Rscripts/script_raster.R')")         ; load r file with function for gaussian kernel.

  r:eval "r.sim <- focal(raster, w=GaussianKernel(s, 9))"                             ; Create autocorrelated raster using 9x9 Gaussian Kernel with a sigma= s


  r:eval "e <- as(extent(-49, 49, -49, 49), 'SpatialPolygons')"                       ; Crop raster to 100x100
  r:eval "crs(e) <- crs(r.sim)"
  r:eval "r.final <- crop(r.sim, e)"

  r:eval "writeRaster(r.final, 'C:/NetLogo_models/thesisModel/GIS/rasterFile.asc', format = 'ascii', overwrite = T)"
end

to setup-world
  ; Use data from raster file to setup world
  set elevation-data gis:load-dataset "C:/NetLogo_models/thesisModel/GIS/rasterFile.asc"
  resize-world 0 (gis:width-of  elevation-data) 0 (gis:height-of elevation-data)
  gis:set-world-envelope gis:envelope-of elevation-data

  ; display elevation in patches
  gis:apply-raster elevation-data elevation


  let minelev (min [elevation] of patches)
  let maxelev (max [elevation] of patches)

  ask patches
  [if (elevation <= 0) or (elevation >= 0)
    [ set pcolor palette:scale-scheme  "Divergent" "BrBG" 8 elevation maxelev minelev                                   ; Easiest way with fixed sequential scale
      set owner nobody
      set pastOwners []
    ]
  ]
  setup-slopeAspect
end


to setup-slopeAspect ; ATTENTION: in this way you loose first and last rows and col for slope and aspect you should calculate this earlier before cropping to 100x100

  let horizontal-gradient gis:convolve elevation-data 3 3 [ 1 1 1 0 0 0 -1 -1 -1 ] 1 1
  let vertical-gradient gis:convolve elevation-data 3 3 [ 1 0 -1 1 0 -1 1 0 -1 ] 1 1
  set slope-data gis:create-raster gis:width-of elevation-data gis:height-of elevation-data gis:envelope-of elevation-data
  set aspect-data gis:create-raster gis:width-of elevation-data gis:height-of elevation-data gis:envelope-of elevation-data
  let x 0
  repeat (gis:width-of slope-data)
  [ let y 0
    repeat (gis:height-of slope-data)
    [ let gx gis:raster-value horizontal-gradient x y
      let gy gis:raster-value vertical-gradient x y
      if ((gx <= 0) or (gx >= 0)) and ((gy <= 0) or (gy >= 0))
      [ let s sqrt ((gx * gx) + (gy * gy))
        gis:set-raster-value slope-data x y s
        ifelse (gx != 0) or (gy != 0)
        [ gis:set-raster-value aspect-data x y atan gy gx ]
        [ gis:set-raster-value aspect-data x y 0 ] ]
      set y y + 1 ]
    set x x + 1 ]
  gis:set-sampling-method aspect-data "bilinear"

  gis:apply-raster slope-data slope
  gis:apply-raster aspect-data aspect

  ; rescale slope on a 0-1 scale
  let minSlope min [slope] of patches
  let maxSlope max [slope] of patches
  ask patches [set slope ((slope - minSlope ) / (maxSlope - minSlope))]
end

to setup-climate
  let max-elevation max [elevation] of patches
  ask patches [
    ifelse Dry?
    [set microclimate GSavgTemp + 0.0098 * (max-elevation - elevation)]
    [set microclimate GSavgTemp + 0.006 *  (max-elevation - elevation)]  ; add increase dep on elevation.
  ]
end

to setup-quality
  ask patches [set SoilQ (random-float 1 )]
  repeat SmoothS [ diffuse SoilQ 0.5]
  ask patches [
    set wineQ (((1 - climateW) * SoilQ) + climateW * (1 - (abs((Opt_GSavgTemp - microclimate)/ Opt_GSavgTemp))))
  ]
end



;##################
;# --- AGENTS --- #
;##################

;# --Farmers

to setup-farmers
  set-default-shape farms "person"
  set-default-shape ftokens "square 3"

; MAX ONE FARMS PER PLOT and differenciate color from neighbouring farms
  ask n-of nFarms patches [
    sprout-farms 1
    set owner [who] of farms-here
    set farmStead 1
    set ageVines 20  ; let's assume that age is kept constant to 20 years in each hectare age in only interesting when new vines are installed
    ask farms-here [
      let poscol filter [ x -> not member? x ([color] of (other farms in-radius 6)) ] base-colors
      ifelse not empty? poscol [set color one-of poscol] [set color one-of base-colors]
    ]
  ]
  ask farms [
;EACH FARM LOOKS AROUND ITS NEIGHBOURHOOD AND PICKS 0 to 8 plots if they have no other farms on already.
    set myPlots (patch-set patch-here (n-of random (count neighbors with [owner = nobody]) neighbors with [owner = nobody]))
    ask myPlots [
      set owner [who] of myself
      sprout-fTokens 1
      set ageVines 20
      ]
; Just a square sign to grafically represents farms land endowments
    ask fTokens-on myPlots [set color [color] of myself]
; Farms calculate their average quality
    set myQuality mean [wineQ] of myPlots
    set pastVintages (list )
; Capital higher for bigger farms
    set capital 5000 * (count myPlots)
  ]
end


;# --GI

to setup-giArea
  set qualityStandard mean [wineQ] of patches with [any? fTokens-here]  ; Quality standard as average of all farms quality

  ifelse manualArea?

; GI AREA manually DEFINED in hectares IT WOULD MAKE SENSE TO LOCATE FARMS IN THE x BEST PATCHES AT THE START.
  [let giPatches max-n-of giArea patches [wineQ]
   ask giPatches [set giLabel 1 set plabel-color black set plabel giLabel]]

; GI AREA DEFINED with QUALITY STANDARD
  [let giPatches patches with [wineQ >= qualityStandard]
   ask giPatches [set giLabel 1
                  set plabel-color black set plabel giLabel]
  ]
end

to setup-giBoard
  set-default-shape giBoards "giboard"
  create-giBoards 1
  ask giBoards [
    set capital 100 * nFarms
  ]
end


;###########################
;# --- OTHER VARIABLES --- #
;###########################


to setup-landPrice
  ask patches [set landPrice ((wineQ * 10000) + (giLabel * 5000))]
end


to setup-giPrestige
;  set giPrestige (min (list (mean [wineQ] of giPatches) brandValueStock)   ])
end


to setup-brandValueStock
end


to setup-winePrice
end








;# ##################################### #
;# --- Model Dynamics and Iterations --- #
;# ##################################### #


to go
  climateChange
  operationalArena
  collectiveChoiceArena
  giBoardAction
  tick
end


to climateChange

; INCREASE TEMPERATURE AND UPDATE PATCHES WINE QUALITY  (YOU CAN MAKE IT MUCH LIGHTER AND FAST)
  ask patches [
    ifelse ClimateScenario = "ParisAgreement"
    [set microclimate microclimate + 0.02
     update-wineQuality]
    [set microclimate microclimate + 0.035
     update-wineQuality]
  ]
end

to update-wineQuality
  set wineQ (((1 - climateW) * SoilQ) + climateW * (1 - (abs((Opt_GSavgTemp - microclimate)/ Opt_GSavgTemp))))
end


to operationalArena
  giBoardAction
  farmersHeuristic
;  farmersOptimisation

end

to collectiveChoiceArena
  voting
end


to farmersHeuristic
  ask farms [

    loop [
;   # Check QUALITY
      set myQuality mean [wineQ] of myPlots
      set totalCost sum [fixCost] of myPlots + sum [varCost] of myPlots with [ageVines > 3]

      ifelse myQuality < qualityStandard

;   1) Quatilty is lower than standard

;     # Check neighbouring plots with quality higher than standard and no owner
      [
        let interestingPlots ifelse-value inRadius? [patch-set (patches in-radius radius with [wineQ > qualityStandard and owner = nobody  and (member? [who] of myself pastOwners) = false])]
                                                    [patch-set ([neighbors] of myPlots with [wineQ > qualityStandard and owner = nobody])]      ; [neighbors] of myPlots or patches in-radius x? include distance from farmstead in cost calculation

        ifelse any? interestingPlots

;     1.1) There is at least one interesting plot
        [
          ifelse capital >= (first ([landPrice] of interestingPlots with-min [landPrice])) + totalCost   ; Can buy but also need money for production in that year

;         1.1.1) The farmer has enough capital to buy
;         # BUY NEW PLOT with higher quality and lowest price
          [
            let chosenPlot interestingPlots with-min [landPrice]
            ask chosenPlot [set owner [who] of myself                                        ; ATTENTION HERE EACH FARM DOES THIS ONE AFTER THE OTHER, SOMEBODY WILL RANDOMLY BE SELECTED TO MOVE FIRST THEREFORE HAVING MORE PLOTS TO CHOOSE FROM.
                            sprout-fTokens 1
                            set ageVines 1]
            ask fTokens-on chosenPlot [set color [color] of myself]
            set myPlots (patch-set myPlots chosenPlot)
            set capital capital - first [landPrice] of chosenPlot                            ; they are list you must select the first "0th" element of the list
          ]


;         1.1.2) The farmer has not enough capital to buy
;         # SELL PLOT with the lowest quality
          [
            if  (myPlots with [farmStead = 0]) = no-patches [ask patch-here [set owner nobody
                                                                             set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)]
                                                                             ask fTokens-on patch-here [die]
                                                                             die]
            let chosenPlot myPlots with [farmStead = 0] with-min [wineQ]
            ask chosenPlot [set owner nobody set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself) set ageVines 0]
            ask fTokens-on chosenPlot [die]
            set myPlots myPlots with [not member? self patch-set chosenPlot]
            set capital capital + first [landPrice] of chosenPlot
          ]
        ]
;     1.2) There are no interesting plots
;       # SELL PLOT with the lowest quality
        [
          if  (myPlots with [farmStead = 0]) = no-patches [ask patch-here [set owner nobody
                                                                           set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)]
                                                                           ask fTokens-on patch-here [die]
                                                                           die]
          let chosenPlot myPlots with [farmStead = 0] with-min [wineQ]
          ask chosenPlot [set owner nobody set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself) set ageVines 0]
          ask fTokens-on chosenPlot [die]
          set myPlots myPlots with [not member? self patch-set chosenPlot]
          set capital capital + first [landPrice] of chosenPlot
        ]
      ]

;   2) Quatilty is higher than standard
;     # Just sell grapes and end.
      [
        set totalCost sum [fixCost] of myPlots + sum [varCost] of myPlots with [ageVines > 3]
        let totalRevenue ((giWinePrice * (wineYield * count myPlots with [ageVines >= 3 and giLabel = 1] )) + (stdWinePrice * (wineYield * count myPlots with [ageVines >= 3 and giLabel = 0])))
        set capital (capital - totalCost + totalRevenue)   ; Now they can only sell at GI price from plots with high quality
        set pastVintages insert-item (length pastVintages) pastVintages (mean [wineQ] of myPlots)        ; Save quality of vintage in farm's memory
        if ageVines < 20 [set ageVines ageVines + 1]
        set color green
        stop
      ]
    ]
  ]
end


to voting
end


to institutinalChange
end




to giBoardAction
  ask patches [ifelse wineQ < qualityStandard [set giLabel 0] [set giLabel 1]]    ; For now it doesn't make any sense, and it doesn't interact with Farms. You could do that every 5 years the GI area is redefined (exogenous area setting).
  refreshElev
  ask patches with [giLabel = 1] [set plabel-color black set plabel giLabel]
  ask patches with [giLabel = 0] [set plabel ""]
  ask patches [update-landPrices
    set giWinePrice 1]

end


to update-landPrices
  set landPrice ((wineQ * 10000) + (giLabel * 5000))
end




;-----------------------------------------

to refreshElev

let minelev (min [elevation] of patches)
let maxelev (max [elevation] of patches)

ask patches
  [
    if (elevation <= 0) or (elevation >= 0)
    [set pcolor palette:scale-scheme  "Divergent" "BrBG" 8 elevation maxelev minelev ]
    ]
end
@#$#@#$#@
GRAPHICS-WINDOW
71
10
659
599
-1
-1
5.8
1
6
1
1
1
0
0
0
1
0
99
0
99
1
1
1
Year
30.0

BUTTON
5
10
68
43
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
663
106
836
139
sigma_GaussKernel
sigma_GaussKernel
1
5
1.4
0.1
1
NIL
HORIZONTAL

MONITOR
664
154
753
199
Mean elevation
precision mean [elevation] of patches 3
17
1
11

MONITOR
754
154
836
199
SD elevation
precision standard-deviation [elevation] of patches 3
17
1
11

SLIDER
663
32
835
65
MinimumElevation
MinimumElevation
0
300
0.0
1
1
NIL
HORIZONTAL

SLIDER
663
69
835
102
MaximumElevation
MaximumElevation
300
1000
800.0
1
1
NIL
HORIZONTAL

SLIDER
847
32
1032
65
GSavgTemp
GSavgTemp
0
30
19.0
0.1
1
NIL
HORIZONTAL

TEXTBOX
665
12
815
30
RASTER
11
0.0
1

TEXTBOX
847
10
997
28
CLIMATE
11
0.0
1

CHOOSER
847
68
1032
113
ClimateScenario
ClimateScenario
"ParisAgreement" "BAU"
0

MONITOR
664
199
753
244
Max elevation
precision max [elevation] of patches 3
17
1
11

MONITOR
664
244
753
289
Min elevation
precision min [elevation] of patches 3
17
1
11

MONITOR
953
118
1030
163
Mean temp
precision mean [microclimate] of patches 3
17
1
11

MONITOR
953
168
1030
213
Max temp
precision max [microclimate] of patches 3
17
1
11

MONITOR
952
217
1030
262
Min temp
precision min [microclimate] of patches 3
17
1
11

BUTTON
663
297
723
330
Elevation
refreshElev
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
663
333
723
366
Slope
let minslope (min [slope] of patches)\nlet maxslope (max [slope] of patches)\n\nask patches\n  [\n    if (slope <= 0) or (slope >= 0)\n    [set pcolor palette:scale-scheme   \"Sequential\"  \"YlGnBu\" 8 slope minslope maxslope]\n    ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
663
369
723
402
Aspect
let minaspect (min [aspect] of patches)\nlet maxaspect (max [aspect] of patches)\n\nask patches\n  [\n    if (aspect <= 0) or (aspect >= 0)\n    [set pcolor palette:scale-scheme    \"Sequential\"  \"Greys\" 8 aspect minaspect maxaspect]\n    ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
847
117
950
150
Dry?
Dry?
0
1
-1000

TEXTBOX
854
270
1004
288
QUALITY
11
0.0
1

TEXTBOX
1052
13
1202
31
FARMERS
11
0.0
1

TEXTBOX
1260
14
1410
32
GI Board
11
0.0
1

MONITOR
730
296
794
341
Max slope
precision max [slope] of patches 3
17
1
11

MONITOR
730
342
795
387
Mean slope
precision mean [slope] of patches 3
17
1
11

MONITOR
730
389
794
434
Min slope
precision min [slope] of patches 3
17
1
11

SLIDER
847
300
1032
333
Opt_GSavgTemp
Opt_GSavgTemp
0
30
21.0
0.1
1
NIL
HORIZONTAL

BUTTON
847
372
920
405
Wine Quality
\nlet minWineQ  min  [WineQ] of patches\nlet maxWineQ  max  [WineQ] of patches\n\nask patches\n  [\n    if (WineQ <= 0) or (WineQ >= 0)\n    [set pcolor palette:scale-scheme \"Sequential\" \"Purples\" 8 WineQ minWineQ maxWineQ]\n    ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
847
155
950
188
Microclimate
let minmicroclim (min [microclimate] of patches)\nlet maxmicroclim (max [microclimate] of patches)\nask patches\n  [\n    if (microclimate <= 0) or (microclimate >= 0)\n    [set pcolor palette:scale-scheme  \"Sequential\"  \"Reds\" 8 microclimate minmicroclim maxmicroclim]\n    ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
847
336
921
369
Soil Quality
let minSQ min [SoilQ] of patches \nlet maxSQ max [SoilQ] of patches \n\nask patches\n  [\n    if (WineQ <= 0) or (WineQ >= 0)\n    [set pcolor palette:scale-scheme \"Sequential\" \"Greens\" 8 SoilQ minSQ maxSQ]\n    ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
923
335
1033
368
SmoothS
SmoothS
1
10
7.0
1
1
NIL
HORIZONTAL

SLIDER
923
372
1033
405
climateW
climateW
0
1
0.6
0.01
1
NIL
HORIZONTAL

BUTTON
847
409
922
442
Show
  \nlet minWineQ  min  [WineQ] of patches\nlet maxWineQ  max  [WineQ] of patches\n\nask patches\n  [\n    if (WineQ <= 0) or (WineQ >= 0)\n    [set pcolor palette:scale-scheme \"Sequential\" \"Purples\" 9 WineQ minWineQ maxWineQ]\n    ]\n  \n  \n  \n  let bestp max-n-of top patches [wineQ]\n  ask bestp [set pcolor yellow]\n  
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
924
409
1033
442
top
top
1
2000
2000.0
1
1
NIL
HORIZONTAL

BUTTON
849
445
923
478
Show
\nlet minWineQ  min  [WineQ] of patches\nlet maxWineQ  max  [WineQ] of patches\n\nask patches\n  [\n    if (WineQ <= 0) or (WineQ >= 0)\n    [set pcolor palette:scale-scheme \"Sequential\" \"Purples\" 9 WineQ minWineQ maxWineQ]\n    ]\n\n\nask patches [\nif wineQ > Quality_> [set pcolor yellow]]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
924
445
1033
478
Quality_>
Quality_>
0
1
0.78
0.01
1
NIL
HORIZONTAL

SLIDER
1044
33
1228
66
nFarms
nFarms
1
1000
679.0
1
1
NIL
HORIZONTAL

MONITOR
849
482
944
527
% Patches > 0.8
(count patches with [wineq > 0.8]) / count patches
17
1
11

BUTTON
4
51
68
84
GO 50
go\nif ticks = 50 [stop]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1044
104
1137
137
giArea
giArea
676
7324
2210.0
1
1
NIL
HORIZONTAL

MONITOR
1044
140
1103
185
GI AREA
(count patches with [giLabel = 1])
17
1
11

SWITCH
1044
67
1152
100
manualArea?
manualArea?
1
1
-1000

MONITOR
1104
141
1154
186
Farms
count farms
17
1
11

MONITOR
945
482
1037
527
Quality Standard
precision qualityStandard 4
17
1
11

MONITOR
1154
141
1239
186
Mean Capital
precision mean [capital] of farms 2
17
1
11

TEXTBOX
1047
239
1235
313
TWO WIDELY DIFFERENT SCENARIOS HERE: \nif you allow farmers to buy new patches from in a certain radius around the farmsted or only in the neighbourhood of its own plots. 
9
15.0
1

SWITCH
1046
299
1138
332
inRadius?
inRadius?
0
1
-1000

SLIDER
1139
299
1231
332
radius
radius
1
10
3.0
1
1
NIL
HORIZONTAL

PLOT
1046
339
1232
467
Count GI-AREA
Time
Number
0.0
50.0
0.0
10000.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -8630108 true "" "plot count patches with [giLabel = 1]"

PLOT
1046
470
1233
602
Count Farms
Time
Number
0.0
50.0
0.0
1000.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot count Farms"

MONITOR
1154
189
1240
234
Mean Size
precision mean [count myPlots] of farms 2
17
1
11

BUTTON
4
91
68
124
GO once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
851
532
926
577
Mean Quality
precision mean [wineQ] of patches 3
17
1
11

MONITOR
928
532
978
577
min
precision min [wineQ] of patches 3
17
1
11

MONITOR
980
532
1030
577
max
precision max [wineQ] of patches 3
17
1
11

PLOT
1046
611
1235
745
Mean Capital
TIME
Euros
0.0
50.0
0.0
1000000.0
true
false
"" ""
PENS
"default" 1.0 0 -11085214 true "" "plot precision mean [capital] of farms 2"

@#$#@#$#@
## NEXT




## WHAT IS IT?

With a R-Netlogo interaction we create a 100x100 random raster for elevation using unform distribution and a Gaussian Kernel (neighbourhood = 9) to generate spatial autocorrelation and smooth the lattice.

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

gi
false
0
Rectangle -7500403 false true 60 60 165 90
Rectangle -7500403 false true 45 60 75 255
Rectangle -7500403 false true 45 225 150 255
Rectangle -7500403 false true 135 180 165 255
Rectangle -7500403 false true 105 165 165 195
Rectangle -7500403 false true 210 60 240 255

giboard
false
0
Circle -7500403 true true 103 13 95
Rectangle -7500403 true true 105 120 195 285

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

square 3
false
0
Rectangle -7500403 true true 0 0 300 30
Rectangle -7500403 true true 0 0 30 300
Rectangle -7500403 true true 270 0 300 300
Rectangle -7500403 true true 0 270 300 300

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
