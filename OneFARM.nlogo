breed [farms farm]         ; N. agents
breed [giBoards giBoard]   ; One agent
breed [ftokens ftoken]     ; Just Farms' patches tokens to visualise farms

globals [
  nFarms           ; 100
  wineYield        ; 50 hl/ha
  GSavgTemp        ; 19
  Opt_GSavgTemp    ; 21
  qualityStandard  ; mean wineQ of all starting vine plots
  giWinePrice      ; 1.5
  stdWinePrice     ; 1
  reshapeArea      ; Count 5 years reshape GI AREA
  bankruptcies
  globalPlantingRights
]
patches-own [
  elevation    ; 0-800 Interval randomly gen and smoothed
  microclimate
  soilQ        ; 0-1 Interval randomly gen and smoothed
  wineQ        ; 0-1 Interval wighted function microclimate and soilQ
  ageVines     ; Important, only after 3 years can produce
  owner        ; Farm number or nobody
  pastOwners   ; Agentset all past owners
  farmStead    ; Bolean
  fallow       ; Plot left fallow (with vines)
  varCost      ; For productive plots (ageVines >3); Increasing with slope
  fixCost      ; For each plot owned
  giLabel      ; Bolean
  landPrice    ; Function of Quality and GI label
]

farms-own [
  capital
  myPlots      ; Agentset farm's land
  myQuality    ; Avg quality of myPlots
  plantingRights
  pastVintages ; List, Memory on past vintages' quality
]


;# ###################### #
;# --- Initialisation --- #
;# ###################### #


to setup
  clear-all
  initiate-globals
  setup-elevation
  setup-microclimate
  setup-soilQ
  setup-quality
  setup-patches
  setup-farmers
  setup-giArea
  setup-landPrice
  reset-ticks
end

to initiate-globals
  set wineYield 5000
  set nFarms 100
  set giWinePrice 1.5
  set stdWinePrice 1
  ask patches [set varCost 3000 set fixCost 2000]    ; is you sell wine at normal price of 1 can barely cover variable costs of one hectare
;  set reshapeArea 0                                 ; only if GI board reshapes GI area every 5 years NOT IMPLEMENTED!
end


;# --Patches

to setup-elevation
  ask patches [set elevation (random-float 800)]
  repeat SmoothElev [ diffuse elevation 0.5]
end

to setup-soilQ
  ask patches [set soilQ (random-float 1)]
  repeat SmoothSoil [ diffuse soilQ 0.5]
end

to setup-microclimate
  set GSavgTemp 19
  let max-elevation max [elevation] of patches
  ask patches [set microclimate GSavgTemp + 0.0098 * (max-elevation - elevation)]
end

to setup-quality
  set Opt_GSavgTemp 21
  ask patches [set wineQ (0.4 * soilQ ) + (0.6 * (1 - (abs((Opt_GSavgTemp - microclimate)/ Opt_GSavgTemp))))]
end

to setup-patches
resize-world 0 20 0 20

  let minquality (min [wineQ] of patches)
  let maxquality (max [wineQ] of patches)

  ask patches [
    set pcolor scale-color violet WineQ maxquality minquality
    set owner nobody
    set pastOwners []
  ]
end


;# --Farmers

to setup-farmers
  set-default-shape farms "person"
  set-default-shape ftokens "square 3"

; MAX ONE FARMS PER PLOT and differenciate color from neighbouring farms
  ask patch 10 10 [
    sprout-farms 1
    set owner [who] of farms-here
    set farmStead 1
    set ageVines 20  ; let's assume that age is kept constant to 20 years in each hectare age in only interesting when new vines are installed
    ask farms-here [set color orange]
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
    set pastVintages (list)
; Capital higher for bigger farms
    set capital 5000 * (count myPlots)
  ]
end


to setup-giArea
  set qualityStandard mean [wineQ] of patches with [any? fTokens-here]  ; Quality standard as average of all farms quality

; GI AREA DEFINED with QUALITY STANDARD
  let giPatches patches with [wineQ >= qualityStandard]
  ask giPatches [set giLabel 1
                  set plabel-color yellow set plabel giLabel]
end


to setup-landPrice
  ask patches [set landPrice ((wineQ * 10000) + (giLabel * 5000))]
end





;# ##################################### #
;# --- Model Dynamics and Iterations --- #
;# ##################################### #


to go
  climateChange
  operationalArena
  collectiveChoiceArena
  tick
end


to climateChange

; INCREASE TEMPERATURE AND UPDATE PATCHES WINE QUALITY  (YOU CAN MAKE IT MUCH LIGHTER AND FAST)
  ask patches [
    ifelse ClimateScenario = "Paris"
    [set microclimate microclimate + 0.02
     update-wineQuality]
    [set microclimate microclimate + 0.035
     update-wineQuality]
  ]
  refreshWorld
end

to update-wineQuality
  set wineQ ((0.4 * SoilQ) + 0.6 * (1 - (abs((Opt_GSavgTemp - microclimate)/ Opt_GSavgTemp))))
end



to operationalArena
  giBoardAction
  farmersHeuristic
;  ask farms [newHeuristic]
;  ask farms [farmersOptimisation]
end


;--- FARMS related----------------------------------------------------------

to farmersHeuristic
  ask farms
  [
    heuristic
  ]
end

to heuristic

; # Reactivate fallow plots that are now suitable (it could happen with high elevation plots and with institutional change)
  print "###-NEW VINTAGE!-###"
  let goodFallow  myPlots with [fallow = 1 and wineQ > qualityStandard ]
  ask goodFallow [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches) print "LOWQ to HIGHQ!"]

  let myProdPlots myPlots with [ageVines > 3 and fallow = 0]

; # If no productive plots (e.g., just planted plots and fallow plots, wait and update capital).
  if not any? myProdPlots
  [
    print "NO PRODUCTIVE PLOTS!"
    let totalCost sum [fixCost] of myPlots + sum [varCost] of myProdPlots

    if capital < totalCost
;   # Bankruptcy EXIT -------------------------------------------------------------------------
    [
      set bankruptcies bankruptcies + 1
      ifelse myquality > qualitystandard [print "Dead high"][print "Dead low"]
      type "Bankrupcies: " print bankruptcies
      ask myPlots [
        set owner nobody
        set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)
        ask fTokens-on self [die]
        set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)
      ]
      set globalPlantingRights globalPlantingRights + plantingRights
      die
    ]
;   #  ---------------------------------------------------------------------------------------

;   # Update Capital
    let profit  (- totalCost)
    type "Profit = " print profit
    set capital (capital + profit)
;   # Update age of vines
    ask myPlots [if ageVines < 20 [set ageVines ageVines + 1]]                                           ; Assume that farmers manage vines to keep average age no higher than 20.

;   # Skip sell wine and go to land module
    sellBuyPlots
    stop
  ]

; # Check Average quality, and expected profit

  set myQuality mean [wineQ] of myProdPlots
  let totalCost sum [fixCost] of myPlots + sum [varCost] of myProdPlots

; # TWO options, selling all GI or selling GI and standard wine (two different revenues)
  let totalRevenue ifelse-value myQuality > qualityStandard
  [(giWinePrice * (wineYield * count myProdPlots ))]   ;If average quality higher than standard you sell all production at higher price
  [((giWinePrice * (wineYield * count myProdPlots with [giLabel = 1] )) + (stdWinePrice * (wineYield * count myProdPlots with [giLabel = 0])))] ;If average quality lower than standard you sell only higher quality plot at higher price

; # Calculation of first expected profit
  let eProfit1 totalRevenue - totalCost

  ifelse myQuality < qualityStandard

; # 1) Quatilty is lower than standard can try to increase profit by letting one plot fallow and sell all GI
  [ print "Avg Quality < Quality Std"
;   # Leave worse plot fallow if it increases profits (confront profits of
    let chosenPlot myProdPlots with-min [wineQ]
    let myProdPlots_new myProdPlots with [not member? self patch-set chosenPlot]

    ifelse any? myProdPlots_new
    [
      let myQuality_new mean [wineQ] of myProdPlots_new
      let totalCost_new sum [fixCost] of myPlots + sum [varCost] of myProdPlots_new

;     # Recalculate revenues and expected profit as before
      let totalRevenue_new ifelse-value myQuality_new > qualityStandard
      [(giWinePrice * (wineYield * count myProdPlots_new ))]   ;If average quality higher than standard you sell all production at higher price
      [((giWinePrice * (wineYield * count myProdPlots_new with [giLabel = 1] )) + (stdWinePrice * (wineYield * count myProdPlots_new with [giLabel = 0])))] ;If average quality lower than standard you sell only higher quality plot at higher price

      let eProfit2 totalRevenue_new - totalCost_new

      ifelse eProfit2 > eProfit1
      [
        print "ONE PLOT SET FALLOW!"
        ask chosenPlot [set fallow 1 set pcolor black]
        set myProdPlots myProdPlots_new
        set myQuality myQuality_new
        ifelse myQuality > qualityStandard [print "ALL GI WINE!"] [ifelse any?  myProdPlots with [giLabel = 1] [print "MIXED GI AND STD WINE!"] [print "ONLY STD WINE!"]]
      ]
      [
        ifelse any?  myProdPlots with [giLabel = 1]
        [
          let stillProfPlots myPlots with [fallow = 1 and (fixCost + varCost) <= wineYield * stdWinePrice]    ; REACTIVATE low quality plot still profitable at standard wine price
          ask stillProfPlots [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
          print "MIXED GI AND STD WINE!"
        ]
        [
          let stillProfPlots myPlots with [fallow = 1 and (fixCost + varCost) <= wineYield * stdWinePrice]    ; REACTIVATE low quality plot still profitable at standard wine price
          ask stillProfPlots [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
          print "ONLY STD WINE!"
        ]
      ]
    ]

    [
      let totalCost_new sum [fixCost] of myPlots   ; here no productive plot only fixed costs
      let totalRevenue_new 0
      let eProfit2 (- totalCost_new)

      ifelse eProfit2 > eProfit1
      [
        print "LAST PLOT SET FALLOW!"
        ask chosenPlot [set fallow 1 set pcolor black]
        set myProdPlots nobody
        set myQuality 0

        print "NO PRODUCTIVE PLOTS!"
        set totalCost sum [fixCost] of myPlots
        if capital < totalCost
        [
          set bankruptcies bankruptcies + 1
          ifelse myquality > qualitystandard [print "Dead high"][print "Dead low"]
          type "Bankrupcies: " print bankruptcies
          ask myPlots [
            set owner nobody
            set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)
            ask fTokens-on self [die]
            set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)
          ]
          set globalPlantingRights globalPlantingRights + plantingRights
          die
        ]
        let profit  (- totalCost)
        type "Profit = " print profit
        set capital (capital + profit)      ; Now they can only sell at GI price from plots with high quality

        ask myPlots [if ageVines < 20 [set ageVines ageVines + 1]]                                           ; Assume that farmers manage vines to keep average age no higher than 20.
        sellBuyPlots
        stop
      ]
      [
        ifelse any?  myProdPlots with [giLabel = 1]
        [
          let stillProfPlots myPlots with [fallow = 1 and (fixCost + varCost) <= wineYield * stdWinePrice]    ; REACTIVATE low quality plot still profitable at standard wine price
          ask stillProfPlots [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
          print "MIXED GI AND STD WINE!"
        ]
        [
          let stillProfPlots myPlots with [fallow = 1 and (fixCost + varCost) <= wineYield * stdWinePrice]   ; REACTIVATE low quality plot still profitable at standard wine price
          ask stillProfPlots [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
          print "ONLY STD WINE!"
        ]
      ]
    ]
  ]

  [
    print "Avg Quality > Quality Std"
    print "ALL GI WINE!"
  ]

  sellWine
  sellBuyPlots
end



  to sellWine
; # 2) SELL WINE
  let myProdPlots myPlots with [ageVines > 3 and fallow = 0]
  let totalCost sum [fixCost] of myPlots + sum [varCost] of myProdPlots
  if capital < totalCost
  [
    set bankruptcies bankruptcies + 1
    ifelse myquality > qualitystandard [print "Dead high"][print "Dead low"]
    type "Bankrupcies: " print bankruptcies
    ask myPlots [
      set owner nobody
      set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)
      ask fTokens-on self [die]
      set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)
    ]
    set globalPlantingRights globalPlantingRights + plantingRights
    die
  ]
; # Calculate final profit and and update capital
  let totalRevenue ifelse-value myQuality > qualityStandard
  [(giWinePrice * (wineYield * count myProdPlots ))]   ;If average quality higher than standard you sell all production at higher price
  [((giWinePrice * (wineYield * count myProdPlots with [giLabel = 1] )) + (stdWinePrice * (wineYield * count myProdPlots with [giLabel = 0])))]

  let profit totalRevenue - totalCost
  type "Profit = " print profit
  set capital (capital + profit)      ; Now they can only sell at GI price from plots with high quality

  set pastVintages insert-item (length pastVintages) pastVintages (mean [wineQ] of myProdPlots)        ; Save quality of vintage in farm's memory
  ask myPlots [if ageVines < 20 [set ageVines ageVines + 1]]                                           ; Assume that farmers manage vines to keep average age no higher than 20.
end



  to sellBuyPlots
; # 3) Buy new plot

; # Is my quality decreasing in the last three years? I need to get new plots!
  if length pastVintages > 3
  [
    let meanPV mean (sublist pastVintages 0 4)
    if meanPV > myQuality
    [
      print "MY QUALITY IS DECREASING!"
      sellLowestQplot
      if plantingRights > 0 [buyNewPlot]
    ]
    if meanPV < myQuality
    [ print "MY QUALITY IS INCREASING!"]
  ]
  print " "
end



to sellLowestQplot

; # In this way the farmers will sell all low elevation plots up to the point in which the lowest plot is the farmstead plot.
  let chosenPlot myPlots with [farmStead = 0 and elevation < mean [elevation] of [myPlots] of myself] with-min [wineQ]

  ifelse any? chosenPlot
  [ print "SOLD A PLOT!"
    ask chosenPlot
    [
      set owner nobody set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself) set ageVines 0
      set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)
    ]
    ask fTokens-on chosenPlot [die]

    set myPlots myPlots with [not member? self patch-set chosenPlot]
    set capital capital + first [landPrice] of chosenPlot
    set plantingRights plantingRights + 1
  ]
  [print "NO PLOT TO SELL!"]

  type "PLANTING RIGHTS: " print plantingRights       ; They could capitalise the plainting rights by selling them to other farms. NOT IMPLEMENTED NOW.
end



to buyNewPlot
  ; # Check neighbouring plots with quality higher than standard and no owner

  let interestingPlots ifelse-value inRadius?
  [patch-set (patches in-radius radius with [wineQ > qualityStandard and owner = nobody and (member? [who] of myself pastOwners) = false and elevation > mean [elevation] of [myPlots] of myself])]   ; they know a minimum about the elevation trick!
  [patch-set ([neighbors] of myPlots with [wineQ > qualityStandard and owner = nobody and (member? [who] of myself pastOwners) = false and elevation > mean [elevation] of [myPlots] of myself])]      ; [neighbors] of myPlots or patches in-radius x? include distance from farmstead in cost calculation

  if any? interestingPlots
  [
    let installCost 3 * fixCost

    if capital >= (first ([landPrice] of interestingPlots with-max [wineQ])) + installCost + fixCost * count myPlots with [ageVines < 3] ; the installation cost is the fix cost of maintaning the new implatented vineyard which is going to be non productive for 3 years.
    [
      print "BOUGHT A PLOT!"
      let chosenPlot interestingPlots with-max [wineQ]
      ask chosenPlot [set owner [who] of myself                                        ; ATTENTION HERE EACH FARM DOES THIS ONE AFTER THE OTHER, SOMEBODY WILL RANDOMLY BE SELECTED TO MOVE FIRST THEREFORE HAVING MORE PLOTS TO CHOOSE FROM.
                      sprout-fTokens 1
                      set ageVines 0]
      ask fTokens-on chosenPlot [set color [color] of myself]
      set myPlots (patch-set myPlots chosenPlot)
      set capital capital - first [landPrice] of chosenPlot
      set plantingRights plantingRights - 1
      type "PLANTING RIGHTS: " print plantingRights
    ]
  ]
end


;------------------------------------------------------------------------------


;--- GI BOARD related----------------------------------------------------------

to giBoardAction
   ask patches [ifelse wineQ < qualityStandard [set giLabel 0] [set giLabel 1]]
   ask patches with [giLabel = 1] [set plabel-color yellow set plabel giLabel]
   ask patches with [giLabel = 0] [set plabel ""]
   ask patches [ update-landPrices ]

end


to update-landPrices
  set landPrice ((wineQ * 10000) + (giLabel * 5000))
end

;-------------------------------------------------------------------------------


to collectiveChoiceArena
  institutinalChange
end

to institutinalChange
end





;------------------------------------------------------------------------------------
;#------ graphic stuff
;------------------------------------------------------------------------------------


to refreshworld
  let minquality (min [wineQ] of patches)
  let maxquality (max [wineQ] of patches)

  ask patches with [fallow = 0] [set pcolor scale-color violet WineQ maxquality minquality]
end





;------------------------------------------------------------------------------------
;# NEW HEURISTIC trials
;------------------------------------------------------------------------------------

;to newHeuristic
;
;  set myQuality mean [wineQ] of myPlots with [ageVines > 3]
;  set totalCost sum [fixCost] of myPlots + sum [varCost] of myPlots with [ageVines > 3]
;
;  let totalRevenue ifelse-value myQuality > qualityStandard
;  [(giWinePrice * (wineYield * count myPlots with [ageVines >= 3] ))]   ;If average quality higher than standard you sell all production at higher price
;  [((giWinePrice * (wineYield * count myPlots with [ageVines >= 3 and giLabel = 1] )) + (stdWinePrice * (wineYield * count myPlots with [ageVines >= 3 and giLabel = 0])))] ;If average quality lower than standard you sell only higher quality plot at higher price
;  let eProfit1 totalRevenue - totalCost
;
;  print eProfit1
;  ifelse myQuality > qualityStandard [print "allright!"] [print "NOT allright!"]
;
;  set capital (capital + eProfit1)                                             ; Now they can only sell at GI price from plots with high quality
;  set pastVintages insert-item 0 pastVintages (mean [wineQ] of myPlots)        ; Save quality of vintage in farm's memory (first element is the most recent quality
;  ask myPlots [if ageVines < 20 [set ageVines ageVines + 1] ]                                                  ; Assume that farmers manage vines to keep average age no higher than 20]
;
;  if length pastVintages > 5
;  [ let meanPV mean (sublist pastVintages 0 6)
;    if meanPV > myQuality [buyCheaphighquality]
;  ]
;
;  if capital < 0  [
;    set bankruptcies bankruptcies + 1
;    show bankruptcies
;    ask myPlots [
;      set owner nobody
;      set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)
;      ask fTokens-on self [die]
;    ]
;    die
;  ]
;
;
;
;end
;
;;  let bauProfit totalRevenue - totalCost
;
;
;to ...
;  if capital < totalCost  [
;                            set bankruptcies bankruptcies + 1
;                            show bankruptcies
;                            ask myPlots [
;                                          set owner nobody
;                                          set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)
;                                          ask fTokens-on self [die]
;                                        ]
;                            die
;                          ]
;
;
;
;  let totalRevenue ((giWinePrice * (wineYield * count myPlots with [ageVines >= 3 and giLabel = 1] )) + (stdWinePrice * (wineYield * count myPlots with [ageVines >= 3 and giLabel = 0])))
;  set capital (capital - totalCost + totalRevenue)      ; Now they can only sell at GI price from plots with high quality
;  set pastVintages insert-item (length pastVintages) pastVintages (mean [wineQ] of myPlots)        ; Save quality of vintage in farm's memory
;  if ageVines < 20 [set ageVines ageVines + 1]                                                     ; Assume that farmers manage vines to keep average age no higher than 20]
;
;
; ;   # Check QUALITY
;  set myQuality mean [wineQ] of myPlots with [ageVines > 3]
;
;  ifelse myQuality < qualityStandard
;  [
;    buyCheaphighquality
;;    sellLowestQplot
;  ]
;  [stop]
;end
;
;
;
;
;to buyCheaphighquality
;  let interestingPlots ifelse-value inRadius? [patch-set (patches in-radius radius with [wineQ > qualityStandard and elevation > mean [elevation] of [myPlots] of myself and owner = nobody and (member? [who] of myself pastOwners) = false])]
;                                              [patch-set ([neighbors] of myPlots with [wineQ > qualityStandard and owner = nobody and (member? [who] of myself pastOwners) = false])]      ; [neighbors] of myPlots or patches in-radius x? include distance from farmstead in cost calculation
;
;  if any? interestingPlots
;  [
;    let installCost fixCost
;
;    if capital >= (first ([landPrice] of interestingPlots with-max [wineQ])) + totalCost + installCost ; must also count the maintenance cost of the plot you want to buy (there should be an installation cost)
;    [
;      let chosenPlot interestingPlots with-max [wineQ]
;      ask chosenPlot [set owner [who] of myself                                        ; ATTENTION HERE EACH FARM DOES THIS ONE AFTER THE OTHER, SOMEBODY WILL RANDOMLY BE SELECTED TO MOVE FIRST THEREFORE HAVING MORE PLOTS TO CHOOSE FROM.
;                      sprout-fTokens 1
;                      set ageVines 1]
;      ask fTokens-on chosenPlot [set color [color] of myself]
;      set myPlots (patch-set myPlots chosenPlot)
;      set capital capital - first [landPrice] of chosenPlot
;    ]
;  ]
;end
;
;to sellLowestQplot
;  if  (myPlots with [farmStead = 0]) = no-patches [ask patch-here [set owner nobody
;                                                   set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself)]
;                                                   ask fTokens-on patch-here [die]
;                                                   die]
;  let chosenPlot myPlots with [farmStead = 0 and elevation < mean [elevation] of [myPlots] of myself] with-min [soilQ]
;  if any? chosenPlot
;  [
;    ask chosenPlot [set owner nobody set pastOwners insert-item (length pastOwners) pastOwners ([who] of myself) set ageVines 0]
;    ask fTokens-on chosenPlot [die]
;    set myPlots myPlots with [not member? self patch-set chosenPlot]
;    set capital capital + first [landPrice] of chosenPlot
;  ]
;end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
480
481
-1
-1
22.0
1
8
1
1
1
0
0
0
1
0
20
0
20
0
0
1
ticks
30.0

SLIDER
490
91
669
124
SmoothElev
SmoothElev
0
10
6.0
1
1
NIL
HORIZONTAL

BUTTON
490
10
553
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

MONITOR
492
177
553
222
mean elev
precision mean [elevation] of patches 2
17
1
11

MONITOR
556
177
611
222
max elev
precision max [elevation] of patches 2
17
1
11

MONITOR
615
177
667
222
min elev
precision min [elevation] of patches 2
17
1
11

MONITOR
492
231
551
276
mean clim
precision mean [microclimate] of patches 2
17
1
11

MONITOR
556
232
615
277
max clim
precision max [microclimate] of patches 2
17
1
11

MONITOR
617
232
668
277
min clim
precision min [microclimate] of patches 2
17
1
11

MONITOR
491
286
565
331
WineQ mean
precision mean [wineQ] of patches 2
17
1
11

SLIDER
490
133
671
166
SmoothSoil
SmoothSoil
0
10
7.0
1
1
NIL
HORIZONTAL

MONITOR
567
286
617
331
max
precision max [wineQ] of patches 2
17
1
11

MONITOR
618
286
668
331
min
precision min [wineQ] of patches 2
17
1
11

MONITOR
491
340
569
385
Quality STD
precision qualityStandard 3
17
1
11

CHOOSER
716
11
854
56
ClimateScenario
ClimateScenario
"Paris" "BAU"
1

BUTTON
872
16
938
49
GO 50
repeat 50\n[\ngo\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
558
14
708
56
Map shows quality (dark = high)\nGI area: patches with lable \"1\"
10
0.0
1

SWITCH
709
250
805
283
inRadius?
inRadius?
0
1
-1000

SLIDER
807
250
899
283
radius
radius
1
10
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
490
72
640
90
PATCHES SETUP
11
0.0
1

TEXTBOX
715
70
865
88
FARMS HEURISTICS
11
0.0
1

BUTTON
944
18
1023
51
GO once
go
NIL
1
T
OBSERVER
NIL
G
NIL
NIL
1

MONITOR
572
340
652
385
GI AREA
precision ((count patches with [giLabel = 1])/(count patches)) 2
17
1
11

MONITOR
712
105
786
150
NIL
count farms
17
1
11

MONITOR
789
104
864
149
Mean Capital
precision mean [capital] of farms 2
17
1
11

MONITOR
867
103
936
148
Mean Size
precision mean [count myPlots] of farms 2
17
1
11

TEXTBOX
708
200
1019
241
TWO WIDELY DIFFERENT SCENARIOS HERE: \nif you allow farmers to buy new patches from in a certain radius around the farmsted or only in the neighbourhood of its own plots. 
10
14.0
1

PLOT
873
294
1033
414
GI AREA
TIME
N
0.0
50.0
0.0
441.0
true
false
"" ""
PENS
"default" 1.0 0 -7858858 true "" "plot count patches with [giLabel = 1]"

PLOT
1035
294
1195
414
Average Capital
TIME
EURO
0.0
50.0
0.0
1000000.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot precision mean [capital] of farms 2"

PLOT
710
294
870
414
Mean Quality
TIME
Quality
0.0
50.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -955883 true "" "plot precision mean [wineQ] of patches 2"

MONITOR
940
103
1051
148
LowQuality Farms
count farms with [myquality < qualitystandard]
17
1
11

MONITOR
1053
102
1116
147
Bankrupt
bankruptcies
17
1
11

MONITOR
712
154
786
199
Total Output
wineYield * count fTokens-on patches with [ageVines > 3 and fallow = 0]
17
1
11

MONITOR
492
391
591
436
Mean Land Price
precision mean [landPrice] of patches 2
17
1
11

MONITOR
593
391
659
436
max
precision max [landPrice] of patches 2
17
1
11

MONITOR
593
440
659
485
min
precision min [landPrice] of patches 2
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

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
Rectangle -7500403 true true 0 0 30 300
Rectangle -7500403 true true 270 0 300 300
Rectangle -7500403 true true 0 0 300 30
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
