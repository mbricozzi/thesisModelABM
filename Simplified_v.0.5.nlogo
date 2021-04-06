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

  bankruptcies
  globalPlantingRights

  giFees
  collBrandValue
  qualityLev
  mktExpLev
  giFarmsLev
  giPrestige

  countdown
  ballotBox
  historyVotes
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
  profit
  myPlots      ; Agentset farm's land
  myQuality    ; Avg quality of myPlots
  plantingRights
  pastVintages ; List, Memory on past vintages' quality
  ballot       ; My vote to change quality standard
]

giBoards-own [
  capital
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
  setup-giBoard
  setup-giArea
  setup-landPrice
  reset-ticks
end

; # --- Globals

to initiate-globals
  set wineYield 5000
  set nFarms 100
  set giWinePrice 1.5
  set stdWinePrice 1
  ask patches [set varCost 3000 set fixCost 2000]    ; is you sell wine at normal price of 1 can barely cover variable costs of one hectare
;  set reshapeArea 0                                 ; only if GI board reshapes GI area every 5 years
  set ballotBox []                                   ; List will collect ballots for institutinal change
  set countdown everyXyears
  set historyVotes []
  set collBrandValue 0
  set qualityLev 0
  set mktExpLev 0
  set giFarmsLev 0
end


; # --- Patches

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
  set Opt_GSavgTemp OptiTemp
  ask patches [set wineQ (0.4 * soilQ ) + (0.6 * (1 - (abs((Opt_GSavgTemp - microclimate)/ Opt_GSavgTemp))))]
end

to setup-patches
resize-world 0 49 0 49

  let minquality (min [wineQ] of patches)
  let maxquality (max [wineQ] of patches)

  ask patches [
    set pcolor scale-color violet WineQ maxquality minquality
    set owner nobody
    set pastOwners []
  ]
end


; # --- Farmers

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
    set pastVintages (list)
; Capital higher for bigger farms
    set capital 5000 * (count myPlots)
  ]
end

to setup-giBoard
  create-giboards 1
  ask giBoards [set capital 100 * nFarms]
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

; --------------------------------------------------------------------------
; # Environmental Change
; --------------------------------------------------------------------------

to climateChange

; INCREASE TEMPERATURE AND UPDATE PATCHES WINE QUALITY
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

; --------------------------------------------------------------------------
; # Operational Arena
; --------------------------------------------------------------------------

to operationalArena
  farmersHeuristic
  giBoardAction
end


; # --- FARMS

to farmersHeuristic
  ask farms[ heuristic ]
end

to heuristic

; # Reactivate fallow plots that are now suitable (it could happen with high elevation plots)
;  print "###-NEW VINTAGE!-###"
  let goodFallow  myPlots with [fallow = 1 and wineQ > qualityStandard ]
  ask goodFallow [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches) ] ; print "LOWQ to HIGHQ!"

  let myProdPlots myPlots with [ageVines > 3 and fallow = 0]

; # If no productive plots (e.g., just planted plots and fallow plots, wait and update capital).
  if not any? myProdPlots [
;    print "NO PRODUCTIVE PLOTS!"
    let totalCost sum [fixCost] of (patch-set myProdPlots myPlots with [ageVines < 3]) + sum [varCost] of myProdPlots
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
    set profit  (- totalCost)
;    type "Profit = " print profit
    set capital (capital + profit)      ; Now they can only sell at GI price from plots with high quality

    ask myPlots [if ageVines < 20 [set ageVines ageVines + 1]]                                           ; Assume that farmers manage vines to keep average age no higher than 20.
    stop
  ]

; # Check Average quality, and expected profit
  set myQuality mean [wineQ] of myProdPlots
  let totalCost sum [fixCost] of (patch-set myProdPlots myPlots with [ageVines < 3]) + sum [varCost] of myProdPlots

; # TWO options, selling all GI or selling GI and standard wine (two different revenues)
  let totalRevenue ifelse-value myQuality > qualityStandard
  [(giWinePrice * (wineYield * count myProdPlots ))]   ;If average quality higher than standard you sell all production at higher price
  [((giWinePrice * (wineYield * count myProdPlots with [giLabel = 1] )) + (stdWinePrice * (wineYield * count myProdPlots with [giLabel = 0])))] ;If average quality lower than standard you sell only higher quality plot at higher price

; # Calculation of first expected profit
  let eProfit1 totalRevenue - totalCost

  ifelse myQuality < qualityStandard

; # 1) Quatilty is lower than standard can try to increase profit by letting one plot fallow and sell all GI
  [ ;print "Avg Quality < Quality Std"
;   # Leave worse plot fallow if it increases profits (confront profits of
    let chosenPlot myProdPlots with-min [wineQ]
    let myProdPlots_new myProdPlots with [not member? self patch-set chosenPlot]

    ifelse any? myProdPlots_new
    [
      let myQuality_new mean [wineQ] of myProdPlots_new
      let totalCost_new sum [fixCost] of (patch-set myProdPlots_new myPlots with [ageVines < 3]) + sum [varCost] of myProdPlots_new

;     # Recalculate revenues and expected profit as before
      let totalRevenue_new ifelse-value myQuality_new > qualityStandard
      [(giWinePrice * (wineYield * count myProdPlots_new ))]   ;If average quality higher than standard you sell all production at higher price
      [((giWinePrice * (wineYield * count myProdPlots_new with [giLabel = 1] )) + (stdWinePrice * (wineYield * count myProdPlots_new with [giLabel = 0])))] ;If average quality lower than standard you sell only higher quality plot at higher price

      let eProfit2 totalRevenue_new - totalCost_new

      ifelse eProfit2 > eProfit1
      [
;        print "ONE PLOT SET FALLOW!"
        ask chosenPlot [set fallow 1 set pcolor black]
        set myProdPlots myProdPlots_new
        set myQuality myQuality_new
;        if myQuality > qualityStandard [print "ALL GI WINE!"]
      ]
      [
        ifelse any?  myProdPlots with [giLabel = 1]
        [
          ask myPlots with [fallow = 1] [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
;          print "MIXED GI AND STD WINE!"
        ]
        [
;          print "ONLY STD WINE!"
          ask myPlots with [fallow = 1] [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
        ]
      ]
    ]

    [
      ifelse any?  myProdPlots with [giLabel = 1]
      [
        ask myPlots with [fallow = 1] [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
;        print "MIXED GI AND STD WINE!"
      ]
      [
;        print "ONLY STD WINE!"
        ask myPlots with [fallow = 1] [set fallow 0 set pcolor scale-color violet WineQ (max [wineQ] of patches) (min [wineQ] of patches)]
      ]
    ]
  ]

  [
;    print "Avg Quality > Quality Std"
;    print "ALL GI WINE!"
  ]
; # 2) SELL WINE

  set totalCost sum [fixCost] of (patch-set myProdPlots myPlots with [ageVines < 3]) + sum [varCost] of myProdPlots
  if capital < totalCost
  [
    set bankruptcies bankruptcies + 1
    ifelse myquality > qualitystandard [print "Dead high"][print "Dead low"]
;    type "Bankrupcies: " print bankruptcies
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
  set totalRevenue ifelse-value myQuality > qualityStandard
  [(giWinePrice * (wineYield * count myProdPlots ))]   ;If average quality higher than standard you sell all production at higher price
  [((giWinePrice * (wineYield * count myProdPlots with [giLabel = 1] )) + (stdWinePrice * (wineYield * count myProdPlots with [giLabel = 0])))]

  set profit totalRevenue - totalCost
  ;type "Profit = " print profit
  set capital (capital + profit)      ; Now they can only sell at GI price from plots with high quality

  set pastVintages insert-item (length pastVintages) pastVintages (mean [wineQ] of myProdPlots)        ; Save quality of vintage in farm's memory
  ask myPlots [if ageVines < 20 [set ageVines ageVines + 1]]                                           ; Assume that farmers manage vines to keep average age no higher than 20.


; # 3) Buy new plot

; # Is my quality decreasing in the last three years? I need to get new plots!
  if length pastVintages > 3
  [
    let meanPV mean (sublist pastVintages 0 4)
    if meanPV > myQuality
    [
      ;print "MY QUALITY IS DECREASING!"
      sellLowestQplot
      if plantingRights > 0 [buyNewPlot]
    ]
    if meanPV < myQuality
    [] ;[ print "MY QUALITY IS INCREASING!"]
  ]
;  print " "
end

to sellLowestQplot

; # In this way the farmers will sell all low elevation plots up to the point in which the lowest plot is the farmstead plot.
  let chosenPlot myPlots with [farmStead = 0 and elevation < mean [elevation] of [myPlots] of myself] with-min [wineQ]

  ifelse any? chosenPlot
  [ ;print "SOLD A PLOT!"
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
  [];  [print "NO PLOT TO SELL!"]

;  type "PLANTING RIGHTS: " print plantingRights       ; They could capitalise the plainting rights by selling them to other farms. NOT IMPLEMENTED NOW.
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
;      print "BOUGHT A PLOT!"
      let chosenPlot interestingPlots with-max [wineQ]
      ask chosenPlot [set owner [who] of myself                                        ; ATTENTION HERE EACH FARM DOES THIS ONE AFTER THE OTHER, SOMEBODY WILL RANDOMLY BE SELECTED TO MOVE FIRST THEREFORE HAVING MORE PLOTS TO CHOOSE FROM.
                      sprout-fTokens 1
                      set ageVines 0]
      ask fTokens-on chosenPlot [set color [color] of myself]
      set myPlots (patch-set myPlots chosenPlot)
      set capital capital - first [landPrice] of chosenPlot
      set plantingRights plantingRights - 1
    ]
  ]
end

;------------------------------------------------------------------------------


; # --- GI BOARD

to giBoardAction

  reshapeGIarea
  update-landPrices
  collectFees
  qualityControl
  investMarketing
  update-giPrestige
  update-winePrice

end

to reshapeGIarea
  ;  set reshapeArea reshapeArea + 1

;  if reshapeArea = 5                        ; Every fine years the GI board reassign the label to high quality plots  too complicated!
;  [

   ask patches [ifelse wineQ < qualityStandard [set giLabel 0] [set giLabel 1]]    ; For now it doesn't make any sense, and it doesn't interact with Farms. You could do that every 5 years the GI area is redefined (exogenous area setting).
   ask patches with [giLabel = 1] [set plabel-color yellow set plabel giLabel]
   ask patches with [giLabel = 0] [set plabel ""]
;  ]

;  if reshapeArea = 5 [set reshapeArea 0]
end

to update-landPrices
  ask patches [ set landPrice ((wineQ * 10000) + (giLabel * 5000)) ]
end

to collectFees
  ask farms
  [
    set capital capital - %fee * profit
    set giFees giFees + %fee * profit
  ]
end

to investMarketing

  ask giBoards
  [
    set capital capital + giFees
    let prevValue collBrandValue
    set collBrandValue (collBrandValue + (mktDelta * giFees) - (mktRho * collBrandValue))
    set capital capital - (mktDelta * giFees)
    set giFees 0
  ]
end

to qualityControl
  ask giBoards
  [
  set capital capital - ((1 - mktDelta) * giFees)   ; Suppose they make no profit, all is spent in quality controlls and all the rest invested in Marketing / collective brand value
  ]
end

to update-giPrestige

  let giProducers farms with [any? myPlots with [ageVines > 3 and giLabel = 1]]
  let giprodQuality ifelse-value any? patches with [ageVines > 3 and giLabel = 1]
  [mean [wineQ] of patches with [ageVines > 3 and giLabel = 1]] [0]

; # GI prestige in levels

; # QUALITY LEVELS
; # Level 1
  set qualityLev 0

  if  giprodQuality >= 0.5 and giprodQuality < 0.7
  [set qualityLev 1]

; # Level 2
  if  giprodQuality >= 0.7 and giprodQuality < 0.8
  [set qualityLev 2]

; # Level 3
  if  giprodQuality >= 0.8 and giprodQuality < 0.9
  [set qualityLev 3]

; # Level 4
  if  giprodQuality >= 0.9
  [set qualityLev 4]


; # Marketing Investment LEVELS
; # Level 1

  set mktExpLev 0

  if collBrandValue >= basBrandValue and collBrandValue < 2 * basBrandValue
  [set mktExpLev 1]

; # Level 2
  if collBrandValue >= 2 * basBrandValue and collBrandValue < 3 * basBrandValue
  [set mktExpLev 2]

; # Level 3
  if collBrandValue >= 3 * basBrandValue and collBrandValue < 4 * basBrandValue
  [set mktExpLev 3]

; # Level 4
  if collBrandValue >= 4 * basBrandValue
  [set mktExpLev 4]


; # GI Farms LEVELS
; # Level 1
  set giFarmsLev 0

  if (count giProducers / count farms) >= 0.5 and (count giProducers / count farms) < 0.7
  [set giFarmsLev 1]

; # Level 2
  if (count giProducers / count farms) >= 0.7 and (count giProducers / count farms) < 0.8
  [set giFarmsLev 2]

; # Level 3
  if (count giProducers / count farms) >= 0.8 and (count giProducers / count farms) < 0.9
  [set giFarmsLev 3]

; # Level 4
  if (count giProducers / count farms) >= 0.9
  [set giFarmsLev 4]



; ##########################

  set giPrestige round mean (list qualityLev mktExpLev giFarmsLev)

end



to update-winePrice
  If giPrestige = 0 [set giWinePrice 1]
  If giPrestige = 1 [set giWinePrice 1.5]
  If giPrestige = 2 [set giWinePrice 1.75]
  If giPrestige = 3 [set giWinePrice 2]
  If giPrestige = 4 [set giWinePrice 3]
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

to collectiveChoiceArena
  if votingMechanism != "NO VOTE" [institutionalChange]
  if ticks = 49 or ticks = 99 [type "VOTING HISTORY :" print historyVotes]
end

to institutionalChange
; # Each Farm vote
  set countdown countdown - 1
  if countdown = 0
  [
    set ballotBox []
    clear-output
    let lowerStandard precision (qualityStandard - qualityDelta) prec
    let higherStandard precision (qualityStandard + qualityDelta) prec

    ask farms
    [
      if length pastVintages > memory
      [
        let meanPV precision (mean (sublist pastVintages 0 (memory + 1))) prec

        if meanPV > precision myQuality prec
        [
          set ballot -1
          set ballotBox insert-item (length ballotBox) ballotBox ballot
        ]

        if meanPV < precision myQuality prec and higherStandard <= precision (myQuality) prec    ; You want to avoid that they increase the standard and they cannot even respect it.
        [
          set ballot 1
          set ballotBox insert-item (length ballotBox) ballotBox ballot
        ]

        if meanPV = precision myQuality prec
        [
          set ballot 0
          set ballotBox insert-item (length ballotBox) ballotBox ballot
        ]
      ]
    ]


;--------------------------------------------------------------------------
;   # RELATIVE MAJORITY
    if votingMechanism = "REL Majority"
    [
      set ballotBox sort ballotBox
      let freq map [ i -> frequency i ballotBox] (range -1 2)
      let mode modes ballotBox

      if mode = [1]
      [
        ifelse (qualityStandard + qualityDelta) < 1 [set qualityStandard qualityStandard + qualityDelta] [set qualityStandard 1]
        let result "INCREASE!"
        set historyVotes insert-item (length historyVotes) historyVotes result
        output-type "YEAR " output-print (ticks + 1)
        output-print "BALLOT COUNT:"
        output-print " D  C  I "
        output-print freq
        output-print result
      ]

      if mode = [-1]
      [
        ifelse (qualityStandard - qualityDelta) > 0.5 [set qualityStandard qualityStandard - qualityDelta] [set qualityStandard 0.5]
        let result "DECREASE!"
        set historyVotes insert-item (length historyVotes) historyVotes result
        output-type "YEAR " output-print (ticks + 1)
        output-print "BALLOT COUNT:"
        output-print " D  C  I "
        output-print freq
        output-print result
      ]
      if mode = [0]
      [
        let result "CONSTANT!"
        set historyVotes insert-item (length historyVotes) historyVotes result
        output-type "YEAR " output-print (ticks + 1)
        output-print "BALLOT COUNT:"
        output-print " D  C  I "
        output-print freq
        output-print result
      ]
      set countdown everyXyears
    ]


;--------------------------------------------------------------------------
;   # ABSOLUTE MAJORITY
    if votingMechanism = "ABS Majority"
    [
      set ballotBox sort ballotBox
      let freq map [ i -> frequency i ballotBox] (range -1 2)

      if item 2 freq >= (count farms / 2) + 1
      [
        ifelse (qualityStandard + qualityDelta) < 1 [set qualityStandard qualityStandard + qualityDelta] [set qualityStandard 1]
        let result "INCREASE!"
        set historyVotes insert-item (length historyVotes) historyVotes result
        output-type "YEAR " output-print (ticks + 1)
        output-print "BALLOT COUNT:"
        output-print " D  C  I "
        output-print freq
        output-print result
      ]

      if item 0 freq >= (count farms  / 2) + 1
      [
        ifelse (qualityStandard - qualityDelta) > 0.5 [set qualityStandard qualityStandard - qualityDelta] [set qualityStandard 0.5]
        let result "DECREASE!"
        set historyVotes insert-item (length historyVotes) historyVotes result
        output-type "YEAR " output-print (ticks + 1)
        output-print "BALLOT COUNT:"
        output-print " D  C  I "
        output-print freq
        output-print result
      ]
      if item 1 freq >= (count farms  / 2) + 1
      [
        let result "CONSTANT!"
        set historyVotes insert-item (length historyVotes) historyVotes result
        output-type "YEAR " output-print (ticks + 1)
        output-print "BALLOT COUNT:"
        output-print " D  C  I "
        output-print freq
        output-print result
      ]
      set countdown everyXyears
    ]
  ]


end

to-report frequency [an-item a-list]
    report length (filter [ i -> i = an-item] a-list)
end


;# ##################### #
;# --- Graphic Stuff --- #
;# ##################### #

to refreshworld
  let minquality (min [wineQ] of patches)
  let maxquality (max [wineQ] of patches)

  ask patches with [fallow = 0] [set pcolor scale-color violet WineQ maxquality minquality]
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
618
619
-1
-1
12.0
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
49
0
49
0
0
1
ticks
30.0

SLIDER
632
72
747
105
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
632
10
695
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
633
155
694
200
mean elev
precision mean [elevation] of patches 2
17
1
11

MONITOR
698
155
753
200
max elev
precision max [elevation] of patches 2
17
1
11

MONITOR
757
155
809
200
min elev
precision min [elevation] of patches 2
17
1
11

MONITOR
633
211
692
256
mean clim
precision mean [microclimate] of patches 2
17
1
11

MONITOR
698
212
757
257
max clim
precision max [microclimate] of patches 2
17
1
11

MONITOR
758
212
809
257
min clim
precision min [microclimate] of patches 2
17
1
11

MONITOR
633
262
707
307
WineQ mean
precision mean [wineQ] of patches 2
17
1
11

SLIDER
632
113
747
146
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
709
262
759
307
max
precision max [wineQ] of patches 2
17
1
11

MONITOR
760
262
810
307
min
precision min [wineQ] of patches 2
17
1
11

MONITOR
633
314
711
359
Quality STD
precision qualityStandard 3
17
1
11

CHOOSER
859
10
997
55
ClimateScenario
ClimateScenario
"Paris" "BAU"
0

BUTTON
1014
14
1080
47
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
702
10
843
50
Map shows quality (dark = high)\nGI area: patches with lable \"1\"
10
0.0
1

SWITCH
860
119
956
152
inRadius?
inRadius?
1
1
-1000

SLIDER
958
119
1050
152
radius
radius
1
10
3.0
1
1
NIL
HORIZONTAL

TEXTBOX
633
53
783
71
PATCHES SETUP
11
0.0
1

TEXTBOX
860
60
966
79
FARMS HEURISTICS
11
0.0
1

BUTTON
1088
15
1167
48
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
716
314
796
359
GI AREA
(count patches with [giLabel = 1])/(count patches)
17
1
11

MONITOR
859
156
914
201
Farms
count farms
17
1
11

MONITOR
1011
206
1087
251
Mean Capital
precision mean [capital] of farms 2
17
1
11

MONITOR
915
156
976
201
Mean Size
precision mean [count myPlots] of farms 2
17
1
11

TEXTBOX
860
76
1171
117
TWO WIDELY DIFFERENT SCENARIOS HERE: \nif you allow farmers to buy new patches from in a certain radius around the farmsted or only in the neighbourhood of its own plots. 
10
14.0
1

PLOT
633
373
793
493
GI AREA
TIME
N
0.0
50.0
0.0
2500.0
true
false
"" ""
PENS
"default" 1.0 0 -7858858 true "" "plot count patches with [giLabel = 1]"

PLOT
860
254
1020
374
Farms
TIME
N
0.0
50.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot count farms"

PLOT
1021
254
1181
374
Mean Capital
TIME
EURO
0.0
50.0
0.0
500000.0
true
false
"" ""
PENS
"default" 1.0 0 -9276814 true "" "plot precision mean [capital] of farms 2"

PLOT
633
496
793
616
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
1044
156
1145
201
LowQuality Farms
count farms with [myquality < qualitystandard]
17
1
11

MONITOR
1090
206
1147
251
Bankrupt
bankruptcies
17
1
11

MONITOR
860
206
940
251
Total Output
(wineYield * count patches with [ageVines > 3 and fallow = 0])
2
1
11

PLOT
857
494
1050
629
Ballot
Vote
Count
0.0
2.0
0.0
20.0
true
false
"set-plot-x-range -1 2\nset-histogram-num-bars 3" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram ballotBox"

OUTPUT
1068
522
1246
627
14

CHOOSER
1057
452
1234
497
votingMechanism
votingMechanism
"ABS Majority" "REL Majority" "NO VOTE"
1

TEXTBOX
1065
504
1267
523
Outcome Collective Choice Situation
12
104.0
1

SLIDER
860
416
953
450
memory
memory
3
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
860
454
1032
487
everyXyears
everyXyears
3
10
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
862
391
1024
410
INSTITUTIONAL CHANGE
11
0.0
1

SLIDER
1057
414
1159
448
qualityDelta
qualityDelta
0
0.5
0.01
0.01
1
NIL
HORIZONTAL

INPUTBOX
754
82
815
142
OptiTemp
21.0
1
0
Number

TEXTBOX
1295
381
1361
399
GI BOARD
11
0.0
1

TEXTBOX
880
517
939
535
Decreasing
10
15.0
1

TEXTBOX
993
517
1048
535
Increasing
10
55.0
1

INPUTBOX
1292
409
1342
469
%fee
0.05
1
0
Number

INPUTBOX
1346
409
1398
469
mktDelta
0.6
1
0
Number

INPUTBOX
1400
409
1452
469
mktRho
0.2
1
0
Number

MONITOR
941
206
1010
251
GI Output
wineYield * count patches with [ageVines > 3 and fallow = 0 and giLabel = 1]
17
1
11

MONITOR
979
156
1042
201
GI Farms
count farms with [any? myPlots with [ageVines > 3 and giLabel = 1]]
17
1
11

MONITOR
1292
599
1360
645
GI Prestige
giPrestige
17
1
11

MONITOR
1150
205
1211
251
Profits
sum [profit] of farms
17
1
11

MONITOR
1371
599
1432
645
NIL
qualityLev
17
1
11

MONITOR
1434
599
1500
645
NIL
mktExpLev
17
1
11

MONITOR
1501
599
1571
645
NIL
giFarmsLev
17
1
11

INPUTBOX
1455
409
1541
469
basBrandValue
100000.0
1
0
Number

SLIDER
953
416
1046
450
prec
prec
1
5
2.0
1
1
NIL
HORIZONTAL

PLOT
1183
254
1343
374
Total Profits
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
"default" 1.0 0 -15040220 true "" "plot sum [profit] of farms"

PLOT
1291
473
1485
593
Collective Brand Value
TIME
EURO
0.0
50.0
0.0
500000.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot collBrandValue"

PLOT
1488
473
1648
593
GI Wine Price
TIME
EURO
0.0
50.0
0.0
3.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot giWinePrice"

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
Rectangle -7500403 true true 15 270 315 300

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
