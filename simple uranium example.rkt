#lang racket
;(require json)
;(string->jsexpr (read-file "json test.txt"))

(require 2htdp/batch-io)

; notes:
; - by having a facility type and a node type, i'm trying
;   to separate the data specific to that site from the
;   data specific to the site's place in the supply chain.
;   if a mine starts selling its ore somewhere new, that's
;   a change in the supply chain, but everything about the
;   mine's production should be the same.

; -------------------------------------------------------------
; DATA DEFINITIONS
; -------------------------------------------------------------

; a Fraction is a range, [0, 1].

; a USD/KG is a Number
; interpretation a price in dollars per kilogram.

; a MassRate is a number
; interpretation kg/year.

; an Ore is one of:
; - "high"
; - "low"

; a Fuel is one of:
; - "salt"
; - "rods"
; - "pebbles"

(define-struct mine [location material-in material-out max-production price/kg])
; a Mine is a (make-mine String Material Material MassRate USD/KG)
; interpretation the geographical location, type of ore,
; rate of ore production, maximum production rate possible
; if mine were to ramp up, and price of raw ore.

(define-struct processor [location material-in material-out max-production price/kg])
; a Processor is a (make-processor String Material Material MassRate USD/KG)
; interpretation information about a facility which converts ore
; to fabricated fuel.

(define-struct plant [location material-in material-out max-production burnup])
; a Plant is a (make-plant String Material Material MassRate Fraction)
; interpretation a power plant which consumes fabricated fuel.

(define-struct generation [])

(define-struct destruction [])

; a Facility is one of:
; - Mine
; - Processor
; - Plant
; - Generation
; - Destruction
; data examples:
(define fcMNTZ (make-mine "tanzania" "none" "high" 1e3 100))
(define fcPRUK (make-processor "bristol" "high" "rods" 100 250))
(define fcPLAZ (make-plant "arizona" "salt" "snf" 80 0.75))
(define fcPLFK (make-plant "fukushima" "rods" "snf" 20 0.08))
(define GEN (make-generation))
(define DES (make-destruction))

; a Material is one of:
; - Ore
; - Fuel
; - SNF
; - "none"

(define-struct sc [facility accumulation sources sinks])
; a SupplyChain is a (make-sc Facility [List-of Number]
;                                   [List-of Facility] [List-of Facility])
; interpretation a facility has an inlet and outlet material. the list of
; numbers contains the holdups of those two materials. the two lists of
; facilities are sources of inlet materials and sinks of outlet materials.
; data examples:
(define scMNTZ (make-sc fcMNTZ (list 2e3 1e3) (list GEN) (list fcPRUK)))
(define scPRUK (make-sc fcPRUK (list 60 20) (list fcMNTZ) (list fcPLAZ fcPLFK)))
(define scPLAZ (make-sc fcPLAZ (list 40 700) (list fcPRUK) (list DES)))
(define scPLFK (make-sc fcPLFK (list 41 601) (list fcPRUK) (list DES)))

(define-struct ms [origin destination flow material])
; a MaterialStream is a (make-ms SupplyChain SupplyChain MassFlow Material)
; interpretation the material flowing between two sc's.
; data examples:
(define msGENTZ GEN scMNTZ 1e3 "high")

; a SupplyNetwork is a [List-of SupplyChain]
; interpretation all sc's which are connected

; -------------------------------------------------------------
; CONSTANT DEFINITIONS
; -------------------------------------------------------------

; -------------------------------------------------------------
; FUNCTION DEFINITIONS
; -------------------------------------------------------------
; SupplyChain SupplyChain -> MassRate
; gets amount of material flowing from first sc to second sc.
; this simplistic fn just divides the source output by number
; of sinks. sink-sc argument currently not used.
(define (material-stream-mass/kg source-sc sink-sc)
  (/ (get-max-production source-sc) (number-of-sinks source-sc)))

; SupplyChain SupplyChain -> MaterialStream
(define (get-ms source-sc sink-sc)
  (make-ms source-sc
           sink-sc
           (material-stream-mass/kg source-sc sink-sc)
           ...))

; SupplyChain -> MassRate
; extracts production from the sc.
(define (get-max-production sc)
  (cond
    [(mine? (sc-facility sc)) (mine-max-production (sc-facility sc))]
    [(processor? (sc-facility sc)) (processor-max-production (sc-facility sc))] ; what would be a good data defn such that mines, processors, plants, etc. can each have their own unique set of information, but such that all are under the umbrella of facilities?
    [(plant? (sc-facility sc)) (plant-max-production (sc-facility sc))]))

; SupplyChain -> Number
; counts number of sinks sc has.
(define (number-of-sinks sc)
  (length (sc-sinks sc)))

; SupplyChain [Facility -> Y] -> Y
; gets desired facility property, given a sc.
#|
(define (facility-property selector sc)
  (cond
    [(mine? (sc-facility sc)) (selector ...  (sc-facility sc))]
    [(processor? (sc-facility sc)) (selector ... (sc-facility sc))]
    [(plant? (sc-facility sc)) (selector ... (sc-facility sc))]))
|#

; SupplyChain -> SupplyNetwork
; lists all sc's in the network.


; SupplyNetwork -> [List-of MassRate]
; solves the flows between sc's.