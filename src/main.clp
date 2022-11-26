(deftemplate Person
    (slot Name (type SYMBOL) (default ?NONE))
    (slot Prediction (type STRING) (default ""))
)

(deftemplate Classification
    (slot Name (type SYMBOL) (default ?NONE))
    (slot Type (type SYMBOL) (default ?NONE))
)

(defrule RunProgram
    =>
    (assert
        (Person (Name Kirito))
        (Classification (Name Checkup) (Type positive))
    )
)

(defrule T-HBsAg
    (Classification (Name Checkup) (Type positive))
    =>
    (printout t "HBsAg? ")
    (bind ?HBsAg (read))
    (while (and (neq ?HBsAg positive) (neq ?HBsAg negative))
        (printout t "Hanya menerima 'positive' dan 'negative'" crlf)
        (printout t "HBsAg? ")
        (bind ?HBsAg (read))
    )
    (assert (Classification (Name HBsAg) (Type ?HBsAg)))
)

(defrule T-antiHDV
    (Classification (Name HBsAg) (Type positive))
    ?p <- (Person (Name ?name) (Prediction ""))
    =>
    (printout t "anti-HDV? ")
    (bind ?antiHDV (read))
    (while (and (neq ?antiHDV positive) (neq ?antiHDV negative))
        (printout t "Hanya menerima 'positive' dan 'negative'" crlf)
        (printout t "anti-HDV? ")
        (bind ?antiHDV (read))
    )
    (assert (Classification (Name antiHDV) (Type ?antiHDV)))
    (if (eq ?antiHDV positive) then
        (modify ?p (Name ?name) (Prediction "Hepatitis B+D"))
    )
)

(defrule T-antiHBcL
    (Classification (Name antiHDV) (Type negative))
    ?p <- (Person (Name ?name) (Prediction ""))
    =>
    (printout t "anti-HBc? ")
    (bind ?antiHBc (read))
    (while (and (neq ?antiHBc positive) (neq ?antiHBc negative))
        (printout t "Hanya menerima 'positive' dan 'negative'" crlf)
        (printout t "anti-HBc? ")
        (bind ?antiHBc (read))
    )
    (assert (Classification (Name antiHBc) (Type ?antiHBc)))
    (if (eq ?antiHBc negative) then
        (modify ?p (Name ?name) (Prediction "Uncertain configuration"))
    )
)

(defrule T-antiHBcR
    (Classification (Name antiHBs) (Type ?typeHBs))
    ?p <- (Person (Name ?name) (Prediction ""))
    =>
    (printout t "anti-HBc? ")
    (bind ?antiHBc (read))
    (while (and (neq ?antiHBc positive) (neq ?antiHBc negative))
        (printout t "Hanya menerima 'positive' dan 'negative'" crlf)
        (printout t "anti-HBc? ")
        (bind ?antiHBc (read))
    )
    (assert (Classification (Name antiHBc) (Type ?antiHBc)))
    (if (and (eq ?antiHBc positive) (eq ?typeHBs positive)) then
        (modify ?p (Name ?name) (Prediction "Cured"))
    )
    (if (and (eq ?antiHBc negative) (eq ?typeHBs positive)) then
        (modify ?p (Name ?name) (Prediction "Vaccinated"))
    )
    (if (and (eq ?antiHBc positive) (eq ?typeHBs negative)) then
        (modify ?p (Name ?name) (Prediction "Unclear (possible resolved)"))
    )
    (if (and (eq ?antiHBc negative) (eq ?typeHBs negative)) then
        (modify ?p (Name ?name) (Prediction "Health not vaccinated or suspicious")) 
    )
)

(defrule T-antiHBs
    (Classification (Name HBsAg) (Type ?type))
    ?p <- (Person (Name ?name) (Prediction ""))
    =>
    (printout t "anti-HBs? ")
    (bind ?antiHBs (read))
    (while (and (neq ?antiHBs positive) (neq ?antiHBs negative))
        (printout t "Hanya menerima 'positive' dan 'negative'" crlf)
        (printout t "anti-HBs? ")
        (bind ?antiHBs (read))
    )
    (assert (Classification (Name antiHBs) (Type ?antiHBs)))
    (if (and (eq ?antiHBs positive) (eq ?type positive)) then
        (modify ?p (Name ?name) (Prediction "Uncertain configuration"))
    )
)

(defrule T-lgm-antiHBc
    (Classification (Name HBsAg) (Type positive))
    (Classification (Name antiHBs) (Type negative))
    ?p <- (Person (Name ?name) (Prediction ""))
    =>
    (printout t "lgm anti-HBc? ")
    (bind ?antilHBc (read))
    (while (and (neq ?antilHBc positive) (neq ?antilHBc negative))
        (printout t "Hanya menerima 'positive' dan 'negative'" crlf)
        (printout t "lgm anti-HBc? ")
        (bind ?antilHBc (read))
    )
    (assert (Classification (Name antilHBc) (Type ?antilHBc)))
    (if (eq ?antilHBc positive) then
        (modify ?p (Name ?name) (Prediction "Acute infection"))
    )
    (if (eq ?antilHBc negative) then
        (modify ?p (Name ?name) (Prediction "Chronic infection"))
    )
)

(defrule GetPrediction
    (Person (Name ?name) (Prediction ?p))
    (not (Person (Name ?name) (Prediction "")))
    =>
    (printout t crlf "Hasil Prediksi: " ?p crlf)
)