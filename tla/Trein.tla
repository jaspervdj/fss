---- MODULE Trein ----
EXTENDS Naturals, Sequences
VARIABLES deuren, conducteurDeur, ac, seinlicht
(* deuren: variabele die aangeeft of de conducteur de deuren sluit *)
(* conducteurDeur: variabele die aangeeft of de conducteur zijn eigen deur sluit *)
(* ac: variabele die aangeeft of de conducteur het Action Completed signaal heeft gegeven *)
(* seinlicht: kleur van het seinlicht *)
----
vars == <<deuren, conducteurDeur, ac, seinlicht>>

Kleuren == {"rood", "wit"}

Types == /\ deuren \in 0..1
      	 /\ conducteurDeur \in 0..1
      	 /\ ac \in 0..1
      	 /\ seinlicht \in Kleuren

Init == deuren = 0 /\ conducteurDeur = 0 /\ ac = 0 /\ seinlicht = "rood"

Fluitsignaal == deuren = 0
      /\ deuren' = 1 /\ UNCHANGED <<ac, conducteurDeur, seinlicht >>

Action == deuren = 1
      /\ ac' = 1
      /\ ac = 0
      /\ UNCHANGED <<seinlicht, deuren, conducteurDeur>>

Vertrek == ac = 1
      /\ seinlicht = "rood"
      /\ seinlicht' = "wit"
      /\ conducteurDeur = 0
      /\ conducteurDeur' = 1
      /\ UNCHANGED <<deuren, ac>>

Reset == ac = 1
      /\ seinlicht = "wit"
      /\ conducteurDeur = 1
      /\ deuren = 1
      /\ seinlicht' = "rood"
      /\ conducteurDeur' = 0
      /\ deuren' = 0
      /\ ac' = 0

Next == Fluitsignaal \/ Action \/ Vertrek \/ Reset

Live == WF_vars(Fluitsignaal) /\ WF_vars(Action) /\ WF_vars(Vertrek) /\ WF_vars(Reset)

Spec == Init /\ [][Next]_vars /\ Live

----
VertrekNietVoorAc == (ac = 0) ~> (seinlicht = "rood")

RoodSeinlichtDefault == []<>(seinlicht = "rood")

----
THEOREM Spec => []VertrekNietVoorAc
THEOREM Spec => RoodSeinlichtDefault
====
