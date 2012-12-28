---- MODULE Trein ----
EXTENDS Naturals, Sequences
VARIABLES deuren, conducteurDeur, ac, seinlicht, vertrek
(* deuren: variabele die aangeeft of de conducteur de deuren sluit *)
(* conducteurDeur: variabele die aangeeft of de conducteur zijn eigen deur sluit *)
(* ac: variabele die aangeeft of de conducteur het Action Completed signaal heeft gegeven *)
(* seinlicht: kleur van het seinlicht *)
(* vertrek: variabele die aangeeft of de trein vertrokken is *)
----
vars == <<deuren, conducteurDeur, ac, seinlicht, vertrek>>

Kleuren == {"rood", "wit"}

Types == /\ deuren \in 0..1
      	 /\ conducteurDeur \in 0..1
      	 /\ ac \in 0..1
      	 /\ seinlicht \in Kleuren
         /\ vertrek \in 0..1

Init == deuren = 0 /\ conducteurDeur = 0 /\ ac = 0 /\ seinlicht = "rood" /\ vertrek = 0

Fluitsignaal ==
      /\ deuren = 0
      /\ seinlicht = "rood"
      /\ deuren' = 1 /\ UNCHANGED <<ac, conducteurDeur, seinlicht, vertrek>>

Action == deuren = 1
      /\ ac = 0
      /\ ac' = 1
      /\ UNCHANGED <<seinlicht, deuren, conducteurDeur, vertrek>>

SeinLicht == ac = 1
      /\ seinlicht = "rood"
      /\ seinlicht' = "wit"
      /\ UNCHANGED <<deuren, ac, conducteurDeur, vertrek>>

Vertrek == seinlicht = "wit"
      /\ vertrek = 0
      /\ vertrek' = 1
      /\ UNCHANGED <<deuren, conducteurDeur, ac, seinlicht>>

Reset ==
      /\ vertrek = 1
      /\ seinlicht' = "rood"
      /\ conducteurDeur' = 0
      /\ deuren' = 0
      /\ ac' = 0
      /\ vertrek' = 0

Deur == vertrek = 1
      /\ conducteurDeur = 0
      /\ conducteurDeur' = 1
      /\ UNCHANGED <<deuren, ac, seinlicht, vertrek>>

Next == Fluitsignaal \/ Action \/ SeinLicht \/ Vertrek \/ Reset \/ Deur

Live == WF_vars(Fluitsignaal) /\ WF_vars(Action) /\ WF_vars(Vertrek) /\ WF_vars(SeinLicht) /\ WF_vars(Deur) /\ WF_vars(Reset)

Spec == Init /\ [][Next]_vars /\ Live

----
VertrekNietVoorAc == (ac = 0) ~> (seinlicht = "rood")

RoodSeinlichtDefault == []<>(seinlicht = "rood")

Fairness == (seinlicht = "wit") ~> (vertrek = 1)
Veiligheid == (vertrek = 1) => [](conducteurDeur = 1)

OoitVertrek == []<>(vertrek = 1)

----
THEOREM Spec => []Types
THEOREM Spec => RoodSeinlichtDefault
THEOREM Spec => OoitVertrek
THEOREM Spec => VertrekNietVoorAc
THEOREM Spec => Fairness
THEOREM Spec => Veiligheid
====
