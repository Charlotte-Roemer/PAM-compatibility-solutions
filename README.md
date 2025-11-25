# PAM-compatibility-solutions
Solutions for big datasets with multiple recorder sensitivities in the PAM of bats

Authors of the scripts: Nathan Besnier, Laureen Dret, Alexandre Grave, Charlotte Roemer

## fun_for_bats.R
Assembles all functions necessary for the other scripts.

## 1_Create_activity_file.R
Calculates the number of bat passes per night according to:
- A chosen threshold for the probability of ID. If a probability of 0.5 is chosen, 
the number of bat passes per night will exclude bat passes with a probability < 0.5
- A chosen time interval. If a time interval of 5 seconds is chosen, the bat passes
will not be aggregated. If a time interval of one minute is chosen, if there are
more than one bat passes for a given species in an interval of one minute, only 
the bat pass with the highest probability of identification will be kept.

## 2_Create_non-target_activity_file.R
This file is created to control the influence of non-target sounds (other than the focus species).
The focus species will be removed from this table in the subsequent scripts.
Calculates the number of bat passes per night any sound event recorded at 0 s
according to:
- A chosen threshold for the probability of ID. If a probability of 0.5 is chosen, 
the number of bat passes per night will exclude bat passes with a probability < 0.5

## 3_Environmental_effects.R
Different tests to assess the effect of the distance to the vegetation edges or
the activity of non-target sounds on the sensitivity differences of bat recorders.

## 4_Sensitivity_curves.R
Build models to model the sensitivity curves of the bat recorders.

## 5_Correction_factors.R
Predicts a correction factor according to 
- The time interval chosen to count bat activity
- The recorders in use in a dataset
Applies the correction factors to the activity counts to obtain corrected activity counts.


