# Youth Mental Health Map (YMHM) – Analysis Repository

This repository contains the analysis scripts for a two-part study that developed and validated the Youth Mental Health Map (YMHM) – a digital, youth-centric self-report questionnaire designed to assess transdiagnostic mental health symptoms in young people.

---

## Overview

In this two-part study, we developed and validated a digital, youth-centric self-report mental health questionnaire that measures scores on four core transdiagnostic symptom clusters identified using a novel data-driven, machine learning-based approach:

1. **Uncontrollable Thinking Patterns**  
2. **Re-experiencing Difficult Events**  
3. **Rigid High Standards**  
4. **Emotional Agency**
  

**Study 1 (EFA & Item Reduction):**

Factor analytic approach extracted four latent symptom dimensions
Multi-target lasso regression reduced 196 questionnaire items to 34


**Study 2 (Validation & Reliability):**

Test-retest reliability
Internal consistency
Construct validity
Predictive validity


---

## YMHM Questionnaire Scoring

YMHM_scoring_Python.ipynb
This notebook explains how to calculate scores for each of the four Youth Mental Health Map dimensions from raw questionnaire data using Python.
Each item is multiplied by its corresponding factor weight, the weighted item scores are summed, and the model intercept is added to produce the final factor scores for each dimension.
A figure is generated to illustrate the score distributions, and the resulting scores are added back into the original dataset.

YMHM_scoring_R.ipynb
This notebook provides the same scoring procedure implemented in R, following the same computation and visualisation steps.






