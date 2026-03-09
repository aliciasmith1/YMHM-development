# Youth Mental Health Map (YMHM) – Questionnaire Scoring and Study Analysis

The Youth Mental Health Map is a digital, youth-centric self-report questionnaire designed to assess transdiagnostic mental health symptoms in young people.

This repository contains the analysis scripts for the two-part study that developed and validated the Youth Mental Health Map, as well as code (both in Python and R) that can be used for scoring the four Youth Mental Health Map dimensions.


---

## Overview

In this four-part study, we developed and validated a digital, youth-centric self-report mental health assessment that measures scores on four core transdiagnostic symptom clusters identified using a novel data-driven, machine learning-based approach:

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

**Study 3 (Clinician Feedback):**

Added value
Feasibility
Acceptability

**Study 4 (Treatment Allocation):**

Hierarchical Bayesian logistic regression to determine the suitability of each treatment technique for each transdiagnostic dimension

---

## Study analysis

Analyses for Study 1, 2, 3 and 4 are contained in this repository. Study 1 scripts were used for the development of the Youth Mental Health Map (e.g., factor analyses and item reduction), Study 2 for the validation process, Study 3 for clinician feedback, and Study 4 for determining the most appropriate treatment technique for each transdiagnostic dimension.

All analyses were conducted in R and Python and annotated scripts are provided in the relevant folders.


---

## Questionnaire Scoring

YMHM_scoring_Python.ipynb
  
This notebook explains how to calculate scores for each of the four Youth Mental Health Map dimensions from raw questionnaire data using Python.
Each item is multiplied by its corresponding factor weight, the weighted item scores are summed, and the model intercept is added to produce the final factor scores for each dimension.
A figure is generated to illustrate the score distributions, and the resulting scores are added back into the original dataset.

YMHM_scoring_R.ipynb
  
This notebook provides the same scoring procedure implemented in R, following the same computation and visualisation steps.



