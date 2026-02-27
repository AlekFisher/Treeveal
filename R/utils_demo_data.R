# utils_demo_data.R
# Pure functions: demo dataset generators with data dictionaries

# --- Metadata for UI display ---
get_demo_datasets <- function() {
  list(
    hcp = list(
      name = "HCP GLP-1 Prescribing Survey",
      description = "Attitudinal survey of 500 healthcare providers on GLP-1 prescribing behavior. Binary classification outcome (High/Low prescriber) with 23 attitudinal and demographic predictors.",
      n_obs = 500,
      n_vars = 24,
      type = "Classification"
    ),
    readmission = list(
      name = "Patient Readmission",
      description = "Hospital readmission data for 400 patients. Binary classification outcome (Readmitted: Yes/No) with clinical, demographic, and utilization predictors. Right-skewed length-of-stay variable creates natural outliers.",
      n_obs = 400,
      n_vars = 13,
      type = "Classification"
    ),
    treatment = list(
      name = "Treatment Response",
      description = "Treatment response study with 300 patients. Continuous regression outcome (Symptom_Improvement: 0-100) with treatment, demographic, and clinical predictors. Demonstrates regression trees.",
      n_obs = 300,
      n_vars = 12,
      type = "Regression"
    )
  )
}

# --- Dispatcher (default "hcp" for backward compat) ---
generate_demo_dataset <- function(name = "hcp") {
  switch(name,
    hcp         = generate_hcp_dataset(),
    readmission = generate_readmission_dataset(),
    treatment   = generate_treatment_dataset(),
    stop("Unknown demo dataset: ", name)
  )
}

# --- HCP GLP-1 Prescribing Survey (500 obs, classification) ---
generate_hcp_dataset <- function() {
  set.seed(123)
  n <- 500

  # === HCP Demographics ===
  specialty <- sample(
    c("Endocrinology", "Primary Care", "Internal Medicine", "Cardiology"),
    n, replace = TRUE,
    prob = c(0.15, 0.45, 0.25, 0.15)
  )

  years_practice <- round(runif(n, 1, 35))

  practice_setting <- sample(
    c("Academic Medical Center", "Large Group Practice", "Small Private Practice", "Health System"),
    n, replace = TRUE,
    prob = c(0.15, 0.30, 0.25, 0.30)
  )

  region <- sample(
    c("Northeast", "Southeast", "Midwest", "Southwest", "West"),
    n, replace = TRUE
  )

  # === Attitudinal Statements ===
  enthusiast_trait <- rnorm(n, 0, 1)
  barrier_trait <- rnorm(n, 0, 1)

  att_glp1_effective_a1c <- pmin(5, pmax(1, round(3.5 + 0.7 * enthusiast_trait + rnorm(n, 0, 0.8))))
  att_glp1_effective_weight <- pmin(5, pmax(1, round(3.4 + 0.8 * enthusiast_trait + rnorm(n, 0, 0.8))))
  att_glp1_cv_benefit <- pmin(5, pmax(1, round(3.0 + 0.5 * enthusiast_trait + rnorm(n, 0, 1))))

  att_gi_side_effects_concern <- pmin(5, pmax(1, round(3.0 - 0.3 * enthusiast_trait + 0.4 * barrier_trait + rnorm(n, 0, 0.9))))
  att_pancreatitis_concern <- pmin(5, pmax(1, round(2.5 + 0.5 * barrier_trait + rnorm(n, 0, 1))))
  att_patient_tolerability <- pmin(5, pmax(1, round(3.2 + 0.6 * enthusiast_trait - 0.3 * barrier_trait + rnorm(n, 0, 0.8))))

  att_cost_barrier <- pmin(5, pmax(1, round(3.3 + 0.7 * barrier_trait + rnorm(n, 0, 0.8))))
  att_pa_burden <- pmin(5, pmax(1, round(3.5 + 0.6 * barrier_trait + rnorm(n, 0, 0.9))))
  att_patient_afford <- pmin(5, pmax(1, round(2.8 - 0.6 * barrier_trait + rnorm(n, 0, 0.9))))

  att_early_intervention <- pmin(5, pmax(1, round(3.2 + 0.6 * enthusiast_trait + rnorm(n, 0, 0.9))))
  att_weight_tx_priority <- pmin(5, pmax(1, round(3.3 + 0.5 * enthusiast_trait + rnorm(n, 0, 0.9))))
  att_prefer_oral_first <- pmin(5, pmax(1, round(3.0 - 0.5 * enthusiast_trait + 0.3 * barrier_trait + rnorm(n, 0, 1))))

  att_comfortable_initiating <- pmin(5, pmax(1, round(3.0 + 0.8 * enthusiast_trait - 0.2 * barrier_trait + rnorm(n, 0, 0.8))))
  att_adequate_training <- pmin(5, pmax(1, round(3.0 + 0.5 * enthusiast_trait + rnorm(n, 0, 1))))

  att_trust_clinical_data <- pmin(5, pmax(1, round(3.5 + 0.4 * enthusiast_trait + rnorm(n, 0, 0.8))))
  att_peer_influence <- pmin(5, pmax(1, round(3.0 + rnorm(n, 0, 1))))
  att_guideline_adherence <- pmin(5, pmax(1, round(3.5 + 0.3 * enthusiast_trait + rnorm(n, 0, 0.9))))

  att_patients_interested <- pmin(5, pmax(1, round(3.2 + 0.4 * enthusiast_trait + rnorm(n, 0, 1))))
  att_patients_compliant <- pmin(5, pmax(1, round(2.9 + 0.3 * enthusiast_trait - 0.2 * barrier_trait + rnorm(n, 0, 1))))

  prescribe_score <-
    0.35 * (att_comfortable_initiating - 3) +
    0.20 * (att_glp1_effective_weight - 3) +
    0.15 * (att_glp1_effective_a1c - 3) +
    -0.25 * (att_cost_barrier - 3) +
    -0.15 * (att_prefer_oral_first - 3) +
    0.20 * (att_early_intervention - 3) +
    0.15 * (att_weight_tx_priority - 3) +
    -0.10 * (att_gi_side_effects_concern - 3) +
    0.5 * (specialty == "Endocrinology") +
    0.2 * (specialty == "Internal Medicine") +
    rnorm(n, 0, 0.5)

  threshold <- quantile(prescribe_score, 0.55)

  high_prescriber <- factor(
    ifelse(prescribe_score > threshold, "High", "Low"),
    levels = c("Low", "High")
  )

  hcp_data <- data.frame(
    High_Prescriber = high_prescriber,
    Specialty = factor(specialty),
    Years_in_Practice = years_practice,
    Practice_Setting = factor(practice_setting),
    Region = factor(region),
    Effective_for_A1C = att_glp1_effective_a1c,
    Effective_for_Weight = att_glp1_effective_weight,
    CV_Benefit_Belief = att_glp1_cv_benefit,
    GI_Side_Effect_Concern = att_gi_side_effects_concern,
    Pancreatitis_Concern = att_pancreatitis_concern,
    Patient_Tolerability = att_patient_tolerability,
    Cost_is_Barrier = att_cost_barrier,
    Prior_Auth_Burden = att_pa_burden,
    Patients_Can_Afford = att_patient_afford,
    Prefer_Early_Intervention = att_early_intervention,
    Weight_Tx_Priority = att_weight_tx_priority,
    Prefer_Oral_First = att_prefer_oral_first,
    Comfortable_Initiating = att_comfortable_initiating,
    Adequate_Training = att_adequate_training,
    Trust_Clinical_Data = att_trust_clinical_data,
    Peer_Influence = att_peer_influence,
    Follow_Guidelines = att_guideline_adherence,
    Patients_Ask_About_GLP1 = att_patients_interested,
    Patients_Compliant = att_patients_compliant
  )

  hcp_dict <- data.frame(
    variable = c(
      "High_Prescriber", "Specialty", "Years_in_Practice", "Practice_Setting", "Region",
      "Effective_for_A1C", "Effective_for_Weight", "CV_Benefit_Belief",
      "GI_Side_Effect_Concern", "Pancreatitis_Concern", "Patient_Tolerability",
      "Cost_is_Barrier", "Prior_Auth_Burden", "Patients_Can_Afford",
      "Prefer_Early_Intervention", "Weight_Tx_Priority", "Prefer_Oral_First",
      "Comfortable_Initiating", "Adequate_Training",
      "Trust_Clinical_Data", "Peer_Influence", "Follow_Guidelines",
      "Patients_Ask_About_GLP1", "Patients_Compliant"
    ),
    label = c(
      "High vs Low GLP-1 Prescriber", "Medical Specialty", "Years in Practice", "Practice Setting", "Geographic Region",
      "GLP-1s are effective for A1C reduction (1-5)", "GLP-1s are effective for weight loss (1-5)", "GLP-1s have cardiovascular benefit (1-5)",
      "Concerned about GI side effects (1-5)", "Concerned about pancreatitis risk (1-5)", "Patients tolerate GLP-1s well (1-5)",
      "Cost is a barrier to prescribing (1-5)", "Prior authorization is burdensome (1-5)", "My patients can afford GLP-1s (1-5)",
      "Prefer early intervention with GLP-1s (1-5)", "Weight management is a treatment priority (1-5)", "Prefer oral medications first (1-5)",
      "Comfortable initiating GLP-1 therapy (1-5)", "Have adequate training on GLP-1s (1-5)",
      "Trust the clinical trial data (1-5)", "Influenced by peer prescribing (1-5)", "Follow clinical guidelines closely (1-5)",
      "Patients ask about GLP-1 medications (1-5)", "Patients are compliant with GLP-1s (1-5)"
    ),
    stringsAsFactors = FALSE
  )

  list(data = hcp_data, dict = hcp_dict)
}

# --- Patient Readmission (400 obs, classification) ---
generate_readmission_dataset <- function() {
  set.seed(456)
  n <- 400

  Age <- round(rnorm(n, 62, 15))
  Age <- pmax(18, pmin(95, Age))

  Gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))

  # Right-skewed LOS creates natural outliers
  LOS <- round(rlnorm(n, meanlog = 1.5, sdlog = 0.8), 1)
  LOS <- pmax(1, LOS)

  Number_Diagnoses <- rpois(n, lambda = 5) + 1
  Number_Procedures <- rpois(n, lambda = 2)

  Insurance_Type <- factor(sample(
    c("Medicare", "Medicaid", "Private", "Self-Pay"),
    n, replace = TRUE,
    prob = c(0.40, 0.15, 0.35, 0.10)
  ))

  Discharge_Disposition <- factor(sample(
    c("Home", "SNF", "Home Health", "Rehab"),
    n, replace = TRUE,
    prob = c(0.50, 0.20, 0.20, 0.10)
  ))

  Prior_Admissions <- rpois(n, lambda = 1)
  Comorbidity_Score <- round(runif(n, 0, 10), 1)
  Medication_Count <- rpois(n, lambda = 7) + 1

  Emergency_Admission <- factor(
    sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.35, 0.65)),
    levels = c("No", "Yes")
  )

  Follow_Up_Scheduled <- factor(
    sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.70, 0.30)),
    levels = c("No", "Yes")
  )

  # Logistic model drives outcome
  logit_score <-
    -1.5 +
    0.02 * (Age - 62) +
    0.08 * LOS +
    0.10 * Number_Diagnoses +
    0.15 * Prior_Admissions +
    0.12 * Comorbidity_Score +
    0.05 * Medication_Count +
    0.4 * (Emergency_Admission == "Yes") +
    -0.5 * (Follow_Up_Scheduled == "Yes") +
    0.3 * (Discharge_Disposition == "SNF") +
    -0.2 * (Insurance_Type == "Private") +
    rnorm(n, 0, 0.5)

  prob_readmit <- 1 / (1 + exp(-logit_score))
  Readmitted <- factor(
    ifelse(runif(n) < prob_readmit, "Yes", "No"),
    levels = c("No", "Yes")
  )

  readmission_data <- data.frame(
    Readmitted = Readmitted,
    Age = Age,
    Gender = Gender,
    LOS = LOS,
    Number_Diagnoses = Number_Diagnoses,
    Number_Procedures = Number_Procedures,
    Insurance_Type = Insurance_Type,
    Discharge_Disposition = Discharge_Disposition,
    Prior_Admissions = Prior_Admissions,
    Comorbidity_Score = Comorbidity_Score,
    Medication_Count = Medication_Count,
    Emergency_Admission = Emergency_Admission,
    Follow_Up_Scheduled = Follow_Up_Scheduled
  )

  readmission_dict <- data.frame(
    variable = c(
      "Readmitted", "Age", "Gender", "LOS", "Number_Diagnoses",
      "Number_Procedures", "Insurance_Type", "Discharge_Disposition",
      "Prior_Admissions", "Comorbidity_Score", "Medication_Count",
      "Emergency_Admission", "Follow_Up_Scheduled"
    ),
    label = c(
      "Readmitted within 30 days (Yes/No)", "Patient age in years", "Patient gender",
      "Length of stay in days", "Number of diagnoses at discharge",
      "Number of procedures performed", "Insurance type",
      "Discharge disposition", "Number of prior admissions in past year",
      "Charlson comorbidity index score (0-10)", "Number of medications at discharge",
      "Admitted via emergency department (Yes/No)", "Follow-up appointment scheduled (Yes/No)"
    ),
    stringsAsFactors = FALSE
  )

  list(data = readmission_data, dict = readmission_dict)
}

# --- Treatment Response (300 obs, regression) ---
generate_treatment_dataset <- function() {
  set.seed(789)
  n <- 300

  Age <- round(rnorm(n, 50, 12))
  Age <- pmax(21, pmin(80, Age))

  Gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52)))

  Baseline_Severity <- round(runif(n, 30, 90), 1)

  Treatment_Group <- factor(sample(
    c("Drug A", "Drug B", "Placebo"),
    n, replace = TRUE,
    prob = c(0.40, 0.40, 0.20)
  ))

  Duration_Weeks <- sample(4:24, n, replace = TRUE)

  Adherence_Pct <- pmin(100, pmax(10, round(rnorm(n, 78, 15), 1)))

  Prior_Treatments <- rpois(n, lambda = 2)

  Comorbidity_Count <- rpois(n, lambda = 1)

  # BMI with some natural outliers
  BMI <- round(rnorm(n, 27, 5), 1)
  BMI <- pmax(16, BMI)

  Smoking_Status <- factor(sample(
    c("Never", "Former", "Current"),
    n, replace = TRUE,
    prob = c(0.45, 0.35, 0.20)
  ))

  Exercise_Frequency <- factor(
    sample(c("None", "1-2x/week", "3-4x/week", "5+/week"),
           n, replace = TRUE,
           prob = c(0.25, 0.30, 0.30, 0.15)),
    levels = c("None", "1-2x/week", "3-4x/week", "5+/week"),
    ordered = TRUE
  )

  # Linear model + noise drives outcome
  improvement <-
    25 +
    15 * (Treatment_Group == "Drug A") +
    10 * (Treatment_Group == "Drug B") +
    -0.3 * Baseline_Severity +
    0.5 * Duration_Weeks +
    0.2 * Adherence_Pct +
    -2 * Comorbidity_Count +
    -0.3 * BMI +
    5 * (Smoking_Status == "Never") +
    3 * (as.numeric(Exercise_Frequency) - 1) +
    rnorm(n, 0, 10)

  Symptom_Improvement <- round(pmin(100, pmax(0, improvement)), 1)

  treatment_data <- data.frame(
    Symptom_Improvement = Symptom_Improvement,
    Age = Age,
    Gender = Gender,
    Baseline_Severity = Baseline_Severity,
    Treatment_Group = Treatment_Group,
    Duration_Weeks = Duration_Weeks,
    Adherence_Pct = Adherence_Pct,
    Prior_Treatments = Prior_Treatments,
    Comorbidity_Count = Comorbidity_Count,
    BMI = BMI,
    Smoking_Status = Smoking_Status,
    Exercise_Frequency = Exercise_Frequency
  )

  treatment_dict <- data.frame(
    variable = c(
      "Symptom_Improvement", "Age", "Gender", "Baseline_Severity",
      "Treatment_Group", "Duration_Weeks", "Adherence_Pct",
      "Prior_Treatments", "Comorbidity_Count", "BMI",
      "Smoking_Status", "Exercise_Frequency"
    ),
    label = c(
      "Symptom improvement score (0-100)", "Patient age in years", "Patient gender",
      "Baseline symptom severity score (30-90)",
      "Treatment group assignment", "Treatment duration in weeks",
      "Medication adherence percentage",
      "Number of prior treatments tried", "Number of comorbid conditions",
      "Body mass index (kg/m2)",
      "Smoking status", "Exercise frequency per week"
    ),
    stringsAsFactors = FALSE
  )

  list(data = treatment_data, dict = treatment_dict)
}
