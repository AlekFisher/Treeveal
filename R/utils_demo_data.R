# utils_demo_data.R
# Pure function: generates the HCP GLP-1 Prescribing Survey demo dataset

generate_demo_dataset <- function() {
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

  list(data = hcp_data, dict = NULL)
}
