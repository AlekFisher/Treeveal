# 🌳 Treeveal: AI-Powered Decision Tree Analysis

Treeveal is an interactive R Shiny app for building, visualizing, and interpreting decision trees—with optional AI-assisted interpretation powered by {ellmer} (Claude / Gemini / GPT). Upload your own CSV or explore the included demo HCP GLP-1 prescribing dataset, tune model parameters, and get clear, conversational insights about what your tree is actually saying.

================================================================================
WHAT TREEVEAL DOES
================================================================================
- Upload a CSV (or load the built-in demo dataset)
- Select an outcome and predictors
- Fit a decision tree using {rpart}
- Explore results across tabs:
  • Data Preview (summary + table)
  • Decision Tree (plot + variable importance + stats + confusion matrix)
  • AI Interpretation (chat + quick insight prompts)
  • Model Details (tree rules + CP table)
- Optionally provide study context to make AI output more relevant and accurate

================================================================================
AI ASSISTANCE (via {ellmer})
================================================================================
Treeveal uses {ellmer} to connect to:
- Anthropic (Claude)
- Google (Gemini)
- OpenAI (GPT)

The app builds a system prompt that includes:
- tree rules
- variable importance
- model stats (and confusion matrix + accuracy for classification)
- your optional “Study Context”

Then you can ask natural-language questions (or click quick actions like Interpret Tree, Key Insights, Recommendations, Limitations).

Note: You’ll need valid provider credentials configured for the provider you select.

================================================================================
PACKAGES USED
================================================================================
- shiny, bslib
- rpart, rpart.plot
- ellmer
- DT
- dplyr, tidyr
- ggplot2

================================================================================
GETTING STARTED
================================================================================

1) Install R packages

Run:
  install.packages(c(
    "shiny", "bslib", "rpart", "rpart.plot",
    "DT", "dplyr", "tidyr", "ggplot2"
  ))

ellmer:
- If you don’t already have it installed:
    install.packages("ellmer")
- If your setup requires GitHub installation (adjust as needed):
    install.packages("remotes")
    remotes::install_github("tidyverse/ellmer")

2) Run the app

Save your app as app.R, then run:
  shiny::runApp("path/to/your/app")

================================================================================
USING YOUR OWN DATA
================================================================================
1) Upload a CSV with headers
2) Choose an Outcome Variable
3) Choose Predictor Variables
4) Click Build Decision Tree

Notes / Tips
- Classification vs regression is auto-detected:
  • If your outcome is a factor → classification (method = "class")
  • Otherwise → regression (method = "anova")

- For best results in classification, ensure your outcome column is a factor:
    df$Outcome <- as.factor(df$Outcome)

================================================================================
MODEL PARAMETERS
================================================================================
Treeveal exposes key rpart.control() settings:

- cp (Complexity Parameter)
  Lower cp → more splits → potentially more complex trees

- minbucket (Minimum Bucket Size)
  Minimum observations in terminal nodes (helps prevent tiny, noisy leaves)

- maxdepth (Maximum Depth)
  Caps tree depth to reduce overfitting and keep the tree readable

================================================================================
INCLUDED DEMO DATASET
================================================================================
Toggle “Use Demo Dataset (HCP GLP-1 Prescribing)” to load a synthetic dataset representing:
- HCP demographics (specialty, region, setting, years in practice)
- attitudes/beliefs (efficacy, barriers, safety, confidence, philosophy)
- outcome: High_Prescriber (High vs Low)

This demo is designed to produce interpretable, decision-boundary-friendly trees so you can quickly test the app’s full workflow.

================================================================================
AI INTERPRETATION: QUICK ACTIONS
================================================================================
Once a model is built, Treeveal provides one-click prompts:

- Interpret Tree: full walkthrough of key paths and drivers
- Key Insights: top 3–5 actionable findings
- Recommendations: what to do with the results + next steps
- Limitations: overfitting, generalizability, data caveats

You can also type your own questions like:
- “What’s the most important split and why?”
- “Which segment looks most likely to be High?”
- “How confident should I be in these paths?”
- “What would you validate next?”

================================================================================
API KEYS / CREDENTIALS
================================================================================
Treeveal doesn’t hardcode keys. Configure credentials for your chosen provider using your preferred method (environment variables, .Renviron, secrets manager, etc.).

Common approach (example — do NOT commit secrets):

In ~/.Renviron:
  ANTHROPIC_API_KEY="..."
  OPENAI_API_KEY="..."
  GOOGLE_API_KEY="..."

Restart R after editing .Renviron.

================================================================================
TYPICAL FILE STRUCTURE
================================================================================
Treeveal/
  app.R
  README.md

This app is currently written as a single-file Shiny app (app.R).

================================================================================
TROUBLESHOOTING
================================================================================
Model builds but confusion matrix doesn’t show
- Confusion matrix only appears for classification trees (outcome is a factor).

AI tab errors
- Verify your provider credentials are set correctly.
- Try switching models/providers in the AI Configuration accordion.

Weird splits / unstable trees
- Increase minbucket, increase cp, or reduce maxdepth.
- Check for missing data patterns and high-cardinality categorical variables.

================================================================================
LICENSE
================================================================================
Choose a license that fits your needs (MIT is common for open source projects).

================================================================================
CREDITS
================================================================================
Built with R Shiny, {rpart}, and {ellmer} to make decision trees easier to build—and easier to explain.
