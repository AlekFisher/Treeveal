# Treeveal

**AI-Powered Decision Tree Analysis**

Treeveal is an interactive R Shiny application that makes decision tree analysis accessible to everyone. Build, visualize, and interpret decision trees with the help of AI — no statistics background required.

---

## Why Treeveal?

Decision trees are powerful tools for understanding what drives outcomes, but interpreting them can be challenging. Treeveal bridges that gap by combining traditional statistical methods with modern AI to deliver clear, actionable insights.

**Perfect for:**
- Market researchers exploring customer segments
- Healthcare analysts identifying patient risk factors
- Business analysts understanding key drivers
- Anyone who needs to explain "what factors matter most"

---

## Key Features

### Multi-Provider AI Interpretation

Treeveal integrates with leading AI providers through the `{ellmer}` package:

| Provider | Model | Best For |
|----------|-------|----------|
| **Azure OpenAI** | GPT-4 | Enterprise/Production (secure) |
| **Anthropic** | Claude | Nuanced analysis |
| **Google** | Gemini | General interpretation |
| **OpenAI** | GPT | Versatile insights |
| **Ollama** | Local models | Privacy-sensitive data |

The AI receives full context about your model — decision rules, variable importance, accuracy metrics, and confusion matrix — enabling rich, domain-aware interpretations.

### Tailored Response Styles

Choose how the AI communicates based on your audience:

- **Executive Summary** — Brief, action-focused bullet points
- **Project Team** — Clear narrative suitable for reports and presentations
- **Statistician** — Technical detail with diagnostic considerations

### Smart Quick Prompts

One-click insights without crafting prompts:

- **Interpret** — Plain-language explanation of what the tree shows
- **Insights** — Key findings and patterns
- **Recommendations** — Actionable next steps based on the analysis
- **Limitations** — Honest assessment of model caveats

### Production Mode

A dedicated secure mode that restricts AI providers to Azure OpenAI only — designed for working with sensitive client data while maintaining enterprise compliance.

---

## Data Quality Intelligence

Before building your model, Treeveal automatically analyzes your data:

- **Missing Data Detection** — Flags variables with high missingness
- **Low Variance Warnings** — Identifies near-constant variables that won't contribute meaningful splits
- **Correlation Analysis** — Highlights highly correlated predictors
- **Sample Size Assessment** — Validates you have enough data for reliable analysis
- **Smart Recommendations** — Actionable suggestions to improve model quality

---

## Model Validation

Treeveal goes beyond single-model analysis:

- **Random Forest Comparison** — Validates variable importance using 500-tree ensemble
- **Side-by-Side Rankings** — Compare importance from decision tree vs. random forest
- **Confusion Matrix** — Visual accuracy breakdown for classification models
- **CP Table Inspection** — Explore complexity parameter trade-offs

---

## Flexible Data Import

Supports the formats you actually use:

| Format | Extensions |
|--------|------------|
| CSV | `.csv` |
| Excel | `.xlsx`, `.xls` |
| SPSS | `.sav` |
| R Data | `.rds`, `.RData`, `.rda` |

**Optional Data Dictionary** — Upload variable labels and descriptions to enhance AI interpretations with your domain terminology.

---

## Getting Started

### Prerequisites

```r
install.packages(c(
  "shiny", "bslib", "rpart", "rpart.plot",
  "ellmer", "DT", "dplyr", "tidyr", "ggplot2",
  "readxl", "haven", "officer", "rvg", "randomForest"
))
```

### API Configuration

Set your AI provider credentials in `.Renviron`:

```
ANTHROPIC_API_KEY=your_key_here
GOOGLE_API_KEY=your_key_here
OPENAI_API_KEY=your_key_here
AZURE_OPENAI_API_KEY=your_key_here
```

### Run the App

```r
shiny::runApp()
```

Or open `Treeveal.Rproj` in RStudio and click **Run App**.

---

## Quick Start Guide

1. **Upload Data** — Drag and drop your file or use the demo dataset
2. **Select Variables** — Choose your outcome and predictors
3. **Build Tree** — Click "Build Decision Tree" with default parameters
4. **Get AI Insights** — Use quick prompts or ask custom questions
5. **Export** — Download your tree visualization as PowerPoint

---

## Demo Dataset

Treeveal includes a sample **HCP GLP-1 Prescribing** dataset for exploration. Toggle "Use Demo Dataset" to immediately start building and interpreting trees without uploading your own data.

---

## Built With

- [Shiny](https://shiny.posit.co/) — Interactive web framework
- [bslib](https://rstudio.github.io/bslib/) — Bootstrap 5 theming
- [rpart](https://cran.r-project.org/package=rpart) — Decision tree algorithm
- [ellmer](https://ellmer.tidyverse.org/) — Multi-provider LLM integration
- [randomForest](https://cran.r-project.org/package=randomForest) — Ensemble validation

---

## License

MIT

---

<p align="center">
  <em>Transform complex decision trees into clear, actionable insights.</em>
</p>
