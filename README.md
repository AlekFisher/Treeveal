# 🌳 Treeveal: AI-Powered Decision Tree Analysis

**Treeveal** is an interactive **R Shiny** application for building, visualizing, and interpreting decision trees — with optional **AI-assisted interpretation** powered by **{ellmer}** (Claude, Gemini, and GPT).

Upload your own CSV or explore the included **HCP GLP-1 prescribing demo dataset**, tune model parameters, and get clear, conversational insights into what your decision tree is *actually telling you*.

---

## ✨ Features

- Upload and explore CSV datasets
- Automatic detection of **classification vs regression** trees
- Interactive **decision tree visualization**
- Variable importance, model statistics, and confusion matrix
- Full rule extraction and CP table inspection
- **AI-powered interpretation** with chat and one-click insight prompts
- Optional **study context** to ground AI explanations in your domain

---

## 🧠 AI Assistance (via {ellmer})

Treeveal integrates with **{ellmer}** to provide natural-language interpretation using:

- **Anthropic (Claude)**
- **Google (Gemini)**
- **OpenAI (GPT)**

The AI is automatically given:
- decision tree rules
- variable importance
- model statistics
- confusion matrix and accuracy (for classification)
- your optional *Study Context*

You can:
- Ask free-form questions about the model
- Use quick prompts like **Interpret Tree**, **Key Insights**, **Recommendations**, and **Limitations**

> **Note:** Valid API credentials are required for the selected provider.

---

## 📦 Packages Used

- **shiny**, **bslib**
- **rpart**, **rpart.plot**
- **ellmer**
- **DT**
- **dplyr**, **tidyr**
- **ggplot2**

---


