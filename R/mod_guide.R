# mod_guide.R
# Guide & Glossary tab — pure static UI, no server logic needed

guide_ui <- function(id) {
  ns <- NS(id)

  card(
    card_body(
      class = "p-4",

      div(
        class = "mb-4",
        h4("Dendro User Guide"),
        p(class = "text-muted", "Everything you need to know about decision trees and how to use this app.")
      ),

      accordion(
        id = "guide_accordion",
        open = FALSE,

        # What is a Decision Tree
        accordion_panel(
          title = "What is a Decision Tree?",
          value = "what_is",
          icon = bsicons::bs_icon("diagram-3"),

          p(
            class = "lead",
            "A decision tree is a visual tool that shows how different factors lead to an outcome.
            Think of it like a flowchart that asks yes/no questions to arrive at a prediction."
          ),
          h6("A Simple Example"),
          p(
            "Imagine predicting whether an HCP is a high prescriber of GLP-1 medications.
            The tree might work like this:"
          ),
          div(
            class = "p-3 my-3",
            style = "background: var(--bg-secondary); border-radius: var(--radius-md); border-left: 4px solid var(--color-accent);",
            tags$ol(
              tags$li("Is the physician comfortable initiating GLP-1s? (rating \u2265 4)"),
              tags$ul(
                tags$li(tags$strong("Yes \u2192"), " Are they an Endocrinologist?"),
                tags$ul(
                  tags$li(tags$strong("Yes \u2192"), " Likely a High Prescriber (85% probability)"),
                  tags$li(tags$strong("No \u2192"), " Check if they believe in early intervention...")
                ),
                tags$li(tags$strong("No \u2192"), " Likely a Low Prescriber (72% probability)")
              )
            )
          ),
          p(
            "Each \"branch\" represents a question about the data. The tree automatically finds
            the questions that best separate high prescribers from low prescribers."
          )
        ),

        # When to Use
        accordion_panel(
          title = "When to Use Decision Trees",
          value = "when_to_use",
          icon = bsicons::bs_icon("check-circle"),

          layout_columns(
            col_widths = c(6, 6),

            div(
              h6(class = "text-success", "Great For"),
              tags$ul(
                tags$li(tags$strong("Segmentation:"), " Understanding what differentiates groups"),
                tags$li(tags$strong("Identifying key drivers:"), " Finding which factors matter most"),
                tags$li(tags$strong("Creating actionable rules:"), " \"If X and Y, then likely Z\""),
                tags$li(tags$strong("Communicating findings:"), " Visual output stakeholders understand"),
                tags$li(tags$strong("Exploratory analysis:"), " Discovering unexpected patterns")
              )
            ),

            div(
              h6(class = "text-danger", "Limitations"),
              tags$ul(
                tags$li(tags$strong("Overfitting risk:"), " Trees can memorize noise if too complex"),
                tags$li(tags$strong("Instability:"), " Small data changes can produce different trees"),
                tags$li(tags$strong("Linear relationships:"), " May miss smooth, gradual effects"),
                tags$li(tags$strong("Sample size:"), " Needs enough data in each segment")
              )
            )
          )
        ),

        # How to Use This App
        accordion_panel(
          title = "How to Use This App",
          value = "how_to_use",
          icon = bsicons::bs_icon("play-circle"),

          div(
            class = "step-item d-flex mb-3",
            div(class = "step-number me-3", "1"),
            div(
              tags$strong("Upload Your Data"),
              p(class = "text-muted mb-0 small", "Use a CSV file or try the demo dataset to explore.")
            )
          ),

          div(
            class = "step-item d-flex mb-3",
            div(class = "step-number me-3", "2"),
            div(
              tags$strong("Select Your Variables"),
              p(class = "text-muted mb-0 small",
                "Outcome = what you're predicting. Predictors = factors that might influence it.")
            )
          ),

          div(
            class = "step-item d-flex mb-3",
            div(class = "step-number me-3", "3"),
            div(
              tags$strong("Adjust Parameters (Optional)"),
              p(class = "text-muted mb-0 small", "Defaults work well. Adjust if tree is too simple or complex.")
            )
          ),

          div(
            class = "step-item d-flex",
            div(class = "step-number me-3", "4"),
            div(
              tags$strong("Build & Interpret"),
              p(class = "text-muted mb-0 small", "Click 'Build Decision Tree' and use the AI to help interpret.")
            )
          )
        ),

        # Parameter Guide
        accordion_panel(
          title = "Parameter Guide",
          value = "parameters",
          icon = bsicons::bs_icon("sliders"),

          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Parameter"),
                tags$th("What It Does"),
                tags$th("When to Adjust")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("Complexity (cp)")),
                tags$td("Controls tree simplicity. Lower = more complex."),
                tags$td("Raise if too complex, lower if too simple.")
              ),
              tags$tr(
                tags$td(tags$strong("Min Bucket")),
                tags$td("Minimum observations in final groups."),
                tags$td("Raise for more reliable segments.")
              ),
              tags$tr(
                tags$td(tags$strong("Max Depth")),
                tags$td("Maximum levels of questions."),
                tags$td("Lower (3-5) for simpler trees.")
              )
            )
          ),
          div(
            class = "alert alert-info mt-2 small",
            bsicons::bs_icon("lightbulb", class = "me-2"),
            "Start with defaults. Adjust only if your tree has 1-2 splits (lower cp) or 20+ splits (raise cp)."
          )
        ),

        # How to Read the Tree
        accordion_panel(
          title = "How to Read the Tree",
          value = "reading",
          icon = bsicons::bs_icon("eye"),

          p("Each box in the tree contains:"),
          tags$ul(
            tags$li(tags$strong("Top line:"), " The predicted class (e.g., \"High\" or \"Low\")"),
            tags$li(tags$strong("Percentages:"), " Probability of each class"),
            tags$li(tags$strong("Bottom %:"), " What portion of data is in this node")
          ),

          h6(class = "mt-3", "Reading Splits"),
          tags$ul(
            tags$li(tags$strong("Yes/Left:"), " Observations meeting the condition go left"),
            tags$li(tags$strong("No/Right:"), " Observations NOT meeting the condition go right")
          ),

          div(
            class = "alert alert-warning mt-2 small",
            bsicons::bs_icon("info-circle", class = "me-2"),
            "Follow a path top-to-bottom to describe a segment: \"HCPs who are comfortable initiating AND believe in early intervention have 85% probability of being high prescribers.\""
          )
        ),

        # Glossary
        accordion_panel(
          title = "Glossary",
          value = "glossary",
          icon = bsicons::bs_icon("journal-text"),

          div(
            class = "row",
            div(
              class = "col-md-6",
              tags$dl(
                class = "small",
                tags$dt("Accuracy"),
                tags$dd(class = "text-muted", "Percentage of correct predictions."),

                tags$dt("Classification Tree"),
                tags$dd(class = "text-muted", "Predicts categories (High/Low, Yes/No)."),

                tags$dt("Confusion Matrix"),
                tags$dd(class = "text-muted", "Table of correct vs incorrect predictions."),

                tags$dt("CP (Complexity Parameter)"),
                tags$dd(class = "text-muted", "Controls tree pruning. Higher = simpler tree."),

                tags$dt("Leaf Node"),
                tags$dd(class = "text-muted", "Final box with prediction and segment size."),

                tags$dt("Overfitting"),
                tags$dd(class = "text-muted", "Tree too complex, learns noise not patterns.")
              )
            ),
            div(
              class = "col-md-6",
              tags$dl(
                class = "small",
                tags$dt("Predictor Variable"),
                tags$dd(class = "text-muted", "Factors used to make predictions."),

                tags$dt("Pruning"),
                tags$dd(class = "text-muted", "Removing branches to simplify the tree."),

                tags$dt("Root Node"),
                tags$dd(class = "text-muted", "First split; most important variable."),

                tags$dt("Split"),
                tags$dd(class = "text-muted", "Decision point dividing data by a variable."),

                tags$dt("Variable Importance"),
                tags$dd(class = "text-muted", "Score of each variable's contribution.")
              )
            )
          )
        )
      )
    )
  )
}
