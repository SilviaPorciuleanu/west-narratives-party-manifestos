# Us or “the Other”? Narratives of “the West” in Central and Eastern European Party Rhetoric

This repository contains the code used to analyse how political parties in Central and Eastern Europe refer to the West, Western countries, and Europe in their election manifestos. The code corresponds to the paper *“Us or ‘the Other’? Narratives of ‘the West’ in Central and Eastern European Party Rhetoric”*. The project is part of my PhD research on perspectives of the West in Central and Eastern Europe.

The analysis combines dictionary-based text extraction, LLM-assisted annotation, manual validation, and quantitative analysis of party manifestos from ten countries.

## Code

The files follow the main steps of the analysis:

- `01_full_corpus.R` — Downloads and combines the manifesto corpus using the Manifesto Project API.
- `02_West_Sentences.R` — Prepares the text, identifies sentences mentioning the West and related referents, and checks sentence segmentation.
- `O3_Actor_identification_and_validation.ipynb` — Identifies referents using an LLM and compares the classifications with manual annotations.
- `04_Metrics.ipynb` — Compares LLM classifications using accuracy, F1 scores, Cohen's kappa, and processing costs.
- `06_Final_Annotation.ipynb` — Applies the selected classification approach to the full set of extracted sentences.
- `07_Variables.R` — Constructs manifesto-level measures and adds party and contextual variables.
- `10_Analysis_GitHub.Rmd` — Produces descriptive figures and regression analyses of references to the West and their narratives.

## Data and outputs

The manifesto texts come from the [Manifesto Project](https://manifesto-project.wzb.eu/). Additional party-level information and manually annotated data are used in later steps. The scripts refer to intermediate CSV files created earlier in the workflow.

The manually annotated and LLM-annotated data are not included in this repository and will be made available upon publication of the paper.

Some scripts require access to the original datasets and API credentials. API keys are not included in the repository, and the annotation notebooks should be checked before enabling any paid API calls.
