# LDA_NTX-IGA
Lebenzeitanalyse fuer NTX und IGA Patienten

### ToDo:
1. LDA
  - [ ] Kapplan-Maier-curve
  - [ ] Cox-regression/Hazard-ratio
2. EDA

| Task                        | Status  | Comment         |
|-----------------------------|---------|-----------------|
| follow-up (mean)            | O.K.    |                 |
| age patients (yrs.)         | O.K.    |                 |
| male sex                    | O.K.    |                 |
| BMI (mean.)                 | O.K.    |                 |
| living D.                   | O.K.    |                 |
| deceased D.                 | O.K.    |                 |
| HLA-mm (0-6)                | O.K.    |                 |
| age donor (mean.)           | O.K.    |                 |
| cold-ischemia time (hours)  | O.K.    |                 |
| PRA current (mean)          | O.K.    |                 |
| PRA highest (mean)          | O.K     |                 |


3. TBA

#### folder structure:

```
.
├── archive
│   ├── ...
│   ├── ...
│   └── etc.
├── data (.gitignore)
│   ├── IgA_masterfile-Tango_2020_04_21 NEU.xlsx
│   ├── iga_sheet1.csv
│   ├── iga_sheet2.csv
│   ├── ntx_daten.csv
│   └── ntx-fu-alina_Aussortierte_Daten.xlsx
├── misc (.gitignore)
│   ├── ...
│   ├── ...
│   └── etc.
├── R
│   ├── LDA_IGA-NTX.nb.html
│   ├── LDA_IGA-NTX.Rmd
│   ├── read_data.R
│   └── scratch.R
└── README.md

```
