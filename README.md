# LDA_NTX-IGA
Lebenzeitanalyse fuer NTX und IGA Patienten

### ToDo:
1. LDA
  - [ ] Kapplan-Maier-curve
  - [ ] Cox-regression/Hazard-ratio
2. EDA
  - [x] follow-up (mean)
  - [x] age patients (yrs.)
  - [x] male sex
  - [x] BMI (mean.)
  - [x] living D.
  - [x] deceased D.
  - [x] HLA-mm (0-6)
  - [x] age donor (mean.)
  - [x] cold-ischemia time (hours)
  - [x] PRA current (mean)
  - [x] PRA highest (mean)
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
