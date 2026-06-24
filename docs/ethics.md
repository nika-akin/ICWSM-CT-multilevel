# Ethical Considerations

## Data Privacy

This study uses publicly available data from the Voat platform (2014-2020). The following ethical safeguards are implemented:

### Anonymization
- **No usernames reported**: All analyses aggregate at user or community level
- **User identifiers**: Hashed/anonymized IDs only; no mapping to real identities
- **Text content**: No direct quotes or identifiable excerpts in publications

### IRB Considerations
This research analyzes publicly archived social media data. Key ethical points:

1. **Public nature of data**: Voat was a public platform accessible without authentication
2. **Minimal risk**: Aggregated analysis poses minimal risk to individuals
3. **No vulnerable populations**: Study does not target protected groups

### Data Access
Raw data are not distributed in this repository due to:
- File size constraints (>800MB)
- Privacy preservation considerations
- Terms of service of data hosting platforms

Researchers seeking access should consult the data dictionary (`data/dictionaries/`) for variable documentation.

## Research Integrity

### Reproducibility
- Complete `targets` pipeline with 40+ interdependent targets
- Containerized environment via Docker
- Package version locking via `renv`

### Statistical Rigor
- Mixed-effects models account for nested data structure
- Time-varying coefficients in survival models capture dynamic effects
- GAMM smooth terms model non-linear relationships

### Limitations
- Observational design limits causal inference
- Platform shutdown may introduce survivorship bias
- CT classification based on semantic similarity, not manual annotation
