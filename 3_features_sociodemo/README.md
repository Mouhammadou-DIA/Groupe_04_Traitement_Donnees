# 3_features_sociodemo — Variables dérivées (socio-démo & accès)

Étape **additive** branchée en aval du nettoyage. Elle réutilise le cadre du
groupe (`config.R`, `utils.R`, `here()`) et n'altère aucun fichier existant.

## Entrée / sortie
- **Entrée** : `data/output/findex_uemoa_clean.csv` (sortie nettoyée du pipeline)
- **Sortie** : `data/output/findex_uemoa_features.csv` (mêmes lignes + variables dérivées)

## Variables ajoutées
| Variable | Définition |
|---|---|
| `groupe_age` | Tranches d'âge : 15-24, 25-34, 35-44, 45-54, 55-64, 65+ |
| `jeune_15_24` | 1 = individu de 15 à 24 ans, 0 sinon |
| `detention_carte` | 1 = possède une carte de débit OU de crédit, 0 sinon (NA si inconnu) |
| `inclusion_financiere` | Indicateur synthétique = possède un compte (1/0) |

## Lancement
```bash
# via l'orchestrateur
Rscript run_all.R features
# ou directement
Rscript --vanilla 3_features_sociodemo/1_derive_sociodemo_access.R
```

Le script affiche un contrôle qualité (répartition par tranche d'âge, taux
d'inclusion financière par pays, détention de carte).
