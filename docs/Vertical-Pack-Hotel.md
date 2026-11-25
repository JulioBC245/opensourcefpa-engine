# Hotel vertical pack

Design only. The wizard still asks for a column mapping. This note is the contract a hotel pack would pre-fill, so steps 3 and 4 become a check against a known file rather than a search.

## Who it is for

An independent hotel, or a small group, that already exports three files each month: a P&L extract, an occupancy file, and a parameter file. The pack was proved against a 36-month hotel dataset, which is not included in this repository.

## The three files the pack expects

P&L extract, one row per caption and month:

- `Poste comptable`
- `Periode` as `YYYY-MM`
- `Montant (€)`, costs as positive numbers

Occupancy file, one row per month:

- `periode`
- `capacite`
- `taux_utilisation`
- `tarif`
- `nuitees_vendues`
- `effectif`
- `cout_moyen`

Parameters, one row per month, with the assumption ids the engine already knows: `taux.impot`, `dso`, `dpo`, `dio`, `cout.variable.pct`, `charge.fixe.loyer`.

Revenue in this pack is nights times rate. That is the only quantity-times-price mechanism the pack turns on. Payroll rate times headcount stays off until the occupancy file also carries `effectif_budget` and `cout_moyen_budget`.

## What the wizard would stop asking

A hotel pack would lock these and show them as a calibration, not as empty dropdowns:

- P&L grain is monthly. History is not interpolated.
- Caption `Chiffre d'affaires` maps to `REV`. Tax on profit maps to `TAX`. Other tax captions map to `OPEX_OTHER`.
- Volume columns map to the six names above, by those exact headers.
- Fiscal year ends in December unless the user changes one field.
- Horizon defaults to the rest of the fiscal year once a forecast is wanted. Until then it stays zero, so a historical run stays historical.

The user still confirms the folder path and which months are closed. The pack does not guess those.

## Code

- Contract: the hotel pack in the opensourcefpa.com configurator.
- Wizard: when the sector is hotel, step 3 shows the pack banner and proposes caption maps locally before calling the assistant. Step 4 names the hotel mechanism socle and the volume headers.
