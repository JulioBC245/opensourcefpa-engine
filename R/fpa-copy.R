# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-copy.R — New screen sentences, French and English side by side.
# LAST CHANGE: 2026-09-22
# Parameter: from this date, a new user-facing sentence is stored here as fr and en.
# Strings written before this date stay as they are. The screen reads fr until a
# later translation pass. Do not recite both languages in the chat.

fpa_copy <- list(
  nav_sources = c(fr = "Sources", en = "Sources"),
  nav_etats = c(fr = "États", en = "Statements"),
  nav_controle = c(fr = "Contrôle", en = "Control"),
  nav_pilotage = c(fr = "Pilotage", en = "Steering"),
  readings_lead = c(
    fr = paste(
      "Ce tableau pose trois questions à chaque série. Plus la case est foncée, plus la réponse est forte. Une case en pointillé veut dire que la mesure n'a pas pu être faite : ce n'est pas un zéro.",
      "Un gros montant en euros et un lien solide sont deux choses différentes. Une série peut peser lourd sans que l'historique le prouve, et une série prouvée peut peser peu. Cliquez une case pour ouvrir la fiche de la série.",
      sep = "\n"
    ),
    en = paste(
      "This table asks each series three questions. The darker the cell, the stronger the answer. A dotted cell means the measure could not be made: that is not a zero.",
      "A large euro amount and a solid link are two different things. A series can weigh a lot without the history proving it, and a proven series can weigh little. Click a cell to open the series card.",
      sep = "\n"
    )
  ),
  readings_score = c(
    fr = "Une note de 0 à 1. Quand le lien est mesuré, elle mélange sa force (75 %) et la confiance déclarée au départ (25 %). Pour une série qui entre dans un calcul, la note affichée est cette confiance de départ. Sans mesure, la note garde 35 % de la confiance de départ.",
    en = "A mark from 0 to 1. When the link is measured, it blends its strength (75%) with the confidence declared at setup (25%). For a series that is part of a calculation, the mark shown is that starting confidence. With no measure, the mark keeps 35% of the starting confidence."
  ),
  readings_partial = c(
    fr = "Deux séries peuvent sembler liées simplement parce que l'année a été bonne. Le moteur retire l'effet de %s et regarde ce qui reste, entre −1 et +1. Si le lien disparaît, il venait de la saison et non de la série.",
    en = "Two series can look linked only because the year was a good one. The engine removes the effect of %s and looks at what remains, between −1 and +1. If the link disappears, it came from the season and not from the series."
  ),
  readings_partial_none = c(
    fr = "Aucune série de volume n'a été reconnue dans vos fichiers, donc ce test n'a pas pu être fait. Ce n'est pas un résultat nul.",
    en = "No volume series was recognised in your files, so this test could not be run. That is not a zero result."
  ),
  readings_euro = c(
    fr = "L'écart de résultat d'exploitation quand cette seule série passe de 0,9 à 1,1 fois son niveau, tout le reste égal par ailleurs. Le montant vient du calcul du moteur, pas d'une corrélation. « Pas chiffré » veut dire qu'aucun calcul ne relie encore la série au compte de résultat.",
    en = "The change in operating income when this series alone moves from 0.9 to 1.1 times its level, everything else held. The amount comes from the engine calculation, not from a correlation. \"Not priced\" means no calculation links the series to the income statement yet."
  ),
  readings_euro_hist = c(
    fr = "Aucun montant pour l'instant : aucun calcul ne relie encore ces séries au compte de résultat. Une corrélation ne suffit pas à chiffrer un effet, car en saison le tarif, le taux d'utilisation et les nuitées montent tous ensemble.",
    en = "No amount yet: no calculation links these series to the income statement. A correlation is not enough to price an effect, because in season the rate, occupancy and nights all rise together."
  ),
  catchup_click = c(
    fr = "Un rectangle en retard s'ouvre. On y voit les deux dernières dates, puis la date qui manque, avec une cellule vide.",
    en = "A late rectangle opens. It shows the last two dates, then the missing date, with an empty cell."
  ),
  catchup_lead = c(
    fr = "Les deux dernières dates du fichier, puis la date qui lève le retard. Une cellule vide n'est pas écrite. Les mois entre les deux ne sont pas remplis.",
    en = "The last two dates in the file, then the date that clears the late mark. An empty cell is not written. The months in between are not filled."
  ),
  catchup_path = c(
    fr = "Le moteur écrit dans votre fichier, ici.",
    en = "The engine writes into your file, here."
  ),
  catchup_one = c(
    fr = "Une seule date est dans le fichier.",
    en = "The file has only one date."
  ),
  catchup_series = c(
    fr = "Série",
    en = "Series"
  ),
  catchup_enter = c(
    fr = "à saisir",
    en = "to enter"
  ),
  catchup_excel = c(
    fr = "Ce retard vient de la date déclarée dans la configuration. Le fichier est un classeur Excel. Cet écran ne le réécrit pas.",
    en = "This late mark is the date declared in the configuration. The file is an Excel workbook. This screen does not rewrite it."
  ),
  catchup_unavailable = c(
    fr = "Ce fichier ne peut pas être complété ici.",
    en = "This file cannot be completed here."
  ),
  catchup_save = c(
    fr = "Enregistrer",
    en = "Save"
  ),
  catchup_close = c(
    fr = "Fermer",
    en = "Close"
  ),
  catchup_none = c(
    fr = "Aucune valeur saisie.",
    en = "No value was entered."
  ),
  catchup_done = c(
    fr = "%s ligne(s) ajoutée(s). La prévision ne relit ce fichier qu'au prochain lancement.",
    en = "%s line(s) added. The forecast does not re-read this file until the next run."
  ),
  catchup_year = c(
    fr = "La nouvelle colonne porte l'année. La prévision continue de lire la colonne déjà reliée dans la configuration.",
    en = "The new column carries the year. The forecast keeps reading the column already linked in the configuration."
  ),
  catchup_cells = c(
    fr = "%s valeur(s) écrite(s) dans la colonne de l'année. La prévision continue de lire la colonne déjà reliée.",
    en = "%s value(s) written in the year column. The forecast keeps reading the column already linked."
  ),
  bs_ok = c(
    fr = "Bilan équilibré.",
    en = "The balance sheet balances."
  ),
  bs_bad = c(
    fr = "Bilan déséquilibré.",
    en = "The balance sheet does not balance."
  ),
  bs_line = c(
    fr = "%s : actif %s, passif et capitaux %s, écart %s.",
    en = "%s: assets %s, liabilities and equity %s, gap %s."
  ),
  bs_note = c(
    fr = "La teinte suit la taille du montant sur sa ligne. Le fond ambre d'un mois est une prévision.",
    en = "The tint follows the size of the amount on its own line. An amber month is a forecast."
  ),
  bs_hidden = c(
    fr = "Ligne masquée, tout à zéro : %s.",
    en = "Line hidden, all zero: %s."
  ),
  bs_window = c(
    fr = "Douze derniers mois.",
    en = "Last twelve months."
  ),
  var_none = c(
    fr = "Aucun mois ne dépasse le seuil de matérialité.",
    en = "No month clears the materiality floor."
  ),
  var_fav = c(
    fr = "favorable",
    en = "favourable"
  ),
  var_unfav = c(
    fr = "défavorable",
    en = "unfavourable"
  ),
  var_sign = c(
    fr = "Écart défavorable en dépense et écart défavorable en chiffre d'affaires s'écrivent en négatif ici.",
    en = "Unfavourable expense and unfavourable revenue variances are written as negative here."
  ),
  var_headline_plain = c(
    fr = "%s est %s au %s de %s EUR sur %s.",
    en = "%s is %s to %s by %s EUR in %s."
  ),
  var_headline_one = c(
    fr = "%s est %s au %s de %s EUR sur %s, porté par %s (%s EUR).",
    en = "%s is %s to %s by %s EUR in %s, driven by %s (%s EUR)."
  ),
  var_headline_driven = c(
    fr = "%s est %s au %s de %s EUR sur %s, porté par %s (%s EUR), partiellement compensé par %s (%s EUR).",
    en = "%s is %s to %s by %s EUR in %s, mostly driven by %s (%s EUR), partially offset by %s (%s EUR)."
  ),
  var_unexplained_left = c(
    fr = "%s EUR de cet écart restent à expliquer.",
    en = "%s EUR of this variance remain to explain."
  ),
  var_why = c(
    fr = "Porté par %s, %s EUR.",
    en = "Driven by %s, %s EUR."
  ),
  var_why_none = c(
    fr = "Pas de décomposition sur cette ligne.",
    en = "No decomposition on this line."
  ),
  var_finding = c(
    fr = "%s %s : écart %s EUR, dont %s EUR restent à expliquer.",
    en = "%s %s: variance %s EUR, of which %s EUR remain to explain."
  ),
  var_findings_title = c(
    fr = "Écarts au-dessus du seuil",
    en = "Variances above the floor"
  ),
  var_col_periode = c(fr = "Période", en = "Period"),
  var_col_actual = c(fr = "Réalisé", en = "Actual"),
  var_col_budget = c(fr = "Budget", en = "Budget"),
  var_col_variance = c(fr = "Écart", en = "Variance"),
  var_col_pct = c(fr = "Écart %", en = "Variance %"),
  var_col_price = c(fr = "Prix", en = "Price"),
  var_col_volume = c(fr = "Volume", en = "Volume"),
  var_col_mix = c(fr = "Mix", en = "Mix"),
  var_col_rate = c(fr = "Taux", en = "Rate"),
  var_col_head = c(fr = "Effectifs", en = "Headcount"),
  var_col_unexplained = c(fr = "Reste à expliquer", en = "Still to explain"),
  var_hidden_cols = c(
    fr = "Colonnes masquées (tout à zéro) : %s.",
    en = "Hidden columns (all zero): %s."
  ),
  var_card_total = c(fr = "Écart total", en = "Total variance"),
  var_card_year = c(
    fr = "Écart d'atterrissage de l'année",
    en = "Year landing variance"
  ),
  var_col_landing = c(fr = "Atterrissage", en = "Landing"),
  var_part_actual = c(fr = "réalisé, %s", en = "actual, %s"),
  var_part_fc = c(fr = "prévision, %s", en = "forecast, %s"),
  var_col_actual_fc = c(fr = "Réalisé ou prévision", en = "Actual or forecast"),
  var_chip_fc = c(fr = "prévision", en = "forecast"),
  var_trace_actual = c(fr = "Réalisé", en = "Actual"),
  var_trace_fc = c(fr = "Prévision", en = "Forecast"),
  var_fc_note = c(
    fr = "Les lignes marquées « prévision » (%s) comparent la prévision au budget. Ces mois ne sont pas encore clos.",
    en = "Rows marked forecast (%s) compare the forecast with the budget. Those months are not closed yet."
  ),
  var_eq_total = c(
    fr = "Les morceaux ci-dessous s'additionnent pour retrouver l'écart.",
    en = "The pieces below add up to the variance."
  ),
  var_eq_price = c(
    fr = "Sur chaque mois, le prix est la quantité budgétée multipliée par l'écart de tarif. Le carton est la somme de ces mois.",
    en = "Each month, price is budget quantity times the tariff gap. The card is the sum of those months."
  ),
  var_eq_volume = c(
    fr = "Sur chaque mois, le volume est le tarif budgété multiplié par l'écart de quantité. Le carton est la somme de ces mois.",
    en = "Each month, volume is the budget tariff times the quantity gap. The card is the sum of those months."
  ),
  var_eq_mix = c(
    fr = "Sur chaque mois, le mix est l'écart de quantité multiplié par l'écart de tarif. Le carton est la somme de ces mois.",
    en = "Each month, mix is the quantity gap times the tariff gap. The card is the sum of those months."
  ),
  var_eq_rate = c(
    fr = "Sur chaque mois, le taux est l'effectif budgété multiplié par l'écart de coût moyen. Le carton est la somme de ces mois.",
    en = "Each month, the rate is budget headcount times the average-cost gap. The card is the sum of those months."
  ),
  var_eq_head = c(
    fr = "Sur chaque mois, les effectifs sont le coût moyen budgété multiplié par l'écart d'effectif. Le carton est la somme de ces mois.",
    en = "Each month, headcount is the budget average cost times the headcount gap. The card is the sum of those months."
  ),
  var_eq_unexp = c(
    fr = "Sur chaque mois, le reste est l'écart moins les morceaux déjà nommés. Le carton est la somme de ces mois.",
    en = "Each month, the remainder is the variance minus the pieces already named. The card is the sum of those months."
  ),
  var_rest_none = c(
    fr = "Aucun mois de prévision sur cette ligne.",
    en = "No forecast month on this line."
  ),
  var_rest_flat = c(
    fr = "Reste de l'année, %s à %s : l'écart au budget reste sous le seuil. L'année tient dans les mois clos, %s EUR.",
    en = "Rest of the year, %s to %s: the budget gap stays under the floor. The year sits in the closed months, %s EUR."
  ),
  var_rest_gap = c(
    fr = "Reste de l'année, %s à %s : écart au budget de %s EUR. Avec les mois clos, l'année est à %s EUR.",
    en = "Rest of the year, %s to %s: budget gap of %s EUR. With the closed months, the year is %s EUR."
  ),
  var_rest_save = c(
    fr = "Enregistrer cette version",
    en = "Save this version"
  ),
  var_kicker = c(fr = "Pilotage · Écarts au budget", en = "Steering · Budget variances"),
  var_title = c(fr = "Écarts : ce qui sépare l'année du budget", en = "Variances: what separates the year from the budget"),
  var_intro = c(
    fr = "Chaque ligne du compte de résultat est comparée à son budget, mois par mois. Les mois clos sont du réalisé, les mois ouverts sont la prévision en cours. Sur cette page, un écart positif aide le résultat et un écart négatif le pénalise, qu'il s'agisse d'une recette ou d'une dépense.",
    en = "Each income-statement line is compared with its budget, month by month. Closed months are actuals, open months are the current forecast. On this page, a positive variance helps the result and a negative one hurts it, whether the line is income or expense."
  ),
  var_empty = c(
    fr = "Pas encore d'écarts. Le moteur les calcule dès qu'un budget est chargé, puis les écrit dans work/variance-monthly.csv.",
    en = "No variances yet. The engine computes them once a budget is loaded, then writes them to work/variance-monthly.csv."
  ),
  var_tile_closed = c(fr = "Mois clos", en = "Closed months"),
  var_tile_rest = c(fr = "Reste de l'année", en = "Rest of the year"),
  var_tile_year = c(fr = "Atterrissage de l'année", en = "Year landing"),
  var_tile_closed_sub = c(fr = "%s, réalisé contre budget", en = "%s, actual against budget"),
  var_tile_rest_sub = c(fr = "%s, prévision contre budget", en = "%s, forecast against budget"),
  var_tile_year_sub = c(fr = "Mois clos et prévision additionnés", en = "Closed months and forecast added"),
  var_tile_none_sub = c(fr = "Aucun mois de prévision", en = "No forecast month"),
  var_join_then = c(fr = "puis", en = "then"),
  var_join_eq = c(fr = "égale", en = "equals"),
  var_tile_calc = c(fr = "Voir le calcul", en = "Show the calculation"),
  var_math_lines = c(fr = "Lignes du résultat", en = "Result lines"),
  var_math_months = c(fr = "Mois par mois", en = "Month by month"),
  var_math_periods = c(fr = "Les deux périodes", en = "The two periods"),
  var_math_total = c(fr = "Total", en = "Total"),
  var_math_hidden = c(fr = "Lignes masquées, toutes à zéro : %s.", en = "Hidden lines, all zero: %s."),
  var_math_months_hidden = c(fr = "Mois masqués, tous à zéro : %s.", en = "Hidden months, all zero: %s."),
  var_tile_head = c(fr = "Résultat d'exploitation contre budget", en = "Operating income against budget"),
  var_hero_oi = c(
    fr = "Le résultat d'exploitation finit l'année à %s EUR du budget : %s EUR sur les mois clos, %s EUR sur la prévision.",
    en = "Operating income ends the year %s EUR from budget: %s EUR on the closed months, %s EUR on the forecast."
  ),
  var_hero_oi_closed = c(
    fr = "Le résultat d'exploitation est à %s EUR du budget sur les mois clos.",
    en = "Operating income is %s EUR from budget on the closed months."
  ),
  var_hero_top = c(
    fr = "La ligne qui pèse le plus sur l'année : %s, %s EUR. Elle est ouverte en détail plus bas.",
    en = "The line that weighs most on the year: %s, %s EUR. It is open in detail below."
  ),
  var_span = c(fr = "%s à %s", en = "%s to %s"),
  var_heat_title = c(fr = "Toutes les lignes, mois par mois", en = "Every line, month by month"),
  var_heat_lead = c(
    fr = "Chaque case est l'écart du mois. Bleu-vert avec un signe + : l'écart aide le résultat. Brun avec un signe − : il le pénalise. Plus la case est foncée, plus l'écart est grand. Les mois de prévision ont un bord en pointillé. Cliquez une ligne ou une case pour l'ouvrir en détail.",
    en = "Each cell is the month's variance. Blue-green with a + sign: the variance helps the result. Brown with a − sign: it hurts it. The darker the cell, the bigger the variance. Forecast months have a dotted border. Click a line or a cell to open it in detail."
  ),
  var_heat_band_actual = c(fr = "Réalisé", en = "Actual"),
  var_heat_band_fc = c(fr = "Prévision", en = "Forecast"),
  var_heat_band_tot = c(fr = "Totaux", en = "Totals"),
  var_heat_col_line = c(fr = "Ligne", en = "Line"),
  var_heat_col_closed = c(fr = "Mois clos", en = "Closed"),
  var_heat_col_rest = c(fr = "Reste", en = "Rest"),
  var_heat_col_year = c(fr = "Année", en = "Year"),
  var_heat_oi_tip = c(
    fr = "La somme des écarts des lignes d'exploitation, du chiffre d'affaires aux dotations. Le résultat financier, l'exceptionnel et l'impôt sont en dessous.",
    en = "The sum of the operating lines' variances, from revenue to depreciation. Financial result, exceptional items and tax sit below."
  ),
  var_heat_cell_tip = c(
    fr = "%s, %s : %s %s EUR, budget %s EUR, écart %s EUR.",
    en = "%s, %s: %s %s EUR, budget %s EUR, variance %s EUR."
  ),
  var_heat_oi_cell_tip = c(
    fr = "Résultat d'exploitation, %s : écart %s EUR, toutes lignes d'exploitation additionnées.",
    en = "Operating income, %s: variance %s EUR, all operating lines added."
  ),
  var_heat_word_actual = c(fr = "réalisé", en = "actual"),
  var_heat_word_fc = c(fr = "prévision", en = "forecast"),
  var_heat_hidden = c(fr = "Lignes sans aucun écart, masquées : %s.", en = "Lines with no variance at all, hidden: %s."),
  var_heat_dense = c(
    fr = "La période est longue : les montants sont cachés dans les cases. Survolez une case pour le lire.",
    en = "The period is long: amounts are hidden in the cells. Hover over a cell to read it."
  ),
  var_detail_kicker = c(fr = "Ligne ouverte", en = "Open line"),
  var_s_year = c(
    fr = "Mois clos, %s : %s EUR du budget. Prévision, %s : %s EUR. L'année atterrit à %s EUR du budget.",
    en = "Closed months, %s: %s EUR from budget. Forecast, %s: %s EUR. The year lands %s EUR from budget."
  ),
  var_s_year_closed = c(
    fr = "Mois clos, %s : %s EUR du budget. Aucun mois de prévision sur cette ligne.",
    en = "Closed months, %s: %s EUR from budget. No forecast month on this line."
  ),
  var_s_flat = c(
    fr = "Le budget de cette ligne est plat : %s EUR chaque mois. Le réalisé, lui, va de %s à %s EUR. Les écarts mensuels mesurent donc d'abord la saison, pas la performance du mois.",
    en = "This line's budget is flat: %s EUR every month. Actuals range from %s to %s EUR. Monthly variances therefore measure the season first, not the month's performance."
  ),
  var_s_parts = c(fr = "%s expliquent %s EUR sur l'année.", en = "%s explain %s EUR over the year."),
  var_s_part_one = c(fr = "%s explique %s EUR sur l'année.", en = "%s explains %s EUR over the year."),
  var_and = c(fr = " et ", en = " and "),
  var_s_phase = c(
    fr = "Le phasage du budget en explique %s : sur l'année, le budget du compte de résultat compte %s EUR, alors que quantité × tarif budgétés donnent %s EUR, répartis selon la saison.",
    en = "Budget phasing explains %s: over the year, the income-statement budget holds %s EUR, while budgeted quantity × rate give %s EUR, spread by season."
  ),
  var_s_unlinked = c(
    fr = "Il reste %s EUR sans explication, dont %s EUR en %s : ce mois-là, le compte de résultat dit %s EUR alors que quantité × tarif réalisés donnent %s EUR.",
    en = "%s EUR remain unexplained, of which %s EUR in %s: that month, the income statement says %s EUR while actual quantity × rate give %s EUR."
  ),
  var_s_unlinked_small = c(fr = "Il reste %s EUR sans explication.", en = "%s EUR remain unexplained."),
  var_s_unexp_rest = c(fr = "Il reste %s EUR à expliquer.", en = "%s EUR remain to explain."),
  var_s_unexp_only = c(
    fr = "Aucun mécanisme ne découpe cette ligne : tout l'écart de l'année, %s EUR, reste à expliquer.",
    en = "No mechanism splits this line: the whole year variance, %s EUR, remains to explain."
  ),
  var_mode_gap = c(fr = "Écart par mois", en = "Variance by month"),
  var_mode_level = c(fr = "Réalisé contre budget", en = "Actual against budget"),
  var_plot_cum = c(fr = "Cumul de l'écart", en = "Cumulative variance"),
  var_plot_budget = c(fr = "Budget", en = "Budget"),
  var_plot_vol_budget = c(fr = "Quantité × tarif budgétés", en = "Budgeted quantity × rate"),
  var_plot_actual = c(fr = "Réalisé", en = "Actual"),
  var_plot_fc = c(fr = "Prévision", en = "Forecast"),
  var_plot_gap_cap = c(
    fr = "Chaque barre est l'écart du mois, découpé par cause. Au-dessus de zéro, l'écart aide le résultat ; en dessous, il le pénalise. La ligne suit le cumul depuis %s. Les barres pâles sont des mois de prévision.",
    en = "Each bar is the month's variance, split by cause. Above zero, the variance helps the result; below, it hurts it. The line tracks the running total since %s. Pale bars are forecast months."
  ),
  var_plot_level_cap = c(
    fr = "Les barres sont le montant du mois : pleines pour les mois clos, pâles pour la prévision. La ligne orange est le budget.",
    en = "Bars are the month's amount: solid for closed months, pale for the forecast. The orange line is the budget."
  ),
  var_plot_level_vol = c(
    fr = "La ligne en pointillé est quantité × tarif budgétés : l'écart entre les deux lignes est le phasage du budget.",
    en = "The dotted line is budgeted quantity × rate: the gap between both lines is the budget phasing."
  ),
  var_bridge_title = c(fr = "Du budget à l'atterrissage", en = "From budget to landing"),
  var_bridge_lead = c(
    fr = "Le budget de l'année, puis chaque cause d'écart, jusqu'au montant atterri, mois clos et prévision compris. Les colonnes foncées sont des montants, les autres des écarts.",
    en = "The year budget, then each cause of variance, up to the landed amount, closed months and forecast included. Dark columns are amounts, the others are variances."
  ),
  var_bridge_budget = c(fr = "Budget de l'année", en = "Year budget"),
  var_bridge_landing = c(fr = "Atterrissage", en = "Landing"),
  var_bridge_closed = c(fr = "Écart des mois clos", en = "Closed-month variance"),
  var_bridge_fc = c(fr = "Écart de la prévision", en = "Forecast variance"),
  var_bridge_tie = c(
    fr = "Budget de l'année %s EUR, atterrissage %s EUR : un écart de %s EUR pour le résultat. Les colonnes d'écart montent quand le montant de la ligne augmente ; leur couleur et leur signe disent si cela aide ou pénalise le résultat.",
    en = "Year budget %s EUR, landing %s EUR: a variance of %s EUR for the result. Variance columns rise when the line amount grows; their colour and sign say whether that helps or hurts the result."
  ),
  var_parts_title = c(fr = "Ce qui explique l'écart", en = "What explains the variance"),
  var_parts_lead = c(
    fr = "Les causes de l'écart de l'année, de la plus grande à la plus petite. Cliquez une cause pour voir son calcul et les fichiers lus.",
    en = "The causes of the year variance, largest first. Click a cause to see its calculation and the files read."
  ),
  var_parts_none = c(
    fr = "Aucune cause nommée sur cette ligne : le moteur n'a pas de mécanisme pour la découper.",
    en = "No named cause on this line: the engine has no mechanism to split it."
  ),
  var_part_phase = c(fr = "Phasage du budget", en = "Budget phasing"),
  var_part_unlinked = c(fr = "Non relié", en = "Unreconciled"),
  var_part_share = c(fr = "%s %% des causes", en = "%s%% of causes"),
  var_pt_price = c(
    fr = "Le tarif réalisé s'écarte du tarif budgété, appliqué aux quantités du budget.",
    en = "The actual rate differs from the budgeted rate, applied to budgeted quantities."
  ),
  var_pt_volume = c(
    fr = "Les quantités vendues s'écartent du budget, au tarif budgété.",
    en = "Quantities sold differ from budget, at the budgeted rate."
  ),
  var_pt_mix = c(
    fr = "L'effet croisé : l'écart de quantité multiplié par l'écart de tarif.",
    en = "The cross effect: the quantity gap times the rate gap."
  ),
  var_pt_rate = c(
    fr = "Le coût moyen s'écarte du budget, appliqué à l'effectif budgété.",
    en = "The average cost differs from budget, applied to budgeted headcount."
  ),
  var_pt_head = c(
    fr = "L'effectif s'écarte du budget, au coût moyen budgété.",
    en = "Headcount differs from budget, at the budgeted average cost."
  ),
  var_pt_phase = c(
    fr = "Le budget du compte de résultat est réparti autrement que quantité × tarif budgétés. C'est un écart de calendrier, pas de performance.",
    en = "The income-statement budget is spread differently from budgeted quantity × rate. It is a timing gap, not a performance gap."
  ),
  var_pt_unlinked = c(
    fr = "La part du compte de résultat qui ne se retrouve pas dans quantité × tarif réalisés.",
    en = "The part of the income statement that is not found in actual quantity × rate."
  ),
  var_pt_unexp = c(
    fr = "L'écart qu'aucun mécanisme ne découpe encore.",
    en = "The variance no mechanism splits yet."
  ),
  var_pt_closed = c(fr = "Somme des écarts des mois clos.", en = "Sum of the closed-month variances."),
  var_pt_fc = c(fr = "Somme des écarts des mois de prévision.", en = "Sum of the forecast-month variances."),
  var_tbl_title = c(fr = "Mois par mois", en = "Month by month"),
  var_tbl_lead = c(
    fr = "Montants en euros. Les écarts sont signés : + aide le résultat, − le pénalise. Cliquez un mois pour le surligner dans le graphique et dans ce tableau.",
    en = "Amounts in euros. Variances are signed: + helps the result, − hurts it. Click a month to highlight it in the chart and in this table."
  ),
  var_col_cum = c(fr = "Cumul", en = "Running total"),
  var_col_phase = c(fr = "Phasage", en = "Phasing"),
  var_col_unlinked = c(fr = "Non relié", en = "Unreconciled"),
  var_tbl_total = c(fr = "Total", en = "Total"),
  var_pv_title = c(fr = "Prix ou volume : d'où vient l'écart du chiffre d'affaires", en = "Price or volume: where the revenue variance comes from"),
  var_pv_lead = c(
    fr = "Chaque mois compare deux choses au budget : les nuitées vendues et le tarif moyen. L'effet volume valorise l'écart de nuitées au tarif budgété. L'effet prix applique l'écart de tarif aux nuitées du budget. Le mix est le produit des deux écarts. Le calcul est écrit sous chaque montant.",
    en = "Each month compares two things with the budget: nights sold and the average rate. The volume effect values the night gap at the budgeted rate. The price effect applies the rate gap to the budgeted nights. Mix is the product of both gaps. The calculation is written under each amount."
  ),
  var_pv_chart_cap = c(
    fr = "Trois barres par mois, toujours dans le même ordre : volume, prix, mix. Les trois lignes, sur l'axe de droite, suivent le cumul de chaque effet depuis le premier mois. Les deux axes passent par zéro au même endroit : au-dessus, l'effet aide le résultat ; en dessous, il le pénalise. Une barre pâle est un mois de prévision. Survolez une barre pour le calcul, cliquez-la pour la retrouver dans le tableau.",
    en = "Three bars a month, always in the same order: volume, price, mix. The three lines, on the right axis, track the running total of each effect from the first month. Both axes cross zero at the same place: above, the effect helps the result; below, it hurts it. A pale bar is a forecast month. Hover a bar for the calculation, click it to find it in the table."
  ),
  var_pv_cum_volume = c(fr = "Cumul volume", en = "Cumulative volume"),
  var_pv_cum_price = c(fr = "Cumul prix", en = "Cumulative price"),
  var_pv_cum_mix = c(fr = "Cumul mix", en = "Cumulative mix"),
  var_pv_axis_cum = c(fr = "Cumul, EUR", en = "Cumulative, EUR"),
  var_pv_src = c(fr = "D'où viennent ces chiffres", en = "Where these figures come from"),
  var_pv_src_lead = c(
    fr = "Les nuitées vendues, le tarif, les nuitées du budget et le tarif du budget sont lus dans ce fichier. Le prix, le volume et le mix sont le calcul écrit sous chaque montant.",
    en = "Nights sold, the rate, budget nights and the budget rate are read from this file. Price, volume and mix are the calculation written under each amount."
  ),
  var_pv_src_cols = c(
    fr = "Colonnes lues : nuitées vendues, tarif, nuitées du budget, tarif du budget.",
    en = "Columns read: nights sold, rate, budget nights, budget rate."
  ),
  var_pv_src_role = c(fr = "Nuitées et tarifs", en = "Nights and rates"),
  var_pv_src_none = c(
    fr = "Aucun fichier de volumes n'est relié à ce tableau.",
    en = "No volumes file is linked to this table."
  ),
  var_pv_year = c(
    fr = "%s : le prix apporte %s EUR, le volume %s EUR, le mix %s EUR. Ensemble, %s EUR.",
    en = "%s: price brings %s EUR, volume %s EUR, mix %s EUR. Together, %s EUR."
  ),
  var_pv_top_price = c(fr = "Sur ces mois, le tarif pèse plus que les nuitées.", en = "Over these months, the rate weighs more than the nights."),
  var_pv_top_volume = c(fr = "Sur ces mois, les nuitées pèsent plus que le tarif.", en = "Over these months, the nights weigh more than the rate."),
  var_pv_rate_line = c(fr = "Tarif sous le budget : %s. Au-dessus : %s.", en = "Rate under budget: %s. Above: %s."),
  var_pv_qty_line = c(fr = "Nuitées sous le budget : %s. Au-dessus : %s.", en = "Nights under budget: %s. Above: %s."),
  var_pv_none = c(fr = "aucun mois", en = "no month"),
  var_pv_up = c(fr = "au-dessus du budget", en = "above budget"),
  var_pv_down = c(fr = "sous le budget", en = "under budget"),
  var_pv_eq = c(fr = "au budget", en = "on budget"),
  var_pv_col_month = c(fr = "Mois", en = "Month"),
  var_pv_col_qty = c(fr = "Nuitées, budget puis vendues", en = "Nights, budget then sold"),
  var_pv_col_rate = c(fr = "Tarif moyen en EUR, budget puis réalisé", en = "Average rate in EUR, budget then actual"),
  var_pv_col_vol = c(fr = "Effet volume", en = "Volume effect"),
  var_pv_col_price = c(fr = "Effet prix", en = "Price effect"),
  var_pv_col_mix = c(fr = "Mix", en = "Mix"),
  var_pv_col_sum = c(fr = "Prix, volume et mix", en = "Price, volume and mix"),
  var_pv_rest = c(
    fr = "Le reste de l'écart du mois, le phasage du budget et la part non reliée, ne vient ni du tarif ni des nuitées. Il figure dans le graphique au-dessus et dans le tableau Mois par mois.",
    en = "The rest of the month's variance, budget phasing and the unreconciled part, comes from neither the rate nor the nights. It is in the chart above and in the Month by month table."
  ),
  var_pv_fc = c(
    fr = "Mois de prévision non découpés, %s : leurs nuitées et leur tarif sont ceux du budget.",
    en = "Forecast months not split, %s: their nights and rate are the budget ones."
  ),
  var_fold_open_all = c(fr = "Tout ouvrir", en = "Expand all"),
  var_fold_close_all = c(fr = "Tout replier", en = "Collapse all"),
  var_heat_fact = c(
    fr = "Année : résultat d'exploitation %s EUR. Ligne ouverte, %s : %s EUR.",
    en = "Year: operating income %s EUR. Open line, %s: %s EUR."
  ),
  var_heat_fact_oi = c(
    fr = "Année : résultat d'exploitation %s EUR.",
    en = "Year: operating income %s EUR."
  ),
  var_line_fact = c(fr = "%s. %s", en = "%s. %s"),
  var_pv_none_line = c(
    fr = "Cette ligne ne se découpe pas en prix et en volume.",
    en = "This line does not split into price and volume."
  ),
  var_bridge_fact = c(
    fr = "Budget %s EUR, atterrissage %s EUR, écart %s EUR.",
    en = "Budget %s EUR, landing %s EUR, variance %s EUR."
  ),
  var_parts_fact = c(
    fr = "La plus grande cause : %s, %s EUR, %s %% des causes.",
    en = "The largest cause: %s, %s EUR, %s%% of the causes."
  ),
  var_parts_fact_none = c(
    fr = "Aucune cause nommée sur cette ligne.",
    en = "No named cause on this line."
  ),
  var_tbl_fact = c(
    fr = "Année : réalisé %s EUR, budget %s EUR, écart %s EUR.",
    en = "Year: actual %s EUR, budget %s EUR, variance %s EUR."
  ),
  sea_title = c(fr = "Années qui dessinent le budget", en = "Years that draw the budget"),
  sea_lead = c(
    fr = "Cochez les exercices passés à garder. Le total voté de l'année en cours ne change pas : seuls les mois bougent. Décochez une année qui ne ressemble pas aux autres.",
    en = "Tick the past years to keep. This year's voted total does not change: only the months move. Untick a year that does not look like the others."
  ),
  sea_off = c(
    fr = "Le budget reste celui qui a été voté, mois par mois. Pour le caler sur les années passées, la case se trouve à l'étape Société du paramétrage.",
    en = "The budget stays as voted, month by month. The box that follows past years is on the Company step of the setup."
  ),
  sea_apart = c(fr = "%s · à part", en = "%s · stands apart"),
  sea_totals = c(fr = "%s.", en = "%s."),
  sea_total_one = c(fr = "%s : %s EUR de chiffre d'affaires", en = "%s: %s EUR of revenue"),
  sea_dropped = c(fr = "Mois laissés de côté, trop différents des autres années : %s.", en = "Months set aside, too different from the other years: %s."),
  sea_dropped_none = c(fr = "Aucun mois n'est assez différent des autres pour être laissé de côté.", en = "No month is different enough from the others to be set aside."),
  sea_need = c(fr = "Aucun exercice coché. Le budget reste plat.", en = "No year ticked. The budget stays flat."),
  sea_one = c(
    fr = "Un seul exercice : le budget reprend sa forme, mois par mois. Sans une autre année, aucun mois ne peut être écarté.",
    en = "One year only: the budget takes that year's shape, month by month. With no other year, no month can be set aside."
  ),
  sea_short = c(
    fr = "%s n'est pas proposé : l'exercice n'est pas complet (%s EUR).",
    en = "%s is not offered: the year is not complete (%s EUR)."
  ),
  sea_echo = c(fr = "Le budget de l'année est calé sur %s. Le total voté ne change pas.", en = "This year's budget follows %s. The voted total does not change."),
  sea_flat_echo = c(fr = "Budget plat : le même montant chaque mois, comme voté.", en = "Flat budget: the same amount each month, as voted."),
  var_rest_saved = c(
    fr = "Version enregistrée (%s). Elle reprend la prévision déjà calculée, sans la recalculer.",
    en = "Version saved (%s). It keeps the forecast already computed, and does not recompute it."
  ),
  synth_margin = c(
    fr = "Dernier mois clos %s. Budget %s. Même mois l'an dernier %s.",
    en = "Last closed month %s. Budget %s. Same month last year %s."
  ),
  synth_margin_no_budget = c(
    fr = "Dernier mois clos %s. Pas de marge budget.",
    en = "Last closed month %s. No budget margin."
  ),
  synth_margin_py_only = c(
    fr = "Dernier mois clos %s. Même mois l'an dernier %s. Pas de marge budget.",
    en = "Last closed month %s. Same month last year %s. No budget margin."
  ),
  synth_margin_no_py = c(
    fr = "Dernier mois clos %s. Budget %s. Pas de même mois l'an dernier.",
    en = "Last closed month %s. Budget %s. No same month last year."
  ),
  synth_what = c(
    fr = "Le plus haut chiffre d'affaires réalisé est %s, %s EUR.",
    en = "The strongest realized revenue month is %s, %s EUR."
  ),
  synth_what_none = c(
    fr = "Pas encore de mois réalisé avec un chiffre d'affaires.",
    en = "No realized revenue month yet."
  ),
  synth_why = c(
    fr = "Trait plein : mois réalisés. Trait pointillé : prévision.",
    en = "Solid line: realized months. Dotted line: forecast."
  ),
  synth_so = c(
    fr = "La prévision commence en %s.",
    en = "The forecast starts in %s."
  ),
  synth_so_none = c(
    fr = "Aucun mois de prévision.",
    en = "No forecast months."
  ),
  synth_cash_what = c(
    fr = "Clôture du mois clos %s EUR.",
    en = "Closed-month close %s EUR."
  ),
  synth_cash_fcst = c(
    fr = "Prévision %s %s EUR.",
    en = "Forecast %s %s EUR."
  ),
  synth_cash_why = c(
    fr = "Le plus bas est %s, %s EUR.",
    en = "The low is %s, %s EUR."
  ),
  synth_cash_so_under = c(
    fr = "%s mois passent sous zéro.",
    en = "%s months go under zero."
  ),
  synth_cash_so_ok = c(
    fr = "Aucun mois ne passe sous zéro.",
    en = "No month goes under zero."
  ),
  synth_cash_none = c(
    fr = "Pas de trésorerie mensuelle.",
    en = "No monthly cash."
  ),
  pnl_headline = c(
    fr = "%s au-dessus du budget de %s EUR sur %s, écart le plus large de la fenêtre.",
    en = "%s above budget by %s EUR in %s, the widest gap in the window."
  ),
  pnl_headline_below = c(
    fr = "%s sous le budget de %s EUR sur %s, écart le plus large de la fenêtre.",
    en = "%s below budget by %s EUR in %s, the widest gap in the window."
  ),
  pnl_headline_mom = c(
    fr = "%s bouge de %s EUR sur %s face au mois précédent, plus large mouvement de la fenêtre.",
    en = "%s moves by %s EUR in %s versus the prior month, the largest move in the window."
  ),
  pnl_headline_none = c(
    fr = "Aucun mouvement matériel dans la fenêtre affichée.",
    en = "No material move in the visible window."
  ),
  bs_headline = c(
    fr = "Trésorerie %s%s EUR sur le mois, portée par une variation du BFR de %s%s EUR.",
    en = "Cash %s%s EUR on the month, carried by a working-capital move of %s%s EUR."
  ),
  bs_headline_parallel = c(
    fr = "Trésorerie %s%s EUR sur le mois. BFR %s%s EUR.",
    en = "Cash %s%s EUR on the month. Working capital %s%s EUR."
  ),
  bs_headline_cash_only = c(
    fr = "Trésorerie %s%s EUR sur le mois.",
    en = "Cash %s%s EUR on the month."
  ),
  bs_headline_one = c(
    fr = "Un seul mois de bilan.",
    en = "Only one balance-sheet month."
  ),
  close_headline = c(
    fr = "Résultat d'exploitation %s au départ de %s EUR sur %s entre les deux prévisions.",
    en = "Operating income is %s to the starting vintage by %s EUR in %s between the two forecasts."
  ),
  close_headline_none = c(
    fr = "Aucun écart matériel entre les deux prévisions.",
    en = "No material gap between the two forecasts."
  ),
  cfg_page = c(
    fr = "Configuration",
    en = "Configuration"
  ),
  nav_sync_group = c(
    fr = "Synchroniser",
    en = "Sync"
  ),
  nav_sync = c(
    fr = "Fichiers locaux",
    en = "Local files"
  ),
  nav_sync_title = c(
    fr = "Relire les fichiers de ce poste et recalculer s'ils ont changé. Cela ne récupère pas les réglages du site.",
    en = "Reread the files on this computer and recalculate if they changed. This does not fetch the settings from the site."
  ),
  nav_sync_busy = c(
    fr = "Relecture des fichiers du poste.",
    en = "Rereading the files on this computer."
  ),
  file_saved = c(
    fr = "Dernier enregistrement :",
    en = "Last save:"
  ),
  file_saved_none = c(
    fr = "Aucun fichier source sur le disque.",
    en = "No source file is on disk."
  ),
  file_synced = c(
    fr = "Dernière synchronisation avec le calcul : %s.",
    en = "Last synchronisation with the calculation: %s."
  ),
  file_synced_none = c(
    fr = "Pas encore synchronisé avec le calcul.",
    en = "Not synchronised with the calculation yet."
  ),
  sync_reread = c(
    fr = "Recalculé. Les fichiers sources ont été relus.",
    en = "Rebuilt. The source files were read again."
  ),
  nav_sync_site = c(
    fr = "Paramètres serveur",
    en = "Server settings"
  ),
  nav_sync_site_title = c(
    fr = "Récupérer les réglages publiés sur le site, puis relancer le calcul si une nouvelle révision arrive.",
    en = "Fetch the settings published on the site, then rerun the calculation when a new revision arrives."
  ),
  nav_sync_site_off = c(
    fr = "Indisponible en mode local. Repassez en mode connecté dans Paramètres.",
    en = "Unavailable in local mode. Switch back to connected in Parameters."
  ),
  hdr_today = c(fr = "Aujourd'hui", en = "Today"),
  hdr_month_left = c(
    fr = "%s jours avant la fin du mois",
    en = "%s days until the end of the month"
  ),
  hdr_month_left_one = c(
    fr = "1 jour avant la fin du mois",
    en = "1 day until the end of the month"
  ),
  hdr_month_last = c(
    fr = "Dernier jour du mois",
    en = "Last day of the month"
  ),
  hdr_year_done = c(
    fr = "%s %% de l'exercice annuel écoulé",
    en = "%s%% of the annual financial year has elapsed"
  ),
  drv_split_no_period = c(
    fr = "Les deux fichiers de cette série n'ont pas de colonne periode, jour, date ou day. Un mois présent dans les deux ne peut pas être départagé.",
    en = "The two files of this series have no periode, jour, date, or day column. A month present in both cannot be separated."
  ),
  drv_living_filled = c(
    fr = "Ces mois n'étaient pas dans le fichier. Le même mois de l'année précédente a été repris : %s.",
    en = "These months were not in the file. The same month of the previous year was used: %s."
  ),
  nav_path_copy = c(
    fr = "Copier le dossier de travail",
    en = "Copy working folder"
  ),
  nav_path_copied = c(
    fr = "Chemin copié",
    en = "Path copied"
  ),
  nav_params = c(
    fr = "Paramètres",
    en = "Parameters"
  ),
  nav_params_title = c(
    fr = "Ouvrir les paramètres",
    en = "Open parameters"
  ),
  nav_params_back = c(
    fr = "Retour",
    en = "Back"
  ),
  nav_params_back_title = c(
    fr = "Retour à l'écran principal",
    en = "Back to the main screen"
  ),
  nav_params_blank = c(
    fr = "Paramètres — contenu à venir.",
    en = "Parameters — content coming next."
  ),
  params_open = c(
    fr = "Modifier les réglages",
    en = "Edit settings"
  ),
  params_open_title = c(
    fr = "Ouvrir l'assistant de configuration sur ce projet",
    en = "Open the configuration assistant on this project"
  ),
  params_open_hint = c(
    fr = "Le mot de passe du compte n'est pas demandé. La clé est celle de ce fichier.",
    en = "The account password is not asked. The key is the one in this file."
  ),
  params_no_key = c(
    fr = "Ce fichier n'a pas encore de clé de liaison. Synchronisez depuis l'assistant pour en écrire une.",
    en = "This file does not have a link key yet. Synchronise from the assistant to write one."
  ),
  rates_title = c(fr = "Taux", en = "Rates"),
  rates_lead = c(
    fr = "Le fichier local et la configuration portent les mêmes taux. Le plus récent des deux est gardé. Actualiser applique le fichier au calcul.",
    en = "The local file and the configuration carry the same rates. The more recent of the two is kept. Refresh applies the file to the calculation."
  ),
  rates_save = c(fr = "Enregistrer les taux", en = "Save the rates"),
  rates_saved = c(
    fr = "Taux enregistrés. Actualiser relit le fichier.",
    en = "Rates saved. Refresh reads the file again."
  ),
  rates_empty = c(
    fr = "Aucun taux chiffré. Une valeur saisie ici écrit le fichier.",
    en = "No rate has a number yet. A value typed here writes the file."
  ),
  rates_param = c(fr = "Paramètre", en = "Parameter"),
  rates_valeur = c(fr = "Valeur", en = "Value"),
  params_go_config = c(
    fr = "Ouvrir la configuration",
    en = "Open the configuration"
  ),
  params_sync_none = c(
    fr = "Rien de nouveau à synchroniser. Révision %s · %s.",
    en = "Nothing new to synchronise. Revision %s · %s."
  ),
  params_card_rev = c(fr = "Révision %s", en = "Revision %s"),
  sync_pull_got = c(
    fr = "Réglages révision %s récupérés.",
    en = "Settings revision %s fetched."
  ),
  pm_title = c(fr = "Paramètres", en = "Settings"),
  pm_mode_aria = c(fr = "Mode de liaison avec le serveur", en = "Link mode with the server"),
  pm_mode_connected = c(fr = "Connecté au serveur", en = "Connected to the server"),
  pm_mode_connected_sub = c(fr = "abonnement actif", en = "active subscription"),
  pm_mode_local = c(fr = "Local et souverain", en = "Local and sovereign"),
  pm_mode_local_sub = c(fr = "hors ligne · mode avion", en = "offline · airplane mode"),
  pm_banner_connected = c(
    fr = "Connecté. Le site garde la configuration du projet et publie les scripts R. Ce poste calcule tout : aucun chiffre ne le quitte.",
    en = "Connected. The site keeps the project configuration and publishes the R scripts. This computer does all the calculation: no figure leaves it."
  ),
  pm_banner_local = c(
    fr = "Local et souverain. Aucun échange avec le serveur, comme en mode avion. Le calcul continue sur le dernier fichier de réglages reçu. Repassez en mode connecté pour synchroniser.",
    en = "Local and sovereign. No exchange with the server, as in airplane mode. Calculation goes on with the last settings file received. Switch back to connected to synchronise."
  ),
  pm_project_title = c(fr = "Ce projet", en = "This project"),
  pm_site_name = c(fr = "Nom du projet sur le site", en = "Project name on the site"),
  pm_company_line = c(fr = "Société : %s", en = "Company: %s"),
  pm_tile_rev = c(fr = "Révision des réglages", en = "Settings revision"),
  pm_tile_saved = c(fr = "Publiée le", en = "Published on"),
  pm_tile_calc = c(fr = "Dernier calcul", en = "Last calculation"),
  pm_tile_files = c(fr = "Fichiers déclarés", en = "Declared files"),
  pm_tile_drivers = c(fr = "Drivers", en = "Drivers"),
  pm_none = c(fr = "aucun", en = "none"),
  pm_cfg_title = c(fr = "Réglages du projet", en = "Project settings"),
  pm_cfg_lead = c(
    fr = "Vous changez les réglages sur le site. Synchroniser les amène ici, puis relance le calcul si une nouvelle révision arrive.",
    en = "You change the settings on the site. Synchronise brings them here, then reruns the calculation when a new revision arrives."
  ),
  pm_cfg_name = c(fr = "Nom du projet", en = "Project name"),
  pm_cfg_checked = c(fr = "Dernière vérification", en = "Last check"),
  pm_cfg_received = c(fr = "Dernière réception", en = "Last received"),
  pm_never = c(fr = "jamais", en = "never"),
  pm_status_unchanged = c(fr = "rien de nouveau", en = "nothing new"),
  pm_status_pulled = c(fr = "révision %s reçue", en = "revision %s received"),
  pm_status_offline = c(fr = "serveur injoignable", en = "server not reached"),
  pm_status_refused = c(fr = "clé refusée par le serveur", en = "key refused by the server"),
  pm_status_missing = c(fr = "rien de publié sur le site", en = "nothing published on the site"),
  pm_btn_sync = c(fr = "Synchroniser les réglages", en = "Synchronise the settings"),
  sync_wait_site = c(
    fr = "Lecture des réglages publiés sur le site. Après 10 secondes, la lecture s'arrête.",
    en = "Reading the settings published on the site. After 10 seconds, the reading stops."
  ),
  sync_wait_files = c(
    fr = "Relecture des fichiers de ce poste. Après 90 secondes, la fenêtre se ferme.",
    en = "Rereading the files on this computer. After 90 seconds, the window closes."
  ),
  sync_wait_stopped = c(
    fr = "La synchronisation n'a pas fini. Relancez-la.",
    en = "The synchronisation did not finish. Start it again."
  ),
  sync_dismiss = c(fr = "Fermer", en = "Close"),
  pm_scripts_title = c(fr = "Scripts R", en = "R scripts"),
  pm_scripts_lead = c(
    fr = "Deux fichiers font tourner ce poste. Le site publie leurs nouvelles versions. Vous les téléchargez et remplacez les anciens dans ce dossier.",
    en = "Two files run this computer. The site publishes their new versions. You download them and replace the old ones in this folder."
  ),
  pm_script_role_engine = c(fr = "le moteur de calcul", en = "the calculation engine"),
  pm_script_role_screen = c(fr = "cet écran", en = "this screen"),
  pm_script_ok = c(fr = "À jour", en = "Up to date"),
  pm_script_old = c(fr = "%s disponible", en = "%s available"),
  pm_script_missing = c(fr = "Absent · %s sur le serveur", en = "Missing · %s on the server"),
  pm_script_ahead = c(fr = "Plus récent que le serveur (%s)", en = "Newer than the server (%s)"),
  pm_script_unchecked = c(fr = "Pas encore vérifié", en = "Not checked yet"),
  pm_scripts_checked = c(fr = "Versions vérifiées %s.", en = "Versions checked %s."),
  pm_scripts_never = c(fr = "Versions jamais vérifiées.", en = "Versions never checked."),
  pm_scripts_failed = c(fr = " Dernier essai : serveur injoignable.", en = " Last try: server not reached."),
  pm_btn_check = c(fr = "Vérifier les nouvelles versions", en = "Check for new versions"),
  pm_check_ok = c(fr = "Scripts à jour : fpa-engine.R %s, app.R %s.", en = "Scripts up to date: fpa-engine.R %s, app.R %s."),
  pm_check_fail = c(fr = "Serveur injoignable. Le dernier résultat connu reste affiché.", en = "Server not reached. The last known result stays on screen."),
  pm_off_badge = c(fr = "Coupé en mode local", en = "Off in local mode"),
  pm_local_title = c(fr = "Sur ce poste", en = "On this computer"),
  pm_local_badge = c(fr = "Toujours actif", en = "Always on"),
  pm_local_cfg = c(fr = "Fichier de réglages", en = "Settings file"),
  pm_local_work = c(fr = "Dossier de travail", en = "Working folder"),
  pm_local_scripts = c(fr = "Dossier des scripts R", en = "R scripts folder"),
  pm_local_overlay = c(fr = "Réglages propres au poste", en = "Computer-only settings"),
  pm_local_overlay_yes = c(fr = "fpa-config.local.json · chemins et plancher de trésorerie", en = "fpa-config.local.json · paths and cash floor"),
  pm_local_overlay_no = c(fr = "aucun · fpa-config.local.json absent", en = "none · fpa-config.local.json not present"),
  pm_local_note = c(
    fr = "Le calcul reste ici dans les deux modes. Actualiser relit vos fichiers sources sans passer par le serveur.",
    en = "Calculation stays here in both modes. Refresh rereads your source files without going through the server."
  ),
  pm_copy = c(fr = "Copier", en = "Copy"),
  pm_copied = c(fr = "Copié", en = "Copied"),
  pm_ago_today = c(fr = "aujourd'hui à %s", en = "today at %s"),
  pm_ago_yesterday = c(fr = "hier à %s", en = "yesterday at %s"),
  pm_ago_days = c(fr = "il y a %s jours (%s)", en = "%s days ago (%s)"),
  pm_mode_set_local = c(
    fr = "Mode local et souverain. Plus aucun échange avec le serveur.",
    en = "Local and sovereign mode. No more exchange with the server."
  ),
  pm_mode_set_connected = c(
    fr = "Mode connecté. Synchronisez ou vérifiez les versions quand vous voulez.",
    en = "Connected mode. Synchronise or check versions whenever you want."
  ),
  params_script_same = c(
    fr = "%s %s · identique au serveur",
    en = "%s %s · same as the server"
  ),
  params_script_gap = c(
    fr = "%s : vous avez %s, le serveur a %s",
    en = "%s: you have %s, the server has %s"
  ),
  params_script_missing = c(
    fr = "%s : absent de ce dossier, le serveur a %s",
    en = "%s: not in this folder, the server has %s"
  ),
  params_script_unchecked = c(
    fr = "%s %s · serveur non joint",
    en = "%s %s · server not reached"
  ),
  params_script_advice = c(
    fr = "Nouvelle version sur le serveur. Ouvrez la configuration, téléchargez %s et remplacez l'ancien fichier dans ce dossier.",
    en = "New version on the server. Open the configuration, download %s and replace the old file in this folder."
  ),
  params_delta_head = c(
    fr = "Dernière synchronisation (révision %s, %s) : %s.",
    en = "Last synchronisation (revision %s, %s): %s."
  ),
  params_delta_files_add = c(fr = "%s fichier(s) ajouté(s) : %s", en = "%s file(s) added: %s"),
  params_delta_files_drop = c(fr = "%s fichier(s) retiré(s) : %s", en = "%s file(s) removed: %s"),
  params_delta_drv_add = c(fr = "%s driver(s) ajouté(s) : %s", en = "%s driver(s) added: %s"),
  params_delta_drv_drop = c(fr = "%s driver(s) retiré(s) : %s", en = "%s driver(s) removed: %s"),
  params_delta_other = c(
    fr = "réglages modifiés sans nouveau fichier ni driver",
    en = "settings changed with no new file or driver"
  ),
  sync_pull_same = c(
    fr = "Déjà à jour, révision %s.",
    en = "Already current, revision %s."
  ),
  sync_pull_offline = c(
    fr = "Serveur injoignable. Le fichier local est conservé.",
    en = "Server unreachable. The local file is kept."
  ),
  sync_pull_refused = c(
    fr = "Clé refusée. Le fichier local est conservé.",
    en = "Key refused. The local file is kept."
  ),
  sync_pull_missing = c(
    fr = "Rien n'a encore été synchronisé depuis l'assistant.",
    en = "Nothing has been synchronised from the assistant yet."
  ),
  sync_pull_noconfig = c(
    fr = "Pas de fpa-config.json et le serveur est injoignable.",
    en = "No fpa-config.json and the server is unreachable."
  ),
  sync_fresh = c(
    fr = "À jour. Aucun fichier source plus récent que le dernier calcul.",
    en = "Up to date. No source file newer than the last calculation."
  ),
  sync_rebuilt = c(
    fr = "Recalculé. Fichiers plus récents : %s. Les drivers ont été relus.",
    en = "Rebuilt. Newer files: %s. Drivers were re-read."
  ),
  sync_run_fail = c(
    fr = "Synchronisation interrompue au calcul : %s",
    en = "Sync stopped during calculation: %s"
  ),
  sync_drv_fail = c(
    fr = "États recalculés. Drivers interrompus : %s",
    en = "Statements rebuilt. Drivers stopped: %s"
  ),
  sync_bad = c(
    fr = "Configuration illisible. Relancez depuis le dossier qui contient fpa-config.json.",
    en = "Config unreadable. Relaunch from the folder that holds fpa-config.json."
  ),
  raw_view = c(
    fr = "Voir les données brutes",
    en = "View raw data"
  ),
  raw_missing = c(
    fr = "Ce fichier est absent.",
    en = "This file is missing."
  ),
  map_path_missing = c(
    fr = "%s : le fichier attendu est introuvable à ce chemin : %s",
    en = "%s: the expected file is not at this path: %s"
  ),
  map_path_empty = c(
    fr = "%s : aucun chemin n'est indiqué pour ce fichier.",
    en = "%s: no path is set for this file."
  ),
  path_map_gap = c(
    fr = "%s est introuvable. Le crochet revient quand le chemin pointe vers le fichier.",
    en = "%s is missing. The check returns when the path points at the file."
  ),
  path_map_gaps = c(
    fr = "%s fichiers attendus sont introuvables. Le crochet revient quand chaque chemin pointe vers son fichier.",
    en = "%s expected files are missing. The check returns when each path points at its file."
  ),
  header_todo_files = c(
    fr = "Fichiers manquants",
    en = "Missing files"
  ),
  header_todo_path = c(
    fr = "Introuvable à ce chemin : %s",
    en = "Not found at this path: %s"
  ),
  header_todo_path_empty = c(
    fr = "Aucun chemin n'est indiqué.",
    en = "No path is set."
  ),
  map_sync_stale = c(
    fr = "Ces fichiers sont plus récents que le dernier calcul : %s. Les traits lisent le fichier. Les chiffres du modèle attendent Actualiser.",
    en = "These files are newer than the last calculation: %s. The bars read the file. The model figures wait for Refresh."
  ),
  raw_fail = c(
    fr = "Lecture impossible : %s",
    en = "Could not read the file: %s"
  ),
  raw_all = c(
    fr = "%s lignes, %s colonnes.",
    en = "%s rows, %s columns."
  ),
  raw_cut = c(
    fr = "%s premières lignes sur %s. %s colonnes.",
    en = "First %s rows of %s. %s columns."
  ),
  raw_sheet = c(
    fr = "Première feuille : %s. Autres feuilles : %s.",
    en = "First sheet: %s. Other sheets: %s."
  ),
  raw_unlock = c(
    fr = "Déverrouiller l'édition du fichier",
    en = "Unlock editing of source file"
  ),
  raw_save = c(fr = "Enregistrer le fichier", en = "Save the file"),
  raw_edit_lead = c(
    fr = "Les cases sont ouvertes. Enregistrez, puis Actualiser pour que les chiffres du modèle suivent le fichier.",
    en = "The cells are open. Save, then Refresh so the model figures follow the file."
  ),
  raw_saved = c(
    fr = "%s case(s) enregistrée(s). Actualiser relit le fichier.",
    en = "%s cell(s) saved. Refresh reads the file again."
  ),
  raw_saved_none = c(fr = "Aucune case modifiée.", en = "No cell was changed."),
  scen_band = c(
    fr = "Sur les mois ouverts, le résultat d'exploitation attendu est %s EUR. Prudent est à %s EUR, ambitieux à %s EUR. Le budget voté, sur ces mêmes mois, est %s EUR.",
    en = "On the open months, expected operating income is %s EUR. Cautious is %s EUR, ambitious is %s EUR. The voted budget, on those same months, is %s EUR."
  ),
  scen_band_win = c(
    fr = "De %s, le résultat d'exploitation attendu est %s EUR. Prudent est à %s EUR, ambitieux à %s EUR. Le budget voté, sur ces mêmes mois, est %s EUR.",
    en = "From %s, expected operating income is %s EUR. Cautious is %s EUR, ambitious is %s EUR. The voted budget, on those same months, is %s EUR."
  ),
  scen_band_nobudget = c(
    fr = "Sur les mois ouverts, le résultat d'exploitation attendu est %s EUR. Prudent est à %s EUR, ambitieux à %s EUR.",
    en = "On the open months, expected operating income is %s EUR. Cautious is %s EUR, ambitious is %s EUR."
  ),
  scen_band_nobudget_win = c(
    fr = "De %s, le résultat d'exploitation attendu est %s EUR. Prudent est à %s EUR, ambitieux à %s EUR.",
    en = "From %s, expected operating income is %s EUR. Cautious is %s EUR, ambitious is %s EUR."
  ),
  scen_band_partial_budget = c(
    fr = "Sur les mois ouverts, le résultat d'exploitation attendu est %s EUR. Prudent est à %s EUR, ambitieux à %s EUR. L'écart au budget n'est pas publié : %s mois sur %s n'ont pas de budget.",
    en = "On the open months, expected operating income is %s EUR. Cautious is %s EUR, ambitious is %s EUR. The gap to budget is not published: %s months out of %s have no budget."
  ),
  scen_band_partial_budget_win = c(
    fr = "De %s, le résultat d'exploitation attendu est %s EUR. Prudent est à %s EUR, ambitieux à %s EUR. L'écart au budget n'est pas publié : %s mois sur %s n'ont pas de budget.",
    en = "From %s, expected operating income is %s EUR. Cautious is %s EUR, ambitious is %s EUR. The gap to budget is not published: %s months out of %s have no budget."
  ),
  scen_band_missing = c(
    fr = "Prudent et ambitieux ne sont pas calculés.",
    en = "Cautious and ambitious are not calculated."
  ),
  scnp_gap_partial = c(
    fr = "Écart non publié · %s mois sans budget",
    en = "Gap not published · %s months without budget"
  ),
  scnp_tile_gap_partial = c(
    fr = "Le budget manque sur %s mois de la fenêtre. L'écart n'est pas calculé.",
    en = "The budget is missing on %s months of the window. The gap is not calculated."
  ),
  scen_case_label = c(fr = "Scénario", en = "Scenario"),
  scen_bas = c(fr = "Prudent", en = "Cautious"),
  scen_central = c(fr = "Attendu", en = "Expected"),
  scen_haut = c(fr = "Ambitieux", en = "Ambitious"),
  scen_case_note = c(
    fr = "Les mois clos restent le réalisé. Le budget voté ne bouge pas.",
    en = "Closed months stay as actuals. The voted budget does not move."
  ),
  scen_rank_lead = c(
    fr = "Chaque ligne écarte une seule entrée. Les autres restent à l'attendu. Le scénario choisi au-dessus les applique ensemble.",
    en = "Each row moves one input. The others stay on expected. The scenario chosen above applies them together."
  ),
  scen_rank_title = c(
    fr = "Ce qui écarte le résultat d'exploitation",
    en = "What moves operating income"
  ),
  scen_rank_input = c(fr = "Entrée", en = "Input"),
  scen_rank_span = c(fr = "Ambitieux moins prudent", en = "Ambitious minus cautious"),
  scen_bridge_note = c(
    fr = "Le pont de trésorerie est l'attendu. La clôture du tableau suit le scénario choisi, sur les mois ouverts.",
    en = "The cash bridge is the expected path. The table's closing balance follows the chosen scenario, on the open months."
  ),
  synth_cash_stock = c(
    fr = "La trésorerie est le solde après chaque mois. Ce n'est pas un flux à cumuler une seconde fois.",
    en = "Cash is the balance after each month. It is not a flow to be added up a second time."
  ),
  synth_cum = c(
    fr = "Les deux courbes sont le cumul depuis janvier. Chaque point ajoute le mois au total déjà acquis.",
    en = "Both lines are the total since January. Each point adds the month to what is already earned."
  ),
  scen_band_name = c(fr = "Exploitation, de prudent à ambitieux", en = "Operating income, from cautious to ambitious"),
  scen_cash_band = c(fr = "Trésorerie, de prudent à ambitieux", en = "Cash, from cautious to ambitious"),
  scen_cash_trio = c(
    fr = "Trésorerie, prudent / attendu / ambitieux %s / %s / %s EUR",
    en = "Cash, cautious / expected / ambitious %s / %s / %s EUR"
  ),
  scen_empty = c(
    fr = "Les trois scénarios ne sont pas encore dans work/scenarios-monthly.csv.",
    en = "The three scenarios are not in work/scenarios-monthly.csv yet."
  ),
  span_year = c(fr = "Année civile", en = "Calendar year"),
  span_fy = c(fr = "Exercice", en = "Fiscal year"),
  span_year_meta = c(fr = "janvier à décembre", en = "January to December"),
  span_fy_same = c(
    fr = "clôture en décembre · mêmes bornes que l'année civile",
    en = "closes in December · same bounds as the calendar year"
  ),
  span_fy_end = c(fr = "clôture en %s", en = "closes in %s"),
  pnl_drv_lead = c(
    fr = "Les mois ouverts se règlent ici. Le chiffre d'affaires est les chambres fois l'occupation fois le tarif. Les autres lignes suivent le rythme des %s mois précédents (%s à %s), calé sur le réalisé de l'année.",
    en = "Open months are set here. Revenue is rooms times occupancy times rate. The other lines follow the rhythm of the previous %s months (%s to %s), set to this year's actuals."
  ),
  pnl_drv_occ = c(fr = "Occupation %", en = "Occupancy %"),
  pnl_drv_rate = c(fr = "Tarif", en = "Rate"),
  pnl_drv_head = c(fr = "Effectif", en = "Headcount"),
  pnl_drv_cost = c(fr = "Coût moyen", en = "Average cost"),
  pnl_drv_var = c(fr = "Coût variable %", en = "Variable cost %"),
  pnl_drv_rooms = c(fr = "%s ch.", en = "%s rooms"),
  pnl_lines_head = c(
    fr = "Charges qui suivent l'activité, mesurées sur l'historique mensuel. Elles bougent avec l'occupation et le tarif ci-dessus. Les autres lignes suivent le rythme des mois passés.",
    en = "Charges that follow activity, measured on the monthly history. They move with occupancy and rate above. The other lines follow the rhythm of past months."
  ),
  pnl_line_mixed = c(
    fr = "%s EUR par mois + %s EUR par nuitée, mesuré sur %s mois (R² %s).",
    en = "%s EUR a month + %s EUR per night, measured on %s months (R² %s)."
  ),
  pnl_line_volume = c(
    fr = "%s EUR par nuitée, mesuré sur les %s derniers mois.",
    en = "%s EUR per night, measured on the last %s months."
  ),
  pnl_line_pct = c(
    fr = "%s %% du chiffre d'affaires, mesuré sur %s mois.",
    en = "%s %% of revenue, measured on %s months."
  ),
  pnl_line_contract = c(
    fr = "%s EUR par mois, montant contractuel.",
    en = "%s EUR a month, contracted amount."
  ),
  pnl_lines_shock = c(
    fr = "coûts au volume %s %%",
    en = "volume costs %s %%"
  ),
  pnl_case_central = c(
    fr = "Attendu lit les cases telles quelles. Prudent et ambitieux n'y touchent pas.",
    en = "Expected reads the boxes as they are. Cautious and ambitious leave them unchanged."
  ),
  pnl_case_move = c(
    fr = "%s applique l'écart de la configuration, seulement sur les mois ouverts : %s.",
    en = "%s applies the configured width, on the open months only: %s."
  ),
  pnl_range_title = c(
    fr = "Prudent et ambitieux, mois par mois",
    en = "Cautious and ambitious, month by month"
  ),
  pnl_range_lead = c(
    fr = "Chaque mois a prudent et ambitieux. Le verrou tient le mois entier : ils ne le touchent pas. Les mois clos partent verrouillés. Décochez un mois pour le modifier.",
    en = "Each month has cautious and ambitious. The lock holds the whole month: they leave it alone. Closed months start locked. Clear a month to change it."
  ),
  pnl_range_lock = c(fr = "Verrou", en = "Lock"),
  rng_nav_batch = c(fr = "Régler plusieurs cellules", en = "Set several cells at once"),
  rng_nav_table = c(fr = "Mois par mois", en = "Month by month"),
  rng_nav_closed = c(fr = "Scénario des mois clos", en = "Scenario for the closed months"),
  rng_batch_lead = c(
    fr = "Choisissez la période, les entrées et le côté, puis appliquez. Les mois verrouillés ne bougent pas. Le tableau mois par mois reste là pour une cellule seule.",
    en = "Choose the period, the inputs and the side, then apply. Locked months do not move. The month-by-month table stays for a single cell."
  ),
  rng_batch_from = c(fr = "Du mois", en = "From"),
  rng_batch_to = c(fr = "Au mois", en = "To"),
  rng_batch_rows = c(fr = "Entrées", en = "Inputs"),
  rng_batch_side = c(fr = "Côté", en = "Side"),
  rng_batch_both = c(fr = "Les deux", en = "Both"),
  rng_batch_pct = c(fr = "Largeur (%)", en = "Width (%)"),
  rng_batch_go = c(fr = "Appliquer à la sélection", en = "Apply to the selection"),
  rng_batch_done = c(
    fr = "%s cellules réglées à %s %%, de %s à %s.",
    en = "%s cells set to %s %%, from %s to %s."
  ),
  rng_batch_skip = c(
    fr = "%s mois verrouillés n'ont pas bougé.",
    en = "%s locked months did not move."
  ),
  rng_batch_none = c(
    fr = "Aucune cellule réglée : la période est verrouillée ou aucune entrée n'est cochée.",
    en = "No cell changed: the period is locked or no input is ticked."
  ),
  rng_closed_fact = c(
    fr = "%s à %s. Ce choix ne compte que pour un mois déverrouillé dans le tableau.",
    en = "%s to %s. This choice only counts for a month unlocked in the table."
  ),
  pnl_partner_kicker = c(
    fr = "Avant la clôture",
    en = "Before the close"
  ),
  pnl_partner_lead = c(
    fr = "Il reste %s mois avant la clôture de l'exercice, %s. Le partenaire dit, pour chacun, le scénario qu'il croit. Attendu est déjà rempli.",
    en = "There are %s months left before the close, %s. The partner names the scenario they believe, for each month. Expected is already filled in."
  ),
  pnl_partner_lead_none = c(
    fr = "L'exercice n'a plus de mois ouvert. Les mois au-delà de la clôture restent sur la courbe longue.",
    en = "The financial year has no open month left. Months past the close stay on the longer path."
  ),
  pnl_partner_all = c(
    fr = "Appliquer à tous les mois ouverts",
    en = "Apply to every open month"
  ),
  var_scn_btn = c(fr = "Scénarios", en = "Scenarios"),
  var_scn_all = c(fr = "Tous les mois", en = "Every month"),
  var_scn_tip_hi = c(
    fr = "Scénarios. La moyenne des mois ouverts est ambitieuse.",
    en = "Scenarios. The open months average ambitious."
  ),
  var_scn_tip_base = c(
    fr = "Scénarios. La moyenne des mois ouverts est attendue.",
    en = "Scenarios. The open months average expected."
  ),
  var_scn_tip_lo = c(
    fr = "Scénarios. La moyenne des mois ouverts est prudente.",
    en = "Scenarios. The open months average cautious."
  ),
  var_scn_pct_tip = c(
    fr = "Régler les pourcentages.",
    en = "Set the percents."
  ),
  var_scn_pct_title = c(
    fr = "Prudent et ambitieux",
    en = "Cautious and ambitious"
  ),
  var_scn_pct_close = c(
    fr = "Fermer",
    en = "Close"
  ),
  var_scn_pct_wait = c(
    fr = "Les pourcentages se chargent.",
    en = "The percents are loading."
  ),
  pnl_partner_mean = c(
    fr = "%s %s : occupation %s %%, tarif %s, effectif %s, coût moyen %s.",
    en = "%s %s: occupancy %s %%, rate %s, headcount %s, average cost %s."
  ),
  pnl_partner_path = c(
    fr = "Pour %s, le résultat d'exploitation des mois ouverts est %s EUR. Trésorerie de décembre : %s EUR. Écart au budget voté sur ces mois : %s EUR.",
    en = "For %s, operating income on the open months is %s EUR. December cash: %s EUR. Gap to the voted budget on these months: %s EUR."
  ),
  pnl_partner_path_partial = c(
    fr = "Pour %s, le résultat d'exploitation des mois ouverts est %s EUR. Trésorerie de décembre : %s EUR. L'écart au budget n'est pas publié : %s mois sur %s n'ont pas de budget.",
    en = "For %s, operating income on the open months is %s EUR. December cash: %s EUR. The gap to budget is not published: %s months out of %s have no budget."
  ),
  pnl_partner_path_nobudget = c(
    fr = "Pour %s, le résultat d'exploitation des mois ouverts est %s EUR. Trésorerie de décembre : %s EUR.",
    en = "For %s, operating income on the open months is %s EUR. December cash: %s EUR."
  ),
  pnl_partner_many = c(
    fr = "les %s mois ouverts en %s",
    en = "the %s open months, all %s"
  ),
  pnl_partner_many_mixed = c(
    fr = "les %s mois ouverts",
    en = "the %s open months"
  ),
  pnl_partner_where = c(
    fr = "Le scénario de chaque mois ouvert se choisit dans l'onglet Scénarios.",
    en = "Each open month's scenario is chosen on the Scenarios tab."
  ),
  pnl_partner_go = c(
    fr = "Choisir le scénario",
    en = "Name the scenario"
  ),
  pnl_width_title = c(
    fr = "Prudent et ambitieux",
    en = "Cautious and ambitious"
  ),
  pnl_width_lead = c(
    fr = "Chaque cellule est la largeur, pour une entrée et un mois. Prudent prend moins de chiffre d'affaires et plus de coûts. Ambitieux fait l'inverse. Le signe donne l'effet sur le résultat : sur une charge, prudent l'augmente. La finance tient ces chiffres. Un changement vaut pour les mois déjà marqués de ce scénario.",
    en = "Each cell is the width, for one input and one month. Cautious takes less revenue and more costs. Ambitious does the reverse. The sign is the effect on the result: on a cost, cautious raises it. Finance owns these figures. A change applies to every month already marked with that scenario."
  ),
  pnl_width_sum = c(
    fr = "Prudent − %s %%, ambitieux + %s %%, sur chaque entrée. %s mois verrouillés.",
    en = "Cautious − %s %%, ambitious + %s %%, on every input. %s months locked."
  ),
  pnl_width_sum_mix = c(
    fr = "Les pourcentages diffèrent selon l'entrée ou le mois. %s mois verrouillés.",
    en = "The percents differ by input or by month. %s months locked."
  ),
  pnl_fold_drv_title = c(
    fr = "Moteurs des mois ouverts",
    en = "Drivers of the open months"
  ),
  pnl_fold_drv_bit = c(
    fr = "%s %s %% × %s",
    en = "%s %s %% × %s"
  ),
  pnl_fold_grid_title = c(
    fr = "Compte de résultat",
    en = "Income statement"
  ),
  pnl_fold_grid = c(
    fr = "Tableau sur %s, à partir de %s.",
    en = "Table over %s, from %s."
  ),
  pnl_fold_open = c(
    fr = "Tout ouvrir",
    en = "Expand all"
  ),
  pnl_fold_close = c(
    fr = "Tout fermer",
    en = "Collapse all"
  ),
  pnl_fold_case_wait = c(
    fr = "Le scénario de chaque mois ouvert se nomme ici.",
    en = "Each open month's scenario is named here."
  ),
  health_head = c(
    fr = "Livres clos en %s. Trésorerie %s EUR, dettes à court terme %s EUR, dette financière %s EUR. Résultat d'exploitation depuis %s, premier mois de l'exercice, %s EUR.",
    en = "Books closed at %s. Cash %s EUR, current liabilities %s EUR, financial debt %s EUR. Operating income since %s, the first month of the fiscal year, %s EUR."
  ),
  health_path = c(
    fr = "Décembre suit le scénario choisi dans l'onglet Scénarios. Le mois clos ne bouge pas.",
    en = "December follows the scenario chosen on the Scenarios tab. The closed month does not move."
  ),
  health_skip = c(
    fr = "Aucun coût à la tonne, et aucun multiple d'acheteur dans le fichier. L'écran s'arrête aux livres.",
    en = "No cost per tonne, and no buyer multiple in the file. The screen stops at the books."
  ),
  health_liq = c(fr = "Liquidité", en = "Liquidity"),
  health_liq_q = c(
    fr = "Peut-on payer les échéances du mois ?",
    en = "Can the bills of the month be paid?"
  ),
  health_lev = c(fr = "Dette", en = "Debt"),
  health_lev_q = c(
    fr = "Quelle dette porte la structure ?",
    en = "How much debt does the structure carry?"
  ),
  health_eff = c(fr = "Rotation", en = "Turnover"),
  health_eff_q = c(
    fr = "Le chiffre d'affaires tourne-t-il sur l'actif ?",
    en = "Does revenue turn on the assets?"
  ),
  health_pro = c(fr = "Marge", en = "Margin"),
  health_pro_q = c(
    fr = "L'exploitation dégage-t-elle une marge ?",
    en = "Does the operation earn a margin?"
  ),
  health_scr = c(fr = "Écran d'acheteur", en = "Buyer screen"),
  health_scr_q = c(
    fr = "Que voit-on sur les livres, sans prix de marché ?",
    en = "What do the books show, with no market price?"
  ),
  health_fold_all = c(fr = "%s curseurs.", en = "%s markers."),
  health_fold_one = c(fr = "1 curseur.", en = "1 marker."),
  health_fold_some = c(fr = "%s curseurs sur %s.", en = "%s markers out of %s."),
  health_fold_one_some = c(fr = "1 curseur sur %s.", en = "1 marker out of %s."),
  health_fold_attn = c(fr = "Requiert attention.", en = "Needs attention."),
  health_fold_mid = c(fr = "Santé financière moyenne.", en = "Ordinary financial health."),
  health_fold_good = c(fr = "Bonne santé financière.", en = "Sound financial health."),
  health_thin_read = c(
    fr = "Lecture limitée : passif court de %s EUR.",
    en = "Limited reading: current liabilities of %s EUR."
  ),
  health_fold_thin_none = c(
    fr = "Pas de note : le passif court est trop petit pour lire ces ratios.",
    en = "No mark: current liabilities are too small to read these ratios."
  ),
  health_fold_thin_some = c(
    fr = "%s ratio(s) sur un passif court trop petit laissé(s) hors de la note.",
    en = "%s ratio(s) over too small current liabilities left out of the mark."
  ),
  health_current = c(fr = "Liquidité générale", en = "Current ratio"),
  health_quick = c(fr = "Liquidité réduite", en = "Quick ratio"),
  health_cash = c(fr = "Liquidité immédiate", en = "Cash ratio"),
  health_de = c(fr = "Dette / capitaux propres", en = "Debt to equity"),
  health_da = c(fr = "Dette / actif", en = "Debt to assets"),
  health_cover = c(fr = "Couverture des intérêts", en = "Interest coverage"),
  health_dso = c(fr = "Délai clients", en = "Customer days"),
  health_dio = c(fr = "Délai stocks", en = "Stock days"),
  health_turn = c(fr = "Chiffre d'affaires / actif", en = "Revenue / assets"),
  health_gm = c(fr = "Marge brute", en = "Gross margin"),
  health_ebitda = c(fr = "Marge d'EBITDA", en = "EBITDA margin"),
  health_om = c(fr = "Marge d'exploitation", en = "Operating margin"),
  health_roa = c(fr = "Résultat d'exploitation / actif", en = "Operating income / assets"),
  health_ev = c(fr = "Valeur d'entreprise comptable", en = "Book enterprise value"),
  health_roce = c(fr = "Rentabilité des capitaux employés", en = "Return on capital employed"),
  health_def_current = c(
    fr = "La liquidité générale divise ce qui se transforme en argent dans l'année par ce qui est à payer dans l'année.",
    en = "The current ratio divides what turns into cash within the year by what is due within the year."
  ),
  health_why_current = c(
    fr = "Elle dit si les dettes courtes peuvent être payées sans toucher à l'outil.",
    en = "It says whether the short debts can be paid without touching the plant."
  ),
  health_def_quick = c(
    fr = "La liquidité réduite retire les stocks. Elle garde la trésorerie et les créances, sur le même passif court.",
    en = "The quick ratio leaves stock out. It keeps cash and receivables, over the same short liabilities."
  ),
  health_why_quick = c(
    fr = "Les stocks ne se vendent pas le jour où une dette arrive.",
    en = "Stock does not sell on the day a bill is due."
  ),
  health_def_cash = c(
    fr = "La liquidité immédiate ne compte que la trésorerie, sur le passif court.",
    en = "The cash ratio counts only cash, over short liabilities."
  ),
  health_why_cash = c(
    fr = "Elle dit ce qui peut être payé tout de suite, sans attendre un client.",
    en = "It says what can be paid at once, without waiting for a customer."
  ),
  health_def_de = c(
    fr = "Ce ratio met la dette financière sur les capitaux propres.",
    en = "This ratio puts financial debt over equity."
  ),
  health_why_de = c(
    fr = "Il dit quelle part du financement vient des prêteurs plutôt que des associés.",
    en = "It says how much of the funding comes from lenders rather than the owners."
  ),
  health_def_da = c(
    fr = "Ce ratio met la dette financière sur l'actif total.",
    en = "This ratio puts financial debt over total assets."
  ),
  health_why_da = c(
    fr = "Il dit quelle part de ce que l'entreprise détient est financée par la dette.",
    en = "It says how much of what the company holds is funded by debt."
  ),
  health_def_cover = c(
    fr = "La couverture des intérêts divise le résultat d'exploitation depuis %s, premier mois de l'exercice, par les charges financières de la même période.",
    en = "Interest coverage divides operating income since %s, the first month of the fiscal year, by the finance cost of the same period."
  ),
  health_why_cover = c(
    fr = "Elle dit si l'exploitation paie les intérêts, et de combien.",
    en = "It says whether the operation pays the interest, and by how much."
  ),
  health_def_dso = c(
    fr = "Le délai clients convertit les créances en jours de chiffre d'affaires du mois.",
    en = "Customer days turn receivables into days of that month's revenue."
  ),
  health_why_dso = c(
    fr = "Un délai long veut dire que le chiffre est fait et que l'argent n'est pas encore là.",
    en = "A long delay means the sale is booked and the cash is not in yet."
  ),
  health_def_dio = c(
    fr = "Le délai stocks convertit les stocks en jours d'achats du mois.",
    en = "Stock days turn inventory into days of that month's purchases."
  ),
  health_why_dio = c(
    fr = "Un délai long veut dire que les achats restent en magasin avant d'être vendus.",
    en = "A long delay means purchases sit in store before they are sold."
  ),
  health_def_turn = c(
    fr = "Ce ratio met le chiffre d'affaires depuis %s, premier mois de l'exercice, sur l'actif total.",
    en = "This ratio puts revenue since %s, the first month of the fiscal year, over total assets."
  ),
  health_why_turn = c(
    fr = "Il dit combien de fois l'actif a été transformé en ventes depuis %s, premier mois de l'exercice.",
    en = "It says how many times the assets have been turned into sales since %s, the first month of the fiscal year."
  ),
  health_def_gm = c(
    fr = "La marge brute rapporte ce qui reste après les achats au chiffre d'affaires depuis %s, premier mois de l'exercice.",
    en = "Gross margin puts what remains after purchases over revenue since %s, the first month of the fiscal year."
  ),
  health_why_gm = c(
    fr = "Elle dit si le prix couvre ce que coûte le produit, avant les salaires et le reste.",
    en = "It says whether price covers what the product costs, before payroll and the rest."
  ),
  health_def_ebitda = c(
    fr = "La marge d'EBE rapporte l'excédent brut d'exploitation au chiffre d'affaires depuis %s, premier mois de l'exercice.",
    en = "The EBITDA margin puts operating surplus over revenue since %s, the first month of the fiscal year."
  ),
  health_why_ebitda = c(
    fr = "Elle dit ce que l'exploitation laisse avant les dotations, les intérêts et l'impôt.",
    en = "It says what the operation leaves before depreciation, interest, and tax."
  ),
  health_def_om = c(
    fr = "La marge d'exploitation rapporte le résultat d'exploitation au chiffre d'affaires depuis %s, premier mois de l'exercice.",
    en = "The operating margin puts operating income over revenue since %s, the first month of the fiscal year."
  ),
  health_why_om = c(
    fr = "Elle dit ce que l'exploitation laisse une fois les dotations prises.",
    en = "It says what the operation leaves once depreciation is taken."
  ),
  health_def_roa = c(
    fr = "Ce ratio met le résultat d'exploitation depuis %s, premier mois de l'exercice, sur l'actif total.",
    en = "This ratio puts operating income since %s, the first month of the fiscal year, over total assets."
  ),
  health_why_roa = c(
    fr = "Il dit ce que l'actif a produit comme résultat d'exploitation.",
    en = "It says what the assets produced as operating income."
  ),
  health_def_ev = c(
    fr = "La valeur d'entreprise comptable ajoute la dette aux capitaux propres et retire la trésorerie.",
    en = "Book enterprise value adds debt to equity and takes cash out."
  ),
  health_why_ev = c(
    fr = "Elle donne le prix comptable de l'activité, sans compter la caisse déjà dans les livres.",
    en = "It gives the book price of the activity, without counting the cash already on the books."
  ),
  health_def_roce = c(
    fr = "La rentabilité des capitaux employés met le résultat d'exploitation depuis %s, premier mois de l'exercice, sur la dette plus les capitaux propres, moins la trésorerie.",
    en = "Return on capital employed puts operating income since %s, the first month of the fiscal year, over debt plus equity, minus cash."
  ),
  health_why_roce = c(
    fr = "Elle dit ce que le capital réellement employé a rendu.",
    en = "It says what the capital actually employed returned."
  ),
  health_lab_ca = c(fr = "Actif court", en = "Current assets"),
  health_lab_cl = c(fr = "Passif court", en = "Current liabilities"),
  health_lab_quick = c(fr = "Trésorerie et créances", en = "Cash and receivables"),
  health_lab_cash = c(fr = "Trésorerie", en = "Cash"),
  health_lab_debt = c(fr = "Dette", en = "Debt"),
  health_lab_eq = c(fr = "Capitaux propres", en = "Equity"),
  health_lab_assets = c(fr = "Actif", en = "Assets"),
  health_lab_oi = c(fr = "Résultat d'exploitation", en = "Operating income"),
  health_lab_fin = c(fr = "Charges financières", en = "Finance cost"),
  health_lab_ar = c(fr = "Créances", en = "Receivables"),
  health_lab_days = c(fr = "Jours du mois", en = "Days in the month"),
  health_lab_rev = c(fr = "Chiffre d'affaires du mois", en = "Revenue for the month"),
  health_lab_inv = c(fr = "Stocks", en = "Stock"),
  health_lab_cogs = c(fr = "Achats du mois", en = "Purchases for the month"),
  health_lab_rev_y = c(fr = "Chiffre d'affaires depuis %s", en = "Revenue since %s"),
  health_lab_gp = c(fr = "Marge brute", en = "Gross margin"),
  health_lab_ebitda = c(fr = "EBE", en = "EBITDA"),
  health_lab_book = c(fr = "Capitaux employés", en = "Capital employed"),
  health_src = c(fr = "Fichier source", en = "Source file"),
  health_src_bs = c(
    fr = "Les montants sont ceux du mois clos. Le bilan part de ce fichier.",
    en = "The amounts are the closed month. The balance sheet starts from this file."
  ),
  health_src_pnl = c(
    fr = "Les montants sont ceux du mois clos. Le compte mensuel est ce fichier.",
    en = "The amounts are the closed month. The monthly statement is this file."
  ),
  health_src_both = c(
    fr = "Les montants sont ceux du mois clos. Le bilan part du premier fichier. Le compte mensuel est le second.",
    en = "The amounts are the closed month. The balance sheet starts from the first file. The monthly statement is the second."
  ),
  health_pair = c(fr = "%s EUR sur %s EUR.", en = "%s EUR on %s EUR."),
  health_dso_read = c(
    fr = "Créances %s EUR, chiffre d'affaires du mois %s EUR.",
    en = "Receivables %s EUR, revenue for the month %s EUR."
  ),
  health_dio_read = c(
    fr = "Stocks %s EUR, achats du mois %s EUR.",
    en = "Stock %s EUR, purchases for the month %s EUR."
  ),
  health_ytd = c(fr = "Depuis %s : %s EUR sur %s EUR.", en = "Since %s: %s EUR on %s EUR."),
  health_ev_read = c(
    fr = "Dette %s EUR + capitaux propres %s EUR − trésorerie %s EUR.",
    en = "Debt %s EUR + equity %s EUR − cash %s EUR."
  ),
  health_empty = c(fr = "Pas de bilan pour calculer les ratios.", en = "No balance sheet to compute the ratios."),
  health_na = c(fr = "–", en = "–"),
  health_col_closed = c(fr = "Mois clos", en = "Closed month"),
  health_col_path = c(fr = "Décembre", en = "December"),
  health_col_read = c(fr = "Lecture", en = "Reading"),
  anom_head = c(
    fr = "%s points au-dessus de %s EUR. Prudent ou ambitieux n'en crée pas : ceci est le compte attendu.",
    en = "%s points above %s EUR. Cautious or ambitious does not create one: this is the expected statement."
  ),
  anom_central = c(
    fr = "Prudent ou ambitieux n'en crée pas : ceci est le compte attendu.",
    en = "Cautious or ambitious does not create one: this is the expected statement."
  ),
  anom_cat_act = c(fr = "À éclaircir", en = "To clear up"),
  anom_cat_act_line = c(
    fr = "Un chiffre qui peut faire lire le mois de travers : les livres qui ne se tiennent pas, ou un ratio énorme seulement parce que la base est presque nulle.",
    en = "A figure that can misread the month: books that do not tie, or a ratio that is huge only because the base is almost nothing."
  ),
  anom_cat_watch = c(fr = "À suivre", en = "To follow"),
  anom_cat_watch_line = c(
    fr = "Un écart au-dessus de %s EUR. Le compte clos reste le compte. Le driver et le même mois des années passées servent de repère.",
    en = "A gap above %s EUR. The closed statement stays the statement. The driver and the same month in prior years are the reference."
  ),
  anom_cat_hold = c(fr = "Ce qui se tient", en = "What holds"),
  anom_cat_hold_line = c(
    fr = "Ce contrôle a été fait. Les deux côtés se rejoignent, ou aucun écart ne dépasse le seuil.",
    en = "This check was run. The two sides meet, or no gap clears the floor."
  ),
  anom_cat_none = c(fr = "Aucun point.", en = "No point."),
  anom_head_clear = c(
    fr = "Aucun écart au-dessus de %s EUR. Le bilan et le pont de trésorerie se tiennent.",
    en = "No gap above %s EUR. The balance sheet and the cash bridge tie."
  ),
  anom_books = c(fr = "Livres", en = "Books"),
  anom_books_q = c(
    fr = "Le bilan et le pont de trésorerie se rejoignent-ils ?",
    en = "Do the balance sheet and the cash bridge meet?"
  ),
  anom_books_clear = c(
    fr = "Le bilan et le pont de trésorerie se tiennent, à 1 EUR près, sur tous les mois.",
    en = "The balance sheet and the cash bridge tie, within 1 EUR, in every month."
  ),
  anom_books_bs = c(
    fr = "%s : actif %s EUR, dettes et capitaux propres %s EUR, écart %s.",
    en = "%s: assets %s EUR, liabilities and equity %s EUR, gap %s EUR."
  ),
  anom_books_cash = c(
    fr = "%s : clôture %s EUR, ouverture plus le flux %s EUR, écart %s.",
    en = "%s: closing cash %s EUR, opening cash plus the flow %s EUR, gap %s EUR."
  ),
  anom_driver = c(fr = "Compte et exploitation", en = "Statement and operations"),
  anom_driver_q = c(
    fr = "Le compte retrouve-t-il chambres, occupation et tarif, et l'effectif ?",
    en = "Does the statement meet rooms, occupancy and rate, and headcount?"
  ),
  anom_driver_row = c(
    fr = "%s, %s : le compte porte %s EUR, l'exploitation %s EUR, écart %s.",
    en = "%s, %s: the statement shows %s EUR, operations %s EUR, gap %s EUR."
  ),
  anom_driver_clear = c(
    fr = "Aucun mois n'écarte le compte de l'exploitation de plus de %s EUR.",
    en = "No month separates the statement from operations by more than %s EUR."
  ),
  anom_season = c(fr = "Même mois, années passées", en = "Same month, prior years"),
  anom_season_q = c(
    fr = "Ce mois ressemble-t-il au même mois des années déjà en historique ?",
    en = "Does this month look like the same month in the history already on file?"
  ),
  anom_season_row = c(
    fr = "%s, %s : %s EUR, contre %s EUR le même mois les années passées, écart %s.",
    en = "%s, %s: %s EUR, against %s EUR in the same month of prior years, gap %s EUR."
  ),
  anom_season_many = c(
    fr = "%s : %s mois s'écartent du même mois passé. Le plus large est %s, %s EUR contre %s EUR, écart %s.",
    en = "%s: %s months differ from the same month in prior years. The widest is %s, %s EUR against %s EUR, gap %s EUR."
  ),
  anom_season_clear = c(
    fr = "Aucun mois ne s'écarte de plus de %s EUR, et de plus d'un quart, du même mois passé.",
    en = "No month is more than %s EUR, and more than a quarter, off the same month in prior years."
  ),
  anom_ratio = c(fr = "Dénominateur mince", en = "Thin denominator"),
  anom_ratio_q = c(
    fr = "Un ratio est-il grand seulement parce que la base est presque nulle ?",
    en = "Is a ratio large only because the base is almost nothing?"
  ),
  anom_ratio_row = c(
    fr = "%s : liquidité immédiate %s x. Trésorerie %s EUR, dettes à court terme %s EUR.",
    en = "%s: cash ratio %s x. Cash %s EUR, current liabilities %s EUR."
  ),
  anom_ratio_clear = c(
    fr = "Aucun mois clos ou de décembre n'a une trésorerie qui écrase des dettes à court terme presque nulles.",
    en = "No closed month or December has cash dwarfing almost no current liabilities."
  ),
  anom_empty = c(fr = "Pas de compte mensuel à relire.", en = "No monthly statement to review."),
  anom_wws_what = c(fr = "Quoi.", en = "What."),
  anom_wws_why = c(fr = "Pourquoi.", en = "Why."),
  anom_wws_so = c(fr = "Et alors.", en = "So what."),
  anom_books_why = c(
    fr = "L'actif, les dettes, les capitaux et le pont de trésorerie sont recalculés ensemble. Aucun mois ne dépasse 1 EUR d'écart.",
    en = "Assets, liabilities, equity and the cash bridge are rebuilt together. No month exceeds a 1 EUR gap."
  ),
  anom_books_so = c(
    fr = "Il n'y a pas de trou de bouclage à chercher. Les questions sont dans les autres cartes.",
    en = "There is no balancing hole to hunt. The questions are on the other cards."
  ),
  anom_drv_rev_why = c(
    fr = "Le mois est clos. Le compte porte le chiffre d'affaires arrêté. Le fichier d'exploitation porte encore chambres, occupation et tarif, et ce produit n'est pas le même objet que le compte.",
    en = "The month is closed. The statement holds the booked revenue. The operations file still holds rooms, occupancy and rate, and that product is not the same object as the statement."
  ),
  anom_drv_rev_so = c(
    fr = "La clôture garde le compte. Ne remplacez pas ce mois par le driver, et ne calibrez pas les mois ouverts comme si le driver était le réalisé.",
    en = "The close keeps the statement. Do not replace this month with the driver, and do not set the open months as if the driver were the actual."
  ),
  anom_drv_pay_why = c(
    fr = "Même partage. Effectif × coût moyen est l'hypothèse d'exploitation. La paie du compte est ce qui a été arrêté. Les deux ne décrivent pas le même geste.",
    en = "Same split. Headcount times average cost is the operating hypothesis. Payroll on the statement is what was booked. The two are not the same act."
  ),
  anom_drv_pay_so = c(
    fr = "Un choc d'effectif bougera la prévision, pas ce mois clos. Si le driver reste au-dessus de la paie arrêtée, l'automne sera trop cher par rapport à ce qui a été payé.",
    en = "A headcount shock moves the forecast, not this closed month. If the driver stays above booked payroll, the autumn will be rich relative to what was paid."
  ),
  anom_sea_rev_why = c(
    fr = "L'historique mensuel et le mois clos ne viennent pas du même geste. L'historique porte un mois d'exploitation. Le mois clos est le montant du compte.",
    en = "The monthly history and the closed month are not the same act. The history carries an operating month. The closed month is the statement amount."
  ),
  anom_sea_rev_so = c(
    fr = "L'écart est une question commerciale, pas une facture manquante. S'il s'agit du même périmètre, le mois est en retrait sur un mois passé. Ne remontez pas les mois ouverts à cet historique sans un motif porté par le commercial.",
    en = "The gap is a commercial question, not a missing invoice. If the perimeter is the same, the month is behind a past month. Do not lift the open months to that history without a commercial reason."
  ),
  anom_sea_pay_why = c(
    fr = "Ce n'est pas un mois isolé. Sur la plupart des mois, la paie de cette année est sous le même mois du fichier d'historique.",
    en = "This is not one odd month. In most months, this year's payroll sits below the same month in the history file."
  ),
  anom_sea_pay_so = c(
    fr = "Soit l'équipe s'est réduite, soit les deux fichiers ne décrivent pas la même masse. Le trancher est un périmètre, pas un coefficient. Effectif × coût moyen ne reproduira pas la paie arrêtée.",
    en = "Either the team got smaller, or the two files do not describe the same payroll. Settling that is a perimeter, not a coefficient. Headcount times cost will not reproduce the booked payroll."
  ),
  anom_ratio_why = c(
    fr = "La ligne Dettes du bilan d'ouverture est unique, et la configuration l'a classée toute entière en dette financière. Les dettes à court terme sont reconstruites : achats et charges externes au délai fournisseurs, masse salariale à 15 jours parce que les paramètres n'ont pas de délai de paie. Les créances d'ouverture sont remplacées par les jours clients.",
    en = "The opening balance sheet has a single Debts line, and setup classified all of it as financial debt. Current liabilities are rebuilt: purchases and external charges at supplier days, payroll at 15 days because the parameters have no payroll lag. Opening receivables are replaced by customer days."
  ),
  anom_ratio_so = c(
    fr = "Ne lisez pas ce multiple comme une capacité à payer les factures des milliers de fois. Lisez la trésorerie. Avant un covenant, faites séparer la ligne Dettes entre dette financière et dettes d'exploitation.",
    en = "Do not read this multiple as an ability to pay the bills thousands of times over. Read the cash. Before a covenant, split the Debts line between financial debt and operating creditors."
  ),
  health_band_tight = c(fr = "Serré", en = "Tight"),
  health_band_ordinary = c(fr = "Ordinaire", en = "Ordinary"),
  health_band_ample = c(fr = "Ample", en = "Ample"),
  health_band_light = c(fr = "Léger", en = "Light"),
  health_band_heavy = c(fr = "Lourd", en = "Heavy"),
  health_band_thin = c(fr = "Mince", en = "Thin"),
  health_band_strong = c(fr = "Fort", en = "Strong"),
  health_band_short = c(fr = "Court", en = "Short"),
  health_band_slow = c(fr = "Lent", en = "Slow"),
  health_band_note = c(fr = "Repère usuel %s à %s.", en = "Usual band %s to %s."),
  health_band_beyond = c(
    fr = "Au-delà de l'échelle. Repère usuel %s à %s.",
    en = "Beyond the scale. Usual band %s to %s."
  ),
  health_band_none = c(
    fr = "Pas de seuil unique. Pour un hôtel, l'immeuble pèse sur l'actif.",
    en = "No single threshold. For a hotel, the building sits in the assets."
  ),
  pnl_restate = c(
    fr = "Déverrouiller un mois clos, c'est un retraitement du réalisé, pas une prévision.",
    en = "Unlocking a closed month restates the actual. It is not a forecast."
  ),
  pnl_banked = c(
    fr = "Réalisé %s à %s : chiffre d'affaires %s EUR, résultat d'exploitation %s EUR. Cette partie ne bouge pas.",
    en = "Actual %s to %s: revenue %s EUR, operating income %s EUR. That part does not move."
  ),
  pnl_cone_dates = c(
    fr = "Le trait en pointillés est le %s. La colonne teintée va jusqu'au %s. Le budget voté est en orange.",
    en = "The dotted line is %s. The tinted column runs to %s. The voted budget is in orange."
  ),
  pnl_cone_budget_gap = c(
    fr = "Le fichier %s s'arrête en %s. Ce graphique est l'exercice %s, donc la ligne orange n'a aucun mois à tracer.",
    en = "The file %s stops in %s. This chart is fiscal year %s, so the orange line has no month to plot."
  ),
  pnl_cone_open = c(
    fr = "Ouvrir %s",
    en = "Open %s"
  ),
  pnl_cone_today = c(
    fr = "aujourd'hui %s ",
    en = "today %s "
  ),
  pnl_cone_eom = c(
    fr = " fin de mois %s",
    en = " End of month %s"
  ),
  pnl_budget_toggle = c(
    fr = "Budget",
    en = "Budget"
  ),
  pnl_open_band = c(
    fr = "Mois ouverts, %s : chiffre d'affaires %s EUR, résultat d'exploitation %s EUR. Si ces mois sont tous prudents, le résultat d'exploitation est %s EUR. S'ils sont tous ambitieux, il est %s EUR. Budget voté sur ces mois : %s EUR.",
    en = "Open months, %s: revenue %s EUR, operating income %s EUR. If every one of these months is cautious, operating income is %s EUR. If every one is ambitious, it is %s EUR. Voted budget on these months: %s EUR."
  ),
  pnl_open_band_gap = c(
    fr = "Mois ouverts, %s : chiffre d'affaires %s EUR, résultat d'exploitation %s EUR. Si ces mois sont tous prudents, le résultat d'exploitation est %s EUR. S'ils sont tous ambitieux, il est %s EUR. Le fichier Budget ne couvre pas ces mois.",
    en = "Open months, %s: revenue %s EUR, operating income %s EUR. If every one of these months is cautious, operating income is %s EUR. If every one is ambitious, it is %s EUR. The Budget file does not cover these months."
  ),
  scn_flat_both = c(
    fr = "Prudent et ambitieux ne changent ni le chiffre d'affaires ni le résultat d'exploitation.",
    en = "Cautious and ambitious change neither revenue nor operating income."
  ),
  scn_flat_result = c(
    fr = "Prudent et ambitieux ne changent pas le résultat d'exploitation.",
    en = "Cautious and ambitious do not change operating income."
  ),
  scn_flat_revenue = c(
    fr = "Les scénarios prudent et ambitieux ne changent pas significativement le chiffre d'affaires. Le résultat d'exploitation va de %s (prudent) à %s (ambitieux) EUR.",
    en = "The cautious and ambitious scenarios do not bring a significant change in revenue. Operating income runs from %s (cautious) to %s (ambitious) EUR."
  ),
  scn_flat_invite = c(
    fr = "Pour écarter le chiffre d'affaires, déposez un fichier mensuel avec la quantité et le prix. Hôtel : mois, chambres disponibles, taux d'occupation, tarif moyen. Autre activité : mois, unités vendues, prix unitaire. L'endroit est Données de gestion.",
    en = "To spread revenue, add a monthly file with quantity and price. Hotel: month, rooms available, occupancy, average rate. Another activity: month, units sold, unit price. The place is Management data."
  ),
  scn_flat_map = c(fr = "Carte des liens", en = "Link map"),
  scn_flat_setup = c(
    fr = "Retour à la configuration pour ajouter un fichier",
    en = "Back to the configuration to add a file"
  ),
  cone_legend_band = c(fr = "De prudent à ambitieux", en = "From cautious to ambitious"),
  cone_legend_mid = c(fr = "Autour de l'attendu", en = "Around the expected"),
  cone_legend_line = c(fr = "Attendu %s", en = "Expected %s"),
  cone_hover_line = c(fr = "Attendu : %s EUR", en = "Expected: %s EUR"),
  cone_hover_band = c(fr = "De prudent à ambitieux : %s à %s EUR", en = "From cautious to ambitious: %s to %s EUR"),
  imp_span_title = c(
    fr = "De prudent à ambitieux, résultat d'exploitation",
    en = "From cautious to ambitious, operating income"
  ),
  imp_span_empty = c(
    fr = "Pas encore d'écart en euros entre prudent et ambitieux. Lancez la sensibilité, ou liez un driver.",
    en = "No euro gap between cautious and ambitious yet. Run the sensitivity, or link a driver."
  ),
  span_situation = c(
    fr = "situation · %s",
    en = "position at %s"
  ),
  span_one_date = c(
    fr = "une date · %s",
    en = "one date · %s"
  ),
  span_in_force = c(
    fr = "en vigueur depuis %s, jusqu'au prochain changement",
    en = "in force since %s, until the next change"
  ),
  span_age = c(
    fr = "%s mois depuis le premier mois du fichier",
    en = "%s months since the file's first month"
  ),
  span_ruler_note = c(
    fr = "L'année civile va de janvier à décembre. L'exercice suit le mois de clôture choisi à l'étape Société. Chaque année visible sur le trait est marquée.",
    en = "The calendar year runs from January to December. The fiscal year follows the closing month chosen on the Company step. Each year visible on the line is marked."
  ),
  span_key_history = c(
    fr = "Historique, exercices clos",
    en = "History, closed years"
  ),
  span_key_rolling = c(
    fr = "Suivi, mois et volumes",
    en = "Rolling, months and volumes"
  ),
  span_key_plan = c(
    fr = "Plan, budget voté",
    en = "Plan, voted budget"
  ),
  span_plan_note = c(
    fr = "Le budget voté est un seul jeu de chiffres pour l'année : sa barre ne change pas de couleur. Une barre pâle après aujourd'hui veut dire que le fichier suit le réel, puis estime les mois qui restent.",
    en = "The voted budget is one set of figures for the year: its bar does not change colour. A pale bar after today means the file follows what is recorded, then estimates the months still ahead."
  ),
  budget_year_read = c(
    fr = "Ce fichier est lu comme le budget %s. L'exercice en cours est %s.",
    en = "This file is read as the %s budget. The open exercise is %s."
  ),
  budget_year_ask = c(
    fr = "Ce fichier de budget correspond au budget de",
    en = "This budget file is the budget for"
  ),
  budget_year_open = c(
    fr = "%s, exercice en cours",
    en = "%s, current exercise"
  ),
  budget_year_hint = c(
    fr = "N-1 est l'année d'avant. Le fichier sur le disque ne change pas.",
    en = "N-1 is the year before. The file on disk does not change."
  ),
  budget_year_busy = c(
    fr = "Enregistrement de l'année du budget.",
    en = "Saving the budget year."
  ),
  budget_year_local = c(
    fr = "L'année est enregistrée sur ce PC.",
    en = "The year is saved on this PC."
  ),
  budget_year_site_ok = c(
    fr = "Le site a la même année.",
    en = "The website has the same year."
  ),
  budget_year_kept_pc = c(
    fr = "L'année est gardée sur ce PC. Le compte n'a pas de colonnes de budget, donc le site n'a pas pu enregistrer ce choix.",
    en = "The year stays on this PC. The account copy has no budget columns, so the site could not save this choice."
  ),
  budget_year_site_old = c(
    fr = "L'année est gardée sur ce PC. Le site a encore l'année précédente.",
    en = "The year stays on this PC. The website still has the previous year."
  ),
  budget_year_empty_local = c(
    fr = "Ce budget n'a pas de colonne à dater. Rien n'a été changé.",
    en = "This budget has no column to date. Nothing was changed."
  ),
  budget_year_missing = c(
    fr = "Aucun fichier de budget dans la configuration.",
    en = "No budget file in the configuration."
  ),
  budget_year_write_fail = c(
    fr = "L'année n'a pas été enregistrée.",
    en = "The year was not saved."
  ),
  kpi_close = c(fr = "Fermer", en = "Close"),
  oi_read_title = c(
    fr = "Résultat d'exploitation · %s",
    en = "Operating income · %s"
  ),
  oi_read_lead = c(
    fr = "%s est un mois de prévision. Le fichier mensuel ne porte pas ce montant. L'écran affiche %s. Le calcul donne %s.",
    en = "%s is a forecast month. The monthly file does not hold this amount. The screen shows %s. The calculation gives %s."
  ),
  oi_read_hint = c(
    fr = "Ce cadre ouvre le calcul du mois.",
    en = "This frame opens the month's calculation."
  ),
  oi_read_open = c(
    fr = "Voir le calcul de ce mois",
    en = "See this month's calculation"
  ),
  oi_read_rev = c(
    fr = "Fichier %s, mois %s.",
    en = "File %s, month %s."
  ),
  oi_read_pay = c(
    fr = "Fichier %s, même mois.",
    en = "File %s, same month."
  ),
  oi_fac_cap = c(fr = "Capacité", en = "Capacity"),
  oi_fac_util = c(fr = "taux d'utilisation", en = "utilisation"),
  oi_fac_rate = c(fr = "tarif", en = "rate"),
  oi_fac_head = c(fr = "Effectif", en = "Headcount"),
  oi_fac_cost = c(fr = "coût moyen", en = "average cost"),
  oi_fac_nights = c(fr = "Nuits", en = "Nights"),
  oi_fac_unit = c(fr = "coût par nuit", en = "cost per night"),
  oi_fac_fixed = c(fr = "Fixe", en = "Fixed"),
  oi_file_back = c(fr = "Retour au calcul", en = "Back to the calculation"),
  oi_file_save = c(fr = "Enregistrer", en = "Save"),
  oi_file_lead = c(
    fr = "Les mois déjà réalisés restent figés. Vous pouvez changer les chiffres des mois de prévision. Le résultat à l'écran se met à jour quand vous actualisez.",
    en = "Closed months stay fixed. You can change the figures of the forecast months. The result on screen updates when you refresh."
  ),
  oi_file_none = c(
    fr = "Ce fichier ne contient pas de mois de prévision. Rien n'est ouvert à la saisie.",
    en = "This file has no forecast month. Nothing is open for entry."
  ),
  oi_file_saved = c(
    fr = "%s valeur(s) écrite(s) dans le fichier.",
    en = "%s value(s) written into the file."
  ),
  oi_file_csv = c(
    fr = "Cet écran modifie un fichier csv. Ce classeur reste en lecture.",
    en = "This screen edits a csv file. This workbook stays read-only."
  ),
  oi_read_cogs = c(
    fr = "Ligne %s. Fichier %s.",
    en = "Line %s. File %s."
  ),
  oi_read_opex_driven = c(
    fr = "Ligne %s. Fichier %s.",
    en = "Line %s. File %s."
  ),
  oi_read_opex_rest = c(
    fr = "Le reste de la ligne suit le même mois dans l'historique : %s.",
    en = "The rest of the line follows the same month in the history: %s."
  ),
  oi_read_other = c(
    fr = "Pas de règle de volume. Rythme du même mois dans %s. %s valait %s.",
    en = "No volume rule. Same-month rhythm in %s. %s was %s."
  ),
  oi_read_other_plain = c(
    fr = "Pas de règle de volume. Rythme du même mois dans l'historique.",
    en = "No volume rule. Same-month rhythm in the history."
  ),
  oi_read_da = c(
    fr = "Même chiffre que %s dans %s (%s). Le calendrier d'investissement ne modifie pas ce mois.",
    en = "Same figure as %s in %s (%s). The investment calendar does not change this month."
  ),
  oi_read_da_plain = c(
    fr = "Rythme du même mois dans l'historique. Le calendrier d'investissement ne modifie pas ce mois.",
    en = "Same-month rhythm in the history. The investment calendar does not change this month."
  ),
  oi_read_da_added = c(
    fr = "Le calendrier ajoute %s de dotation ce mois (%s).",
    en = "The calendar adds %s of depreciation this month (%s)."
  ),
  oi_read_below = c(
    fr = "Sous ce résultat, le mois continue. Ces lignes ne font pas partie du résultat d'exploitation.",
    en = "Below this result, the month continues. These lines are outside operating income."
  ),
  oi_read_tax = c(
    fr = "Taux d'impôt dans les paramètres : %s. Le moteur a gardé le chiffre de cette ligne.",
    en = "Tax rate in the parameters: %s. The engine kept this line's figure."
  ),
  oi_read_ni = c(
    fr = "Résultat d'exploitation − résultat financier − exceptionnel − impôt.",
    en = "Operating income − financial result − exceptional − tax."
  ),
  oi_read_seasonal = c(
    fr = "Rythme du même mois dans l'historique.",
    en = "Same-month rhythm in the history."
  ),
  cell_read_title = c(fr = "%s · %s", en = "%s · %s"),
  cell_read_lead = c(
    fr = "%s · %s est un mois de prévision. L'écran affiche %s. Le calcul donne %s.",
    en = "%s · %s is a forecast month. The screen shows %s. The calculation gives %s."
  ),
  cash_read_title = c(fr = "Trésorerie · %s", en = "Cash · %s"),
  cash_read_bridge = c(
    fr = "Ouverture + variation.",
    en = "Opening + change."
  ),
  cash_read_open = c(fr = "Clôture de %s.", en = "Closing balance of %s."),
  cash_read_ni = c(
    fr = "Même chiffre que le résultat net du compte de résultat.",
    en = "Same figure as net income on the income statement."
  ),
  cash_read_da = c(
    fr = "Même chiffre que les dotations du compte de résultat.",
    en = "Same figure as depreciation on the income statement."
  ),
  cash_read_wc = c(
    fr = "Clients = chiffre d'affaires du mois × %s j / %s jours. Stocks = achats × %s j / %s jours. Fournisseurs = (achats + autres charges) × %s j / %s jours. Personnel = masse salariale × 15 j / %s jours. Un délai plus long que le mois compte plus d'un mois de ce flux et ne reprend pas le mois d'avant. Délais dans %s.",
    en = "Receivables = this month's revenue × %s d / %s days. Stock = purchases × %s d / %s days. Payables = (purchases + other costs) × %s d / %s days. Payroll accrual = payroll × 15 d / %s days. A delay longer than the month counts more than one month of that flow and does not reach into the previous month. Days in %s."
  ),
  wc_open_step = c(
    fr = "Part du premier mois qui vient du stock d'ouverture, pas du flux du mois.",
    en = "The part of the first month that comes from the opening stock, not from the month's flow."
  ),
  wc_open_flag = c(
    fr = "Le bilan d'ouverture porte %s de %s. Avec %s jours de délai, le modèle en attend %s en %s. %s sont dénoués en %s. Vérifier le bilan d'ouverture ou le délai.",
    en = "The opening balance sheet carries %s of %s. At %s days, the model expects %s in %s. %s is released in %s. Check the opening balance sheet or the delay."
  ),
  cash_read_wc_plain = c(
    fr = "Effet de la variation des postes du bilan : clients, stocks, fournisseurs, autres dettes.",
    en = "Effect of the balance-sheet move: receivables, stock, payables, other liabilities."
  ),
  cash_read_capex_none = c(fr = "Aucun investissement ce mois.", en = "No capital expenditure this month."),
  cash_read_capex_lines = c(fr = "Investissement dans %s.", en = "Capital expenditure in %s."),
  cash_read_capex_gap = c(
    fr = "Le fichier %s porte %s. Le pont affiché porte %s.",
    en = "File %s holds %s. The bridge on screen holds %s."
  ),
  cash_read_debt_none = c(fr = "Pas de mouvement de dette ce mois.", en = "No debt movement this month."),
  cash_read_debt_off = c(
    fr = "Le fichier %s porte %s de dette ce mois. Le pont ne reprend pas ce mouvement.",
    en = "File %s holds %s of debt this month. The bridge does not take that movement."
  ),
  cash_read_debt_on = c(fr = "Dette nette dans %s.", en = "Net debt in %s."),
  cash_read_flows = c(fr = "Les trois flux", en = "The three flows"),
  cash_read_cfi = c(fr = "\u2212 investissement", en = "\u2212 capital expenditure"),
  cash_read_cff = c(fr = "Dette nette", en = "Net debt"),
  bs_read_title = c(fr = "Bilan · %s", en = "Balance sheet · %s"),
  bs_read_assets = c(fr = "Actif", en = "Assets"),
  bs_read_claims = c(fr = "Passif et capitaux", en = "Liabilities and equity"),
  bs_read_tot_assets = c(fr = "Total actif", en = "Total assets"),
  bs_read_tot_claims = c(fr = "Total passif et capitaux", en = "Total liabilities and equity"),
  bs_read_cash = c(
    fr = "Clôture du pont de trésorerie de ce mois.",
    en = "Closing cash from this month's bridge."
  ),
  bs_read_ar = c(fr = "Délai clients dans %s.", en = "Customer days in %s."),
  bs_read_inv = c(fr = "Délai stocks dans %s.", en = "Stock days in %s."),
  bs_read_ap = c(
    fr = "Délai fournisseurs dans %s. La base est les achats plus les autres charges.",
    en = "Supplier days in %s. The base is purchases plus other external costs."
  ),
  bs_read_ocl = c(
    fr = "Les paramètres ne portent pas de délai de paie. Le moteur retient 15 jours.",
    en = "The parameters have no payroll lag. The engine keeps 15 days."
  ),
  bs_read_ppe = c(
    fr = "Solde de %s, plus l'investissement du mois, moins les dotations.",
    en = "Balance of %s, plus this month's capital expenditure, minus depreciation."
  ),
  bs_read_eq = c(
    fr = "Solde de %s, plus le résultat net du mois.",
    en = "Balance of %s, plus this month's net income."
  ),
  bs_read_hold = c(
    fr = "Solde tenu depuis %s. Le calendrier de dette n'est pas appliqué.",
    en = "Balance held from %s. The debt calendar is not applied."
  ),
  bs_read_oca = c(fr = "Solde tenu depuis %s.", en = "Balance held from %s."),
  bs_read_identity = c(
    fr = "Total actif %s, total passif et capitaux %s, écart %s.",
    en = "Total assets %s, total liabilities and equity %s, gap %s."
  ),
  kpi_ok = c(
    fr = "Rien à signaler. Le chiffre se lit tel qu'il est écrit.",
    en = "Nothing to flag. The figure reads as it is written."
  ),
  kpi_watch = c(
    fr = "À vérifier. Le chiffre est tenu, un point mérite un regard.",
    en = "Worth a look. The figure holds, and one point deserves a check."
  ),
  kpi_alert = c(
    fr = "À éclaircir. Ce chiffre ne se lit pas tel quel.",
    en = "To clear up. This figure should not be read as it stands."
  ),
  kpi_th_item = c(fr = "Chiffre", en = "Figure"),
  kpi_th_amt = c(fr = "Montant", en = "Amount"),
  kpi_th_file = c(fr = "Fichier", en = "File"),
  kpi_th_line = c(fr = "Ligne", en = "Line"),
  kpi_th_rows = c(fr = "Mois", en = "Months"),
  kpi_th_mean = c(fr = "Signification", en = "Meaning"),
  kpi_trace_src = c(
    fr = "Source · %s",
    en = "Source · %s"
  ),
  kpi_trace_months = c(
    fr = "Détail par mois",
    en = "Month by month"
  ),
  kpi_trace_same = c(
    fr = "Même fichier, même ligne et même lecture pour chaque mois ci-dessous.",
    en = "Same file, same line and same reading for every month below."
  ),
  kpi_file_tip = c(
    fr = "Voir les lignes. Un csv de prévision se modifie ici.",
    en = "See the rows. A forecast csv can be edited here."
  ),
  kpi_file_back = c(fr = "Retour au chiffre", en = "Back to the figure"),
  kpi_file_ro = c(
    fr = "Lecture. Ce fichier est produit par le calcul. La saisie directe concerne un csv de prévision.",
    en = "Read only. This file is produced by the calculation. Direct entry is for a forecast csv."
  ),
  kpi_file_json = c(
    fr = "Note locale. Le solde et les mouvements se saisissent sur l'onglet Trésorerie.",
    en = "Local note. The balance and the movements are typed on the Cash tab."
  ),
  kpi_file_empty = c(fr = "Ce fichier ne contient pas de ligne.", en = "This file has no row."),
  kpi_file_missing = c(fr = "Ce fichier est introuvable.", en = "This file cannot be found."),
  kpi_obj_cash = c(
    fr = "Trésorerie à la clôture de l'exercice, lue dans le pont.",
    en = "Cash at the fiscal close, read from the bridge."
  ),
  kpi_obj_oi = c(
    fr = "Résultat d'exploitation cumulé sur les mois de l'exercice.",
    en = "Operating income summed over the months of the fiscal year."
  ),
  kpi_out_open = c(
    fr = "Solde saisi à la main. Il ne remplace pas la trésorerie du mois.",
    en = "Balance typed by hand. It does not replace the month's cash."
  ),
  kpi_out_low = c(
    fr = "Solde le plus bas sur les quatre semaines qui suivent.",
    en = "Lowest balance over the following four weeks."
  ),
  kpi_out_move = c(
    fr = "Mouvement daté saisi à la main.",
    en = "Dated movement typed by hand."
  ),
  kpi_sens_import = c(
    fr = "Mois clos. Importé du fichier mensuel, sur cette ligne et ce mois, puis repris dans le fichier de travail.",
    en = "Closed month. Imported from the monthly file, on this line and this month, then written into the working file."
  ),
  kpi_sens_rule = c(
    fr = "Mois ouvert. Calculé par la règle du moteur et écrit dans le fichier de travail, sur cette ligne et ce mois. Il n'est pas dans le total réalisé.",
    en = "Open month. Computed by the engine rule and written into the working file, on this line and this month. It is not inside the actual total."
  ),
  kpi_sens_book = c(
    fr = "Montant lu dans le fichier de travail, sur cette ligne et ce mois.",
    en = "Amount read from the working file, on this line and this month."
  ),
  kpi_rev_total = c(
    fr = "Somme des mois clos seulement. La prévision est à part.",
    en = "Sum of closed months only. The forecast sits apart."
  ),
  kpi_rev_watch = c(
    fr = "Anomalies, À suivre : un écart existe déjà sur cette ligne. Le total du carton reste la somme des mois clos.",
    en = "Anomalies, To follow: a gap is already open on this line. The card total remains the sum of closed months."
  ),
  kpi_margin_ytd = c(
    fr = "Résultat d'exploitation des mois clos, divisé par le chiffre d'affaires des mêmes mois.",
    en = "Operating income of the closed months, divided by revenue of the same months."
  ),
  kpi_margin_last = c(
    fr = "Marge du dernier mois clos seul, pas le cumul.",
    en = "Margin of the last closed month alone, not the cumulative figure."
  ),
  kpi_margin_nobud = c(
    fr = "Le budget voté n'a pas de marge d'exploitation sur ce mois. La comparaison manque.",
    en = "The voted budget has no operating margin on this month. The comparison is missing."
  ),
  kpi_peak = c(
    fr = "Mois clos dont le chiffre d'affaires est le plus élevé. Un mois, pas un cumul.",
    en = "Closed month with the highest revenue. One month, not a cumulative figure."
  ),
  kpi_cash_end = c(
    fr = "Dernière ligne du pont de trésorerie. Si ce mois est une prévision, ce n'est pas le solde de banque d'aujourd'hui.",
    en = "Last row of the cash bridge. If that month is a forecast, it is not today's bank balance."
  ),
  kpi_cash_low = c(
    fr = "Plus petit solde de clôture du pont, mois clos et mois ouverts ensemble.",
    en = "Lowest closing balance on the bridge, closed and open months together."
  ),
  kpi_cash_open = c(
    fr = "Solde d'ouverture. Ligne Trésorerie du bilan d'ouverture, reprise comme ouverture du premier mois du pont.",
    en = "Opening balance. The cash line of the opening balance sheet, used as the first month's opening cash."
  ),
  kpi_cash_neg = c(
    fr = "Au moins un mois se ferme sous zéro.",
    en = "At least one month closes below zero."
  ),
  kpi_cash_ratio = c(
    fr = "Anomalies, À éclaircir : le ratio de liquidité est énorme parce que les dettes à court terme sont petites. Lire le solde, pas le multiple.",
    en = "Anomalies, To clear up: the cash ratio is huge because current liabilities are small. Read the balance, not the multiple."
  ),
  kpi_f_rev = c(
    fr = "Somme de la ligne %s, mois clos %s, dans cet ordre.",
    en = "Sum of the %s line, closed months %s, in that order."
  ),
  kpi_f_fc = c(
    fr = "Somme des mois ouverts %s. Ces termes ne sont pas dans le total réalisé.",
    en = "Sum of the open months %s. These terms are not inside the actual total."
  ),
  kpi_f_margin = c(
    fr = "Résultat d'exploitation des mois clos, divisé par le chiffre d'affaires des mêmes mois.",
    en = "Operating income of the closed months, divided by revenue of the same months."
  ),
  kpi_f_margin_last = c(
    fr = "Même division, sur le seul mois %s.",
    en = "The same division, on %s alone."
  ),
  kpi_f_nobud = c(
    fr = "Pas de formule : le budget voté n'a pas de marge d'exploitation sur %s.",
    en = "No formula: the voted budget has no operating margin on %s."
  ),
  kpi_f_peak = c(
    fr = "Le mois clos dont le chiffre d'affaires est le plus élevé.",
    en = "The closed month with the highest revenue."
  ),
  kpi_f_cash = c(
    fr = "Clôture = ouverture du mois + résultat net + dotations + effet de BFR + investissements + dette nette.",
    en = "Closing cash = opening cash of the month + net income + depreciation + working-capital effect + capital expenditure + net debt."
  ),
  kpi_f_min = c(
    fr = "Le plus petit solde de clôture du pont.",
    en = "The lowest closing balance on the bridge."
  ),
  kpi_f_read = c(
    fr = "Lecture directe. Pas de calcul.",
    en = "Read as written. No calculation."
  ),
  drv_note = c(
    fr = "Votre note, si vous voulez. Écrivez pourquoi vous confirmez, ou ce que le moteur a mal visé.",
    en = "Your note, if you want one. Write why you confirm, or what the engine aimed at badly."
  ),
  drv_saved = c(
    fr = "Enregistré le %s. Verdict %s.",
    en = "Saved on %s. Verdict %s."
  ),
  drv_pass = c(
    fr = "Réussi",
    en = "Pass"
  ),
  drv_fail = c(
    fr = "Échoué",
    en = "Fail"
  ),
  drv_h_soutien = c(
    fr = "Soutien",
    en = "Support"
  ),
  drv_tests_foot = c(
    fr = "Chaque colonne est un test. Soutien : l'historique montre-t-il un lien entre la série et sa ligne cible ? Indépendance : ce lien tient-il une fois retiré l'effet du volume ? Résultat : la série est-elle liée au résultat d'exploitation ? « Réussi » veut dire que le lien est assez fort pour le nombre de mois et qu'il bat le hasard. Les deux tableaux suivants donnent les chiffres de chaque test.",
    en = "Each column is a test. Support: does the history show a link between the series and its target line? Independence: does that link hold once the volume effect is removed? Result: is the series linked to operating income? \"Pass\" means the link is strong enough for the number of months and beats chance. The two tables below give the figures for each test."
  ),
  drv_map_lead = c(
    fr = "Chaque série, à gauche, rejoint la ligne du compte de résultat qu'elle alimente. Les lignes se regroupent ensuite en marge brute, puis en EBE, et finissent au résultat d'exploitation, à droite. Cliquez une carte ou une ligne pour mettre son chemin en évidence et voir ses chiffres sous le schéma.",
    en = "Each series, on the left, joins the income-statement line it feeds. The lines then group into gross margin, then EBITDA, and end at operating income, on the right. Click a card or a line to highlight its path and see its figures under the diagram."
  ),
  drv_map_shares = c(
    fr = "Les pourcentages à droite des lignes répondent à une autre question. Sur les %s mois réalisés, quelle part des variations d'un mois à l'autre chaque ligne explique-t-elle dans le solde où elle arrive ? Les parts d'un même solde font 100 %%. Une part négative veut dire que la ligne amortit : elle bouge à contre-sens du solde. Une ligne fixe pèse 0 %%, même si son montant est élevé.",
    en = "The percentages to the right of the lines answer another question. Over the %s realised months, what share of the month-to-month variation does each line explain in the subtotal it reaches? Shares of one subtotal add to 100%%. A negative share means the line dampens: it moves against the subtotal. A fixed line weighs 0%%, even when its amount is large."
  ),
  drv_map_shares_short = c(
    fr = "Moins de trois mois réalisés : les parts ne sont pas calculées.",
    en = "Fewer than three realised months: shares are not computed."
  ),
  drv_map_shares_hidden = c(
    fr = "Lignes à zéro sur ces mois, non affichées : %s.",
    en = "Lines at zero over these months, not shown: %s."
  ),
  drv_map_shares_tie = c(
    fr = "Contrôle : le solde %s ne se reconstruit pas à partir de ses lignes (écart %s EUR). Les parts de ce solde ne sont pas affichées.",
    en = "Check: subtotal %s does not rebuild from its lines (gap %s EUR). Its shares are not shown."
  ),
  drv_map_oi_share = c(fr = "résultat %s", en = "result %s"),
  drv_map_fixed = c(fr = "fixe", en = "fixed"),
  drv_map_legend_absorb = c(fr = "Ligne sans série", en = "Line with no series"),
  drv_map_resid = c(
    fr = "L'étiquette posée sur chaque trait décrit le lien entre la série et sa ligne, une fois la saison et la tendance retirées. « bruit » : le lien est trop faible pour le nombre de mois disponibles. « constante » : la série ne bouge pas assez pour être mesurée. « attendu » : la série entre dans le calcul de la ligne, son lien est donc fort par construction. « plus de coût » : monter cette série fait monter la masse salariale. Sous le nom de chaque série, la petite ligne donne son lien avec le résultat d'exploitation.",
    en = "The label on each edge describes the link between the series and its line, once season and trend are removed. \"noise\": the link is too weak for the number of months available. \"constant\": the series does not move enough to be measured. \"expected\": the series is part of the line's calculation, so its link is strong by construction. \"more cost\": raising this series raises payroll. Under each series name, the small line gives its link with operating income."
  ),
  drv_map_noise = c(fr = "bruit · %s mois", en = "noise · %s months"),
  drv_map_constant = c(fr = "constante", en = "constant"),
  drv_map_pay_down = c(fr = "moins de coût", en = "less cost"),
  drv_map_pay_up = c(fr = "plus de coût", en = "more cost"),
  drv_map_pay_factor = c(fr = "plus de coût", en = "more cost"),
  drv_map_result_noise = c(fr = "résultat : bruit", en = "result: noise"),
  drv_map_noise_full = c(
    fr = "La corrélation (%s) est trop faible pour %s mois d'historique : on ne peut pas la distinguer du hasard.",
    en = "The correlation (%s) is too weak for %s months of history: it cannot be told apart from chance."
  ),
  drv_map_pay_down_full = c(
    fr = "Une fois la saison retirée, plus la série monte, plus la masse salariale baisse.",
    en = "Once the season is removed, a higher series goes with a lower payroll."
  ),
  drv_map_pay_up_full = c(
    fr = "Une fois la saison retirée, plus la série monte, plus la masse salariale monte.",
    en = "Once the season is removed, a higher series goes with a higher payroll."
  ),
  drv_map_pay_factor_full = c(
    fr = "L'effectif et le coût moyen composent la masse salariale. Monter l'un ou l'autre fait monter cette ligne.",
    en = "Headcount and average cost make up payroll. Raising either one raises that line."
  ),
  drv_box_against = c(
    fr = "Contre %s",
    en = "Against %s"
  ),
  seg_note_head = c(
    fr = "Jusqu'à %s lignes par mois (%s) regroupées avant chaque test : les volumes sont additionnés.",
    en = "Up to %s rows per month (%s) grouped before each test: volumes are summed."
  ),
  seg_note_weight = c(
    fr = "Les prix et les taux sont pondérés par %s.",
    en = "Prices and rates are weighted by %s."
  ),
  seg_note_plain = c(
    fr = "Moyenne simple, faute de volume pour pondérer : %s.",
    en = "Plain mean, with no volume to weight by: %s."
  ),
  drv_formula_title = c(
    fr = "Formules déclarées",
    en = "Declared formulas"
  ),
  drv_formula_lead = c(
    fr = "Pour chaque formule déclarée, le moteur multiplie les colonnes mois par mois et compare le produit à la ligne comptabilisée, sur les mois clos. Une formule vérifiée fait de ses colonnes un calcul prouvé. Une formule non retenue reste lue par la prévision, mais ses colonnes ne comptent pas comme un calcul prouvé.",
    en = "For each declared formula, the engine multiplies the columns month by month and compares the product with the booked line, on closed months. A proven formula makes its columns a proven calculation. A formula that is not kept is still read by the forecast, but its columns do not count as a proven calculation."
  ),
  drv_formula_h_eq = c(
    fr = "Formule",
    en = "Formula"
  ),
  drv_formula_h_check = c(
    fr = "Vérification",
    en = "Check"
  ),
  drv_formula_ok = c(
    fr = "Vérifiée : écart médian %s %% sur %s mois.",
    en = "Proven: median gap %s%% over %s months."
  ),
  drv_formula_no = c(
    fr = "Non retenue : écart médian %s %%.",
    en = "Not kept: median gap %s%%."
  ),
  drv_chip_reverse = c(
    fr = "sens inverse",
    en = "reverse direction"
  ),
  drv_mech_expected = c(
    fr = "attendu",
    en = "expected"
  ),
  drv_box_mech = c(
    fr = "attendu par construction",
    en = "expected by construction"
  ),
  drv_h_line = c(
    fr = "Ligne cible",
    en = "Target line"
  ),
  drv_h_result = c(
    fr = "Résultat",
    en = "Result"
  ),
  drv_line_title = c(
    fr = "Contre la ligne cible",
    en = "Against the target line"
  ),
  drv_line_method = c(
    fr = "Ce tableau compare chaque série à la ligne qu'elle alimente : le chiffre d'affaires, ou la masse salariale pour l'effectif et le coût moyen. Seuls les mois clos comptent. Le moteur essaie des décalages de trois mois en avance à trois mois en retard. Il mélange ensuite 2 000 fois les mois de la série au hasard, pour voir si le hasard ferait aussi bien.",
    en = "This table compares each series with the line it feeds: revenue, or payroll for headcount and average cost. Only closed months count. The engine tries lags from three months ahead to three months behind. It then shuffles the months of the series 2,000 times at random, to see whether chance would do as well."
  ),
  drv_h_against = c(
    fr = "Contre",
    en = "Against"
  ),
  drv_goal_no_euro = c(
    fr = "Ce tableau ne donne pas de montant en euros. En saison, le tarif, le taux d'utilisation et les nuitées montent tous ensemble, et une corrélation ne sait pas dire laquelle des trois fait le chiffre. Les montants en euros viennent du calcul du moteur, dans l'onglet Impact.",
    en = "This table gives no euro amount. In season, the rate, occupancy and nights all rise together, and a correlation cannot say which of the three makes the figure. Euro amounts come from the engine calculation, in the Impact tab."
  ),
  drv_mech_tag = c(
    fr = "mécanisme",
    en = "mechanism"
  ),
  drv_line_mech = c(
    fr = "Les séries marquées « mécanisme » entrent dans le calcul de la ligne. Leur corrélation est élevée par construction : elle n'est donc pas comptée comme une preuve.",
    en = "Series marked \"mechanism\" are part of the line's calculation. Their correlation is high by construction, so it is not counted as proof."
  ),
  drv_nights = c(
    fr = "Nuitées vendues : le lien avec le chiffre d'affaires est mesuré (Soutien). L'indépendance n'est pas lancée, parce que cette série est le volume retiré des autres. On ne la retire pas d'elle-même.",
    en = "Nights sold: the link with revenue is measured (Evidence). Independence is not run, because this series is the volume removed from the others. It is not removed from itself."
  ),
  drv_nights_result = c(
    fr = "Nuitées vendues contre le résultat d'exploitation, %s mois, de %s à %s. r = %s. %s Le lien dépasse le seuil de corrélation et reste après le tarif (r partiel %s). Il ne bat pas le hasard (rang %s %%). Dans cet historique, 10 %% de nuitées en plus vont avec environ %s EUR de résultat d'exploitation. Ce n'est pas une règle de prévision.",
    en = "Nights sold against operating income, %s months, from %s to %s. r = %s. %s The link clears the correlation bar and remains after the rate (partial r %s). It does not beat chance (rank %s %%). In this history, 10%% more nights go with about %s EUR of operating income. This is not a forecast rule."
  ),
  drv_impact_na = c(
    fr = "Pas chiffré",
    en = "Not valued"
  ),
  drv_indep_na = c(
    fr = "Non fait",
    en = "Not run"
  ),
  drv_confirmed = c(
    fr = "Confirmé",
    en = "Confirmed"
  ),
  drv_card_impact_r = c(
    fr = "Corrélation avec le résultat d'exploitation : %s.",
    en = "Correlation with operating income: %s."
  ),
  drv_card_soutien = c(
    fr = "%s. Avec la ligne %s, la corrélation vaut %s pour un seuil de %s. Le hasard fait aussi bien dans %s %% des tirages.",
    en = "%s. With the line %s, the correlation is %s against a bar of %s. Chance does as well in %s%% of the draws."
  ),
  drv_card_soutien_reverse = c(
    fr = "Mais la ligne bouge avant la série : c'est la ligne qui entraîne la série, donc ce lien ne compte pas comme une preuve.",
    en = "But the line moves before the series: the line drives the series, so this link does not count as proof."
  ),
  drv_card_soutien_mech = c(
    fr = "Attendu. La série entre dans le calcul de la ligne %s, donc sa corrélation (%s) est forte par construction et ne prouve rien de plus.",
    en = "Expected. The series is part of the calculation of %s, so its correlation (%s) is strong by construction and proves nothing more."
  ),
  drv_card_result = c(
    fr = "%s. Avec le résultat d'exploitation, la corrélation vaut %s. Le hasard fait aussi bien dans %s %% des tirages.",
    en = "%s. With operating income, the correlation is %s. Chance does as well in %s%% of the draws."
  ),
  drv_line_missing = c(
    fr = "Le test contre la ligne cible n'a pas pu être fait pour cette série.",
    en = "The target-line test could not be run for this series."
  ),
  drv_line_head = c(
    fr = "Le moteur compare %s à la ligne %s sur %s mois clos, de %s à %s.",
    en = "The engine compares %s with %s over %s closed months, from %s to %s."
  ),
  drv_line_flat = c(
    fr = "La série ne varie pas assez pour calculer une corrélation.",
    en = "The series does not vary enough to compute a correlation."
  ),
  drv_line_mech_card = c(
    fr = "La corrélation (%s) est forte par construction, car la série entre dans le calcul de la ligne.",
    en = "The correlation (%s) is strong by construction, because the series is part of the line's calculation."
  ),
  drv_line_follow = c(
    fr = "La série bouge %s mois après la ligne.",
    en = "The series moves %s months after the line."
  ),
  drv_line_lead = c(
    fr = "La série bouge %s mois avant la ligne, ce qui aide à prévoir.",
    en = "The series moves %s months before the line, which helps to forecast."
  ),
  drv_card_indep = c(
    fr = "%s. Une fois retiré l'effet %s, il reste une corrélation de %s.",
    en = "%s. Once the effect %s is removed, a correlation of %s remains."
  ),
  drv_card_indep_plain = c(
    fr = "%s. Une fois retiré l'effet du volume, il reste une corrélation de %s.",
    en = "%s. Once the volume effect is removed, a correlation of %s remains."
  ),
  drv_card_indep_na = c(
    fr = "%s. Le test n'a pas pu être fait, ce qui ne veut pas dire que le lien est nul.",
    en = "%s. The test could not be run, which does not mean the link is zero."
  ),
  drv_card_confirmed_open = c(
    fr = "Pas encore enregistré.",
    en = "Not saved yet."
  ),
  drv_why_keep = c(
    fr = "Le lien tient une fois la saison retirée, il bat le hasard, et il prévoit mieux que le même mois de l'an dernier.",
    en = "The link holds once the season is removed, it beats chance, and it forecasts better than the same month last year."
  ),
  drv_why_optional = c(
    fr = "Un lien apparaît, mais il ne réussit pas les trois tests à la fois. La série peut servir, sans être prouvée.",
    en = "A link shows up, but it does not pass the three tests at once. The series can help, without being proven."
  ),
  drv_why_reject = c(
    fr = "Sur la ligne proposée, le lien ne se distingue pas du hasard.",
    en = "On the proposed line, the link cannot be told apart from chance."
  ),
  drv_why_short = c(
    fr = "L'historique est trop court pour confirmer la série : il faut au moins 24 mois.",
    en = "The history is too short to confirm the series: it takes at least 24 months."
  ),
  drv_bottom_title = c(
    fr = "Contre le résultat d'exploitation",
    en = "Against operating income"
  ),
  drv_bottom_method = c(
    fr = "Ce tableau compare chaque série au résultat d'exploitation, sur les mois clos. Le moteur essaie des décalages de trois mois en avance à trois mois en retard, puis mélange 2 000 fois les mois de la série pour voir si le hasard ferait aussi bien. La colonne « r partiel » retire l'effet des nuitées vendues (et du tarif pour les nuitées elles-mêmes), pour voir si la série apporte quelque chose de plus que le volume.",
    en = "This table compares each series with operating income, on closed months. The engine tries lags from three months ahead to three months behind, then shuffles the months of the series 2,000 times to see whether chance would do as well. The \"partial r\" column removes the effect of nights sold (and of the rate for nights themselves), to see whether the series adds something beyond volume."
  ),
  drv_bottom_missing = c(
    fr = "Le test contre le résultat d'exploitation n'a pas pu être fait pour %s.",
    en = "The operating-income test could not be run for %s."
  ),
  drv_bottom_flat_full = c(
    fr = "Le moteur compare %s au résultat d'exploitation sur %s mois, de %s à %s. La série ne varie pas assez pour calculer une corrélation.",
    en = "The engine compares %s with operating income over %s months, from %s to %s. The series does not vary enough to compute a correlation."
  ),
  drv_bottom_flat_cell = c(
    fr = "trop plate",
    en = "too flat"
  ),
  drv_bottom_head = c(
    fr = "Le moteur compare %s au résultat d'exploitation sur %s mois, de %s à %s.",
    en = "The engine compares %s with operating income over %s months, from %s to %s."
  ),
  drv_bottom_r = c(
    fr = "La corrélation vaut %s.",
    en = "The correlation is %s."
  ),
  drv_bottom_same = c(
    fr = "Le lien est le plus fort sur le même mois.",
    en = "The link is strongest on the same month."
  ),
  drv_bottom_follow = c(
    fr = "La série bouge %s mois après le résultat.",
    en = "The series moves %s months after the result."
  ),
  drv_bottom_lead = c(
    fr = "La série bouge %s mois avant le résultat, ce qui aide à prévoir.",
    en = "The series moves %s months before the result, which helps to forecast."
  ),
  drv_bottom_partial = c(
    fr = "Une fois retiré l'effet %s, il reste une corrélation de %s.",
    en = "Once the effect %s is removed, a correlation of %s remains."
  ),
  drv_bottom_nopartial = c(
    fr = "Aucun effet de volume n'a pu être retiré.",
    en = "No volume effect could be removed."
  ),
  drv_bottom_rank_in = c(
    fr = "Le hasard ne fait aussi bien que dans %s %% des tirages : le lien bat le hasard.",
    en = "Chance does as well in only %s%% of the draws: the link beats chance."
  ),
  drv_bottom_rank_out = c(
    fr = "Le hasard fait aussi bien dans %s %% des tirages : au-delà de 5 %%, le lien ne bat pas le hasard.",
    en = "Chance does as well in %s%% of the draws: above 5%%, the link does not beat chance."
  ),
  drv_bottom_eur = c(
    fr = "Dans cet historique, 10 %% de plus sur la série vont de pair avec environ %s EUR de résultat d'exploitation.",
    en = "In this history, 10%% more on the series goes with about %s EUR of operating income."
  ),
  drv_bottom_not_rule = c(
    fr = "C'est une observation sur le passé, pas une règle de prévision.",
    en = "This is an observation on the past, not a forecast rule."
  ),
  drv_bottom_bar = c(
    fr = "Une série bat le hasard quand le hasard fait aussi bien dans moins de 5 %% des tirages. Comme %s séries sont testées en même temps, le moteur applique aussi un seuil commun plus strict, de %s %%, pour écarter les coups de chance.",
    en = "A series beats chance when chance does as well in fewer than 5%% of the draws. Because %s series are tested at once, the engine also applies a stricter joint bar, of %s%%, to rule out lucky hits."
  ),
  drv_bottom_one_pass = c(
    fr = "%s (%s %%)",
    en = "%s (%s%%)"
  ),
  drv_bottom_pass_list = c(
    fr = "Prises une par une, ces séries battent le hasard : %s.",
    en = "Taken one at a time, these series beat chance: %s."
  ),
  drv_bottom_none_pass = c(
    fr = "Prise une par une, aucune série ne bat le hasard.",
    en = "Taken one at a time, no series beats chance."
  ),
  drv_bottom_family_none = c(
    fr = "Aucune ne passe le seuil commun.",
    en = "None clears the joint bar."
  ),
  drv_bottom_family_pass = c(
    fr = "%s passe aussi le seuil commun.",
    en = "%s also clears the joint bar."
  ),
  drv_bottom_family_pass_n = c(
    fr = "%s passent aussi le seuil commun.",
    en = "%s also clear the joint bar."
  ),
  drv_line_reverse = c(
    fr = "Le moteur écarte du décompte %s : la ligne bouge avant la série, c'est donc la ligne qui entraîne la série et non l'inverse.",
    en = "The engine leaves %s out of the count: the line moves before the series, so the line drives the series and not the reverse."
  ),
  drv_mark_expected = c(
    fr = "Attendu",
    en = "Expected"
  ),
  drv_bottom_slope = c(
    fr = "Le montant 10 % est la pente sur ces mois.",
    en = "The 10% amount is the slope on these months."
  ),
  path_final = c(
    fr = "L'objectif final de Structure : chaque série a un crochet dans Drivers. La prévision ne change pas.",
    en = "The final aim of Structure: every series has a check in Drivers. The forecast does not change."
  ),
  path_map_kicker = c(
    fr = "Structure · étape 1 sur 2",
    en = "Structure · step 1 of 2"
  ),
  path_map_title = c(
    fr = "Lire la carte des fichiers",
    en = "Read the file map"
  ),
  path_map_shows = c(
    fr = "Cette page montre jusqu'où va chaque fichier, quelles règles le lisent, et où le montant arrive.",
    en = "This page shows how far each file goes, which rules read it, and where the amount lands."
  ),
  path_map_obj = c(
    fr = "L'objectif de cette étape est de voir les fichiers avant la prévision. Un fichier en retard reste marqué sur le trait.",
    en = "The aim of this step is to see the files before the forecast. A late file stays marked on the span."
  ),
  path_map_do = c(
    fr = "Lisez le trait « Jusqu'où va chaque fichier ». Puis cliquez le bouton. Le crochet se pose sur l'onglet Cartographie.",
    en = "Read the span « how far each file goes ». Then click the button. The check lands on the Cartographie tab."
  ),
  path_map_btn = c(
    fr = "J'ai lu la carte",
    en = "I have read the map"
  ),
  path_map_done = c(
    fr = "Carte lue le %s.",
    en = "Map read on %s."
  ),
  path_drv_kicker = c(
    fr = "Structure · étape 2 sur 2",
    en = "Structure · step 2 of 2"
  ),
  path_drv_title = c(
    fr = "Enregistrer un avis sur chaque série",
    en = "Record a view on every series"
  ),
  path_drv_shows = c(
    fr = "Cette page montre le verdict du moteur et le lien de chaque série avec le résultat d'exploitation.",
    en = "This page shows the engine verdict and each series' link with operating income."
  ),
  path_drv_obj = c(
    fr = "L'objectif de cette étape est d'enregistrer un avis sur chaque ligne. La liste garde le crochet. Vous n'avez pas à retenir celles qui sont déjà faites.",
    en = "The aim of this step is to record a view on every row. The list keeps the check. You do not have to remember which ones are already done."
  ),
  path_drv_do = c(
    fr = "Ouvrez une série. Lisez le verdict. Enregistrez. Le crochet apparaît sur la ligne. L'onglet Drivers reçoit le crochet quand toutes les lignes sont enregistrées.",
    en = "Open a series. Read the verdict. Save. The check appears on the row. The Drivers tab receives the check when every row is saved."
  ),
  path_drv_progress = c(
    fr = "%s séries enregistrées sur %s.",
    en = "%s series saved out of %s."
  ),
  path_drv_progress_one = c(
    fr = "%s série enregistrée sur %s.",
    en = "%s series saved out of %s."
  ),
  path_drv_all = c(
    fr = "Toutes les séries sont enregistrées.",
    en = "Every series is saved."
  ),
  path_goto_drv = c(
    fr = "Passer à Drivers",
    en = "Go to Drivers"
  ),
  path_goto_map = c(
    fr = "Revenir à Cartographie",
    en = "Back to Cartographie"
  ),
  path_open = c(
    fr = "Ouvrir",
    en = "Open"
  ),
  path_opened = c(
    fr = "Ouverte",
    en = "Open"
  ),
  path_todo = c(
    fr = "À enregistrer",
    en = "To save"
  ),
  path_saved_short = c(
    fr = "Enregistrée",
    en = "Saved"
  ),
  path_next = c(
    fr = "Série suivante",
    en = "Next series"
  ),
  path_graphs = c(
    fr = "La série ouverte se traite dans « Série en cours ». Les autres onglets sont les graphiques de cette même série.",
    en = "The open series is handled in « Series in progress ». The other tabs are the charts of that same series."
  ),
  path_h_verdict = c(
    fr = "Verdict du moteur",
    en = "Engine verdict"
  ),
  path_h_result = c(
    fr = "Résultat d'exploitation",
    en = "Operating income"
  ),
  drv_v_keep = c(
    fr = "Garder",
    en = "Keep"
  ),
  drv_v_optional = c(
    fr = "Possible",
    en = "Optional"
  ),
  drv_v_reject = c(
    fr = "Écarter",
    en = "Reject"
  ),
  drv_v_short = c(
    fr = "Pas assez d'historique",
    en = "Needs more history"
  ),
  drv_engine_pick = c(
    fr = "recommandation du moteur",
    en = "engine recommendation"
  ),
  drv_engine_was = c(
    fr = "Le moteur proposait %s.",
    en = "The engine proposed %s."
  ),
  drv_box_yours = c(
    fr = "Votre verdict : %s.",
    en = "Your verdict: %s."
  ),
  drv_in_formula = c(
    fr = "Dans la formule",
    en = "In the formula"
  ),
  drv_state_proved_stat = c(
    fr = "Dans la formule vérifiée · test statistique concluant",
    en = "In the verified formula · statistical test holds"
  ),
  drv_state_proved_open = c(
    fr = "Dans la formule vérifiée · test statistique non concluant",
    en = "In the verified formula · statistical test inconclusive"
  ),
  drv_state_failed = c(
    fr = "Formule déclarée non retenue",
    en = "Declared formula not kept"
  ),
  drv_state_failed_used = c(
    fr = "Formule déclarée non retenue · la prévision la lit encore",
    en = "Declared formula not kept · the forecast still reads it"
  ),
  drv_badge_failed = c(fr = "Non retenue", en = "Not kept"),
  drv_lead_uses = c(
    fr = "La prévision se sert de %s.",
    en = "The forecast uses %s."
  ),
  drv_lead_uses_none = c(
    fr = "La prévision ne se sert d'aucune série de volume : chaque ligne suit sa propre saison.",
    en = "The forecast uses no volume series: each line follows its own season."
  ),
  drv_lead_proves = c(
    fr = "Formules vérifiées : %s. Sur %s mois clos, le calcul s'écarte des comptes de %s %% en médiane.",
    en = "Checked formulas: %s. Over %s closed months, the calculation misses the books by %s%% at the median."
  ),
  drv_lead_proves_exact = c(
    fr = "Formules vérifiées : %s. Sur %s mois clos, le calcul redonne le montant inscrit dans les comptes, sans écart.",
    en = "Checked formulas: %s. Over %s closed months, the calculation gives back the amount in the books, with no gap."
  ),
  drv_lead_proves_none = c(
    fr = "Aucune formule déclarée ne retrouve encore les comptes sur les mois clos.",
    en = "No declared formula matches the books on the closed months yet."
  ),
  drv_lead_choice = c(
    fr = "Il vous reste à trancher : %s.",
    en = "Left for you to decide: %s."
  ),
  drv_lead_choice_failed = c(
    fr = "la formule %s, qui s'écarte des comptes de %s %% en médiane",
    en = "the formula %s, which misses the books by %s%% at the median"
  ),
  drv_lead_choice_open = c(
    fr = "%s séries que l'historique ne prouve pas encore",
    en = "%s series the history does not prove yet"
  ),
  drv_lead_choice_open_one = c(
    fr = "une série que l'historique ne prouve pas encore",
    en = "one series the history does not prove yet"
  ),
  drv_lead_choice_none = c(
    fr = "Il ne reste rien à trancher sur les séries.",
    en = "Nothing is left to decide on the series."
  ),
  synth_obj_k = c(fr = "Objectif de fin d'année", en = "Year-end goal"),
  synth_obj_s = c(
    fr = "Trésorerie à fin %s, puis résultat d'exploitation de l'exercice. Fixer un objectif.",
    en = "Cash at the end of %s, then operating income for the fiscal year. Set a goal."
  ),
  synth_out_k = c(fr = "Trésorerie à 4 semaines", en = "Cash in 4 weeks"),
  synth_out_low = c(
    fr = "Semaine la plus basse, le %s.",
    en = "Lowest week, on %s."
  ),
  synth_out_need = c(fr = "Saisir le solde bancaire", en = "Enter the bank balance"),
  synth_out_need_s = c(
    fr = "Solde hypothétique du compte bancaire. Les mouvements datés sont facultatifs.",
    en = "Hypothetical bank-account balance. Dated movements are optional."
  ),
  kpi_land_lead = c(
    fr = "Chaque carton est l'atterrissage de l'exercice et ce à quoi le chiffre se compare.",
    en = "Each card is the year's landing and what the figure is compared with."
  ),
  kpi_land_rev_k = c(
    fr = "Atterrissage du chiffre d'affaires",
    en = "Revenue landing"
  ),
  kpi_land_booked = c(
    fr = "%s %% est déjà dans les comptes. Il reste %s EUR en prévision.",
    en = "%s%% is already in the books. %s EUR is still a forecast."
  ),
  kpi_land_split = c(
    fr = "%s EUR réalisés · %s EUR prévus",
    en = "%s EUR actual · %s EUR forecast"
  ),
  kpi_land_pace = c(
    fr = "Au rythme des mois clos, les mois ouverts feraient %s EUR. La prévision en met %s.",
    en = "At the pace of the closed months, the open months would be %s EUR. The forecast puts %s."
  ),
  kpi_land_pace_close = c(
    fr = "La prévision des mois ouverts suit le rythme des mois clos (%s EUR).",
    en = "The open-month forecast follows the closed-month pace (%s EUR)."
  ),
  kpi_land_oi_k = c(
    fr = "Atterrissage du résultat d'exploitation",
    en = "Operating income landing"
  ),
  kpi_land_oi_cut = c(
    fr = "Les mois clos portent %s EUR. Les mois ouverts en retirent %s.",
    en = "Closed months carry %s EUR. Open months take %s back."
  ),
  kpi_land_oi_add = c(
    fr = "Les mois clos portent %s EUR. Les mois ouverts en ajoutent %s.",
    en = "Closed months carry %s EUR. Open months add %s."
  ),
  kpi_land_margin = c(
    fr = "Marge des mois clos : %s. Marge de l'exercice, prévision comprise : %s.",
    en = "Margin on closed months: %s. Margin for the year, forecast included: %s."
  ),
  kpi_land_cash_k = c(
    fr = "Trésorerie à la clôture",
    en = "Cash at the close"
  ),
  kpi_land_cash_down = c(
    fr = "%s EUR au dernier mois clos. La prévision en retire %s d'ici %s.",
    en = "%s EUR at the last closed month. The forecast takes %s out by %s."
  ),
  kpi_land_cash_up = c(
    fr = "%s EUR au dernier mois clos. La prévision en ajoute %s d'ici %s.",
    en = "%s EUR at the last closed month. The forecast adds %s by %s."
  ),
  kpi_land_cash_same = c(
    fr = "%s EUR au dernier mois clos. La prévision laisse ce solde en %s.",
    en = "%s EUR at the last closed month. The forecast leaves that balance in %s."
  ),
  kpi_land_floor_ok = c(
    fr = "Aucun mois ne passe sous zéro. Le plus bas est %s, à %s EUR.",
    en = "No month goes below zero. The low is %s, at %s EUR."
  ),
  kpi_land_floor_bad = c(
    fr = "%s mois passent sous zéro. Le plus bas est %s, à %s EUR.",
    en = "%s months go below zero. The low is %s, at %s EUR."
  ),
  kpi_land_bud_miss = c(
    fr = "Le fichier Budget s'arrête en %s. Cet exercice n'a pas de montant voté en face.",
    en = "The Budget file stops in %s. This fiscal year has no voted amount beside it."
  ),
  kpi_land_bud_vs = c(
    fr = "En face, le budget voté de l'exercice est %s EUR, soit un écart de %s EUR.",
    en = "Beside it, the voted budget for the year is %s EUR, a gap of %s EUR."
  ),
  kpi_out_click = c(
    fr = "Le clic ouvre la saisie : la date, puis le montant.",
    en = "The click opens the entry: the date, then the amount."
  ),
  synth_origin_actual = c(fr = "réalisé", en = "actual"),
  synth_origin_rebuilt = c(fr = "reconstruit", en = "reconstructed"),
  synth_origin_forecast = c(fr = "prévision", en = "forecast"),
  synth_origin_plan = c(fr = "plan", en = "plan"),
  synth_origin_declared = c(fr = "déclaré", en = "declared"),
  synth_origin_move = c(fr = "mouvement", en = "movement"),
  synth_cash_marks = c(
    fr = "Losange : solde déclaré (contrôle). Triangle : saisie qui déplace la courbe mensuelle. Un clic sur le nom les retire ou les remet.",
    en = "Diamond: declared balance (a check). Triangle: an entry that moves the monthly curve. A click on the name removes or restores them."
  ),
  synth_origin_unknown = c(fr = "inconnu", en = "unknown"),
  synth_trust_limited = c(fr = "Lecture limitée", en = "Limited reading"),
  synth_trust_cash_rebuilt = c(
    fr = "Trésorerie reconstruite : aucun solde bancaire n'entre dans le pont mensuel.",
    en = "Cash is reconstructed: no bank balance enters the monthly bridge."
  ),
  synth_trust_need_balance = c(
    fr = "Solde bancaire non saisi pour l'aperçu à quatre semaines.",
    en = "No bank balance typed for the four-week outlook."
  ),
  synth_trust_balance_only = c(
    fr = "Solde déclaré sans encaissement ni paiement daté.",
    en = "Declared balance with no dated receipt or payment."
  ),
  synth_trust_cash_gap = c(
    fr = "Écart de %s EUR entre le solde déclaré et la trésorerie modélisée du même mois.",
    en = "%s EUR gap between the declared balance and modeled cash for the same month."
  ),
  synth_trust_bud_partial = c(
    fr = "Le budget voté ne couvre pas tous les mois de l'exercice.",
    en = "The voted budget does not cover every month of the fiscal year."
  ),
  synth_trust_unknown_months = c(
    fr = "%s mois sans statut réalisé ou prévision.",
    en = "%s months without an actual or forecast status."
  ),
  synth_find_need_bal_t = c(fr = "À examiner en priorité", en = "To examine first"),
  synth_find_need_bal_b = c(
    fr = "Au %s, la trésorerie modélisée est d'environ %s EUR. En %s elle atterrit à %s EUR. Le montant du jour est une indication pour le solde bancaire.",
    en = "On %s, modeled cash is about %s EUR. In %s it lands at %s EUR. Today's amount is a guide for the bank balance."
  ),
  synth_find_need_bal_b_ye = c(
    fr = "La trésorerie modélisée atterrit à %s EUR en %s. Saisissez le solde bancaire pour lire les quatre prochaines semaines.",
    en = "Modeled cash lands at %s EUR in %s. Enter the bank balance to read the next four weeks."
  ),
  synth_find_need_bal_a = c(fr = "Saisir le solde bancaire", en = "Enter the bank balance"),
  synth_find_bal_only_t = c(fr = "Prévision de trésorerie à compléter", en = "Cash outlook to complete"),
  synth_find_bal_only_b = c(
    fr = "Solde déclaré : %s EUR au %s. Aucun encaissement ni décaissement saisi.",
    en = "Declared balance: %s EUR on %s. No receipt or payment typed."
  ),
  synth_find_bal_only_a = c(fr = "Actualiser le solde et les échéances", en = "Update the balance and due items"),
  synth_find_gap_t = c(fr = "Solde déclaré et modèle à rapprocher", en = "Declared balance and model to reconcile"),
  synth_find_gap_b = c(
    fr = "Solde déclaré %s EUR au %s. Modèle du mois : %s EUR. Écart %s EUR. Vérifiez le périmètre avant une décision de trésorerie.",
    en = "Declared balance %s EUR on %s. Model for the month: %s EUR. Gap %s EUR. Check the scope before a cash decision."
  ),
  synth_find_gap_a = c(fr = "Rapprocher la trésorerie", en = "Reconcile cash"),
  synth_find_gap_quiet_t = c(fr = "Solde déclaré et modèle proches", en = "Declared balance and model are close"),
  synth_find_gap_quiet_b = c(
    fr = "Solde déclaré %s EUR au %s. Modèle du mois : %s EUR. Écart %s EUR, sous le seuil de rapprochement.",
    en = "Declared balance %s EUR on %s. Model for the month: %s EUR. Gap %s EUR, under the reconcile threshold."
  ),
  synth_floor_note = c(
    fr = "Plancher de trésorerie : %s EUR. Le réglage est dans Scénarios.",
    en = "Cash floor: %s EUR. The control lives on Scenarios."
  ),
  synth_wc_open_note = c(
    fr = "En %s, %s EUR de BFR d'ouverture sont dénoués. C'est un effet de bilan d'ouverture, pas de la trésorerie que l'hôtel gagnera à nouveau.",
    en = "In %s, %s EUR of opening working capital are released. That is an opening-balance effect, not cash the hotel will earn again."
  ),
  synth_hotel_revpar = c(
    fr = "RevPAR des mois ouverts : %s EUR (occupation × tarif).",
    en = "Open-month RevPAR: %s EUR (occupancy × rate)."
  ),
  synth_hotel_payroll = c(
    fr = "Masse salariale : %s du chiffre d'affaires.",
    en = "Payroll: %s of revenue."
  ),
  synth_horizon_caption = c(
    fr = "Après le dernier mois clos, chaque mois reprend le même mois de l'année précédente, sans dette ni investissements. Ce n'est pas un nouveau budget.",
    en = "After the last closed month, each month repeats the same month of the previous year, without debt or capex. This is not a new budget."
  ),
  synth_fy_mark = c(
    fr = "Le trait pointillé est le début d'un exercice.",
    en = "The dotted line is the start of a fiscal year."
  ),
  synth_cash_window_label = c(fr = "Fenêtre", en = "Window"),
  synth_cash_window_fy = c(fr = "Exercice", en = "Fiscal year"),
  synth_cash_window_prior = c(fr = "Exercice précédent", en = "Previous fiscal year"),
  synth_cash_window_empty = c(
    fr = "L'exercice précédent n'a aucun mois sur cette courbe. Elle commence en %s.",
    en = "The previous fiscal year has no month on this curve. It starts in %s."
  ),
  sync_wait_calc = c(
    fr = "Réglages reçus. Le calcul reprend. Après 90 secondes, la fenêtre se ferme.",
    en = "Settings received. The calculation is running again. After 90 seconds, the window closes."
  ),
  synth_horizon_label = c(
    fr = "Jusqu'où voulez-vous projeter ?",
    en = "How far ahead do you want to project?"
  ),
  synth_find_floor_t = c(fr = "Trésorerie sous le plancher", en = "Cash under the floor"),
  synth_find_floor_b = c(
    fr = "La trésorerie modélisée atteint %s EUR en %s, sous le plancher de %s EUR.",
    en = "Modeled cash reaches %s EUR in %s, under the floor of %s EUR."
  ),
  synth_find_floor_a = c(fr = "Examiner la trajectoire", en = "Inspect the path"),
  synth_find_oi_t = c(fr = "Le résultat d'exploitation s'érode", en = "Operating income is eroding"),
  synth_find_oi_b = c(
    fr = "Les mois clos portent %s EUR. Les mois ouverts en retirent %s. Atterrissage %s EUR.",
    en = "Closed months carry %s EUR. Open months take %s back. Landing %s EUR."
  ),
  synth_find_oi_a = c(fr = "Choisir le scénario", en = "Choose the scenario"),
  synth_find_calm_t = c(fr = "Rien de matériel à ce stade", en = "Nothing material at this stage"),
  synth_find_calm_b = c(
    fr = "Chiffre d'affaires %s EUR, résultat d'exploitation %s EUR, trésorerie de clôture %s EUR.",
    en = "Revenue %s EUR, operating income %s EUR, closing cash %s EUR."
  ),
  synth_find_calm_a = c(fr = "Ouvrir le compte de résultat", en = "Open the income statement"),
  synth_find_empty_t = c(fr = "Pas encore de calcul", en = "No calculation yet"),
  synth_find_empty_b = c(
    fr = "Le compte de résultat mensuel manque. Relancez le calcul depuis Paramètres.",
    en = "The monthly income statement is missing. Rerun the calculation from Settings."
  ),
  synth_find_empty_a = c(fr = "Ouvrir Paramètres", en = "Open Settings"),
  synth_card_rev = c(fr = "Chiffre d'affaires de l'exercice", en = "Fiscal-year revenue"),
  synth_card_oi = c(fr = "Résultat d'exploitation de l'exercice", en = "Fiscal-year operating income"),
  synth_card_cash = c(fr = "Trésorerie à fin d'exercice", en = "Cash at fiscal year-end"),
  synth_card_booked = c(
    fr = "%s %% déjà dans les comptes · %s EUR prévus",
    en = "%s%% already in the books · %s EUR forecast"
  ),
  synth_card_bud_vs = c(
    fr = "vs budget %s EUR (%s)",
    en = "vs budget %s EUR (%s)"
  ),
  synth_card_bud_miss = c(
    fr = "Pas de budget voté comparable pour cet exercice",
    en = "No comparable voted budget for this fiscal year"
  ),
  synth_card_bud_partial = c(
    fr = "Budget partiel : pas d'écart annuel affiché",
    en = "Partial budget: no annual gap shown"
  ),
  synth_card_oi_open = c(
    fr = "Mois ouverts : %s EUR · marge annuelle %s",
    en = "Open months: %s EUR · annual margin %s"
  ),
  synth_card_cash_basis = c(
    fr = "Modèle mensuel · %s · plancher %s EUR",
    en = "Monthly model · %s · floor %s EUR"
  ),
  synth_card_cash_closed = c(
    fr = "Dernier mois clos : %s EUR",
    en = "Last closed month: %s EUR"
  ),
  synth_out_panel_k = c(fr = "Solde bancaire déclaré", en = "Declared bank balance"),
  synth_out_planned = c(fr = "prévu", en = "planned"),
  synth_out_partial_tag = c(fr = "partiel", en = "partial"),
  synth_out_complete = c(
    fr = "Prévision partielle selon les échéances saisies. Minimum %s EUR le %s.",
    en = "Partial outlook from the typed items. Low %s EUR on %s."
  ),
  out_asof_planned = c(
    fr = "Date prévue %s : ce n'est pas encore un solde observé.",
    en = "Planned date %s: this is not yet an observed balance."
  ),
  out_open_from_journal = c(
    fr = "Solde repris du journal : %s EUR au %s.",
    en = "Balance taken from the journal: %s EUR on %s."
  ),
  nav_period_restored = c(
    fr = "Fenêtre enregistrée affichée. Retour à l'exercice en cours.",
    en = "Saved window shown. Return to the current fiscal year."
  ),
  drv_reject_still_formula = c(
    fr = "Vous avez choisi Écarter. La prévision l'utilise encore comme entrée de formule.",
    en = "You chose Set aside. The forecast still uses it as a formula input."
  ),
  drv_reject_not_formula = c(
    fr = "Vous avez choisi Écarter. La prévision ne s'en sert plus comme entrée de formule.",
    en = "You chose Set aside. The forecast no longer uses it as a formula input."
  ),
  health_fold_limited = c(
    fr = "Lecture limitée : trésorerie reconstruite.",
    en = "Limited reading: cash is reconstructed."
  ),
  synth_out_stale = c(
    fr = "Horizon à actualiser : le solde date du %s.",
    en = "Horizon to refresh: the balance is dated %s."
  ),
  synth_evidence_sum = c(
    fr = "Détail du calcul · du résultat à la trésorerie · méthode",
    en = "Calculation detail · from result to cash · method"
  ),
  synth_bridge_title = c(
    fr = "Du résultat d'exploitation à la trésorerie",
    en = "From operating income to cash"
  ),
  synth_drivers_title = c(
    fr = "Ce qui explique l'évolution (mois ouverts)",
    en = "What explains the move (open months)"
  ),
  synth_drivers_none = c(
    fr = "Aucune contribution calculée pour les mois ouverts. Inspectez les hypothèses dans Drivers ou Scénarios.",
    en = "No calculated contribution for the open months. Inspect assumptions in Drivers or Scenarios."
  ),
  synth_queue_title = c(fr = "Prochaines actions", en = "Next actions"),
  synth_queue_none = c(fr = "Rien en attente.", en = "Nothing waiting."),
  synth_hero_oi = c(fr = "Résultat mensuel", en = "Monthly result"),
  synth_hero_cash = c(fr = "Trajectoire de trésorerie", en = "Cash path"),
  synth_yscale_fit = c(fr = "Axe Y : ajusté aux données", en = "Y-axis: fitted to data"),
  synth_yscale_zero = c(fr = "Axe Y : départ à zéro", en = "Y-axis: starts at zero"),
  bridge_win_1 = c(fr = "Un mois", en = "One month"),
  bridge_win_q = c(fr = "Trimestre", en = "Quarter"),
  bridge_win_6 = c(fr = "6 mois", en = "6 months"),
  bridge_win_12 = c(fr = "12 mois", en = "12 months"),
  bridge_win_all = c(fr = "Période entière", en = "Full period"),
  synth_yscale_k = c(fr = "Échelle verticale", en = "Vertical scale"),
  synth_band_shocks = c(
    fr = "Prudent ↔ ambitieux (chocs sur lignes)",
    en = "Cautious ↔ ambitious (line shocks)"
  ),
  synth_hover_edge = c(
    fr = "Prudent %s EUR. Ambitieux %s EUR.",
    en = "Cautious %s EUR. Ambitious %s EUR."
  ),
  synth_hover_edge_on = c(
    fr = "Au %s, prudent %s EUR et ambitieux %s EUR.",
    en = "On %s, cautious %s EUR and ambitious %s EUR."
  ),
  synth_go_pnl = c(fr = "Compte de résultat", en = "Income statement"),
  synth_go_cash = c(fr = "Voir la trajectoire", en = "See the path"),
  synth_go_scn = c(fr = "Comprendre le résultat", en = "Understand the result"),
  home_open_tab = c(fr = "Ouvrir %s", en = "Open %s"),

  out_balance_k = c(fr = "Solde du compte", en = "Account balance"),
  out_balance_lead = c(
    fr = "Solde hypothétique du compte bancaire de la société à cette date. Il s'enregistre dès la saisie. Ce n'est ni un encaissement ni un paiement.",
    en = "Hypothetical balance of the company bank account on this date. It is saved as you type. It is neither a receipt nor a payment."
  ),
  out_balance_fy = c(
    fr = "Ce solde aide à voir l'écart avec le modèle. Il ne change pas la clôture de fin d'exercice.",
    en = "This balance helps you see the gap with the model. It does not change the year-end close."
  ),
  out_moves_fy = c(
    fr = "Un flux nouveau est calculé dans la clôture de fin d'exercice. Un décalage change le mois, pas cette clôture, tant que les deux dates sont dans l'exercice.",
    en = "A new flow is included in the year-end close. A timing shift changes the month, not that close, while both dates stay inside the year."
  ),
  out_moves_k = c(fr = "Mouvements attendus, facultatifs", en = "Expected movements, optional"),
  out_balance_kicker = c(fr = "Entrée 1 · le solde", en = "Entry 1 · the balance"),
  out_moves_kicker = c(fr = "Entrée 2 · facultatif", en = "Entry 2 · optional"),
  out_moves_alone = c(
    fr = "Le solde suffit pour le contrôle. Un mouvement saisi ici déplace le modèle mensuel.",
    en = "The balance is enough for the check. A movement entered here moves the monthly model."
  ),
  out_kind_hint = c(
    fr = "Encaissement : l'argent entre. Paiement : l'argent sort.",
    en = "Receipt: money comes in. Payment: money goes out."
  ),
  out_line_pending = c(
    fr = "Ces lignes sont dans le fichier des saisies manuelles. Elles déplacent la trésorerie, le bilan, et le résultat quand la nature est Nouveau.",
    en = "These lines sit in the manual-entries file. They move cash, the balance sheet, and the result when the kind is New."
  ),
  out_open_modeled = c(
    fr = "Point de départ : trésorerie modélisée, environ %s EUR au %s. Le solde bancaire n'est pas demandé.",
    en = "Starting point: modeled cash, about %s EUR on %s. A typed bank balance is not required."
  ),
  journal_outlook_from_model = c(
    fr = "Point de départ : trésorerie modélisée, environ %s EUR au %s.",
    en = "Starting point: modeled cash, about %s EUR on %s."
  ),
  journal_outlook_move = c(
    fr = "%s · %s · %s EUR le %s.",
    en = "%s · %s · %s EUR on %s."
  ),
  out_moves_lead = c(
    fr = "Nouveau : un flux que le modèle ne contient pas. Décalage : le modèle attendait déjà ce montant à une autre date.",
    en = "New: a flow the model does not contain. Timing shift: the model already expected this amount on another date."
  ),
  man_nature = c(fr = "Nature", en = "Kind"),
  man_nouveau = c(fr = "Nouveau", en = "New"),
  man_decalage = c(fr = "Décalage", en = "Timing shift"),
  man_prevue = c(fr = "Date prévue au modèle", en = "Date expected in the model"),
  man_ligne = c(fr = "Ligne du compte de résultat", en = "P&L line"),
  man_ligne_other_inc = c(fr = "Autres produits", en = "Other income"),
  man_ligne_opex = c(fr = "Charges externes", en = "Other opex"),
  man_ligne_rev = c(fr = "Chiffre d'affaires", en = "Revenue"),
  man_ligne_cogs = c(fr = "Achats", en = "Cost of sales"),
  man_ligne_pay = c(fr = "Salaires", en = "Payroll"),
  man_ligne_fin = c(fr = "Charges financières", en = "Finance costs"),
  man_ligne_excep = c(fr = "Exceptionnel", en = "Exceptional"),
  man_apply = c(fr = "Enregistrer et recalculer", en = "Save and recalculate"),
  man_rerun = c(fr = "Recalcul du modèle…", en = "Recalculating the model…"),
  man_need_prevue = c(
    fr = "Pour un décalage, indiquez la date où le modèle attendait déjà ce montant.",
    en = "For a timing shift, give the date when the model already expected this amount."
  ),
  man_closed_refuse = c(
    fr = "Refusé : %s",
    en = "Refused: %s"
  ),
  man_ack_cash = c(
    fr = "Trésorerie %s : %s → %s EUR.",
    en = "Cash %s: %s → %s EUR."
  ),
  man_ack_ni = c(
    fr = "Résultat de l'exercice : %s → %s EUR.",
    en = "Full-year result: %s → %s EUR."
  ),
  man_ack_ni_flat = c(
    fr = "Résultat de l'exercice : inchangé.",
    en = "Full-year result: unchanged."
  ),
  man_gap = c(
    fr = "Solde déclaré %s EUR. Trésorerie modélisée %s EUR. Écart non expliqué : %s EUR. Ajoutez des mouvements pour le réduire.",
    en = "Declared balance %s EUR. Modeled cash %s EUR. Unexplained gap: %s EUR. Add movements to narrow it."
  ),
  man_gap_ok = c(
    fr = "Solde déclaré %s EUR. Il colle à la trésorerie modélisée.",
    en = "Declared balance %s EUR. It matches modeled cash."
  ),
  man_toggle = c(fr = "Saisies", en = "Entries"),
  man_kind = c(fr = "Manuelles", en = "Manual"),
  man_toggle_on = c(
    fr = "Les saisies sont dans le calcul. Le curseur les en retire et décembre bouge. Le fichier reste. Le mot Saisies ouvre la liste.",
    en = "The entries are in the calculation. The switch takes them out and December moves. The file stays. The word Entries opens the list."
  ),
  man_switch_on = c(fr = "incluses", en = "included"),
  man_switch_off = c(fr = "exclues", en = "excluded"),
  man_toggle_off = c(
    fr = "Les saisies sont hors calcul. Le curseur les remet et décembre bouge. Le fichier reste. Le mot Saisies ouvre la liste.",
    en = "The entries are out of the calculation. The switch puts them back and December moves. The file stays. The word Entries opens the list."
  ),
  man_go_tip = c(
    fr = "Ouvre les saisies en cours : libellé, montant, date, et les deux clôtures.",
    en = "Opens the live entries: label, amount, date, and both closes."
  ),
  man_bar = c(
    fr = "Ici, vous pouvez faire une saisie manuelle d'un montant non prévu dans les comptes, lorsqu'il a un impact significatif : %s € ou plus, soit environ 2 %% de la trésorerie de l'année. De plus petits montants peuvent être saisis, mais leur impact est limité dans la vue d'ensemble.",
    en = "Here you can record a manual amount that was not in the accounts, when its effect is material: %s € or more, about 2 %% of the year's cash. Smaller amounts can be entered, but their effect on the overall picture is limited."
  ),
  man_bar_tip = c(
    fr = "Saisie d'un montant non prévu, à partir de %s €. De plus petits montants restent possibles.",
    en = "An unplanned amount, from %s €. Smaller amounts can still be entered."
  ),
  man_line_hint = c(
    fr = "Un encaissement sur une charge réduit cette charge. Une créance irrécouvrable récupérée se range plutôt en Autres produits ou en Chiffre d'affaires.",
    en = "A receipt booked on an expense reduces that expense. A recovered write-off belongs under Other income or Revenue."
  ),
  man_pop_title = c(fr = "Saisies en cours", en = "Live entries"),
  man_pop_hist = c(fr = "Historique", en = "History"),
  man_pop_cash = c(fr = "Trésorerie", en = "Cash"),
  man_pop_pnl = c(fr = "Compte de résultat", en = "Income statement"),
  man_pop_empty = c(fr = "Aucune saisie active.", en = "No active entry."),
  man_pop_empty_n = c(
    fr = "Aucune saisie active. %s ligne(s) retirée(s) restent dans l'historique.",
    en = "No active entry. %s retired line(s) remain in the history."
  ),
  man_list_on = c(
    fr = "Ces saisies sont dans le calcul. Avec elles, la clôture de %s est de %s EUR. Sans ces lignes, la même clôture serait de %s EUR.",
    en = "These entries are in the calculation. With them, the %s close is %s EUR. Without these lines, that close would be %s EUR."
  ),
  man_list_off = c(
    fr = "Ces saisies sont hors calcul. Sans elles, la clôture de %s est de %s EUR. Avec ces lignes, la même clôture serait de %s EUR.",
    en = "These entries are out of the calculation. Without them, the %s close is %s EUR. With these lines, that close would be %s EUR."
  ),
  man_list_empty = c(
    fr = "Aucune saisie active.",
    en = "No active entry."
  ),
  man_list_line = c(
    fr = "%s · %s · %s EUR · %s",
    en = "%s · %s · %s EUR · %s"
  ),
  man_bridge_on = c(
    fr = "%s, %s de %s EUR le %s, est dans la clôture %s (%s EUR). Sans ces lignes : %s EUR.",
    en = "%s, %s of %s EUR on %s, is in the %s close (%s EUR). Without these lines: %s EUR."
  ),
  man_bridge_off = c(
    fr = "%s, %s de %s EUR le %s, est hors calcul. Clôture %s : %s EUR. Avec ces lignes : %s EUR.",
    en = "%s, %s of %s EUR on %s, is out of the calculation. %s close: %s EUR. With these lines: %s EUR."
  ),
  man_cone_on = c(
    fr = "%s (%s, %s EUR) est dans le calcul. Cette ligne ne change pas le chiffre d'affaires cumulé. Trésorerie %s : %s EUR. Sans ces lignes : %s EUR.",
    en = "%s (%s, %s EUR) is in the calculation. This line does not change cumulative revenue. Cash %s: %s EUR. Without these lines: %s EUR."
  ),
  man_cone_off = c(
    fr = "%s (%s, %s EUR) est hors calcul. Le chiffre d'affaires cumulé ne la contient pas. Trésorerie %s : %s EUR. Avec ces lignes : %s EUR.",
    en = "%s (%s, %s EUR) is out of the calculation. Cumulative revenue does not include it. Cash %s: %s EUR. With these lines: %s EUR."
  ),
  man_span_off = c(
    fr = "%s ligne(s) active(s) · CSV · hors calcul · clic pour ouvrir et modifier",
    en = "%s active line(s) · CSV · out of the calculation · click to open and edit"
  ),
  man_file = c(fr = "Saisies manuelles", en = "Manual entries"),
  man_file_lead = c(
    fr = "Encaissement : l'argent entre et le résultat du mois monte. Paiement : l'argent sort et le résultat du mois baisse. Un encaissement se pose sur un produit. Un paiement se pose sur une charge.",
    en = "Receipt: money comes in and the month result rises. Payment: money goes out and the month result falls. A receipt sits on an income line. A payment sits on a cost line."
  ),
  man_file_edit = c(fr = "Modifier", en = "Edit"),
  man_file_saved = c(fr = "Enregistré. Le modèle a recalculé.", en = "Saved. The model has recalculated."),
  man_file_same = c(fr = "Aucune modification.", en = "No change."),
  man_file_empty = c(fr = "Aucune saisie dans le fichier.", en = "No entry in the file."),
  man_statut = c(fr = "Statut", en = "Status"),
  man_statut_on = c(fr = "En cours", en = "Active"),
  man_statut_off = c(fr = "Retirée", en = "Retired"),
  man_span_meta = c(
    fr = "%s ligne(s) active(s) · CSV · dans le calcul · clic pour ouvrir et modifier",
    en = "%s active line(s) · CSV · in the calculation · click to open and edit"
  ),
  man_legend = c(fr = "Saisie manuelle", en = "Manual entry"),
  man_mark_cash = c(
    fr = "Cette saisie déplace la trésorerie du mois.",
    en = "This entry moves that month's cash."
  ),
  man_mark_rev = c(
    fr = "Cette saisie entre dans le chiffre d'affaires.",
    en = "This entry is inside revenue."
  ),
  man_mark_not_rev = c(
    fr = "Cette saisie ne change pas le chiffre d'affaires. Elle est sur la ligne %s.",
    en = "This entry does not change revenue. It sits on line %s."
  ),
  man_mark_hover = c(
    fr = "%s · %s · %s EUR le %s<br>%s<br>Clic : ouvrir le CSV",
    en = "%s · %s · %s EUR on %s<br>%s<br>Click: open the CSV"
  ),
  out_balance_saved = c(
    fr = "Solde de %s EUR au %s enregistré. Sans mouvement, les quatre semaines restent à ce montant.",
    en = "Balance of %s EUR on %s saved. With no movement, the four weeks stay at that amount."
  ),
  map_ask = c(fr = "Quelle ligne voulez-vous suivre ?", en = "Which line do you want to follow?"),
  map_ask_none = c(fr = "Aucune", en = "None"),
  map_howto = c(fr = "Comment lire cette carte", en = "How to read this map"),
  drv_open_map = c(fr = "Ouvrir la carte des liens", en = "Open the link map"),
  drv_map_shares_caveat = c(
    fr = "Une part peut dépasser 100 % ou passer sous 0 : elle mesure une contribution aux variations d'un mois à l'autre. Ce n'est ni une part du résultat ni la preuve qu'une ligne cause l'autre.",
    en = "A share can exceed 100% or fall below 0: it measures a contribution to month-to-month variation. It is neither a share of profit nor proof that one line causes the other."
  ),
  drv_test_open = c(
    fr = "Test non concluant",
    en = "Test inconclusive"
  ),
  drv_keep_lead = c(
    fr = "Pour chaque série, le moteur propose un verdict. « Garder » l'accepte tel quel. « Voir » ouvre la fiche de la série, si vous voulez changer le verdict, choisir un autre décalage ou laisser une note.",
    en = "For each series, the engine proposes a verdict. \"Keep\" accepts it as it is. \"See\" opens the series card, if you want another verdict, another lag, or a note."
  ),
  drv_keep = c(
    fr = "Garder",
    en = "Keep"
  ),
  drv_read = c(
    fr = "Lu",
    en = "Read"
  ),
  drv_read_note = c(
    fr = "Lu : test non concluant, laissé dans la formule.",
    en = "Read: the test did not conclude, left in the formula."
  ),
  drv_change = c(
    fr = "Voir",
    en = "See"
  ),
  drv_more = c(
    fr = "Pour aller plus loin : fiche d'une série, carte des liens, tests et graphiques",
    en = "Going further: series card, link map, tests and charts"
  ),
  drv_formula_note = c(
    fr = "Une série prouvée par une formule reste dans la prévision, même quand ses tests ne se mesurent pas : une série qui bouge peu ne donne pas de corrélation, et ce n'est pas une raison pour la retirer.",
    en = "A series proved by a formula stays in the forecast, even when its tests cannot be measured: a series that barely moves gives no correlation, and that is not a reason to remove it."
  ),
  drv_band_proof = c(fr = "Preuve comptable", en = "Accounting proof"),
  drv_band_tests = c(fr = "Trois tests sur l'historique", en = "Three tests on the history"),
  drv_col_formula = c(fr = "Formule", en = "Formula"),
  drv_col_line = c(fr = "Suit sa ligne", en = "Follows its line"),
  drv_col_indep = c(fr = "Apporte du neuf", en = "Adds something new"),
  drv_col_result = c(fr = "Pèse sur le résultat", en = "Moves the result"),
  drv_col_bilan = c(fr = "Bilan", en = "Overall"),
  drv_bil_proved = c(fr = "Prouvée", en = "Proved"),
  drv_bil_proved_tip = c(
    fr = "Une formule vérifiée sur les comptes relie la série à sa ligne. La prévision s'en sert.",
    en = "A formula checked against the books links the series to its line. The forecast uses it."
  ),
  drv_bil_decide = c(fr = "À trancher", en = "Your call"),
  drv_bil_decide_tip = c(
    fr = "La formule déclarée pour cette série s'écarte des comptes. C'est à vous de dire si vous la gardez.",
    en = "The formula declared for this series misses the books. It is your call whether to keep it."
  ),
  drv_bil_decide_tip_used = c(
    fr = "La formule déclarée pour cette série s'écarte des comptes, mais la prévision la lit encore. C'est à vous de dire si vous la gardez.",
    en = "The formula declared for this series misses the books, but the forecast still reads it. It is your call whether to keep it."
  ),
  drv_bil_called_tip = c(
    fr = "Vous avez tranché : %s. La formule déclarée s'écarte des comptes, et cet avis est enregistré.",
    en = "You have made the call: %s. The declared formula misses the books, and this view is saved."
  ),
  drv_bil_solid = c(fr = "Solide", en = "Solid"),
  drv_bil_solid_tip = c(
    fr = "Aucune formule ne la relie aux comptes, mais tous les tests mesurés passent.",
    en = "No formula links it to the books, but every measured test passes."
  ),
  drv_bil_weak = c(fr = "Fragile", en = "Fragile"),
  drv_bil_weak_tip = c(
    fr = "Aucune formule ne la relie aux comptes, et une partie seulement des tests passe.",
    en = "No formula links it to the books, and only part of the tests pass."
  ),
  drv_bil_none = c(fr = "Sans preuve", en = "No proof"),
  drv_bil_none_tip = c(
    fr = "Aucune formule ne la relie aux comptes, et aucun test ne passe.",
    en = "No formula links it to the books, and no test passes."
  ),
  drv_bil_score = c(fr = "%s tests réussis sur %s", en = "%s of %s tests passed"),
  drv_bil_score_one = c(fr = "1 test réussi sur %s", en = "1 of %s tests passed"),
  drv_bil_score_zero = c(fr = "Aucun test réussi sur %s", en = "None of %s tests passed"),
  drv_bil_score_none = c(fr = "Tests non mesurables", en = "Tests cannot be measured"),
  drv_bil_avis = c(fr = "Votre avis : %s", en = "Your verdict: %s"),
  drv_bil_engine = c(fr = "Proposé : %s", en = "Proposed: %s"),
  drv_pil_form_exact = c(
    fr = "Formule vérifiée : %s. Sur %s mois clos, le calcul redonne les comptes, sans écart.",
    en = "Checked formula: %s. Over %s closed months, the calculation gives back the books, with no gap."
  ),
  drv_pil_form_ok = c(
    fr = "Formule vérifiée : %s. Sur %s mois clos, le calcul s'écarte des comptes de %s %% en médiane.",
    en = "Checked formula: %s. Over %s closed months, the calculation misses the books by %s%% at the median."
  ),
  drv_pil_form_no = c(
    fr = "Formule non retenue : %s. Le calcul s'écarte des comptes de %s %% en médiane.",
    en = "Formula not kept: %s. The calculation misses the books by %s%% at the median."
  ),
  drv_pil_form_none = c(
    fr = "Aucune formule ne relie cette série aux comptes.",
    en = "No formula links this series to the books."
  ),
  drv_pil_gap = c(fr = "écart %s %%", en = "gap %s%%"),
  drv_pil_pass = c(
    fr = "Réussi. r %s avec %s, rang %s %% : le lien bat le hasard.",
    en = "Pass. r %s with %s, rank %s%%: the link beats chance."
  ),
  drv_pil_fail_chance = c(
    fr = "Échoué. r %s avec %s, rang %s %% : le hasard fait aussi bien.",
    en = "Fail. r %s with %s, rank %s%%: chance does as well."
  ),
  drv_pil_fail_bar = c(
    fr = "Échoué. r %s avec %s, rang %s %% : le lien reste sous le seuil de corrélation.",
    en = "Fail. r %s with %s, rank %s%%: the link stays under the correlation bar."
  ),
  drv_pil_reverse = c(
    fr = "Échoué. r %s avec %s, mais la ligne bouge avant la série : c'est la ligne qui entraîne la série.",
    en = "Fail. r %s with %s, but the line moves before the series: the line drives the series."
  ),
  drv_pil_expected = c(
    fr = "Attendu. La série entre dans le calcul de %s, donc r %s est fort par construction. Ce n'est pas une preuve de plus.",
    en = "Expected. The series is part of the calculation of %s, so r %s is strong by construction. It is no extra proof."
  ),
  drv_pil_indep_pass = c(
    fr = "Réussi. Une fois le volume retiré, il reste r %s avec %s : la série apporte une information à elle.",
    en = "Pass. Once volume is removed, r %s with %s remains: the series brings information of its own."
  ),
  drv_pil_indep_fail = c(
    fr = "Échoué. Une fois le volume retiré, il ne reste que r %s avec %s : la série n'apporte rien de plus que le volume.",
    en = "Fail. Once volume is removed, only r %s with %s remains: the series adds nothing beyond volume."
  ),
  drv_pil_flat = c(
    fr = "Non mesuré : la série bouge trop peu pour calculer une corrélation.",
    en = "Not measured: the series moves too little to compute a correlation."
  ),
  drv_pil_na = c(
    fr = "Non mesuré pour cette série.",
    en = "Not measured for this series."
  ),
  drv_pil_oi = c(fr = "le résultat d'exploitation", en = "operating income"),
  drv_pil_line_of = c(fr = "la ligne %s", en = "the %s line"),
  drv_leg_pass = c(fr = "réussi", en = "pass"),
  drv_leg_fail = c(fr = "échoué", en = "fail"),
  drv_leg_expected = c(fr = "attendu par construction", en = "expected by construction"),
  drv_leg_na = c(fr = "non mesuré", en = "not measured"),
  drv_leg_hover = c(
    fr = "Survolez une icône pour lire le chiffre.",
    en = "Hover over an icon to read the figure."
  ),
  map_path_kicker = c(
    fr = "Un chemin, de bout en bout",
    en = "One path, end to end"
  ),
  map_path_note = c(
    fr = "Les groupes en dessous restent fermés. Ouvrez-les pour les colonnes et le détail.",
    en = "The groups below stay closed. Open them for the columns and the detail."
  ),
  home_when = c(
    fr = "Calculé le %s.",
    en = "Calculated on %s."
  ),
  home_when_none = c(
    fr = "Pas encore de calcul enregistré.",
    en = "No saved calculation yet."
  ),
  home_fresh = c(
    fr = "Aucun fichier source n'est plus récent que ce calcul.",
    en = "No source file is newer than this calculation."
  ),
  home_stale = c(
    fr = "Plus récent que le calcul : %s.",
    en = "Newer than the calculation: %s."
  ),
  home_clear = c(
    fr = "Rien à éclaircir dans le contrôle du mois.",
    en = "Nothing to clear in the month check."
  ),
  home_fix = c(
    fr = "%s point(s) à éclaircir.",
    en = "%s point(s) to clear."
  ),
  home_next_fix = c(
    fr = "Ouvrir Anomalies et traiter le premier point.",
    en = "Open Anomalies and take the first point."
  ),
  home_next_drv = c(
    fr = "Il reste %s série(s) à enregistrer dans Drivers.",
    en = "%s series still to save in Drivers."
  ),
  home_next_month = c(
    fr = "Déposer le fichier du mois suivant, puis relancer le calcul.",
    en = "Drop next month's file, then run the calculation again."
  ),
  home_go = c(
    fr = "Y aller",
    en = "Go"
  ),
  home_go_anom = c(
    fr = "Examiner les anomalies",
    en = "Review anomalies"
  ),
  home_go_drv = c(
    fr = "Revoir les drivers",
    en = "Review drivers"
  ),
  home_detail = c(
    fr = "Détail du calcul",
    en = "Calculation detail"
  ),
  home_steps_done = c(
    fr = "Tout est à jour · revoir les étapes",
    en = "Everything is up to date · review the steps"
  ),
  home_form = c(
    fr = "%s à éclaircir · %s à suivre",
    en = "%s to clear · %s to watch"
  ),
  home_changed_title = c(
    fr = "Ce qui a changé",
    en = "What changed"
  ),
  home_changed_since = c(
    fr = "depuis l'actualisation du %s",
    en = "since the refresh of %s"
  ),
  home_changed_none = c(
    fr = "Rien de matériel depuis la dernière actualisation.",
    en = "Nothing material since the last refresh."
  ),
  home_changed_first = c(
    fr = "Première actualisation de cette session. Le prochain passage montrera ce qui a bougé.",
    en = "First refresh of this session. The next one will show what moved."
  ),
  home_test_response = c(
    fr = "Tester une réponse",
    en = "Test a response"
  ),
  home_test_impact = c(
    fr = "Tester sur Impact",
    en = "Test on Impact"
  ),
  anom_where_title = c(
    fr = "Où vérifier",
    en = "Where to check"
  ),
  anom_where_file = c(
    fr = "Fichier source : %s.",
    en = "Source file: %s."
  ),
  anom_where_checks = c(
    fr = "Contrôler la période, les unités, et la couverture du mois.",
    en = "Check the period, the units, and the month coverage."
  ),
  path_map_done_short = c(
    fr = "Carte lue · %s. Revoir le guide.",
    en = "Map read · %s. Review the guide."
  ),
  close_vintage_from = c(
    fr = "Version de départ",
    en = "Starting version"
  ),
  close_vintage_to = c(
    fr = "Version d'arrivée",
    en = "Ending version"
  ),
  close_vintage_keep = c(
    fr = "Garde une copie datée de la prévision déjà calculée. C'est une version, pas un nouveau calcul.",
    en = "Keeps a dated copy of the forecast already computed. It is a version, not a new calculation."
  ),
  close_k_steps = c(fr = "Où en est la clôture", en = "Where the close stands"),
  close_k_cmp = c(fr = "Ce qui a changé entre deux versions", en = "What changed between two versions"),
  close_k_reg = c(fr = "Versions enregistrées", en = "Saved versions"),
  close_k_lock = c(fr = "Mois verrouillés", en = "Locked months"),
  close_k_bud = c(fr = "Budget de référence", en = "Reference budget"),
  close_fact_wait = c(fr = "Lancez un calcul pour voir la clôture.", en = "Run a calculation to see the close."),
  close_fact_ready = c(
    fr = "Clôture de %s prête : %s étapes sur %s faites.",
    en = "The %s close is ready: %s of %s steps done."
  ),
  close_fact_left = c(
    fr = "Clôture de %s : %s étapes sur %s faites. Reste : %s.",
    en = "%s close: %s of %s steps done. Still to do: %s."
  ),
  close_prog = c(fr = "%s étapes sur %s", en = "%s of %s steps"),
  close_state_done = c(fr = "Fait", en = "Done"),
  close_state_now = c(fr = "À faire", en = "To do"),
  close_state_info = c(fr = "Pour lecture", en = "For reading"),
  close_between = c(fr = "entre %s et %s", en = "between %s and %s"),
  close_year_span = c(fr = "de la période", en = "for the period"),
  close_s_data_lab = c(fr = "Réalisé chargé jusqu'à %s", en = "Actuals loaded up to %s"),
  close_s_data_ok = c(
    fr = "%s mois réalisés, %s. Calcul du %s.",
    en = "%s actual months, %s. Calculated on %s."
  ),
  close_s_data_stale = c(
    fr = "Un fichier a changé depuis le calcul : %s. Actualisez avant de clore.",
    en = "A file changed after the calculation: %s. Refresh before closing."
  ),
  close_s_data_none_lab = c(fr = "Aucun mois réalisé", en = "No actual month"),
  close_s_data_none = c(
    fr = "Le fichier ne contient pas encore de mois réalisé. Rien à clore.",
    en = "The file holds no actual month yet. Nothing to close."
  ),
  close_s_chk_lab = c(fr = "Contrôles des comptes", en = "Account checks"),
  close_s_chk_wait = c(fr = "Contrôles pas encore lus. Actualisez une fois.", en = "Checks not read yet. Refresh once."),
  close_s_chk_open = c(
    fr = "%s points à éclaircir et %s à suivre dans Anomalies.",
    en = "%s points to clear and %s to watch in Anomalies."
  ),
  close_s_chk_watch = c(fr = "Rien à éclaircir. %s points restent à suivre.", en = "Nothing to clear. %s points left to watch."),
  close_s_chk_clear = c(fr = "Aucun point ouvert.", en = "No open point."),
  close_s_res_lab = c(fr = "Résultat de %s", en = "%s result"),
  close_s_res = c(
    fr = "Résultat d'exploitation %s EUR pour un budget de %s EUR, soit %s EUR.",
    en = "Operating income %s EUR against a budget of %s EUR, a gap of %s EUR."
  ),
  close_s_res_nobud = c(fr = "Résultat d'exploitation %s EUR. Pas de budget pour ce mois.", en = "Operating income %s EUR. No budget for this month."),
  close_s_lock_one = c(fr = "Verrouiller %s", en = "Lock %s"),
  close_s_lock_n = c(fr = "Verrouiller %s mois réalisés", en = "Lock %s actual months"),
  close_s_lock_open1 = c(
    fr = "Pas encore verrouillé. Un fichier corrigé peut encore changer ce mois.",
    en = "Not locked yet. A corrected file can still change this month."
  ),
  close_s_lock_openn = c(
    fr = "Mois réalisés pas encore verrouillés, %s. Un fichier corrigé peut encore les changer.",
    en = "Actual months not locked yet, %s. A corrected file can still change them."
  ),
  close_s_lock_done_lab = c(fr = "Mois réalisés verrouillés", en = "Actual months locked"),
  close_s_lock_done = c(
    fr = "Les %s mois réalisés sont verrouillés. Un fichier corrigé ne les change plus.",
    en = "The %s actual months are locked. A corrected file no longer changes them."
  ),
  close_s_lock_drift_lab = c(fr = "Montants verrouillés différents du fichier", en = "Locked amounts differ from the file"),
  close_s_lock_drift = c(
    fr = "Le fichier source donne d'autres montants pour %s mois verrouillés, %s. Le calcul garde le verrou. Choisissez la bonne version.",
    en = "The source file states other amounts for %s locked months, %s. The calculation keeps the lock. Choose the right version."
  ),
  close_s_save_lab = c(fr = "Enregistrer la prévision", en = "Save the forecast"),
  close_s_save_since = c(
    fr = "La prévision a changé depuis la dernière version, du %s. Gardez une version datée de cette clôture.",
    en = "The forecast changed after the last version, saved on %s. Keep a dated version of this close."
  ),
  close_s_save_none = c(fr = "Aucune version enregistrée. Gardez une version datée de cette clôture.", en = "No saved version. Keep a dated version of this close."),
  close_s_save_done_lab = c(fr = "Prévision enregistrée", en = "Forecast saved"),
  close_s_save_done = c(fr = "Version du %s, %s. Elle correspond au calcul affiché.", en = "Version of %s, %s. It matches the calculation on screen."),
  close_s_bud_lab = c(fr = "Figer le budget", en = "Freeze the budget"),
  close_s_bud_none = c(
    fr = "Pas encore figé. Figez-le une fois voté pour garder la référence de l'année.",
    en = "Not frozen yet. Freeze it once approved to keep the year's reference."
  ),
  close_s_bud_ok_lab = c(fr = "Budget de référence intact", en = "Reference budget unchanged"),
  close_s_bud_ok = c(fr = "Figé le %s. Le calcul utilise toujours ce budget.", en = "Frozen on %s. The calculation still uses this budget."),
  close_s_bud_moved_lab = c(fr = "Budget changé depuis le gel", en = "Budget changed since the freeze"),
  close_s_bud_moved = c(
    fr = "Le budget du calcul n'est plus le fichier figé le %s. Chiffre d'affaires de l'année : %s EUR dans le calcul, %s EUR dans le gel. Écart : %s EUR.",
    en = "The calculation budget is no longer the file frozen on %s. Revenue for the year: %s EUR in the calculation, %s EUR in the freeze. Gap: %s EUR."
  ),
  close_btn_sync = c(fr = "Actualiser", en = "Refresh"),
  close_btn_anom = c(fr = "Ouvrir Anomalies", en = "Open Anomalies"),
  close_btn_var = c(fr = "Ouvrir Écarts", en = "Open Variances"),
  close_btn_see = c(fr = "Voir le détail", en = "See the detail"),
  close_btn_lock = c(fr = "Verrouiller", en = "Lock"),
  close_btn_lock_n = c(fr = "Verrouiller les mois réalisés %s", en = "Lock the actual months %s"),
  close_btn_save = c(fr = "Enregistrer", en = "Save"),
  close_btn_freeze = c(fr = "Figer le budget", en = "Freeze the budget"),
  close_btn_refreeze = c(fr = "Figer à nouveau", en = "Freeze again"),
  close_btn_cmp = c(fr = "Comparer", en = "Compare"),
  close_btn_export = c(fr = "Exporter l'écart", en = "Export the gap"),
  close_btn_drift = c(fr = "Reprendre les montants du fichier", en = "Take the file amounts"),
  close_btn_drift_ok = c(fr = "Reprendre et recalculer", en = "Take them and recalculate"),
  close_label_def = c(fr = "Clôture %s", en = "%s close"),
  close_cur = c(fr = "Prévision actuelle", en = "Current forecast"),
  close_cur_unsaved = c(fr = "Prévision actuelle, non enregistrée", en = "Current forecast, not saved"),
  close_cur_note = c(fr = "Non enregistrée", en = "Not saved"),
  close_ref_cur = c(fr = "la prévision actuelle", en = "the current forecast"),
  close_ref_ver = c(fr = "la version du %s", en = "the version of %s"),
  close_copies_short = c(fr = "(+%s copies)", en = "(+%s copies)"),
  close_copies = c(fr = "+%s copies identiques depuis le %s", en = "+%s identical copies since %s"),
  close_copy = c(fr = "+1 copie identique du %s", en = "+1 identical copy from %s"),
  close_copy_short = c(fr = "(+1 copie)", en = "(+1 copy)"),
  close_cmp_need2 = c(
    fr = "Il faut deux versions différentes pour comparer. Enregistrez la prévision à chaque clôture.",
    en = "Two different versions are needed to compare. Save the forecast at each close."
  ),
  close_cmp_pick_two = c(fr = "Choisissez deux versions différentes.", en = "Choose two different versions."),
  close_cmp_none = c(fr = "Pas de mois commun entre les deux versions.", en = "No month in common between the two versions."),
  close_cmp_same = c(
    fr = "Entre %s et %s, aucun montant ne change sur %s.",
    en = "Between %s and %s, no amount changes over %s."
  ),
  close_cmp_fact = c(
    fr = "Entre %s et %s, le résultat d'exploitation %s passe de %s à %s EUR, soit %s EUR.",
    en = "Between %s and %s, operating income for %s moves from %s to %s EUR, a change of %s EUR."
  ),
  close_cmp_top = c(fr = "Plus gros mouvement : %s, %s EUR.", en = "Largest move: %s, %s EUR."),
  close_span_of = c(fr = "des %s mois communs (%s)", en = "for the %s months in common (%s)"),
  close_span_on = c(fr = "les %s mois communs (%s)", en = "the %s months in common (%s)"),
  close_cmp_partial = c(
    fr = "Les deux versions ne couvrent pas les mêmes mois : la comparaison porte sur %s.",
    en = "The two versions do not cover the same months: the comparison covers %s."
  ),
  close_br_from = c(fr = "Départ", en = "Start"),
  close_br_to = c(fr = "Arrivée", en = "End"),
  close_br_cap = c(
    fr = "Résultat d'exploitation %s : %s EUR au départ, %s EUR à l'arrivée. Chaque barre du milieu est l'effet d'une ligne. Une charge en plus fait descendre.",
    en = "Operating income %s: %s EUR at the start, %s EUR at the end. Each middle bar is the effect of one line. A higher cost goes down."
  ),
  close_cmp_cap = c(
    fr = "Effet sur le résultat, en euros, arrivée moins départ. Une charge en plus compte en négatif, donc les lignes d'exploitation s'additionnent jusqu'au résultat d'exploitation.",
    en = "Effect on the result, in euros, end minus start. A higher cost counts negative, so the operating lines add up to operating income."
  ),
  close_hidden_m = c(fr = "Mois sans écart masqués, %s.", en = "Months with no gap hidden, %s."),
  close_hidden_l = c(fr = "Lignes sans écart masquées : %s.", en = "Lines with no gap hidden: %s."),
  close_col_line = c(fr = "Ligne", en = "Line"),
  close_col_month = c(fr = "Mois", en = "Month"),
  close_col_act = c(fr = "réalisé", en = "actual"),
  close_col_fc = c(fr = "prévision", en = "forecast"),
  close_col_total = c(fr = "Total %s", en = "Total %s"),
  close_total = c(fr = "Total", en = "Total"),
  close_export_what = c(
    fr = "Écrit le tableau ci-dessus dans le dossier reports, une ligne par poste et une colonne par mois.",
    en = "Writes the table above to the reports folder, one row per line and one column per month."
  ),
  close_ok_export = c(fr = "Écart écrit dans %s.", en = "Gap written to %s."),
  close_reg_none = c(fr = "Aucune version enregistrée.", en = "No saved version."),
  close_reg_fact = c(
    fr = "%s versions enregistrées, dont %s différentes. Dernière le %s.",
    en = "%s saved versions, %s of them different. Last one on %s."
  ),
  close_reg_unsaved = c(fr = "La prévision actuelle n'est pas enregistrée.", en = "The current forecast is not saved."),
  close_reg_when = c(fr = "Enregistrée le", en = "Saved on"),
  close_reg_label = c(fr = "Libellé", en = "Label"),
  close_reg_rev = c(fr = "Chiffre d'affaires %s", en = "Revenue %s"),
  close_reg_oi = c(fr = "Rés. d'exploitation", en = "Operating income"),
  close_reg_ni = c(fr = "Résultat net", en = "Net income"),
  close_reg_cash = c(fr = "Trésorerie de fin", en = "Closing cash"),
  close_reg_gap = c(fr = "Écart vs précédente", en = "Change vs previous"),
  close_reg_part = c(fr = "%s mois sur %s", en = "%s of %s months"),
  close_reg_is_cur = c(fr = "calcul affiché", en = "calculation on screen"),
  close_reg_cap = c(
    fr = "Montants %s du calcul. L'écart compare le résultat d'exploitation à la version différente juste avant. Comparer ouvre l'écart ligne par ligne avec la prévision actuelle.",
    en = "Amounts %s of the calculation. The change compares operating income with the previous different version. Compare opens the line-by-line gap against the current forecast."
  ),
  close_lock_none = c(fr = "Aucun mois verrouillé.", en = "No locked month."),
  close_lock_fact = c(fr = "%s mois verrouillés, %s.", en = "%s locked months, %s."),
  close_lock_open = c(fr = "%s pas encore verrouillé.", en = "%s not locked yet."),
  close_lock_open_n = c(fr = "%s mois réalisés pas encore verrouillés, %s.", en = "%s actual months not locked yet, %s."),
  close_lock_drift_fact = c(fr = "Le fichier source diffère pour %s mois.", en = "The source file differs for %s months."),
  close_lock_what = c(
    fr = "Ajoute les mois réalisés au verrou. Les mois déjà verrouillés gardent leurs montants.",
    en = "Adds the actual months to the lock. Months already locked keep their amounts."
  ),
  close_lock_grid = c(fr = "Montants verrouillés, 12 derniers mois", en = "Locked amounts, last 12 months"),
  close_lock_grid_calc = c(fr = "Montants verrouillés, mois du calcul", en = "Locked amounts, months of the calculation"),
  close_lock_outside = c(
    fr = "%s mois verrouillés sont hors du calcul actuel, %s. Ils n'ont pas d'effet.",
    en = "%s locked months are outside the current calculation, %s. They have no effect."
  ),
  close_drift_wait = c(fr = "Lecture du fichier source impossible. Le verrou n'a pas été comparé.", en = "The source file could not be read. The lock was not compared."),
  close_drift_ok = c(
    fr = "Le fichier source donne les mêmes montants pour les %s mois verrouillés du calcul.",
    en = "The source file states the same amounts for the %s locked months in the calculation."
  ),
  close_drift_head = c(fr = "%s mois verrouillés ne correspondent plus au fichier.", en = "%s locked months no longer match the file."),
  close_drift_lead = c(
    fr = "Le calcul affiche les montants verrouillés. Si le fichier est la bonne version, reprenez ses montants. Sinon, gardez le verrou.",
    en = "The calculation shows the locked amounts. If the file is the right version, take its amounts. Otherwise keep the lock."
  ),
  close_drift_lines = c(fr = "Lignes", en = "Lines"),
  close_drift_rev_lock = c(fr = "CA verrouillé", en = "Locked revenue"),
  close_drift_rev_file = c(fr = "CA du fichier", en = "File revenue"),
  close_drift_oi_lock = c(fr = "RE verrouillé", en = "Locked operating income"),
  close_drift_oi_file = c(fr = "RE du fichier", en = "File operating income"),
  close_drift_oi_gap = c(fr = "Écart RE", en = "Operating income gap"),
  close_drift_keep = c(
    fr = "Ne rien faire garde le verrou. Les autres mois verrouillés ne bougent pas.",
    en = "Doing nothing keeps the lock. The other locked months do not move."
  ),
  close_drift_warn = c(
    fr = "Les montants du fichier remplacent le verrou pour %s mois, %s. Le résultat d'exploitation de ces mois bouge de %s EUR. Le calcul est relancé.",
    en = "The file amounts replace the lock for %s months, %s. Operating income for these months moves by %s EUR. The calculation runs again."
  ),
  close_rerun = c(fr = "Recalcul en cours.", en = "Recalculating."),
  close_ok_drift = c(fr = "%s montants repris du fichier sur %s mois. Calcul relancé.", en = "%s amounts taken from the file over %s months. Calculation run again."),
  close_ok_lock = c(fr = "Mois verrouillés, %s. %s mois au total dans le verrou.", en = "Months locked, %s. %s months in the lock in total."),
  close_ok_lock_none = c(fr = "Tous les mois réalisés étaient déjà verrouillés.", en = "All actual months were already locked."),
  close_ok_save = c(fr = "Version enregistrée le %s : %s.", en = "Version saved on %s: %s."),
  close_ok_freeze = c(fr = "Budget figé le %s.", en = "Budget frozen on %s."),
  close_freeze_warn = c(
    fr = "Le budget figé le %s sera remplacé par le budget du calcul actuel. L'ancien gel ne sera plus la référence.",
    en = "The budget frozen on %s will be replaced by the budget of the current calculation. The old freeze stops being the reference."
  ),
  close_bud_none = c(fr = "Budget pas encore figé.", en = "Budget not frozen yet."),
  close_bud_what = c(
    fr = "Garde une copie du budget voté. Le rephasage et les fichiers suivants ne la changent pas.",
    en = "Keeps a copy of the approved budget. Rephasing and later files do not change it."
  ),
  close_bud_fact_ok = c(
    fr = "Figé le %s, identique au budget du calcul. Chiffre d'affaires %s EUR, résultat d'exploitation %s EUR sur l'année.",
    en = "Frozen on %s, same as the calculation budget. Revenue %s EUR, operating income %s EUR for the year."
  ),
  close_bud_fact_moved = c(
    fr = "Figé le %s. Le budget du calcul a changé depuis : résultat d'exploitation %s EUR sur l'année.",
    en = "Frozen on %s. The calculation budget changed since: operating income %s EUR for the year."
  ),
  close_bud_frozen = c(fr = "Budget figé", en = "Frozen budget"),
  close_bud_current = c(fr = "Budget du calcul", en = "Calculation budget"),
  close_bud_gap = c(fr = "Écart", en = "Gap"),
  close_bud_cap_ok = c(fr = "Montants de l'année, en euros. Les deux budgets sont identiques.", en = "Amounts for the year, in euros. The two budgets are the same."),
  close_bud_cap_moved = c(
    fr = "Montants de l'année, en euros. L'écart est le budget du calcul moins le budget figé.",
    en = "Amounts for the year, in euros. The gap is the calculation budget minus the frozen budget."
  ),
  close_bud_refreeze_what = c(
    fr = "Remplace la référence par le budget du calcul. À faire seulement après un nouveau vote.",
    en = "Replaces the reference with the calculation budget. Do it only after a new approval."
  ),
  home_files = c(
    fr = "Fichiers de ce calcul : %s.",
    en = "Files in this calculation: %s."
  ),
  home_books_ok = c(
    fr = "Bilan et trésorerie : contrôles cohérents.",
    en = "Balance sheet and cash: checks are consistent."
  ),
  home_books_bad = c(
    fr = "Bilan et trésorerie : écarts à traiter.",
    en = "Balance sheet and cash: gaps to clear."
  ),
  home_books_wait = c(
    fr = "Le contrôle des livres n'a pas tourné.",
    en = "The books check did not run."
  ),
  home_month = c(
    fr = "Ce mois : déposer le fichier du mois clos, relancer le calcul, puis emporter le rapport.",
    en = "This month: drop the closed month's file, run the calculation again, then take the report."
  ),
  home_report = c(
    fr = "Rapport de gestion",
    en = "Management report"
  ),
  scn_lead = c(
    fr = "Deux questions, en langage d'exploitation. Chacune écrit dans les mois encore ouverts. L'attendu reste le point de départ.",
    en = "Two operating questions. Each one writes into the months still open. Expected stays the starting point."
  ),
  scn_occ = c(
    fr = "Occupation −5 points",
    en = "Occupancy −5 points"
  ),
  scn_hire = c(
    fr = "Une personne de plus",
    en = "One more person"
  ),
  scn_capex = c(
    fr = "L'investissement suit le calendrier. Le décaler se fait dans ce fichier, pour que la date reste celle que vous avez écrite.",
    en = "The investment follows the calendar. Moving it is done in that file, so the date stays the one you wrote."
  ),
  scn_apply_lead = c(
    fr = "Calcul dans cette session. Rien n'est enregistré.",
    en = "Calculation in this session. Nothing is saved."
  ),
  scn_apply = c(
    fr = "Calculer ce scénario",
    en = "Run this scenario"
  ),
  scn_reset = c(
    fr = "Remettre les curseurs à zéro",
    en = "Reset the dials to zero"
  ),
  scn_board_title = c(
    fr = "Curseurs",
    en = "Dials"
  ),
  scn_board_do = c(
    fr = "1. Déplacez les curseurs. 2. Calculez. Les cartes et le journal ne bougent qu'au calcul.",
    en = "1. Move the dials. 2. Run. The cards and the journal move only on run."
  ),
  obj_sum = c(
    fr = "Objectif de fin d'année",
    en = "Year-end target"
  ),
  obj_help = c(
    fr = "Choisissez un chiffre de fin d'année et un levier. Le calcul trouve le mouvement requis dans la plage du curseur. Les mois clos ne bougent pas. C'est une condition mathématique, pas une prévision de marché.",
    en = "Pick a year-end figure and one lever. The solve finds the move required within the dial range. Closed months stay locked. This is a mathematical condition, not a market forecast."
  ),
  obj_help_manual = c(
    fr = "Mode manuel : cochez un ou plusieurs leviers. Le calcul trouve le mouvement requis dans la plage des curseurs. Les mois clos ne bougent pas. C'est une condition mathématique, pas une prévision de marché.",
    en = "Manual mode: tick one or more levers. The solve finds the move required within the dial ranges. Closed months stay locked. This is a mathematical condition, not a market forecast."
  ),
  obj_help_auto = c(
    fr = "Mode auto : le moteur propose jusqu'à cinq mixes de leviers qui approchent la cible de fin d'année. Choisissez un mix pour l'ajouter aux scénarios. Les mois clos ne bougent pas.",
    en = "Auto mode: the engine proposes up to five lever mixes that approach the year-end target. Pick a mix to add it to the scenarios. Closed months stay locked."
  ),
  obj_mode = c(fr = "Mode", en = "Mode"),
  obj_mode_manual = c(fr = "Manuel", en = "Manual"),
  obj_mode_auto = c(fr = "Auto", en = "Auto"),
  obj_metric = c(fr = "Chiffre cible", en = "Target figure"),
  obj_metric_cash = c(fr = "Trésorerie de fin d'année", en = "Year-end cash"),
  obj_metric_oi = c(fr = "Résultat d'exploitation de fin d'année", en = "Year-end operating income"),
  obj_metric_oi_fy = c(fr = "Résultat d'exploitation de l'exercice", en = "Operating income for the fiscal year"),
  obj_metric_oi_month = c(fr = "Résultat d'exploitation de %s", en = "Operating income for %s"),
  obj_fy_note = c(
    fr = "L'exercice couvre %s et se clôt en %s. Le résultat de l'exercice est la somme de ces mois ; celui du mois de clôture est un seul mois.",
    en = "The fiscal year covers %s and closes in %s. Fiscal-year income is the sum of those months; close-month income is one month."
  ),
  obj_fy_beyond = c(
    fr = "L'exercice se clôt en %s, après la fin de la prévision (%s). Les chiffres sont lus au dernier mois prévu ; le résultat de l'exercice additionne les mois présents (%s).",
    en = "The fiscal year closes in %s, after the forecast ends (%s). Figures are read at the last forecast month; fiscal-year income adds the months present (%s)."
  ),
  obj_line_sens = c(
    fr = "Ce pourcentage s'applique à toute la ligne sur les mois de prévision ; les charges liées ne suivent pas. C'est une sensibilité, pas un plan de volume et de prix.",
    en = "This percentage applies to the whole line over the forecast months; linked costs do not follow. It is a sensitivity, not a volume and price plan."
  ),
  obj_now_cap = c(fr = "Projection actuelle", en = "Current projection"),
  obj_target = c(fr = "Montant cible (EUR)", en = "Target amount (EUR)"),
  obj_anchor_cash = c(
    fr = "Projection actuelle de trésorerie en %s : %s EUR.",
    en = "Current cash projection in %s: %s EUR."
  ),
  obj_anchor_oi = c(
    fr = "Projection actuelle de résultat d'exploitation en %s : %s EUR.",
    en = "Current operating-income projection in %s: %s EUR."
  ),
  obj_anchor_pct = c(
    fr = "La cible est à %s %% de cette projection.",
    en = "The target sits %s%% from this projection."
  ),
  obj_anchor_span_up = c(
    fr = "De %s EUR à %s EUR, il faut %s EUR de plus.",
    en = "From %s EUR to %s EUR, that is %s EUR more."
  ),
  obj_anchor_span_down = c(
    fr = "De %s EUR à %s EUR, la cible est %s EUR en dessous.",
    en = "From %s EUR to %s EUR, the target is %s EUR below."
  ),
  obj_anchor_level = c(
    fr = "La cible est au niveau de cette projection.",
    en = "The target matches this projection."
  ),
  obj_anchor_pct_undef = c(
    fr = "La projection est nulle : le pourcentage n'est pas défini.",
    en = "The projection is zero, so the percent is undefined."
  ),
  obj_lever = c(fr = "Levier", en = "Lever"),
  obj_levers = c(fr = "Leviers", en = "Levers"),
  obj_levers_all = c(fr = "Tout cocher", en = "Check all"),
  obj_levers_none = c(fr = "Tout décocher", en = "Uncheck all"),
  obj_run = c(fr = "Atteindre l'objectif", en = "Reach the target"),
  obj_run_auto = c(fr = "Proposer des mixes", en = "Propose mixes"),
  obj_busy = c(fr = "Recherche du levier…", en = "Searching the lever…"),
  obj_busy_auto = c(fr = "Recherche de mixes…", en = "Searching mixes…"),
  obj_apply_mix = c(fr = "Ajouter aux scénarios", en = "Add to scenarios"),
  obj_mix_title = c(fr = "Mix %s", en = "Mix %s"),
  obj_mix_none = c(
    fr = "Aucun mix atteignable sur la plage des curseurs pour cette cible.",
    en = "No attainable mix within the dial ranges for this target."
  ),
  obj_mixes_ready = c(
    fr = "%s mixes pour %s en %s. Le plus proche atteint %s EUR (écart %s). Ajoutez un mix aux scénarios pour le poser sur les curseurs.",
    en = "%s mixes for %s in %s. The closest reaches %s EUR (gap %s). Add a mix to the scenarios to set it on the dials."
  ),
  obj_applied = c(
    fr = "Mix ajouté aux scénarios pour %s en %s : %s. Résultat %s EUR (écart %s). Les autres curseurs sont remis à zéro.",
    en = "Mix added to the scenarios for %s in %s: %s. Result %s EUR (gap %s). The other dials are reset to zero."
  ),
  obj_no_lever = c(
    fr = "Cochez au moins un levier.",
    en = "Tick at least one lever."
  ),
  obj_hit = c(
    fr = "Pour %s en %s : %s. Atteint %s EUR (écart %s). Les autres curseurs sont remis à zéro.",
    en = "For %s in %s: %s. Reaches %s EUR (gap %s). The other dials are reset to zero."
  ),
  obj_inert = c(
    fr = "Ces leviers ne déplacent pas ce chiffre, ils restent à zéro : %s.",
    en = "These levers do not move this figure, so they stay at zero: %s."
  ),
  obj_try_lines = c(
    fr = "Les lignes non cochées qui déplacent ce chiffre : %s.",
    en = "The unticked lines that move this figure: %s."
  ),
  obj_closest = c(
    fr = "Dans la plage du curseur, le plus proche pour %s en %s est %s → %s EUR (écart %s). La cible %s EUR n'est pas atteinte.",
    en = "Within the dial range, the closest for %s in %s is %s → %s EUR (gap %s). Target %s EUR is not reached."
  ),
  obj_already = c(
    fr = "La base atteint déjà la cible (%s EUR en %s).",
    en = "The base already meets the target (%s EUR in %s)."
  ),
  obj_infeasible = c(
    fr = "Inatteignable avec ce levier. Sur la plage du curseur, %s en %s va de %s à %s EUR. Cible %s EUR.",
    en = "Unreachable with this lever. Across the dial range, %s in %s goes from %s to %s EUR. Target %s EUR."
  ),
  obj_infeasible_multi = c(
    fr = "Inatteignable avec ces leviers. Sur la plage des curseurs, %s en %s va de %s à %s EUR. Cible %s EUR.",
    en = "Unreachable with these levers. Across the dial ranges, %s in %s goes from %s to %s EUR. Target %s EUR."
  ),
  obj_fail = c(
    fr = "Le calcul n'a pas abouti. Vérifiez les fichiers puis réessayez.",
    en = "The solve did not finish. Check the files and try again."
  ),
  obj_bad_target = c(
    fr = "Indiquez un montant cible en euros.",
    en = "Enter a target amount in euros."
  ),
  out_sum = c(fr = "Trésorerie à 4 semaines", en = "Four-week cash"),
  out_help = c(
    fr = "Saisissez le solde bancaire et les encaissements ou paiements datés. Les quatre semaines partent de cette date. Ceci ne modifie pas la trésorerie du modèle.",
    en = "Enter the bank balance and dated receipts or payments. The four weeks start on that date. This does not change the model's cash."
  ),
  out_asof = c(fr = "Solde au", en = "Balance as of"),
  out_open = c(fr = "Solde bancaire (EUR)", en = "Bank balance (EUR)"),
  out_kind = c(fr = "Sens", en = "Direction"),
  out_in = c(fr = "Encaissement", en = "Receipt"),
  out_out = c(fr = "Paiement", en = "Payment"),
  out_label = c(fr = "Libellé", en = "Label"),
  out_amount = c(fr = "Montant (EUR)", en = "Amount (EUR)"),
  out_when = c(fr = "Date", en = "Date"),
  out_uncertain = c(fr = "Encaissement incertain", en = "Uncertain receipt"),
  out_add = c(fr = "Ajouter", en = "Add"),
  out_delay = c(
    fr = "Décaler les encaissements incertains d'une semaine",
    en = "Delay uncertain receipts by one week"
  ),
  out_remove = c(fr = "Retirer", en = "Remove"),
  out_need = c(
    fr = "Indiquez le solde bancaire et sa date.",
    en = "Enter the bank balance and its date."
  ),
  out_balance_only = c(
    fr = "Un solde bancaire seul ne dit pas le besoin de la semaine prochaine. Ajoutez les encaissements et paiements attendus.",
    en = "A bank balance alone does not say what next week needs. Add the expected receipts and payments."
  ),
  out_partial = c(
    fr = "Aperçu partiel : seules les lignes saisies sont comptées. Mise à jour %s.",
    en = "Partial outlook: only the lines you entered are counted. Updated %s."
  ),
  out_low = c(
    fr = "Solde le plus bas : %s EUR le %s.",
    en = "Lowest balance: %s EUR on %s."
  ),
  out_low_items = c(
    fr = "Lignes de ce jour : %s.",
    en = "Lines on that day: %s."
  ),
  out_low_open = c(
    fr = "Solde le plus bas : %s EUR le %s, avant les mouvements.",
    en = "Lowest balance: %s EUR on %s, before the movements."
  ),
  out_week = c(fr = "S%s", en = "W%s"),
  out_col_week = c(fr = "Semaine", en = "Week"),
  out_col_end = c(fr = "Jusqu'au", en = "Through"),
  out_col_in = c(fr = "Encaissements", en = "Receipts"),
  out_col_out = c(fr = "Paiements", en = "Payments"),
  out_col_bal = c(fr = "Solde", en = "Balance"),
  out_beyond = c(
    fr = "%s ligne(s) tombent après les 4 semaines.",
    en = "%s line(s) fall after the 4 weeks."
  ),
  out_before = c(
    fr = "%s ligne(s) sont antérieures au solde et ne sont pas reprises.",
    en = "%s line(s) are before the balance date and are left out."
  ),
  out_delay_on = c(
    fr = "Les encaissements incertains sont décalés de 7 jours dans ce tableau.",
    en = "Uncertain receipts are shifted by 7 days in this table."
  ),
  out_bad_line = c(
    fr = "Indiquez un libellé, un montant et une date.",
    en = "Enter a label, an amount, and a date."
  ),
  out_journal = c(fr = "Enregistrer au journal", en = "Save to the journal"),
  out_journal_simple_title = c(fr = "Enregistrer le solde", en = "Save the balance"),
  out_journal_simple_note = c(fr = "Commentaire, facultatif", en = "Comment, optional"),
  journal_outlook_line = c(
    fr = "Solde déclaré de %s EUR au %s.",
    en = "Declared balance of %s EUR on %s."
  ),
  journal_outlook_flat = c(
    fr = "Sans mouvement saisi, les quatre semaines restent à ce montant.",
    en = "With no movement entered, the four weeks stay at that amount."
  ),
  out_journal_title = c(fr = "Trésorerie à 4 semaines", en = "Four-week cash"),
  out_journal_amt = c(fr = "Solde le plus bas", en = "Lowest balance"),
  out_journal_amt_open = c(
    fr = "Solde déclaré du compte",
    en = "Declared account balance"
  ),
  out_none = c(fr = "Aucune ligne saisie.", en = "No lines entered."),
  scn_floor_title = c(
    fr = "Plancher de trésorerie",
    en = "Cash floor"
  ),
  scn_floor_fact = c(
    fr = "Ce chiffre sert de seuil : la carte compte les mois où la trésorerie passe en dessous. Le modèle ne change pas la trésorerie qu'il a calculée.",
    en = "This figure is the floor: the card counts the months when cash falls below it. The model does not change the cash it has calculated."
  ),
  scn_floor_moves = c(fr = "Ce qui change", en = "What changes"),
  scn_floor_m1 = c(
    fr = "La carte compte les mois dont la trésorerie de clôture passe sous le montant saisi. À zéro, elle compte les mois sous zéro. Un mois égal au montant n'est pas compté. Le sous-titre affiche Plancher, puis le montant. Dès qu'au moins un mois est compté, le cadre de la carte change.",
    en = "The card counts the months whose closing cash falls under the amount you typed. At zero, it counts the months under zero. A month equal to the amount is not counted. The subtitle shows Floor, then the amount. As soon as at least one month is counted, the card's frame changes."
  ),
  scn_floor_m2 = c(
    fr = "Le même montant sert quand vous appliquez un scénario. La carte Mois sous le seuil affiche alors le compte du scénario. Si vous enregistrez le scénario au journal, la ligne visible reste l'écart de trésorerie. Le montant du plancher et la liste des mois sont conservés avec l'entrée.",
    en = "The same amount is used when you apply a scenario. The Months under the floor card then shows the scenario's count. If you save the scenario to the journal, the visible line stays the cash gap. The floor amount and the list of months are kept with the entry."
  ),
  scn_floor_m3 = c(
    fr = "Le montant est enregistré sur cet ordinateur, dans le fichier fpa-config.local.json. Un redémarrage le conserve. Il n'est pas envoyé au site.",
    en = "The amount is saved on this computer, in the file fpa-config.local.json. A restart keeps it. It is not sent to the site."
  ),
  scn_floor_m4 = c(
    fr = "Après Actualiser, la liste Ce qui a changé, sur Synthèse, peut ajouter une ligne : « Trésorerie sous le plancher en », suivi du premier mois qui passe sous le nouveau montant. Elle l'ajoute quand ce premier mois change, et quand la ligne compte parmi les trois changements les plus notables.",
    en = "After Refresh, the What changed list on Summary can add one line: \"Cash under the floor in\", followed by the first month that falls under the new amount. It adds the line when that first month changes, and when the line is among the three most notable changes."
  ),
  scn_floor_stays = c(fr = "Ce qui ne change pas", en = "What stays"),
  scn_floor_s1 = c(
    fr = "La trésorerie de clôture, le mois le plus bas, la trésorerie de fin d'année, les courbes et les leviers restent sur les chiffres déjà calculés.",
    en = "Closing cash, the lowest month, year-end cash, the curves and the levers stay on the figures already calculated."
  ),
  scn_floor_s2 = c(
    fr = "Synthèse ne lit pas ce montant. Quand elle compte des mois sous zéro, le seuil est zéro.",
    en = "Summary does not read this amount. When it counts months under zero, the line is zero."
  ),
  scn_floor_s3 = c(
    fr = "Le solde bancaire saisi pour les quatre semaines ne bouge pas.",
    en = "The bank balance you enter for the four weeks stays as it is."
  ),
  scn_floor_ex = c(
    fr = "Exemple. 37 099 EUR passent sous un plancher de 40 000 EUR. 37 099 EUR ne passent pas sous un plancher de 0 EUR.",
    en = "Example. 37 099 EUR falls under a floor of 40 000 EUR. 37 099 EUR does not fall under a floor of 0 EUR."
  ),
  scn_floor_label = c(
    fr = "Montant en euros",
    en = "Amount in euros"
  ),
  scn_sens_sum = c(
    fr = "Autre outil : sensibilité isolée",
    en = "Other tool: one-at-a-time sensitivity"
  ),
  scn_sens_help = c(
    fr = "Chaque hypothèse est testée seule, hors journal. Le fichier work/sensitivity.csv est écrit. Ce n'est pas un scénario.",
    en = "Each assumption is tested alone, outside the journal. The file work/sensitivity.csv is written. This is not a scenario."
  ),
  scn_sens_btn = c(
    fr = "Lancer la sensibilité isolée",
    en = "Run one-at-a-time sensitivity"
  ),
  scn_pending_wait = c(
    fr = "Aucun scénario calculé. Les cartes montrent la référence.",
    en = "No scenario calculated yet. The cards show the reference."
  ),
  scn_pending_dirty = c(
    fr = "Les curseurs ont bougé. Les cartes et le journal restent au dernier clic.",
    en = "The sliders have moved. The cards and the journal stay on the last click."
  ),
  scn_pending_armed = c(
    fr = "Les curseurs ont bougé. Les cartes restent sur la référence jusqu'au clic.",
    en = "The sliders have moved. The cards stay on the reference until the click."
  ),
  scn_pending_match = c(
    fr = "Les curseurs sont ceux du dernier clic.",
    en = "The sliders match the last click."
  ),
  scn_log_title = c(
    fr = "Scénarios",
    en = "Scenarios"
  ),
  scn_log_count = c(
    fr = "%s sur 10",
    en = "%s of 10"
  ),
  scn_log_empty = c(
    fr = "Le journal se remplit ici, un clic à la fois, jusqu'à dix scénarios.",
    en = "The journal fills in here, one click at a time, up to ten scenarios."
  ),
  scn_log_cap = c(
    fr = "Dix scénarios au plus. Le suivant retire le plus ancien.",
    en = "Ten scenarios at most. The next one drops the oldest."
  ),
  scn_h_time = c(
    fr = "Heure",
    en = "Time"
  ),
  scn_h_moves = c(
    fr = "Ce qui a bougé",
    en = "What moved"
  ),
  scn_h_cash = c(
    fr = "Trésorerie",
    en = "Cash"
  ),
  scn_h_cash_gap = c(
    fr = "Écart de trésorerie",
    en = "Cash gap"
  ),
  scn_h_oi = c(
    fr = "Résultat d'exploitation",
    en = "Operating income"
  ),
  scn_h_oi_gap = c(
    fr = "Écart de RE",
    en = "OI gap"
  ),
  scn_h_gap = c(fr = "Écart", en = "Gap"),
  scn_h_oi_fy = c(fr = "RE de l'exercice", en = "Fiscal-year OI"),
  scn_h_oi_last = c(fr = "RE de %s", en = "OI for %s"),
  scn_none_moved = c(
    fr = "Aucun curseur déplacé",
    en = "No slider moved"
  ),
  scn_trace_dials = c(
    fr = "Curseurs déplacés",
    en = "Sliders moved"
  ),
  scn_lab_dial_gap = c(fr = "écart de ligne", en = "line gap"),
  scn_lab_dial_after = c(fr = "après", en = "after"),
  scn_lab_dial_before = c(fr = "référence", en = "reference"),
  scn_chart = c(
    fr = "Chaque barre est l'écart de trésorerie du scénario moins la référence.",
    en = "Each bar is the cash gap of the scenario minus the reference."
  ),
  scn_kpi_applied = c(
    fr = "Dernier clic",
    en = "Last click"
  ),
  scn_kpi_waiting = c(
    fr = "Pas encore de scénario",
    en = "No scenario yet"
  ),
  scn_kpi_hint = c(
    fr = "Cliquez un carton pour la piste du chiffre et les fichiers lus.",
    en = "Click a card for the trail of the figure and the files read."
  ),
  scn_kpi_open = c(fr = "Ouverture", en = "Opening"),
  scn_kpi_open_s = c(fr = "Premier mois lu", en = "First month read"),
  scn_kpi_ref = c(fr = "Clôture de référence", en = "Reference closing"),
  scn_kpi_ref_s = c(fr = "Sans déplacement", en = "No move"),
  scn_kpi_scen = c(fr = "Clôture du scénario", en = "Scenario closing"),
  scn_kpi_gap = c(fr = "Écart", en = "Gap"),
  scn_kpi_gap_s = c(fr = "Scénario moins référence", en = "Scenario minus reference"),
  scn_kpi_low = c(fr = "Mois le plus bas", en = "Lowest month"),
  scn_kpi_low_month = c(fr = "Mois", en = "Month"),
  scn_kpi_under = c(fr = "Mois sous le seuil", en = "Months under the floor"),
  scn_kpi_months = c(fr = "Mois du parcours", en = "Months in the path"),
  scn_kpi_floor = c(fr = "Plancher", en = "Floor"),
  scn_kpi_oi = c(fr = "Résultat d'exploitation", en = "Operating income"),
  scn_kpi_oi_ref = c(fr = "Référence", en = "Reference"),
  scn_kpi_first_month = c(fr = "Premier mois", en = "First month"),
  scn_kpi_no_calc = c(
    fr = "Pas de calcul à détailler ici.",
    en = "No calculation to show here."
  ),
  scn_th_file = c(fr = "Fichier", en = "File"),
  scn_th_role = c(fr = "Rôle", en = "Role"),
  scn_web_title = c(
    fr = "Toiles",
    en = "Webs"
  ),
  scn_web_note = c(
    fr = "Chaque toile est un scénario. L'anneau du milieu est sans effet sur la trésorerie. Au-delà, le curseur l'améliore. En deçà, il la dégrade.",
    en = "Each web is one scenario. The middle ring has no effect on cash. Beyond it, the slider improves cash. Inside it, the slider makes cash worse."
  ),
  scn_trace_hint = c(
    fr = "Cliquez une barre, une ligne ou une toile pour la piste du chiffre.",
    en = "Click a bar, a row, or a web for the trail of the figure."
  ),
  scn_now = c(
    fr = "Scénario sur les curseurs",
    en = "Scenario on the dials"
  ),
  scn_trace_gap = c(
    fr = "Écart de trésorerie = trésorerie du scénario − trésorerie de référence.",
    en = "Cash gap = scenario cash − reference cash."
  ),
  scn_trace_oi = c(
    fr = "Écart de résultat d'exploitation = résultat du scénario − résultat de référence.",
    en = "Operating-income gap = scenario income − reference income."
  ),
  scn_trace_bridge = c(
    fr = "Clôture du dernier mois = ouverture du mois + résultat net + dotations + effet de BFR + investissements + dette nette.",
    en = "Closing cash of the last month = opening cash of the month + net income + depreciation + working-capital effect + capital expenditure + net debt."
  ),
  scn_lab_gap = c(fr = "écart de trésorerie", en = "cash gap"),
  scn_lab_pass_cash = c(fr = "trésorerie du scénario", en = "scenario cash"),
  scn_lab_ref_cash = c(fr = "trésorerie de référence", en = "reference cash"),
  scn_lab_oi_gap = c(fr = "écart de RE", en = "OI gap"),
  scn_lab_pass_oi = c(fr = "résultat du scénario", en = "scenario income"),
  scn_lab_ref_oi = c(fr = "résultat de référence", en = "reference income"),
  scn_lab_close = c(fr = "Clôture du dernier mois", en = "Closing of the last month"),
  scn_lab_open = c(fr = "ouverture du mois", en = "opening of the month"),
  scn_lab_ni = c(fr = "résultat net", en = "net income"),
  scn_lab_da = c(fr = "dotations", en = "depreciation"),
  scn_lab_wc = c(fr = "effet de BFR", en = "working-capital effect"),
  scn_lab_capex = c(fr = "investissements", en = "capital expenditure"),
  scn_lab_debt = c(fr = "dette nette", en = "net debt"),
  scn_lab_ref_close = c(fr = "Trésorerie de référence", en = "Reference cash"),
  scn_trace_files = c(
    fr = "Fichiers lus pour le calcul",
    en = "Files read for the calculation"
  ),
  scn_trace_span = c(
    fr = "%s à %s",
    en = "%s to %s"
  ),
  scn_close = c(
    fr = "Fermer",
    en = "Close"
  ),
  stmt_table = c(fr = "Tableau", en = "Table"),
  stmt_waterfall = c(fr = "Cascade", en = "Waterfall"),
  stmt_treemap = c(fr = "Carte", en = "Treemap"),
  stmt_abs = c(fr = "Montant", en = "Amount"),
  stmt_pct = c(fr = "% du total", en = "% of total"),
  stmt_pop = c(fr = "Écart au mois", en = "Month on month"),
  stmt_opfin_show = c(fr = "Tags Op/Fin", en = "Op/Fin tags"),
  stmt_opfin_all = c(fr = "Toutes les lignes", en = "All lines"),
  stmt_opfin_only = c(fr = "Op/Fin seulement", en = "Op/Fin only"),
  stmt_opfin_note = c(
    fr = "Op = exploitation, Fin = financement. Les autres lignes restent hors du filtre Op/Fin.",
    en = "Op = operating, Fin = financing. Other lines stay outside the Op/Fin filter."
  ),
  stmt_period = c(fr = "Mois", en = "Month"),
  stmt_heat_note = c(
    fr = "La teinte suit la taille du chiffre sur tout le tableau. Le pavé porte le montant. Le fond ambre d'un mois est une prévision.",
    en = "The tint follows the size of the figure across the whole table. The chip carries the amount. An amber month is a forecast."
  ),
  stmt_pct_pnl = c(
    fr = "Chaque case est le montant divisé par le chiffre d'affaires du mois.",
    en = "Each cell is the amount divided by that month's revenue."
  ),
  stmt_pct_bs = c(
    fr = "Chaque case est le poste divisé par le total de l'actif du mois.",
    en = "Each cell is the line divided by that month's total assets."
  ),
  stmt_pct_cash = c(
    fr = "Chaque case est le flux divisé par la trésorerie de clôture du mois.",
    en = "Each cell is the flow divided by that month's closing cash."
  ),
  stmt_pop_note = c(
    fr = "Chaque case est le mois moins le mois précédent.",
    en = "Each cell is the month minus the previous month."
  ),
  stmt_wf_note = c(
    fr = "La cascade part du chiffre d'affaires et arrive au résultat net. Les barres pleines sont des soldes.",
    en = "The waterfall starts at revenue and ends at net income. The full bars are balances."
  ),
  stmt_wf_one = c(fr = "Un mois", en = "One month"),
  stmt_wf_range = c(fr = "Une période", en = "A span"),
  stmt_wf_from = c(fr = "De", en = "From"),
  stmt_wf_to = c(fr = "à", en = "to"),
  stmt_wf_fifo_title = c(
    fr = "Glissements capturés (5 derniers)",
    en = "Captured shifts (last 5)"
  ),
  stmt_wf_fifo_line = c(
    fr = "Période %s vs %s, plus grands écarts en part du chiffre d'affaires : %s.",
    en = "Period %s vs %s, biggest shifts as a share of revenue: %s."
  ),
  stmt_wf_fifo_empty = c(
    fr = "La lecture live reste ici jusqu'à Capturer. Ensuite elle entre dans cette liste (5 max).",
    en = "The live reading stays above until you Capture. Then it joins this list (5 max)."
  ),
  stmt_wf_fifo_pt = c(fr = "pts du CA", en = "pp of revenue"),
  stmt_wf_fifo_reset = c(fr = "Tout effacer", en = "Clear all"),
  stmt_wf_fifo_del = c(fr = "Retirer cette ligne", en = "Remove this row"),
  stmt_wf_compare = c(fr = "Comparer", en = "Compare"),
  stmt_wf_compare_hide = c(fr = "Masquer", en = "Hide"),
  stmt_wf_capture = c(fr = "Capturer", en = "Capture"),
  stmt_wf_live_title = c(
    fr = "Lecture en part du chiffre d'affaires",
    en = "Reading as a share of revenue"
  ),
  stmt_wf_live_empty = c(
    fr = "Déplacez une des deux périodes : la lecture se met à jour ici. Capturer l'envoie dans la liste.",
    en = "Move either period: the reading updates here. Capture sends it to the list."
  ),
  stmt_wf_range_a = c(fr = "Période A", en = "Period A"),
  stmt_wf_range_b = c(fr = "Période B", en = "Period B"),
  stmt_wf_year = c(fr = "Année", en = "Year"),
  stmt_wf_year_none = c(
    fr = "Aucun mois de %s dans cette plage.",
    en = "No %s months in this span."
  ),
  stmt_wf_year_miss = c(
    fr = "Il manque %s mois.",
    en = "%s month(s) missing."
  ),
  stmt_tm_note = c(
    fr = "La surface suit la taille du poste. Actif d'un côté, passif et capitaux de l'autre.",
    en = "Area follows the size of the line. Assets on one side, liabilities and equity on the other."
  ),
  bs_cmp_a = c(fr = "Période A", en = "Period A"),
  bs_cmp_b = c(fr = "Période B", en = "Period B"),
  bs_map_order_date = c(fr = "Par date", en = "By date"),
  bs_map_order_side = c(fr = "Actifs et passifs groupés", en = "Assets and claims grouped"),
  bs_cmp_asset = c(fr = "Actif", en = "Assets"),
  bs_cmp_claim = c(fr = "Passif et capitaux", en = "Liabilities and equity"),
  bs_cmp_lead = c(
    fr = "De %s à %s, l'actif passe de %s EUR à %s EUR (%s). Le passif et les capitaux passent de %s EUR à %s EUR (%s).",
    en = "From %s to %s, assets move from %s EUR to %s EUR (%s). Liabilities and equity move from %s EUR to %s EUR (%s)."
  ),
  bs_cmp_th_item = c(fr = "Poste", en = "Line"),
  bs_cmp_th_side = c(fr = "Côté", en = "Side"),
  bs_cmp_th_delta = c(fr = "Écart", en = "Change"),
  bs_cmp_th_pct = c(fr = "Variation", en = "Change %"),
  bs_cmp_th_move = c(fr = "Sens", en = "Direction"),
  bs_cmp_th_share = c(fr = "Part", en = "Share"),
  bs_cmp_th_pts = c(fr = "Écart de part", en = "Share change"),
  bs_cmp_up = c(fr = "en hausse", en = "up"),
  bs_cmp_down = c(fr = "en baisse", en = "down"),
  bs_cmp_new = c(fr = "apparu", en = "new"),
  bs_cmp_gone = c(fr = "disparu", en = "gone"),
  bs_cmp_round = c(fr = "Arrondi", en = "Rounding"),
  bs_cmp_total_asset = c(fr = "Total actif", en = "Total assets"),
  bs_cmp_total_claim = c(fr = "Total passif et capitaux", en = "Total liabilities and equity"),
  bs_cmp_top = c(
    fr = "Les trois plus grands écarts",
    en = "The three largest changes"
  ),
  bs_map_minor = c(
    fr = "Les postes trop petits pour une étiquette sont écrits sous la carte. Survolez un rectangle, ou cliquez-le, pour lire le montant.",
    en = "Lines too small for a label are written under the map. Hover a block, or click it, to read the amount."
  ),
  bs_map_note = c(
    fr = "Actif toujours à gauche, passif et capitaux toujours à droite. La surface suit le montant. La part est la place du poste dans son côté. Le tableau mesure l'écart de la période A à la période B.",
    en = "Assets stay on the left, liabilities and equity stay on the right. Area follows the amount. The share is the line's place in its own side. The table measures the change from period A to period B."
  ),
  journal_tab = c(fr = "Journal", en = "Journal"),
  journal_lead = c(
    fr = "Ce qui a été remarqué, testé ou décidé. Les chiffres sont figés à l'enregistrement.",
    en = "What was noticed, tested, or decided. Figures are frozen at save time."
  ),
  journal_empty = c(
    fr = "Aucune entrée. Enregistrez depuis Anomalies, Synthèse, Cascade ou Impact.",
    en = "No entries. Save from Anomalies, Summary, Cascade, or Impact."
  ),
  journal_filter = c(fr = "Filtrer", en = "Filter"),
  journal_filter_all = c(fr = "Tout", en = "All"),
  journal_filter_fav = c(fr = "Favoris", en = "Favorites"),
  journal_sort = c(fr = "Trier", en = "Sort"),
  journal_sort_newest = c(fr = "Plus récent", en = "Newest"),
  journal_sort_oldest = c(fr = "Plus ancien", en = "Oldest"),
  journal_sort_fav = c(fr = "Favoris d'abord", en = "Favorites first"),
  journal_sort_amt_desc = c(fr = "Montant décroissant", en = "Amount high first"),
  journal_sort_amt_asc = c(fr = "Montant croissant", en = "Amount low first"),
  journal_type_observation = c(fr = "Observation", en = "Observation"),
  journal_type_scenario = c(fr = "Scénario exploré", en = "Explored scenario"),
  journal_type_decision = c(fr = "Décision", en = "Decision"),
  journal_due_title = c(fr = "Décisions à revoir", en = "Decisions to review"),
  header_todo = c(fr = "À faire", en = "To do"),
  header_todo_none = c(fr = "Rien en attente.", en = "Nothing waiting."),
  header_todo_lead = c(
    fr = "Cette liste s'ouvre avec le bouton À faire.",
    en = "This list opens from the To do button."
  ),
  header_todo_anom = c(fr = "Anomalies à éclaircir", en = "Anomalies to clear up"),
  header_todo_anom_line = c(
    fr = "%s, %s. À lire dans Anomalies.",
    en = "%s, %s. Read it in Anomalies."
  ),
  header_todo_anom_title = c(fr = "%s · %s", en = "%s · %s"),
  header_todo_anom_ratio = c(
    fr = "Liquidité immédiate %s x. Trésorerie %s EUR, dettes à court terme %s EUR. Ouvre la carte %s, ce mois.",
    en = "Cash ratio %s x. Cash %s EUR, current liabilities %s EUR. Opens the %s card, this month."
  ),
  header_todo_anom_books = c(
    fr = "Actif %s EUR, dettes et capitaux propres %s EUR, écart %s EUR. Ouvre la carte %s, ce mois.",
    en = "Assets %s EUR, liabilities and equity %s EUR, gap %s EUR. Opens the %s card, this month."
  ),
  header_todo_anom_cash = c(
    fr = "Clôture %s EUR, ouverture plus le flux %s EUR, écart %s EUR. Ouvre la carte %s, ce mois.",
    en = "Closing %s EUR, opening plus the flow %s EUR, gap %s EUR. Opens the %s card, this month."
  ),
  anom_ratio_same_card = c(
    fr = "%s mois, une seule carte. %s",
    en = "%s months, one card. %s"
  ),
  header_todo_map = c(fr = "Questions de cartographie", en = "Map questions"),
  todo_col_life = c(
    fr = "La colonne %s est dans le fichier, mais la carte ne lui donne pas de rôle. Le calendrier la lit quand même : c'est le nombre de mois pendant lesquels un achat est amorti. Une case vide, ou un zéro, sur un tirage ou un remboursement ne retire pas cette ligne. Cochez-la si cette lecture vous convient.",
    en = "Column %s is in the file, but the map gives it no role. The calendar still reads it as the number of months a purchase is depreciated. A blank cell, or a zero, on a draw or a repayment does not remove this row. Check it if that reading is the one you want."
  ),
  todo_cols_life = c(
    fr = "Les colonnes %s sont dans le fichier, mais la carte ne leur donne pas de rôle. Ces noms sont lus comme le nombre de mois pendant lesquels un achat est amorti. Remplir les cases, même avec des zéros, ne retire pas cette ligne. Cochez-la si cette lecture vous convient.",
    en = "Columns %s are in the file, but the map gives them no role. These names are read as the number of months a purchase is depreciated. Filling the cells, even with zeros, does not remove this row. Check it if that reading is the one you want."
  ),
  todo_col_norole = c(
    fr = "La colonne %s est dans le fichier, mais la carte ne dit pas à quoi elle sert. Le calcul ne la lit pas. Remplir les cases ne retire pas cette ligne. Ouvrez le fichier pour voir ce qu'elle contient, puis cochez la ligne si vous la laissez ainsi.",
    en = "Column %s is in the file, but the map does not say what it is for. The calculation does not read it. Filling the cells does not remove this row. Open the file to see what it holds, then check the row if you leave it as it is."
  ),
  todo_cols_norole = c(
    fr = "Les colonnes %s sont dans le fichier, mais la carte ne dit pas à quoi elles servent. Le calcul ne les lit pas. Remplir les cases ne retire pas cette ligne. Ouvrez le fichier pour les voir, puis cochez la ligne si vous les laissez ainsi.",
    en = "Columns %s are in the file, but the map does not say what they are for. The calculation does not read them. Filling the cells does not remove this row. Open the file to see them, then check the row if you leave them as they are."
  ),
  todo_file_unused = c(
    fr = "Le fichier %s est bien sur le disque. Aucun calcul activé ne s'en sert : le compte de résultat, le bilan et la trésorerie ne changent pas si vous le modifiez. Ce n'est pas un fichier manquant. Cochez la ligne si vous le gardez seulement en réserve.",
    en = "The file %s is on disk. No switched-on calculation reads it: the income statement, the balance sheet and cash do not change if you edit it. It is not a missing file. Check the row if you are only keeping it in reserve."
  ),
  todo_cleared_scan = c(
    fr = "Retiré le %s, après actualisation : la ligne n'était plus valable.",
    en = "Removed on %s, after refresh: the row was no longer valid."
  ),
  header_todo_sum = c(
    fr = "%s en attente : %s décision(s), %s fichier(s), %s anomalie(s), %s question(s) de carte.",
    en = "%s waiting: %s decision(s), %s file(s), %s anomaly(ies), %s map question(s)."
  ),
  todo_clear_title = c(fr = "Retirer de À faire", en = "Remove from To do"),
  todo_cleared_head = c(fr = "Retirés de À faire", en = "Removed from To do"),
  todo_cleared_lead = c(
    fr = "Lignes retirées de À faire, à la main ou après actualisation. Ce n'est pas une entrée du journal.",
    en = "Rows removed from To do, by hand or after a refresh. This is not a journal entry."
  ),
  todo_cleared_empty = c(
    fr = "Aucune ligne retirée pour le moment.",
    en = "No row removed yet."
  ),
  todo_cleared_journal = c(fr = "Enregistré au journal le %s.", en = "Saved to the journal on %s."),
  todo_cleared_check = c(fr = "Coché le %s.", en = "Checked on %s."),
  todo_restore = c(fr = "Remettre dans À faire", en = "Put back on To do"),
  journal_amt_cash = c(fr = "Trésorerie", en = "Cash"),
  journal_fu_kicker = c(fr = "Suivi", en = "Follow-up"),
  journal_fu_actual = c(
    fr = "Réalisé en %s : %s EUR. Le journal avait figé %s EUR (écart %s).",
    en = "Actual in %s: %s EUR. The journal froze %s EUR (gap %s)."
  ),
  journal_fu_open = c(
    fr = "Le mois %s n'est pas encore réalisé. Chiffre figé : %s EUR.",
    en = "Month %s is not actual yet. Frozen figure: %s EUR."
  ),
  journal_fu_wait = c(
    fr = "Revoir le %s. Chiffre figé : %s EUR.",
    en = "Review on %s. Frozen figure: %s EUR."
  ),
  journal_report_head = c(fr = "Décisions retenues", en = "Selected decisions"),
  journal_report_none = c(
    fr = "Aucune décision étoilée pour ce rapport.",
    en = "No starred decision for this report."
  ),
  journal_save = c(fr = "Enregistrer au journal", en = "Save to journal"),
  journal_save_scenario = c(fr = "Enregistrer ce scénario au journal", en = "Save this scenario to the journal"),
  journal_to_journal = c(fr = "Enregistrer au journal", en = "Save to journal"),
  journal_note_label = c(fr = "Votre commentaire", en = "Your note"),
  journal_note_ph = c(
    fr = "Pourquoi cela compte, ou ce que la direction a décidé…",
    en = "Why it matters, or what management decided…"
  ),
  journal_type_label = c(fr = "Type", en = "Type"),
  journal_review = c(fr = "Date de revue (optionnel)", en = "Review date (optional)"),
  journal_owner = c(fr = "Responsable (optionnel)", en = "Owner (optional)"),
  journal_due = c(fr = "Échéance (optionnel)", en = "Due date (optional)"),
  journal_state = c(fr = "État", en = "State"),
  journal_state_open = c(fr = "Ouvert", en = "Open"),
  journal_state_watching = c(fr = "En suivi", en = "Watching"),
  journal_state_done = c(fr = "Clos", en = "Done"),
  journal_confirm = c(fr = "Enregistrer", en = "Save"),
  journal_cancel = c(fr = "Annuler", en = "Cancel"),
  journal_saved = c(fr = "Entrée enregistrée dans le journal.", en = "Entry saved to the journal."),
  journal_star = c(fr = "Favori", en = "Favorite"),
  journal_unstar = c(fr = "Retirer des favoris", en = "Remove from favorites"),
  journal_open = c(fr = "Ouvrir l'écran", en = "Open the screen"),
  journal_withdraw = c(fr = "Ne plus retenir", en = "Withdraw"),
  journal_restore_keep = c(fr = "Retenir à nouveau", en = "Keep again"),
  journal_withdrawn_badge = c(
    fr = "Retiré du solde déclaré",
    en = "Withdrawn from the declared balance"
  ),
  journal_withdrawn_note = c(
    fr = "Cette entrée reste dans le journal. Elle ne nourrit plus le solde déclaré, la synthèse, ni À faire.",
    en = "This entry stays in the journal. It no longer feeds the declared balance, the summary, or To do."
  ),
  journal_context = c(fr = "Contexte", en = "Context"),
  journal_when = c(fr = "Enregistré le", en = "Saved on"),
  journal_from = c(fr = "Depuis", en = "From"),
  journal_amount = c(fr = "Montant", en = "Amount"),
  journal_snapshot = c(fr = "Chiffres figés", en = "Frozen figures"),
  journal_title_scenario = c(fr = "Scénario Impact", en = "Impact scenario"),
  journal_amt_cash_gap = c(fr = "Écart de trésorerie", en = "Cash gap"),
  journal_amt_gap = c(fr = "Écart", en = "Gap"),
  journal_auto_stamp = c(
    fr = "%s · %s · période %s",
    en = "%s · %s · period %s"
  ),
  journal_count = c(fr = "%s entrée(s)", en = "%s entr(y/ies)"),

  drv_hero_title = c(fr = "Drivers : ce qui fait bouger vos chiffres", en = "Drivers: what moves your numbers"),
  drv_hero_intro = c(
    fr = "Un driver est une série mensuelle tirée de vos fichiers : des nuitées vendues, un tarif, un effectif, une température. Cette page dit lesquelles la prévision utilise et ce que l'historique permet d'en dire, puis vous demande un avis sur chacune. Les mots soulignés en pointillé s'expliquent au survol de la souris.",
    en = "A driver is a monthly series taken from your files: nights sold, a rate, a headcount, a temperature. This page says which ones the forecast uses and what the history can tell about them, then asks for your view on each. Words underlined with dots explain themselves when the mouse hovers over them."
  ),
  drv_tile_mech = c(fr = "Dans un calcul", en = "Part of a calculation"),
  drv_tile_mech_sub = c(
    fr = "Ces séries composent une ligne par une formule vérifiée sur les comptes. La prévision s'en sert directement.",
    en = "These series make up a line through a formula checked against the books. The forecast uses them directly."
  ),
  drv_tile_conf = c(fr = "Retenues par la mesure", en = "Kept on the evidence"),
  drv_tile_conf_sub = c(
    fr = "Aucun calcul ne les relie à une ligne, mais l'historique montre un lien solide. Elles entrent dans la prévision.",
    en = "No calculation links them to a line, but the history shows a solid link. They enter the forecast."
  ),
  drv_tile_watch = c(fr = "À surveiller", en = "To watch"),
  drv_tile_watch_sub = c(
    fr = "Ces séries sont mesurées et classées, sans preuve suffisante pour être retenues. On les garde à l'œil.",
    en = "These series are measured and ranked, without enough proof to be kept. They stay under watch."
  ),
  drv_tile_none = c(fr = "Aucune série pour le moment.", en = "No series for now."),
  drv_gloss_btn = c(fr = "Les mots de cette page", en = "The words on this page"),
  drv_gloss_lead = c(
    fr = "Chaque mot souligné en pointillé, partout dans cet onglet, affiche sa définition au survol de la souris. La liste complète est ici.",
    en = "Every word underlined with dots, anywhere in this tab, shows its definition when the mouse hovers over it. The full list is here."
  ),
  drv_work_title = c(fr = "Votre avis, série par série", en = "Your view, series by series"),
  drv_formula_fold = c(fr = "Formules déclarées · %s vérifiées sur %s", en = "Declared formulas · %s proven out of %s"),
  drv_cap_more = c(fr = "Afficher les %s autres lignes", en = "Show the other %s rows"),
  drv_cap_less = c(fr = "Réduire la liste", en = "Shorten the list"),
  drv_tab_fiche = c(fr = "Fiche de la série", en = "Series card"),
  drv_tab_map = c(fr = "Carte des liens", en = "Link map"),
  drv_tab_tests = c(fr = "Tests, toutes séries", en = "Tests, all series"),
  drv_tab_lag = c(fr = "Décalage", en = "Lag"),
  drv_tab_align = c(fr = "Alignement", en = "Alignment"),
  drv_tab_raw = c(fr = "Brut et résidus", en = "Raw and residuals"),
  drv_tab_hold = c(fr = "Contrôle sur 12 mois", en = "12-month check"),
  drv_tab_review = c(fr = "Tableau technique", en = "Technical table"),
  drv_fiche_tests = c(fr = "Ce que disent les trois tests", en = "What the three tests say"),
  drv_fiche_detail = c(fr = "Les deux comparaisons en détail", en = "The two comparisons in detail"),
  drv_fiche_status = c(fr = "Votre avis", en = "Your view"),
  drv_h_saved = c(fr = "Avis enregistré", en = "View saved"),
  drv_tests_lead = c(
    fr = "Une ligne par série et une colonne par test. Les deux tableaux suivants donnent les chiffres derrière chaque réponse.",
    en = "One row per series and one column per test. The two tables below give the figures behind each answer."
  ),
  drv_method_fold = c(fr = "Comment ce test est fait", en = "How this test works"),
  drv_rec_head = c(fr = "Recommandation du moteur.", en = "Engine recommendation."),
  drv_rec_tested = c(fr = "Le moteur a essayé des décalages de %s à +%s mois.", en = "The engine tried lags from %s to +%s months."),
  drv_rec_noprof = c(fr = "Le moteur n'a pas de profil de décalage pour cette série.", en = "The engine has no lag profile for this series."),
  drv_rec_none = c(fr = "Il ne retient aucun décalage.", en = "It keeps no lag."),
  drv_rec_better = c(
    fr = "Il retient %s mois, où la corrélation vaut %s, contre %s sur le même mois.",
    en = "It keeps %s months, where the correlation is %s, against %s on the same month."
  ),
  drv_rec_same = c(
    fr = "Il retient %s mois, avec une corrélation de %s. Décaler la série n'améliore pas le lien.",
    en = "It keeps %s months, with a correlation of %s. Shifting the series does not improve the link."
  ),
  drv_rec_plain = c(fr = "Il retient %s mois.", en = "It keeps %s months."),
  drv_rec_verdict = c(fr = "Verdict proposé : %s.", en = "Proposed verdict: %s."),
  drv_rec_late = c(
    fr = "Ce décalage arrive trop tard pour servir à prévoir. À +%s mois, le lien reste au-dessus du seuil (corrélation %s) et laisse %s jours pour anticiper.",
    en = "This lag comes too late to help forecast. At +%s months, the link stays above the bar (correlation %s) and leaves %s days to anticipate."
  ),
  drv_confirm_verdict = c(fr = "Votre verdict", en = "Your verdict"),
  drv_confirm_lag = c(fr = "Décalage retenu", en = "Lag kept"),
  drv_confirm_save = c(fr = "Enregistrer mon avis", en = "Save my view"),
  drv_lag_none = c(fr = "Aucun décalage retenu", en = "No lag kept"),
  drv_lag_same = c(fr = "0 mois, le même mois", en = "0 months, the same month"),
  drv_lag_horizon = c(fr = "%s jours pour anticiper", en = "%s days to anticipate"),
  drv_lag_explain = c(fr = "sert à expliquer, pas à prévoir", en = "explains, does not forecast"),
  drv_lag_refused = c(fr = "refusé : la série suit la ligne", en = "refused: the series follows the line"),
  drv_map_intro = c(
    fr = "Cet onglet montre les séries ensemble. Le schéma relie chaque série à la ligne du compte de résultat qu'elle alimente, puis au résultat d'exploitation. Les deux volets fermés plus bas classent les séries et posent trois questions à chacune. Cliquez une série pour ouvrir sa fiche.",
    en = "This tab shows the series together. The diagram links each series to the income-statement line it feeds, then to operating income. The two closed panels below rank the series and ask each one three questions. Click a series to open its card."
  ),
  drv_map_howto = c(fr = "Comment lire ce schéma", en = "How to read this diagram"),
  drv_map_fold_links = c(fr = "Liens vers le compte de résultat", en = "Links to the income statement"),
  drv_map_fold_read = c(fr = "Trois questions par série · %s séries", en = "Three questions per series · %s series"),
  drv_map_fold_tiers = c(fr = "Les trois familles de séries", en = "The three families of series"),
  drv_map_signals = c(fr = "Signaux présents dans ce fichier", en = "Signals in this file"),
  drv_map_click = c(fr = "Cliquez une carte ou une ligne.", en = "Click a card or a line."),
  drv_map_clear = c(fr = "Retirer la surbrillance", en = "Clear the highlight"),
  drv_map_kept = c(fr = "Séries retenues · %s", en = "Series kept · %s"),
  drv_map_kept_note = c(
    fr = "La veille passe au second plan. Mécanisme et confirmé restent nets.",
    en = "Watch series fade. Mechanism and confirmed stay sharp."
  ),
  drv_map_table_lead = c(
    fr = "Le tableau reprend chaque trait du schéma avec ses chiffres.",
    en = "The table repeats each edge of the diagram with its figures."
  ),
  drv_read_series_sub = c(
    fr = "Une ligne par série, de la mieux soutenue à la moins soutenue.",
    en = "One row per series, from the best supported to the least."
  ),
  drv_tier_mech_sub = c(
    fr = "La série entre dans le calcul d'une ligne, par une formule vérifiée sur les comptes. Sa note reste la confiance déclarée au départ.",
    en = "The series is part of a line's calculation, through a formula checked against the books. Its mark stays the confidence declared at setup."
  ),
  drv_tier_conf_sub = c(
    fr = "Le lien est mesuré et retenu. Le moteur garde au plus une série de ce type pour sept mois d'historique, pour ne pas suivre le hasard.",
    en = "The link is measured and kept. The engine keeps at most one such series per seven months of history, so as not to follow chance."
  ),
  drv_tier_watch_sub = c(
    fr = "La série est mesurée et classée, mais elle n'est pas retenue comme preuve pour la prévision.",
    en = "The series is measured and ranked, but it is not kept as proof for the forecast."
  ),
  drv_tier_empty = c(fr = "Aucune série.", en = "No series."),
  drv_flag_identite_t = c(fr = "Dans un calcul", en = "Part of a calculation"),
  drv_flag_identite = c(
    fr = "Certaines séries entrent dans le calcul de leur ligne. Leur corrélation avec cette ligne est donc forte par construction. Le moteur ne la compte pas comme une preuve et garde la confiance déclarée au départ.",
    en = "Some series are part of their line's calculation. Their correlation with that line is therefore strong by construction. The engine does not count it as proof and keeps the confidence declared at setup."
  ),
  drv_flag_late_t = c(fr = "Arrive tard", en = "Comes late"),
  drv_flag_late = c(
    fr = "Certaines séries bougent après leur ligne au lieu de la précéder. Elles aident à expliquer un mois passé, mais pas à annoncer le suivant.",
    en = "Some series move after their line instead of before it. They help explain a past month, but not announce the next one."
  ),
  drv_flag_double_t = c(fr = "Doublon", en = "Duplicate"),
  drv_flag_double = c(
    fr = "Deux séries portent le même signal. Le moteur n'en retient qu'une, pour ne pas compter deux fois le même effet.",
    en = "Two series carry the same signal. The engine keeps only one, so as not to count the same effect twice."
  ),
  drv_flag_reverse_t = c(fr = "Sens inverse", en = "Reverse direction"),
  drv_flag_reverse = c(
    fr = "Pour certaines séries, c'est la ligne qui bouge en premier et la série qui suit. Le moteur refuse alors ce décalage, car la série ne peut pas servir à prévoir la ligne.",
    en = "For some series, the line moves first and the series follows. The engine then refuses that lag, because the series cannot help forecast the line."
  ),
  drv_review_lead = c(
    fr = "Ce tableau reprend toutes les colonnes du moteur pour chaque série. Il sert à vérifier un cas précis. Le décalage affiché reste une suggestion, et enregistrer un avis n'écrit qu'un fichier de confirmations.",
    en = "This table repeats every engine column for each series. It is there to check a specific case. The lag shown stays a suggestion, and saving a view only writes a confirmation file."
  ),
  drv_review_notes = c(fr = "Notes par série · %s", en = "Notes per series · %s"),
  drv_lag_q = c(fr = "%s bouge-t-elle avant %s ?", en = "Does %s move before %s?"),
  drv_lag_x = c(
    fr = "L'axe horizontal donne le décalage en mois. 0 est le même mois. Un nombre positif veut dire que la série bouge avant la ligne.",
    en = "The horizontal axis gives the lag in months. 0 is the same month. A positive number means the series moves before the line."
  ),
  drv_lag_y = c(
    fr = "L'axe vertical donne la corrélation, de −1 à +1. Entre les deux pointillés, le lien ne se distingue pas du hasard.",
    en = "The vertical axis gives the correlation, from −1 to +1. Between the two dotted lines, the link cannot be told apart from chance."
  ),
  drv_plot_lag_none = c(fr = "Pas de profil de décalage pour cette série", en = "No lag profile for this series"),
  drv_plot_lag_flat = c(fr = "Aucun lien mesurable, quel que soit le décalage", en = "No measurable link at any lag"),
  drv_plot_lag_x = c(fr = "Décalage en mois (positif : la série bouge avant la ligne)", en = "Lag in months (positive: the series moves before the line)"),
  drv_plot_lag_y = c(fr = "Corrélation, de −1 à +1", en = "Correlation, from −1 to +1"),
  drv_plot_lag_title = c(fr = "Entre les pointillés, le lien ne bat pas le hasard", en = "Between the dotted lines, the link does not beat chance"),
  drv_plot_align_none = c(fr = "Pas de séries alignées", en = "No aligned series"),
  drv_plot_align_flat = c(fr = "Pas de résidus à tracer pour cette série", en = "No residuals to plot for this series"),
  drv_plot_align_title = c(fr = "Même échelle, décalage de %s mois", en = "Same scale, lag of %s months"),
  drv_plot_raw_x = c(fr = "%s, chiffres bruts", en = "%s, raw figures"),
  drv_plot_raw_xres = c(fr = "%s, hors saison et tendance", en = "%s, without season and trend"),
  drv_plot_raw_yres = c(fr = "%s hors saison et tendance (EUR)", en = "%s without season and trend (EUR)"),
  drv_plot_raw_main = c(fr = "Chiffres bruts. Corrélation %s. Un point = un mois.", en = "Raw figures. Correlation %s. One dot = one month."),
  drv_plot_res_main = c(fr = "Hors saison et tendance. Corrélation %s.", en = "Without season and trend. Correlation %s."),
  drv_plot_hold_none = c(fr = "Pas de contrôle : moins de 24 mois d'historique", en = "No check: fewer than 24 months of history"),
  drv_plot_hold_flat = c(fr = "Contrôle sans valeurs exploitables", en = "Check with no usable values"),
  drv_plot_hold_x = c(fr = "Les 12 derniers mois, dans l'ordre", en = "The last 12 months, in order"),
  drv_plot_hold_y = c(fr = "Écart à la saison, en euros", en = "Gap to the season, in euros"),
  drv_plot_hold_title = c(fr = "La série annonce-t-elle mieux que le même mois l'an dernier ?", en = "Does the series announce better than the same month last year?"),
  drv_plot_hold_actual = c(fr = "Ce qui s'est passé", en = "What happened"),
  drv_plot_hold_pred = c(fr = "Annonce de la série", en = "Series forecast"),
  drv_plot_hold_naive = c(fr = "Même mois l'an dernier", en = "Same month last year"),

  drv_term_serie = c(fr = "série", en = "series"),
  drv_tip_serie = c(
    fr = "Une colonne de chiffres, mois par mois, tirée de vos fichiers : des nuitées vendues, un tarif, un effectif, une température.",
    en = "A column of figures, month by month, taken from your files: nights sold, a rate, a headcount, a temperature."
  ),
  drv_term_ligne = c(fr = "ligne cible", en = "target line"),
  drv_tip_ligne = c(
    fr = "La ligne du compte de résultat que la série est censée faire bouger, par exemple le chiffre d'affaires ou la masse salariale.",
    en = "The income-statement line the series is meant to move, for example revenue or payroll."
  ),
  drv_term_r = c(fr = "r (corrélation)", en = "r (correlation)"),
  drv_tip_r = c(
    fr = "Un chiffre entre −1 et +1 qui dit si deux séries bougent ensemble. Près de +1, elles montent et baissent ensemble. Près de −1, quand l'une monte l'autre baisse. Près de 0, aucun lien n'est visible. Il est calculé une fois la saison et la tendance retirées.",
    en = "A figure between −1 and +1 that says whether two series move together. Near +1, they rise and fall together. Near −1, when one rises the other falls. Near 0, no link is visible. It is computed once season and trend are removed."
  ),
  drv_term_partial = c(fr = "r partiel", en = "partial r"),
  drv_tip_partial = c(
    fr = "La même corrélation, une fois retiré l'effet du volume (les nuitées vendues). Si elle reste forte, la série apporte quelque chose de plus que le volume.",
    en = "The same correlation, once the volume effect (nights sold) is removed. If it stays strong, the series adds something beyond volume."
  ),
  drv_term_lag = c(fr = "décalage", en = "lag"),
  drv_tip_lag = c(
    fr = "Le nombre de mois entre la série et la ligne. +2 : la série bouge deux mois avant la ligne, ce qui aide à prévoir. −2 : elle bouge deux mois après. 0 : le même mois.",
    en = "The number of months between the series and the line. +2: the series moves two months before the line, which helps forecast. −2: it moves two months after. 0: the same month."
  ),
  drv_term_rank = c(fr = "rang", en = "rank"),
  drv_tip_rank = c(
    fr = "Le moteur mélange 2 000 fois les mois de la série au hasard. Le rang dit dans quelle part de ces tirages le hasard fait aussi bien que la vraie série. Sous 5 %, le lien bat le hasard. Plus le chiffre est petit, mieux c'est.",
    en = "The engine shuffles the months of the series 2,000 times at random. The rank says in what share of those draws chance does as well as the real series. Below 5%, the link beats chance. The smaller the figure, the better."
  ),
  drv_term_seuil = c(fr = "seuil", en = "bar"),
  drv_tip_seuil = c(
    fr = "La corrélation minimale pour que le lien compte, compte tenu du nombre de mois. Avec peu de mois, il faut une corrélation plus forte.",
    en = "The minimum correlation for the link to count, given the number of months. With few months, a stronger correlation is needed."
  ),
  drv_term_seuil_commun = c(fr = "seuil commun", en = "joint bar"),
  drv_tip_seuil_commun = c(
    fr = "Quand plusieurs séries sont testées en même temps, l'une d'elles peut passer par chance. Le seuil commun divise 5 % par le nombre de séries pour écarter ces coups de chance.",
    en = "When several series are tested at once, one of them can pass by chance. The joint bar divides 5% by the number of series to rule out those lucky hits."
  ),
  drv_term_mois = c(fr = "mois", en = "months"),
  drv_tip_mois = c(
    fr = "Le nombre de mois clos utilisés pour le test. Les mois de prévision ne comptent pas.",
    en = "The number of closed months used for the test. Forecast months do not count."
  ),
  drv_term_residu = c(fr = "résidu", en = "residual"),
  drv_tip_residu = c(
    fr = "Ce qui reste d'une série une fois retirées sa saison habituelle et sa tendance. Comparer les résidus évite de croire liées deux séries qui montent simplement toutes les deux en été.",
    en = "What remains of a series once its usual season and trend are removed. Comparing residuals avoids believing two series are linked when both simply rise in summer."
  ),
  drv_term_mecanisme = c(fr = "Mécanisme", en = "Mechanism"),
  drv_tip_mecanisme = c(
    fr = "La série entre dans le calcul d'une ligne, par une formule vérifiée sur les comptes. Exemple : chiffre d'affaires = nuitées vendues × tarif.",
    en = "The series is part of a line's calculation, through a formula checked against the books. Example: revenue = nights sold × rate."
  ),
  drv_term_confirme = c(fr = "Confirmé", en = "Confirmed"),
  drv_tip_confirme = c(
    fr = "La série n'entre dans aucun calcul, mais l'historique montre un lien solide avec sa ligne. Elle est retenue pour la prévision.",
    en = "The series is part of no calculation, but the history shows a solid link with its line. It is kept for the forecast."
  ),
  drv_term_veille = c(fr = "Veille", en = "Watch"),
  drv_tip_veille = c(
    fr = "La série est mesurée et classée, sans preuve suffisante pour être retenue. On la garde à l'œil.",
    en = "The series is measured and ranked, without enough proof to be kept. It stays under watch."
  ),
  drv_term_reverse = c(fr = "sens inverse", en = "reverse direction"),
  drv_tip_reverse = c(
    fr = "La ligne bouge avant la série. C'est donc la ligne qui entraîne la série, et la série ne peut pas servir à la prévoir.",
    en = "The line moves before the series. So the line drives the series, and the series cannot help forecast it."
  ),
  drv_term_bruit = c(fr = "bruit", en = "noise"),
  drv_tip_bruit = c(
    fr = "Le lien est trop faible pour le nombre de mois disponibles : on ne peut pas le distinguer du hasard.",
    en = "The link is too weak for the number of months available: it cannot be told apart from chance."
  ),
  drv_term_constante = c(fr = "constante", en = "constant"),
  drv_tip_constante = c(
    fr = "La série ne bouge presque pas sur la période, donc aucune corrélation ne peut être calculée.",
    en = "The series barely moves over the period, so no correlation can be computed."
  ),
  drv_term_attendu = c(fr = "attendu", en = "expected"),
  drv_tip_attendu = c(
    fr = "La série entre dans le calcul de la ligne. Leur lien est fort par construction et ne prouve rien de plus.",
    en = "The series is part of the line's calculation. Their link is strong by construction and proves nothing more."
  ),
  drv_term_plus_cout = c(fr = "plus de coût", en = "more cost"),
  drv_tip_plus_cout = c(
    fr = "Monter cette série fait monter la masse salariale.",
    en = "Raising this series raises payroll."
  ),
  drv_term_soutien = c(fr = "Suit sa ligne", en = "Follows its line"),
  drv_tip_soutien = c(
    fr = "Premier test : l'historique montre-t-il entre la série et sa ligne cible un lien plus net que ne le ferait le hasard ?",
    en = "First test: does the history show a link between the series and its target line, stronger than chance?"
  ),
  drv_term_indep = c(fr = "Apporte du neuf", en = "Adds something new"),
  drv_tip_indep = c(
    fr = "Deuxième test : le lien tient-il une fois retiré l'effet du volume ? Si oui, la série apporte une information à elle.",
    en = "Second test: does the link hold once the volume effect is removed? If so, the series brings information of its own."
  ),
  drv_term_resultat = c(fr = "Pèse sur le résultat", en = "Moves the result"),
  drv_tip_resultat = c(
    fr = "Troisième test : la série est-elle liée au résultat d'exploitation plus nettement que ne le ferait le hasard ?",
    en = "Third test: is the series linked to operating income, more strongly than chance?"
  ),
  drv_term_score = c(fr = "note", en = "mark"),
  drv_tip_score = c(
    fr = "Une note de 0 à 1 qui sert à classer les séries. Elle mélange la force du lien mesuré et la confiance déclarée au départ.",
    en = "A mark from 0 to 1 used to rank the series. It blends the strength of the measured link with the confidence declared at setup."
  ),
  drv_term_prior = c(fr = "confiance de départ", en = "starting confidence"),
  drv_tip_prior = c(
    fr = "La confiance déclarée pour la série lors de la configuration, avant toute mesure, entre 0 et 1.",
    en = "The confidence declared for the series at setup, before any measure, between 0 and 1."
  ),
  drv_term_holdout = c(fr = "contrôle sur 12 mois", en = "12-month check"),
  drv_tip_holdout = c(
    fr = "Le moteur met de côté les 12 derniers mois, puis regarde si la série les aurait mieux annoncés qu'une simple copie du même mois l'an dernier.",
    en = "The engine sets the last 12 months aside, then checks whether the series would have announced them better than a plain copy of the same month last year."
  ),
  drv_term_part = c(fr = "part", en = "share"),
  drv_tip_part = c(
    fr = "La part des variations d'un mois à l'autre d'un solde (marge brute, EBE, résultat) qu'une ligne explique. Les parts d'un même solde font 100 %.",
    en = "The share of the month-to-month variation of a subtotal (gross margin, EBITDA, result) that a line explains. Shares of one subtotal add to 100%."
  ),
  drv_term_verdict = c(fr = "verdict", en = "verdict"),
  drv_tip_verdict = c(
    fr = "La proposition du moteur pour la série : Garder, Possible, Pas assez d'historique ou Écarter. Vous pouvez la changer dans la fiche.",
    en = "The engine proposal for the series: Keep, Possible, Not enough history or Reject. You can change it in the card."
  ),
  drv_term_formule = c(fr = "Formule", en = "Formula"),
  drv_tip_formule = c(
    fr = "La preuve la plus forte : la série entre dans une formule qui redonne les comptes mois par mois, par exemple chiffre d'affaires = nuitées vendues × tarif.",
    en = "The strongest proof: the series is part of a formula that gives back the books month by month, for example revenue = nights sold × rate."
  ),
  drv_term_bilan = c(fr = "Bilan", en = "Overall"),
  drv_tip_bilan = c(
    fr = "La lecture d'ensemble. Prouvée : une formule vérifiée la relie aux comptes. À trancher : sa formule s'écarte des comptes. Solide : tous les tests mesurés passent. Fragile : une partie seulement passe. Sans preuve : rien ne passe.",
    en = "The overall reading. Proved: a checked formula links it to the books. Your call: its formula misses the books. Solid: every measured test passes. Fragile: only part pass. No proof: nothing passes."
  ),
  match_title = c(
    fr = "D'où viennent Prudent et Ambitieux",
    en = "Where Cautious and Ambitious come from"
  ),
  match_lead = c(
    fr = "Chaque ligne du tableau des pourcentages est un levier. Le pourcentage élargit ce levier sur les mois encore ouverts. Prudent prend moins de chiffre d'affaires et plus de coûts. Ambitieux fait l'inverse. On en affiche au plus cinq. Le pourcentage cité est celui enregistré avec les fichiers.",
    en = "Each row of the percent table is a lever. The percent widens that lever on the months still open. Cautious takes less revenue and more cost. Ambitious does the opposite. At most five are shown. The percent quoted is the one saved with the files."
  ),
  match_link = c(
    fr = "Voir d'où viennent ces lignes",
    en = "See where these rows come from"
  ),
  match_sec_on = c(
    fr = "Les leviers affichés",
    en = "The levers on screen"
  ),
  match_sec_held = c(
    fr = "Cochés à la mise en place, sans ligne de pourcentage",
    en = "Switched on at setup, with no percent row"
  ),
  match_sec_off = c(
    fr = "Non cochés, donc absents",
    en = "Switched off, so absent"
  ),
  match_col_formula = c(fr = "Formule", en = "Formula"),
  match_col_lever = c(fr = "Levier", en = "Lever"),
  match_col_effect = c(fr = "Ce que change le pourcentage", en = "What the percent changes"),
  match_none = c(
    fr = "Aucune formule n'est enregistrée.",
    en = "No formula is saved."
  ),
  match_no_line = c(fr = "Pas de ligne de pourcentage", en = "No percent row"),
  match_width = c(
    fr = "Largeur enregistrée : prudent \u2212 %s %%, ambitieux + %s %%.",
    en = "Saved width: cautious \u2212 %s %%, ambitious + %s %%."
  ),
  match_mech_capacity = c(
    fr = "Places × occupation × tarif",
    en = "Places × occupancy × rate"
  ),
  match_mech_qty = c(fr = "Quantité × prix", en = "Quantity × price"),
  match_mech_customers = c(
    fr = "Clients × dépense moyenne",
    en = "Customers × average spend"
  ),
  match_mech_growth = c(
    fr = "Croissance du chiffre d'affaires",
    en = "Revenue growth"
  ),
  match_mech_variable = c(
    fr = "Coût qui suit l'activité",
    en = "Cost that follows activity"
  ),
  match_mech_head = c(
    fr = "Effectif × coût chargé par personne",
    en = "Headcount × loaded cost per person"
  ),
  match_mech_fixed = c(
    fr = "Charges qui reviennent chaque mois",
    en = "Costs that return every month"
  ),
  match_mech_days = c(
    fr = "Délais clients, fournisseurs et stock",
    en = "Customer, supplier and stock delays"
  ),
  match_mech_tax = c(fr = "Taux d'impôt", en = "Tax rate"),
  match_mech_capex = c(
    fr = "Calendrier des investissements",
    en = "Investment calendar"
  ),
  match_mech_debt = c(
    fr = "Calendrier des emprunts",
    en = "Debt calendar"
  ),
  match_do_taux_utilisation = c(
    fr = "Élargit l'occupation des mois ouverts. Prudent vend moins. Ambitieux vend plus. Le nombre de places ne change pas.",
    en = "Widens occupancy on the open months. Cautious sells less. Ambitious sells more. The number of places does not change."
  ),
  match_do_tarif = c(
    fr = "Élargit le tarif moyen. Le nombre de places reste celui du fichier.",
    en = "Widens the average rate. The number of places stays the file's number."
  ),
  match_do_quantite = c(
    fr = "Élargit les quantités vendues.",
    en = "Widens the quantities sold."
  ),
  match_do_prix = c(
    fr = "Élargit le prix unitaire.",
    en = "Widens the unit price."
  ),
  match_do_clients = c(
    fr = "Élargit le nombre de clients actifs.",
    en = "Widens the number of active customers."
  ),
  match_do_depense_moyenne = c(
    fr = "Élargit la dépense moyenne par client.",
    en = "Widens the average spend per customer."
  ),
  match_do_croissance_ca = c(
    fr = "Élargit la croissance du chiffre d'affaires. Cette formule sert quand il n'y a pas de fichier de volumes.",
    en = "Widens revenue growth. This formula is used when there is no volumes file."
  ),
  match_do_cout_variable_pct = c(
    fr = "Élargit les achats qui suivent l'activité. Prudent les augmente. Ambitieux les baisse.",
    en = "Widens purchases that follow activity. Cautious raises them. Ambitious lowers them."
  ),
  match_do_effectif = c(
    fr = "Élargit l'effectif payé sur le mois.",
    en = "Widens the headcount paid that month."
  ),
  match_do_cout_moyen = c(
    fr = "Élargit le coût chargé par personne. L'effectif reste le chiffre saisi dans les moteurs.",
    en = "Widens the loaded cost per person. Headcount stays the figure typed in the open-month drivers."
  ),
  match_do_charges_fixes = c(
    fr = "Élargit le loyer, l'énergie et les contrats. Prudent les augmente. Ambitieux les baisse.",
    en = "Widens rent, energy and contracts. Cautious raises them. Ambitious lowers them."
  ),
  match_do_dso = c(
    fr = "Élargit le délai d'encaissement des clients. Cela bouge la trésorerie, pas le résultat.",
    en = "Widens how long customers take to pay. That moves cash, not the result."
  ),
  match_do_dpo = c(
    fr = "Élargit le délai de paiement des fournisseurs. Cela bouge la trésorerie, pas le résultat.",
    en = "Widens how long suppliers are paid. That moves cash, not the result."
  ),
  match_do_dio = c(
    fr = "Élargit les jours de stock. Cela bouge la trésorerie, pas le résultat.",
    en = "Widens the days of stock. That moves cash, not the result."
  ),
  match_do_other = c(
    fr = "Élargit le chiffre enregistré sous ce nom.",
    en = "Widens the figure saved under this name."
  ),
  match_aside_one = c(
    fr = "Ce levier n'a pas de ligne de pourcentage. Cinq lignes sont déjà affichées, ou il n'a pas été retenu.",
    en = "This lever has no percent row. Five rows are already shown, or it was not kept."
  ),
  match_held_days = c(
    fr = "Ces délais bougent la trésorerie, pas le résultat. Ils n'ont pas de ligne tant que cinq leviers sont déjà affichés.",
    en = "These delays move cash, not the result. They have no row while five levers are already shown."
  ),
  match_held_tax = c(
    fr = "Un taux dans la table des paramètres. L'impôt des mois clos reste celui des comptes.",
    en = "A rate in the parameters table. Tax on closed months stays the books."
  ),
  match_held_capex = c(
    fr = "Des dates et des montants. Le calcul ne les invente pas à partir du compte de résultat.",
    en = "Dates and amounts. The calculation does not invent them from the profit and loss."
  ),
  match_held_debt = c(
    fr = "Tirages et remboursements, dans le même calendrier. Les soldes de départ restent au bilan.",
    en = "Drawdowns and repayments, in the same calendar. Opening balances stay on the balance sheet."
  ),
  match_held_other = c(
    fr = "Cette formule est cochée. Elle n'a pas de ligne dans le tableau des pourcentages.",
    en = "This formula is switched on. It has no row in the percent table."
  ),
  match_off_qty = c(
    fr = "Cette formule n'est pas cochée. Volume et prix n'ont pas de ligne.",
    en = "This formula is switched off. Volume and price have no row."
  ),
  match_off_customers = c(
    fr = "Cette formule n'est pas cochée. Clients et dépense moyenne n'ont pas de ligne.",
    en = "This formula is switched off. Customers and average spend have no row."
  ),
  match_off_growth = c(
    fr = "Cette formule n'est pas cochée. Elle sert seulement quand il n'y a pas de fichier de volumes.",
    en = "This formula is switched off. It is used only when there is no volumes file."
  ),
  match_off_other = c(
    fr = "Cette formule n'est pas cochée.",
    en = "This formula is switched off."
  ),
  match_lever_taux_utilisation = c(
    fr = "Taux d'occupation / d'utilisation",
    en = "Occupancy / utilisation"
  ),
  match_lever_tarif = c(fr = "Tarif moyen", en = "Average rate"),
  match_lever_quantite = c(fr = "Volume vendu", en = "Volume sold"),
  match_lever_prix = c(fr = "Prix unitaire", en = "Unit price"),
  match_lever_clients = c(fr = "Nombre de clients", en = "Number of customers"),
  match_lever_depense_moyenne = c(
    fr = "Dépense moyenne par client",
    en = "Average spend per customer"
  ),
  match_lever_croissance_ca = c(
    fr = "Croissance du chiffre d'affaires",
    en = "Revenue growth"
  ),
  match_lever_cout_variable_pct = c(fr = "Coût variable", en = "Variable cost"),
  match_lever_effectif = c(fr = "Effectif", en = "Headcount"),
  match_lever_cout_moyen = c(fr = "Coût moyen chargé", en = "Average loaded cost"),
  match_lever_charges_fixes = c(fr = "Charges fixes", en = "Fixed costs"),
  match_lever_dso = c(fr = "Délai clients", en = "Customer delay"),
  match_lever_dpo = c(fr = "Délai fournisseurs", en = "Supplier delay"),
  match_lever_dio = c(fr = "Jours de stock", en = "Days of stock"),
  match_lever_days = c(
    fr = "Délai clients, délai fournisseurs, jours de stock",
    en = "Customer delay, supplier delay, days of stock"
  ),
  match_sum = c(
    fr = "%s leviers affichés, %s cochés sans pourcentage, %s non cochés.",
    en = "%s levers on screen, %s switched on without a percent, %s switched off."
  ),
  nav_scn = c(fr = "Scénarios", en = "Scenarios"),
  nav_menu_open = c(fr = "Ouvrir la liste des onglets", en = "Open the list of tabs"),
  scnp_kicker = c(fr = "Sources · Scénarios", en = "Sources · Scenarios"),
  scnp_title = c(
    fr = "Scénarios : prudent, attendu, ambitieux",
    en = "Scenarios: cautious, expected, ambitious"
  ),
  scnp_lead = c(
    fr = "Les mois clos sont le réalisé et ne bougent pas. Pour chaque mois encore ouvert, vous choisissez un chemin : prudent, attendu ou ambitieux. Cette page réunit le choix, ce qu'il donne, les chiffres qui le portent et leur origine.",
    en = "Closed months are actuals and do not move. For each month still open, you pick a path: cautious, expected or ambitious. This page holds the choice, what it gives, the figures behind it and where they come from."
  ),
  scnp_toc = c(fr = "Sur cette page", en = "On this page"),
  scnp_toc_pick = c(fr = "Le choix", en = "The choice"),
  scnp_toc_result = c(fr = "Ce qu'il donne", en = "What it gives"),
  scnp_toc_drv = c(fr = "Les niveaux", en = "The levels"),
  scnp_toc_pct = c(fr = "Les largeurs", en = "The widths"),
  scnp_toc_match = c(fr = "Formules et leviers", en = "Formulas and levers"),
  scnp_toc_src = c(fr = "D'où viennent les chiffres", en = "Where the figures come from"),
  scnp_toc_where = c(fr = "Où le choix se voit", en = "Where the choice shows"),
  scnp_result_k = c(fr = "Ce que donne votre choix", en = "What your choice gives"),
  scnp_result_lead = c(
    fr = "Résultat d'exploitation des mois ouverts, en EUR. La case encadrée est le chemin retenu pour le mois. Le total de la colonne Votre choix est le chiffre repris sur Synthèse.",
    en = "Operating income of the open months, in EUR. The framed cell is the path kept for the month. The total of the Your choice column is the figure shown on Summary."
  ),
  scnp_round = c(
    fr = "Les montants sont arrondis à l'euro. Une colonne peut donc différer d'un euro de son total.",
    en = "Amounts are rounded to the euro, so a column can differ from its total by one euro."
  ),
  scnp_tile_oi = c(fr = "Résultat d'exploitation, mois ouverts", en = "Operating income, open months"),
  scnp_tile_cash = c(fr = "Trésorerie fin %s", en = "Cash at end of %s"),
  scnp_tile_gap = c(fr = "Écart au budget voté", en = "Gap to the voted budget"),
  scnp_tile_gap_sub = c(fr = "Budget voté sur ces mois : %s EUR.", en = "Voted budget on these months: %s EUR."),
  scnp_tile_oi_sub = c(fr = "De %s (prudent) à %s (ambitieux).", en = "From %s (cautious) to %s (ambitious)."),
  scnp_tile_cash_sub = c(fr = "De %s (prudent) à %s (ambitieux).", en = "From %s (cautious) to %s (ambitious)."),
  scnp_col_month = c(fr = "Mois", en = "Month"),
  scnp_col_pick = c(fr = "Votre choix", en = "Your choice"),
  scnp_col_budget = c(fr = "Budget voté", en = "Voted budget"),
  scnp_row_total = c(fr = "Total des mois ouverts", en = "Total of the open months"),
  scnp_row_cash = c(fr = "Trésorerie fin %s", en = "Cash at end of %s"),
  scnp_none = c(
    fr = "Tous les mois de l'exercice sont clos. Il n'y a pas de chemin à choisir.",
    en = "Every month of the year is closed. There is no path to pick."
  ),
  scnp_wait = c(
    fr = "Les moteurs des mois ouverts se chargent. Le tableau suit.",
    en = "The open-month drivers are loading. The table follows."
  ),
  scnp_src_k = c(fr = "D'où viennent les chiffres", en = "Where the figures come from"),
  scnp_src_fact = c(
    fr = "Fichiers, mise en place et cette session : ce qui tient chaque chiffre.",
    en = "Files, setup and this session: what holds each figure."
  ),
  scnp_src_col_what = c(fr = "Élément", en = "Item"),
  scnp_src_col_from = c(fr = "D'où ça vient", en = "Where it comes from"),
  scnp_src_col_here = c(fr = "Ce que vous changez ici", en = "What you change here"),
  scnp_src_closed = c(fr = "Mois clos", en = "Closed months"),
  scnp_src_closed_from = c(
    fr = "Le fichier comptable %s. Les mois clos sont verrouillés.",
    en = "The accounting file %s. Closed months are locked."
  ),
  scnp_src_closed_here = c(
    fr = "Rien. Déverrouiller un mois clos est un retraitement du réalisé.",
    en = "Nothing. Unlocking a closed month restates the actuals."
  ),
  scnp_src_level = c(fr = "Niveau attendu des mois ouverts", en = "Expected level of the open months"),
  scnp_src_level_from = c(
    fr = "Le fichier de volumes %s : occupation, tarif, effectif et coût moyen, mois par mois.",
    en = "The volumes file %s: occupancy, rate, headcount and average cost, month by month."
  ),
  scnp_src_level_here = c(
    fr = "Les moteurs des mois ouverts, sur cette page. La saisie vaut pour cette session.",
    en = "The open-month drivers, on this page. What you type holds for this session."
  ),
  scnp_src_lines = c(fr = "Lignes qui suivent l'activité", en = "Lines that follow activity"),
  scnp_src_lines_from = c(
    fr = "Mesurées sur %s mois d'historique, de %s à %s.",
    en = "Measured on %s months of history, from %s to %s."
  ),
  scnp_src_lines_here = c(
    fr = "Rien. Elles suivent les moteurs des mois ouverts.",
    en = "Nothing. They follow the open-month drivers."
  ),
  scnp_src_width = c(fr = "Largeurs prudent et ambitieux", en = "Cautious and ambitious widths"),
  scnp_src_width_from = c(
    fr = "La page de mise en place, sur le site. La configuration a été enregistrée le %s.",
    en = "The setup page, on the site. The configuration was saved on %s."
  ),
  scnp_src_width_from_nodate = c(
    fr = "La page de mise en place, sur le site, enregistrée dans la configuration.",
    en = "The setup page, on the site, saved in the configuration."
  ),
  scnp_src_width_here = c(
    fr = "Le tableau Prudent et ambitieux, sur cette page. La saisie vaut pour cette session. Une synchronisation reprend les largeurs du site.",
    en = "The Cautious and ambitious table, on this page. What you type holds for this session. A sync brings back the site widths."
  ),
  scnp_src_pick = c(fr = "Chemin de chaque mois ouvert", en = "Path of each open month"),
  scnp_src_pick_from = c(
    fr = "Votre choix. Au démarrage, chaque mois ouvert est attendu.",
    en = "Your choice. At start, every open month is expected."
  ),
  scnp_src_pick_here = c(
    fr = "Le bloc Avant la clôture, en haut de cette page, ou le bouton Scénarios posé sur les graphiques.",
    en = "The Before the close block, at the top of this page, or the Scenarios button on the charts."
  ),
  scnp_src_budget = c(fr = "Budget voté", en = "Voted budget"),
  scnp_src_budget_from = c(
    fr = "Le fichier de budget %s. Il ne bouge pas avec le scénario.",
    en = "The budget file %s. It does not move with the scenario."
  ),
  scnp_src_budget_here = c(
    fr = "Rien. Les années qui dessinent le budget se cochent sur Compte de résultat.",
    en = "Nothing. The years that shape the budget are ticked on the income statement."
  ),
  scnp_src_missing = c(fr = "non trouvé", en = "not found"),
  scnp_where_k = c(fr = "Où le choix se voit", en = "Where the choice shows"),
  scnp_where_fact = c(
    fr = "19 graphiques et tableaux, sur 6 onglets. Chacun porte le bouton Scénarios.",
    en = "19 charts and tables, on 6 tabs. Each one carries the Scenarios button."
  ),
  scnp_where_lead = c(
    fr = "Les mois clos ne bougent nulle part. Le budget voté non plus.",
    en = "Closed months move nowhere. Neither does the voted budget."
  ),
  scnp_where_go = c(fr = "Ouvrir", en = "Open"),
  scnp_w_kpi = c(fr = "Les quatre cartons : la part prévue du chiffre d'affaires, de la marge et de la trésorerie.", en = "The four cards: the forecast part of revenue, margin and cash."),
  scnp_w_trend = c(fr = "Le cumul du chiffre d'affaires et du résultat : la partie prévue des courbes.", en = "Cumulative revenue and result: the forecast part of the lines."),
  scnp_w_cashline = c(fr = "La courbe de trésorerie : la partie prévue.", en = "The cash line: the forecast part."),
  scnp_w_cone = c(fr = "Le cône : le chemin choisi bouge, les bords restent.", en = "The cone: the chosen path moves, the edges stay."),
  scnp_w_wf1 = c(fr = "La cascade d'un mois, quand ce mois est ouvert.", en = "The one-month waterfall, when that month is open."),
  scnp_w_wfr = c(fr = "Les deux cascades de période, quand la période contient un mois ouvert.", en = "The two period waterfalls, when the period holds an open month."),
  scnp_w_grid = c(fr = "Le tableau : les colonnes des mois ouverts.", en = "The table: the open-month columns."),
  scnp_w_bridge = c(fr = "Le pont de trésorerie et sa légende, quand la fenêtre contient un mois ouvert.", en = "The cash bridge and its caption, when the window holds an open month."),
  scnp_w_cashtable = c(fr = "Le tableau de trésorerie : les colonnes des mois ouverts.", en = "The cash table: the open-month columns."),
  scnp_w_bstable = c(fr = "Le tableau du bilan : les colonnes des mois ouverts.", en = "The balance sheet table: the open-month columns."),
  scnp_w_tree = c(fr = "La carte du bilan, quand le mois affiché est ouvert.", en = "The balance sheet map, when the month on screen is open."),
  scnp_w_tiles = c(fr = "Les trois cartons : Reste de l'année et Atterrissage bougent, Mois clos reste.", en = "The three cards: Rest of year and Landing move, Closed months stays."),
  scnp_w_heat = c(fr = "La carte de chaleur : mois prévus, Reste et Année.", en = "The heat map: forecast months, Rest and Year."),
  scnp_w_gap = c(fr = "Écart par mois.", en = "Gap by month."),
  scnp_w_land = c(fr = "Du budget à l'atterrissage.", en = "From budget to landing."),
  scnp_w_parts = c(fr = "Ce qui explique l'écart.", en = "What explains the gap."),
  scnp_w_months = c(fr = "Le tableau mois par mois : les lignes de prévision.", en = "The month-by-month table: the forecast rows."),
  scnp_w_health = c(fr = "La colonne de fin d'année. La colonne du mois clos reste.", en = "The year-end column. The closed-month column stays."),
  scnp_pointer_k = c(fr = "Scénarios des mois ouverts", en = "Scenarios of the open months"),
  scnp_pointer = c(
    fr = "Le chemin de chaque mois ouvert, les niveaux et les largeurs se règlent dans l'onglet Scénarios.",
    en = "The path of each open month, the levels and the widths are set on the Scenarios tab."
  ),
  scnp_pointer_go = c(fr = "Ouvrir Scénarios", en = "Open Scenarios"),
  scnp_drv_pointer = c(
    fr = "Prudent et Ambitieux ont leur onglet. La correspondance entre les formules de la mise en place et les leviers y est.",
    en = "Cautious and Ambitious have their own tab. The match between the setup formulas and the levers is there."
  ),
  scnp_drv_pointer_go = c(fr = "Formules et leviers", en = "Formulas and levers"),
  scnp_dock_more = c(fr = "Tout sur les scénarios", en = "All about scenarios"),
  loc_engine_beside = c(
    fr = "Placez fpa-engine.R dans le meme dossier que app.R, a cote de fpa-config.json.",
    en = "Put fpa-engine.R in the same folder as app.R, next to fpa-config.json."
  ),
  loc_pkg_drivers = c(
    fr = "Les paquets R 'shiny' et 'bslib' sont requis pour l'ecran drivers.\n",
    en = "The R packages shiny and bslib are required for the drivers screen.\n"
  ),
  loc_pkg_install = c(
    fr = "Installez-les une fois: install.packages(c(\"shiny\", \"bslib\"))",
    en = "Install them once: install.packages(c(\"shiny\", \"bslib\"))"
  ),
  loc_no_close_lock = c(
    fr = "Aucun mois clos a verrouiller.",
    en = "No closed month to lock."
  ),
  loc_wc_open_release = c(
    fr = "Dénouement du BFR d'ouverture",
    en = "Release of opening working capital"
  ),
  loc_no_cash = c(
    fr = "Pas de trésorerie.",
    en = "No cash."
  ),
  loc_bridge_same_months = c(
    fr = " Mêmes mois que le graphique. Ouverture et clôture sont des soldes. ",
    en = " Same months as the chart. Opening and closing are balances. "
  ),
  loc_empty_rows_hidden = c(
    fr = " Lignes vides masquées : ",
    en = " Empty rows hidden: "
  ),
  loc_no_driver_result = c(
    fr = "Pas encore de resultat drivers dans ",
    en = "No driver result yet in "
  ),
  loc_use = c(
    fr = "À utiliser",
    en = "To use"
  ),
  loc_to_forecast = c(
    fr = "À prévoir",
    en = "To forecast"
  ),
  loc_to_explain = c(
    fr = "À expliquer",
    en = "To explain"
  ),
  loc_costs = c(
    fr = "Coûts",
    en = "Costs"
  ),
  loc_operations = c(
    fr = "Opérations",
    en = "Operations"
  ),
  loc_hypothesis = c(
    fr = "Hypothèse",
    en = "Assumption"
  ),
  loc_tax = c(
    fr = "Impôt",
    en = "Tax"
  ),
  loc_cost = c(
    fr = "Coût",
    en = "Cost"
  ),
  loc_pnl_monthly = c(
    fr = "Compte de résultat mensuel",
    en = "Monthly income statement"
  ),
  loc_cash_monthly = c(
    fr = "Trésorerie mensuelle",
    en = "Monthly cash"
  ),
  loc_scatter_full = c(
    fr = "Chaque point est un mois. Le nuage de gauche se coupe en mois creux et mois pleins : ",
    en = "Each point is a month. The cloud on the left splits into thin months and full months: "
  ),
  loc_scatter_compose = c(
    fr = " montent ensemble parce que cette série compose la ligne. ",
    en = " rise together because this series makes up the line. "
  ),
  loc_scatter_resid = c(
    fr = "À droite, saison et tendance sont retirées, et le lien reste (r = ",
    en = "On the right, season and trend are removed, and the link remains (r = "
  ),
  loc_not_discovered = c(
    fr = "Conclusion : ce n'est pas un driver découvert. C'est le mécanisme. On ne le retient pas sur cette pente.",
    en = "Conclusion: this is not a driver that was discovered. It is the mechanism. It is not kept on this slope."
  ),
  loc_scatter_resid_long = c(
    fr = "Chaque point est un mois. À gauche, les unités brutes. À droite, ce qui reste une fois la saison et la tendance retirées : c'est ce résidu que le score utilise (r = ",
    en = "Each point is a month. On the left, the raw units. On the right, what remains once season and trend are removed: the score uses that residual (r = "
  ),
  loc_link_survives = c(
    fr = "le lien survit au calendrier, donc il compte dans le score.",
    en = "the link survives the calendar, so it counts in the score."
  ),
  loc_link_calendar = c(
    fr = "une fois le calendrier retiré, le lien ne tient pas. La pente de gauche était le calendrier.",
    en = "once the calendar is removed, the link does not hold. The slope on the left was the calendar."
  ),
  loc_correlation = c(
    fr = ", corrélation ",
    en = ", correlation "
  ),
  loc_capacity = c(
    fr = "capacité",
    en = "capacity"
  ),
  loc_nights_sold = c(
    fr = "nuitées vendues",
    en = "nights sold"
  ),
  loc_temperature = c(
    fr = "température",
    en = "temperature"
  ),
  loc_precip = c(
    fr = "précipitations",
    en = "precipitation"
  ),
  loc_the_nights = c(
    fr = "les nuitées vendues",
    en = "nights sold"
  ),
  loc_of_nights = c(
    fr = "des nuitées vendues",
    en = "of nights sold"
  ),
  loc_page_shows = c(
    fr = "Ce que cette page montre. ",
    en = "What this page shows. "
  ),
  loc_to_do = c(
    fr = "À faire. ",
    en = "To do. "
  ),
  loc_no_series_run = c(
    fr = "Pas encore de séries. Lancez le calcul des drivers.",
    en = "No series yet. Run the driver calculation."
  ),
  loc_fin_result = c(
    fr = "Résultat financier",
    en = "Financial result"
  ),
  loc_company = c(
    fr = "Société",
    en = "Company"
  ),
  loc_chip_closed = c(
    fr = " · mois clos ",
    en = " · closed month "
  ),
  loc_file_of_month = c(
    fr = "Fichier du mois",
    en = "File for the month"
  ),
  loc_no_drivers_engine = c(
    fr = "Pas de drivers calculés. Lancez fpa-engine.R.",
    en = "No drivers calculated. Run fpa-engine.R."
  ),
  loc_calc_aims = c(
    fr = ". Le calcul vise ",
    en = ". The calculation targets "
  ),
  loc_config_said = c(
    fr = " La config déclarait ",
    en = " The configuration declared "
  ),
  loc_resid_r = c(
    fr = " r résiduel = ",
    en = " residual r = "
  ),
  loc_to_glue = c(
    fr = " à ",
    en = " to "
  ),
  loc_oi_band_09 = c(
    fr = " Écart d'exploitation entre 0,9 et 1,1 : ",
    en = " Operating-income gap between 0.9 and 1.1: "
  ),
  loc_lag_suggestion = c(
    fr = " Le décalage affiché est une suggestion. Confirmer n'écrit qu'un fichier.",
    en = " The lag on screen is a suggestion. Confirming writes one file only."
  ),
  loc_not_calculated = c(
    fr = "Pas calculée",
    en = "Not calculated"
  ),
  loc_forecast_cap = c(
    fr = "Prévoir",
    en = "Forecast"
  ),
  loc_no_mechanism_cfg = c(
    fr = "Aucun mécanisme n'est déclaré dans la config.",
    en = "No mechanism is declared in the configuration."
  ),
  loc_line_tested = c(
    fr = "Ligne testée",
    en = "Line tested"
  ),
  loc_independence = c(
    fr = "Indépendance",
    en = "Independence"
  ),
  loc_open_card_of = c(
    fr = "Ouvrir la fiche de",
    en = "Open the card for"
  ),
  loc_data_support = c(
    fr = "Les données soutiennent-elles cette série ?",
    en = "Do the data support this series?"
  ),
  loc_link_volume = c(
    fr = "Le lien tient-il une fois le volume retiré ?",
    en = "Does the link hold once volume is removed?"
  ),
  loc_how_far_series = c(
    fr = "Combien cette série peut-elle déplacer ?",
    en = "How far can this series move the result?"
  ),
  loc_not_separated = c(
    fr = "non distingué",
    en = "not separated"
  ),
  loc_months_suggested = c(
    fr = " mois, suggéré",
    en = " months, suggested"
  ),
  loc_months_refused = c(
    fr = " mois, refusé",
    en = " months, refused"
  ),
  loc_result_colon = c(
    fr = "résultat : ",
    en = "result: "
  ),
  loc_result_r = c(
    fr = "résultat r ",
    en = "result r "
  ),
  loc_is_derived = c(
    fr = "se déduit",
    en = "is derived"
  ),
  loc_line_aimed_dot = c(
    fr = "Ligne visée · ",
    en = "Target line · "
  ),
  loc_series_cards = c(
    fr = "Séries en cartes, reliées à la ligne du compte de résultat qu'elles visent",
    en = "Series as cards, linked to the income-statement line they feed"
  ),
  loc_line_aimed = c(
    fr = "Ligne visée",
    en = "Target line"
  ),
  loc_r_result = c(
    fr = "r résultat",
    en = "result r"
  ),
  loc_oi_gap = c(
    fr = "Écart d'exploitation",
    en = "Operating-income gap"
  ),
  loc_other_charges_rest = c(
    fr = "Reste des autres charges",
    en = "Rest of other charges"
  ),
  loc_no_pnl_engine = c(
    fr = "Pas de compte de resultat. Lancez fpa-engine.R.",
    en = "No income statement. Run fpa-engine.R."
  ),
  loc_no_opfin_window = c(
    fr = "Aucune ligne Op/Fin dans cette fenêtre.",
    en = "No operating or financial line in this window."
  ),
  loc_imported = c(
    fr = "Importé",
    en = "Imported"
  ),
  loc_rule = c(
    fr = "Règle",
    en = "Rule"
  ),
  loc_chip_forecast = c(
    fr = " · prévision",
    en = " · forecast"
  ),
  loc_chip_actual = c(
    fr = " · réalisé",
    en = " · actual"
  ),
  loc_hover_cell = c(
    fr = "Survolez une case : importé, règle, ou hypothèse. ",
    en = "Hover a cell: imported, rule, or assumption. "
  ),
  loc_row_hidden_zero = c(
    fr = "Ligne masquée, tout à zéro : ",
    en = "Row hidden, all zero: "
  ),
  loc_row_hidden_zero_sp = c(
    fr = " Ligne masquée, tout à zéro : ",
    en = " Row hidden, all zero: "
  ),
  loc_no_lines_compare = c(
    fr = "Pas de postes à comparer.",
    en = "No lines to compare."
  ),
  loc_no_month_locked = c(
    fr = "Aucun mois clos verrouillé.",
    en = "No closed month is locked."
  ),
  loc_variances = c(
    fr = "Écarts",
    en = "Variances"
  ),
  loc_nothing_in_config = c(
    fr = "Rien dans la configuration.",
    en = "Nothing in the configuration."
  ),
  loc_close_month_label = c(
    fr = "Mois de clôture",
    en = "Closing month"
  ),
  loc_gap_to_budget = c(
    fr = "Écart au budget",
    en = "Gap to budget"
  ),
  loc_config_no_files = c(
    fr = "Cette configuration ne décrit pas encore de fichiers ni de règles.",
    en = "This configuration does not describe files or rules yet."
  ),
  loc_series_of_file = c(
    fr = "Séries de ce fichier",
    en = "Series in this file"
  ),
  loc_no_series_file = c(
    fr = "Aucune série calculée sur ce fichier.",
    en = "No series calculated on this file."
  ),
  loc_open_card = c(
    fr = "Ouvrir la fiche",
    en = "Open the card"
  ),
  loc_chain_lead = c(
    fr = "Ce que la configuration relie, et ce que le dernier calcul a écrit. ",
    en = "What the configuration connects, and what the last calculation wrote. "
  ),
  loc_file_not_rule = c(
    fr = "Un fichier présent n'est pas une règle. Une règle activée n'est pas encore un montant calculé.",
    en = "A file on disk is not a rule. A rule that is on is not yet a calculated amount."
  ),
  loc_chain_legend = c(
    fr = "Fichiers : classés par cadre, historique, suivi, plan. Règles : lecture en pointillés, calcul en sarcelle. Sorties : où le montant atterrit.",
    en = "Files: grouped by frame, history, tracking, plan. Rules: reading as a dashed line, calculation in teal. Outputs: where the amount lands."
  ),
  loc_chain_today = c(
    fr = "Le trait vertical est aujourd'hui. À gauche, l'ancienneté. À droite, les mois déjà au-delà d'aujourd'hui. En retard : le suivi s'arrête plus d'un mois avant aujourd'hui.",
    en = "The vertical line is today. On the left, how far back. On the right, months already past today. Late: tracking stops more than a month before today."
  ),
  loc_to_treat = c(
    fr = "À traiter. ",
    en = "To deal with. "
  ),
  loc_calc_not_reread = c(
    fr = "Le dernier calcul n'a pas été relu. Les statuts s'arrêtent à la configuration.",
    en = "The last calculation was not read again. The statuses stop at the configuration."
  ),
  loc_how_far_file = c(
    fr = "Jusqu'où va chaque fichier",
    en = "How far each file goes"
  ),
  loc_rules = c(
    fr = "Règles",
    en = "Rules"
  ),
  loc_pick_card = c(
    fr = "Choisissez une carte. Le chemin en amont et en aval se marque, et le détail s'écrit ici.",
    en = "Choose a card. The path upstream and downstream is marked, and the detail is written here."
  ),
  loc_no_file = c(
    fr = "Aucun fichier.",
    en = "No file."
  ),
  loc_without_role = c(
    fr = "Sans rôle :",
    en = "No role:"
  ),
  loc_no_column_mapped = c(
    fr = "Aucune colonne mappée.",
    en = "No column mapped."
  ),
  loc_labels_sp = c(
    fr = " libellés",
    en = " labels"
  ),
  loc_close_month_chip = c(
    fr = " · clôture mois ",
    en = " · month close "
  ),
  loc_no_lever = c(
    fr = "Aucun levier.",
    en = "No lever."
  ),
  loc_summary = c(
    fr = "Synthèse",
    en = "Summary"
  ),
  loc_lag_months = c(
    fr = "Décalage en mois",
    en = "Lag in months"
  ),
  loc_closing = c(
    fr = "Clôture",
    en = "Closing"
  ),
  loc_health = c(
    fr = "Santé",
    en = "Health"
  ),
  loc_cash_colon = c(
    fr = "Trésorerie:",
    en = "Cash:"
  ),
  loc_cash_gap_colon = c(
    fr = "Écart trésorerie:",
    en = "Cash gap:"
  ),
  loc_from_date = c(
    fr = "À partir de",
    en = "From"
  ),
  loc_window_prev = c(
    fr = "Fenêtre précédente",
    en = "Previous window"
  ),
  loc_window_next = c(
    fr = "Fenêtre suivante",
    en = "Next window"
  ),
  loc_detail = c(
    fr = "Détail",
    en = "Detail"
  ),
  loc_bar_per_month = c(
    fr = "Une barre par mois",
    en = "One bar per month"
  ),
  loc_chain_mode = c(
    fr = "Enchaînement",
    en = "Running total"
  ),
  loc_at_zero = c(
    fr = "À zéro",
    en = "From zero"
  ),
  loc_chained = c(
    fr = "Enchaîné",
    en = "Running"
  ),
  loc_close_chip = c(
    fr = " · clôture ",
    en = " · close "
  ),
  loc_bars_from_zero = c(
    fr = ". Les barres partent de zéro et s'additionnent jusqu'à la variation.",
    en = ". The bars start at zero and add up to the movement."
  ),
  loc_bridge_open_close = c(
    fr = "La cascade part de l'ouverture et finit sur la clôture.",
    en = "The bridge starts at the opening balance and ends at the closing balance."
  ),
  loc_each_month_zero = c(
    fr = "Chaque mois repart de zéro.",
    en = "Each month starts again from zero."
  ),
  loc_bar_per_month_sp = c(
    fr = " Une barre par mois.",
    en = " One bar per month."
  ),
  loc_bar_per_line_sp = c(
    fr = " Une barre par poste.",
    en = " One bar per line."
  ),
  loc_hover_or_zoom = c(
    fr = " Survolez une barre, ou tracez un cadre pour zoomer.",
    en = " Hover a bar, or draw a box to zoom."
  ),
  loc_rev_actual = c(
    fr = "Chiffre d'affaires réalisé",
    en = "Actual revenue"
  ),
  loc_strongest_month = c(
    fr = "Mois le plus fort",
    en = "Strongest month"
  ),
  loc_total_actual = c(
    fr = "Total réalisé",
    en = "Actual total"
  ),
  loc_gap_already = c(
    fr = "Écart déjà signalé",
    en = "Gap already flagged"
  ),
  loc_forecast_aside = c(
    fr = "Prévision, à part",
    en = "Forecast, set aside"
  ),
  loc_last_closed = c(
    fr = "Dernier mois clos",
    en = "Last closed month"
  ),
  loc_balance_shown = c(
    fr = "Solde affiché",
    en = "Balance on screen"
  ),
  loc_months_under_zero = c(
    fr = "Mois sous zéro",
    en = "Months below zero"
  ),
  loc_liquidity = c(
    fr = "Ratio de liquidité",
    en = "Liquidity ratio"
  ),
  loc_cash_over_st = c(
    fr = "Trésorerie / dettes à court terme",
    en = "Cash / short-term debt"
  ),
  loc_oi_lower = c(
    fr = "résultat d'exploitation",
    en = "operating income"
  ),
  loc_until_sp = c(
    fr = "Jusqu'à ",
    en = "Up to "
  ),
  loc_forecast_adds = c(
    fr = ". La prévision, à part, ajoute ",
    en = ". The forecast, set aside, adds "
  ),
  loc_oi_actual_sp = c(
    fr = "Résultat d'exploitation réalisé ",
    en = "Actual operating income "
  ),
  loc_eur_closed_only = c(
    fr = " EUR, sur les mois clos seulement.",
    en = " EUR, on closed months only."
  ),
  loc_eur_rev_actual = c(
    fr = " EUR de chiffre d'affaires réalisé.",
    en = " EUR of actual revenue."
  ),
  loc_forecast_sp = c(
    fr = "Prévision ",
    en = "Forecast "
  ),
  loc_months_below_sp = c(
    fr = " mois sous zéro.",
    en = " months below zero."
  ),
  loc_no_month_below = c(
    fr = "Aucun mois sous zéro.",
    en = "No month below zero."
  ),
  loc_no_monthly_pnl = c(
    fr = "Pas de compte de resultat mensuel a cote de fpa-config.json. Le calcul est peut-etre deja fait dans le dossier du projet. Relancez app.R depuis ce dossier, ou Actualiser.",
    en = "No monthly income statement next to fpa-config.json. The calculation may already be in the project folder. Start app.R again from that folder, or Refresh."
  ),
  loc_actual_until = c(
    fr = " · réalisé jusqu'à ",
    en = " · actual up to "
  ),
  loc_hover_month = c(
    fr = " Survolez un mois.",
    en = " Hover a month."
  ),
  loc_hover_month_sp = c(
    fr = " Survolez un mois. ",
    en = " Hover a month. "
  ),
  loc_oi_sp = c(
    fr = "Résultat d'exploitation ",
    en = "Operating income "
  ),
  loc_declared_sp = c(
    fr = "Déclaré ",
    en = "Declared "
  ),
  loc_est_balance = c(
    fr = "<br>Solde estimé ",
    en = "<br>Estimated balance "
  ),
  loc_oi_ascii = c(
    fr = "Resultat d'exploitation",
    en = "Operating income"
  ),
  loc_cash_close_ascii = c(
    fr = "Tresorerie de cloture",
    en = "Closing cash"
  ),
  loc_span = c(
    fr = "Étendue",
    en = "Span"
  ),
  loc_closed_not_set = c(
    fr = "Mois clos : non indiqué",
    en = "Closed month: not set"
  ),
  loc_prepared_on = c(
    fr = "Établi le",
    en = "Prepared on"
  ),
  loc_rev_not_calc = c(
    fr = "Chiffre d'affaires : non calculé",
    en = "Revenue: not calculated"
  ),
  loc_margin_not_calc = c(
    fr = "Marge d'exploitation : non calculée",
    en = "Operating margin: not calculated"
  ),
  loc_cash_last_bridge = c(
    fr = "Trésorerie, dernier solde du pont",
    en = "Cash, last balance on the bridge"
  ),
  loc_cash_not_calc = c(
    fr = "Trésorerie : non calculée",
    en = "Cash: not calculated"
  ),
  loc_rev_no_gap = c(
    fr = "Chiffre d'affaires : pas d'écart au-dessus du seuil.",
    en = "Revenue: no gap above the threshold."
  ),
  loc_oi_no_gap = c(
    fr = "Résultat d'exploitation : pas d'écart au-dessus du seuil.",
    en = "Operating income: no gap above the threshold."
  ),
  loc_open_assumptions = c(
    fr = "Hypothèses des mois ouverts",
    en = "Assumptions for the open months"
  ),
  loc_no_open_assumption = c(
    fr = "Pas d'hypothèse lue sur les mois ouverts.",
    en = "No assumption read on the open months."
  ),
  loc_decisions = c(
    fr = "Décisions",
    en = "Decisions"
  ),
  loc_series_in_drivers = c(
    fr = "série(s) dans Drivers.",
    en = "series in Drivers."
  ),
  loc_drivers_saved = c(
    fr = "Drivers enregistrés.",
    en = "Drivers saved."
  ),
  loc_points_anomalies = c(
    fr = "point(s) à éclaircir dans Anomalies.",
    en = "point(s) to clear in Anomalies."
  ),
  loc_doc_not_books = c(
    fr = "Ce document reprend le calcul de ce poste. Il ne remplace pas le fichier comptable.",
    en = "This document repeats the calculation on this computer. It does not replace the accounting file."
  ),
  loc_amounts_stay = c(
    fr = "Les montants n'ont pas quitté ce poste.",
    en = "The amounts have not left this computer."
  ),
  loc_from_paren = c(
    fr = " (dès ",
    en = " (from "
  ),
  loc_rev_cum_fy = c(
    fr = "CA cumulé de l'exercice, EUR",
    en = "Cumulative revenue for the year, EUR"
  ),
  loc_gaps_sp = c(
    fr = " écarts. ",
    en = " gaps. "
  ),
  loc_oi_sensitivity = c(
    fr = "Sensibilité du résultat d'exploitation",
    en = "Sensitivity of operating income"
  ),
  loc_oi_sensitivity_ascii = c(
    fr = "Sensibilite du resultat d'exploitation",
    en = "Sensitivity of operating income"
  ),
  loc_no_lever_pnl = c(
    fr = "Aucun levier: pas d'hypothese active et pas de compte de resultat.",
    en = "No lever: no live assumption and no income statement."
  ),
  loc_delays = c(
    fr = "Délais",
    en = "Days"
  ),
  loc_assumptions = c(
    fr = "Hypothèses",
    en = "Assumptions"
  ),
  loc_lines_forecast_months = c(
    fr = "Lignes (mois de prévision)",
    en = "Lines (forecast months)"
  ),
  loc_scenario_failed = c(
    fr = "Le scénario n'a pas abouti.",
    en = "The scenario did not finish."
  ),
  loc_sens_written = c(
    fr = "Sensibilite ecrite dans work/sensitivity.csv. ",
    en = "Sensitivity written to work/sensitivity.csv. "
  ),
  loc_eur_together = c(
    fr = " EUR (tout ensemble, moins la somme des effets isoles).",
    en = " EUR (all together, minus the sum of the separate effects)."
  ),
  loc_the_series = c(
    fr = "La série",
    en = "The series"
  ),
  loc_no_lag_signal = c(
    fr = "Aucun décalage ne sort du hasard.",
    en = "No lag stands out from chance."
  ),
  loc_strongest_same_month = c(
    fr = "Le lien le plus fort est le même mois : la série ",
    en = "The strongest link is the same month: the series "
  ),
  loc_strongest_plus = c(
    fr = "Le lien le plus fort est à +",
    en = "The strongest link is at +"
  ),
  loc_months_series = c(
    fr = " mois : la série ",
    en = " months: the series "
  ),
  loc_moves_before = c(
    fr = " bouge avant la ligne. C'est une suggestion, pas une décision.",
    en = " moves before the line. That is a suggestion, not a decision."
  ),
  loc_instead_of_lead = c(
    fr = " au lieu de la précéder.",
    en = " instead of leading it."
  ),
  loc_ordinary_strong = c(
    fr = "0 = mois ordinaire. 1 = mois fort.",
    en = "0 = an ordinary month. 1 = a strong month."
  ),
  loc_traits_expected = c(
    fr = "Si les deux traits se superposent, ils racontent le même mois. Ici c'est attendu : ",
    en = "If the two lines sit on each other, they tell the same month. Here that is expected: "
  ),
  loc_not_a_discovery = c(
    fr = ". Ce n'est pas une découverte.",
    en = ". This is not a discovery."
  ),
  loc_if_traits = c(
    fr = "Si les deux traits se superposent, ",
    en = "If the two lines sit on each other, "
  ),
  loc_traits_diverge = c(
    fr = " racontent le même mois. S'ils divergent, le lien est faible une fois la taille retirée.",
    en = " tell the same month. If they diverge, the link is weak once size is removed."
  ),
  loc_same_size = c(
    fr = " à la même taille, pour voir s'ils bougent ensemble.",
    en = " to the same size, to see whether they move together."
  ),
  loc_axis_not_euro = c(
    fr = "L'axe vertical n'est ni des euros ni un taux. 0 est un mois ordinaire. 1 est un mois fort pour cette série, −1 un mois faible.",
    en = "The vertical axis is neither euros nor a rate. 0 is an ordinary month. 1 is a strong month for this series, −1 a weak month."
  ),
  loc_not_enough_history_ctrl = c(
    fr = "Pas assez d'historique pour ce contrôle.",
    en = "Not enough history for this check."
  ),
  loc_driver_beats_naive = c(
    fr = "Conclusion : sur ces mois, le driver annonce mieux qu'une copie du même mois l'an dernier.",
    en = "Conclusion: on these months, the driver calls the figure better than a copy of the same month last year."
  ),
  loc_driver_not_beat = c(
    fr = "Conclusion : sur ces mois, le driver n'annonce pas mieux qu'une copie du même mois l'an dernier.",
    en = "Conclusion: on these months, the driver does not call the figure better than a copy of the same month last year."
  ),
  loc_holdout_explain = c(
    fr = "On met de côté les 12 derniers mois. On regarde si le driver les aurait mieux annoncés que « le même mois l'an dernier ».",
    en = "The last 12 months are set aside. The question is whether the driver would have called them better than \"the same month last year\"."
  ),
  loc_axis_gap_of = c(
    fr = "L'axe vertical est l'écart de ",
    en = "The vertical axis is the gap in "
  ),
  loc_vs_season_euro = c(
    fr = " à son niveau de saison, en euros. Ce n'est pas le montant du mois.",
    en = " from its seasonal level, in euros. It is not the amount of the month."
  ),
  loc_trait_legend = c(
    fr = "Trait foncé : ce qui s'est passé. Trait orange : l'annonce du driver. Pointillé : le même mois de l'année d'avant.",
    en = "Dark line: what happened. Orange line: what the driver called. Dotted line: the same month the year before."
  ),
  loc_install_shiny = c(
    fr = "Installez shiny une fois: install.packages(\"shiny\")",
    en = "Install shiny once: install.packages(\"shiny\")"
  ),
  loc_copy_app = c(
    fr = "Copiez le fichier texte app.R dans le meme dossier. Ce n'est pas un binaire.",
    en = "Copy the text file app.R into the same folder. It is not a binary."
  ),
  loc_param_cols = c(
    fr = "parametres.csv doit avoir les colonnes periode, parametre, valeur.",
    en = "parametres.csv must have the columns periode, parametre, valeur."
  ),
  loc_param_missing = c(
    fr = "Parametre manquant pour ",
    en = "Missing parameter for "
  ),
  loc_mech_capacity = c(
    fr = "Capacité × utilisation × tarif",
    en = "Capacity × occupancy × rate"
  ),
  loc_mech_staff = c(
    fr = "Effectif × coût moyen",
    en = "Headcount × average cost"
  ),
  loc_mech_days = c(
    fr = "Délais clients, fournisseurs, stocks",
    en = "Customer, supplier and stock days"
  ),
  loc_mech_da_cash = c(
    fr = "Dotations et trésorerie",
    en = "Depreciation and cash"
  ),
  loc_no_path = c(
    fr = "Aucun chemin n'est indiqué pour ce fichier. ",
    en = "No path is given for this file. "
  ),
  loc_file_on_disk = c(
    fr = "Le fichier est sur le disque. ",
    en = "The file is on disk. "
  ),
  loc_file_missing_at = c(
    fr = "Le fichier attendu est introuvable à ce chemin : ",
    en = "The expected file is not at this path: "
  ),
  loc_without_role_sp = c(
    fr = " sans rôle.",
    en = " with no role."
  ),
  loc_actual_then_fc = c(
    fr = "Réalisé importé, puis mois de prévision",
    en = "Imported actuals, then forecast months"
  ),
  loc_opening_from_bs = c(
    fr = "Ouverture lue dans le fichier de bilan",
    en = "Opening read from the balance-sheet file"
  ),
  loc_days_capex_debt = c(
    fr = "Délais, investissement, dette",
    en = "Days, capital expenditure, debt"
  ),
  loc_voted_vs_actual = c(
    fr = "Le budget voté, comparé au réalisé",
    en = "The voted budget, compared with actual"
  ),
  loc_output_not_reread = c(
    fr = "Le dernier calcul n'a pas été relu pour cette sortie.",
    en = "The last calculation was not read again for this output."
  ),
  loc_read_closed_years = c(
    fr = "Lecture des exercices clos",
    en = "Reading of closed years"
  ),
  loc_annual_as_is = c(
    fr = "Le compte annuel est lu tel quel. Il n'est pas découpé en douze mois.",
    en = "The annual accounts are read as they stand. They are not split into twelve months."
  ),
  loc_months_already_closed = c(
    fr = "Mois déjà arrêtés",
    en = "Months already closed"
  ),
  loc_closed_feed_actual = c(
    fr = "Les mois clos de l'exercice en cours alimentent le réalisé. Le fichier annuel ne les remplace pas.",
    en = "Closed months of the current year feed the actuals. The annual file does not replace them."
  ),
  loc_opening_fixes = c(
    fr = "Le bilan d'ouverture fixe les stocks, créances et dettes de départ.",
    en = "The opening balance sheet sets the starting stock, receivables and payables."
  ),
  loc_budget_frozen = c(
    fr = "Le budget sert de référence figée. Il ne devient pas la prévision.",
    en = "The budget is the frozen reference. It does not become the forecast."
  ),
  loc_rule_on_no_file = c(
    fr = " est activé, mais aucun fichier ",
    en = " is on, but no file "
  ),
  loc_is_not_described = c(
    fr = " n'est décrit.",
    en = " is not described."
  ),
  loc_rule_on_missing = c(
    fr = " est activé. Il manque : ",
    en = " is on. Missing: "
  ),
  loc_calc_wrote_line = c(
    fr = " Le dernier calcul a écrit cette ligne.",
    en = " The last calculation wrote this line."
  ),
  loc_calc_wrote_shared = c(
    fr = " Le dernier calcul a écrit la ligne, sans isoler cette règle.",
    en = " The last calculation wrote the line, without isolating this rule."
  ),
  loc_no_rule_downstream = c(
    fr = " : aucune règle en aval.",
    en = ": no rule downstream."
  ),
  loc_closing_sp = c(
    fr = "Clôture ",
    en = "Closing "
  ),
  loc_cash_closed_month = c(
    fr = "Trésorerie du mois clos",
    en = "Cash of the closed month"
  ),
  loc_cash_low_open = c(
    fr = "Plus bas de trésorerie (",
    en = "Lowest cash ("
  ),
  loc_to_clear_dot = c(
    fr = " à éclaircir · ",
    en = " to clear · "
  ),
  loc_to_follow = c(
    fr = " à suivre",
    en = " to follow"
  ),
  loc_files_newer = c(
    fr = "Fichiers plus récents : ",
    en = "Newer files: "
  ),
  loc_cash_under_floor = c(
    fr = "Trésorerie sous le plancher en ",
    en = "Cash under the floor in "
  ),
  loc_config_missing = c(
    fr = "Le fichier de configuration est introuvable: ",
    en = "The configuration file cannot be found: "
  ),
  loc_config_place = c(
    fr = "Placez fpa-config.json (telecharge depuis l'assistant) dans config/.",
    en = "Put fpa-config.json (downloaded from the setup) in config/."
  ),
  loc_config_bad_json = c(
    fr = "Le fichier de configuration n'est pas un JSON valide (",
    en = "The configuration file is not valid JSON ("
  ),
  loc_config_too_old = c(
    fr = ") est trop ancien pour la configuration (exige ",
    en = ") is too old for the configuration (requires "
  ),
  loc_module_list_missing = c(
    fr = "Liste des modules introuvable : ",
    en = "Module list cannot be found: "
  ),
  loc_diag_pnl_hist = c(
    fr = "Compte de resultat (historique)",
    en = "Income statement (history)"
  ),
  loc_diag_pnl_month = c(
    fr = "Compte de resultat mensuel",
    en = "Monthly income statement"
  ),
  loc_step_41 = c(
    fr = "etape 4.1  Compte de resultat",
    en = "step 4.1  Income statement"
  ),
  loc_step_44 = c(
    fr = "etape 4.4  Budget",
    en = "step 4.4  Budget"
  ),
  loc_excel_not_setup = c(
    fr = "EXCEL (le fichier), pas l'assistant",
    en = "EXCEL (the file), not the setup"
  ),
  loc_file_not_at_json = c(
    fr = "Le fichier est introuvable a l'emplacement declare dans le JSON.",
    en = "The file is not at the place declared in the JSON."
  ),
  loc_put_file_here = c(
    fr = "Placez le fichier ici: ",
    en = "Put the file here: "
  ),
  loc_fix_path_step = c(
    fr = "Si le fichier existe ailleurs, corrigez le chemin a l'etape Chemins, puis telechargez a nouveau fpa-config.json.",
    en = "If the file is somewhere else, correct the path at the Paths step, then download fpa-config.json again."
  ),
  loc_readxl_missing = c(
    fr = "Le fichier est un Excel, mais le paquet R 'readxl' n'est pas installe. Ce n'est pas un probleme de mapping.",
    en = "The file is an Excel workbook, but the R package readxl is not installed. This is not a mapping problem."
  ),
  loc_install_readxl = c(
    fr = "Dans R: install.packages(\"readxl\")  puis relancez.",
    en = "In R: install.packages(\"readxl\") then start again."
  ),
  loc_openxlsx_missing = c(
    fr = "Le fichier est un Excel. readxl le lit. openxlsx ecrit dedans, et il n'est pas installe. Ce n'est pas un probleme de mapping.",
    en = "The file is an Excel workbook. readxl reads it. openxlsx writes into it, and it is not installed. This is not a mapping problem."
  ),
  loc_install_openxlsx = c(
    fr = "Avant de relancer, dans R: install.packages(\"openxlsx\")",
    en = "Before you start again, in R: install.packages(\"openxlsx\")"
  ),
  loc_file_unreadable = c(
    fr = "Le fichier existe mais n'a pas pu etre lu. ",
    en = "The file is there but could not be read. "
  ),
  loc_resave_utf8 = c(
    fr = "Ouvrez le fichier, enregistrez-le en .xlsx ou .csv UTF-8, puis relancez.",
    en = "Open the file, save it as .xlsx or .csv UTF-8, then start again."
  ),
  loc_json_cols_missing = c(
    fr = "Le JSON nomme des colonnes absentes du fichier: ",
    en = "The JSON names columns that are not in the file: "
  ),
  loc_rerun_columns = c(
    fr = "A l'etape Colonnes & lignes, relancez l'assistant sur ce fichier, puis telechargez fpa-config.json.",
    en = "At the Columns & lines step, run the setup again on this file, then download fpa-config.json."
  ),
  loc_annual_pnl = c(
    fr = "Ce fichier est un compte de resultat annuel: une ligne par compte, les montants dans des colonnes d'exercice ",
    en = "This file is an annual income statement: one row per account, the amounts in year columns "
  ),
  loc_not_month_rows = c(
    fr = "(Montant N, Montant N-1, ou Montant). Ce n'est pas un fichier en lignes de mois.",
    en = "(Amount N, Amount N-1, or Amount). This is not a file in month rows."
  ),
  loc_not_month_rows_sp = c(
    fr = "(Montant N, Montant N-1, ou Montant). Ce n'est pas un fichier en lignes de mois. ",
    en = "(Amount N, Amount N-1, or Amount). This is not a file in month rows. "
  ),
  loc_amount_cols_on_period = c(
    fr = "Les colonnes de montant suivantes sont mappees sur 'Periode (dans les lignes)': ",
    en = "These amount columns are mapped to 'Period (down the rows)': "
  ),
  loc_json_long_mode = c(
    fr = "Le JSON est en mode long (une colonne Periode + une colonne Montant).",
    en = "The JSON is in long mode (one Period column + one Amount column)."
  ),
  loc_pick_period_amount = c(
    fr = "Pour chaque colonne de montant, choisissez \"Montants d'une periode\" et indiquez la fin d'exercice (N, 2026, 2026-12-31). Une periode n'est pas forcement un mois.",
    en = "For each amount column, choose \"Amounts of one period\" and give the year end (N, 2026, 2026-12-31). A period is not necessarily a month."
  ),
  loc_dont_pick_period = c(
    fr = "Ne choisissez pas \"Periode (dans les lignes)\" sur une colonne de montant: ce role est pour une colonne dont les cellules sont des dates.",
    en = "Do not choose \"Period (down the rows)\" on an amount column: that role is for a column whose cells are dates."
  ),
  loc_switch_wide = c(
    fr = "Passez le fichier en mode large, puis telechargez a nouveau fpa-config.json (etape Revue).",
    en = "Switch the file to wide mode, then download fpa-config.json again (Review step)."
  ),
  loc_period_no_year_end = c(
    fr = "Les colonnes de montant sont bien des periodes, mais aucune date de fin d'exercice n'est renseignee ",
    en = "The amount columns are periods, but no year-end date is filled in "
  ),
  loc_period_empty_note = c(
    fr = "(column_period / periods vides, et aucun millesime dans le titre de colonne).",
    en = "(column_period / periods empty, and no year in the column title)."
  ),
  loc_give_year_end = c(
    fr = "Indiquez la date de fin d'exercice sous chaque colonne de montant (ex. 2025-12-31).",
    en = "Give the year-end date under each amount column (for example 2025-12-31)."
  ),
  loc_excel_total_assets = c(
    fr = "Dans Excel, Total Actif (",
    en = "In Excel, total assets ("
  ),
  loc_not_equal_gap = c(
    fr = ") ne sont pas egaux. Ecart: ",
    en = ") are not equal. Gap: "
  ),
  loc_fix_bs_file = c(
    fr = "Corrigez les montants dans le fichier bilan, enregistrez, puis relancez. L'assistant n'y peut rien.",
    en = "Correct the amounts in the balance-sheet file, save, then start again. The setup cannot do it."
  ),
  loc_excel_totals_unread = c(
    fr = "Les totaux Excel n'ont pas pu etre lus; le diagnostic porte sur le mapping JSON.",
    en = "The Excel totals could not be read; the check is on the JSON mapping."
  ),
  loc_json_partial_bs = c(
    fr = "Le JSON n'importe qu'une partie du bilan. Actif lu: ",
    en = "The JSON imports only part of the balance sheet. Assets read: "
  ),
  loc_lines_excluded = c(
    fr = "Lignes avec un montant, exclues par le plan (",
    en = "Lines with an amount, excluded by the plan ("
  ),
  loc_step43_section = c(
    fr = "A l'etape 4.3, une ligne avec un montant ne doit pas rester SECTION ni SUBTOTAL (sauf les totaux de controle).",
    en = "At step 4.3, a line with an amount must not stay SECTION or SUBTOTAL (except the control totals)."
  ),
  loc_step43_example = c(
    fr = "Exemple: Dettes -> Dettes financieres long terme (LT_DEBT). Resultat reporte -> Capitaux propres (EQUITY).",
    en = "Example: debt -> long-term financial debt (LT_DEBT). Retained earnings -> equity (EQUITY)."
  ),
  loc_imported_unbalanced = c(
    fr = "Les lignes importees ne balancent pas. Verifiez les montants dans Excel, puis le plan a l'etape 4.3.",
    en = "The imported lines do not balance. Check the amounts in Excel, then the plan at step 4.3."
  ),
  loc_extra_file = c(
    fr = "fichier extra",
    en = "extra file"
  ),
  loc_extra_file_open = c(
    fr = "Fichier extra (",
    en = "Extra file ("
  ),
  loc_path_missing_series = c(
    fr = "Chemin declare, fichier introuvable. Le moteur continuera sans cette serie.",
    en = "Path declared, file not found. The engine will continue without this series."
  ),
  loc_file_mapping_ok = c(
    fr = "Fichier present, mapping utilisable.",
    en = "File present, mapping usable."
  ),
  loc_file_fmt = c(
    fr = "          fichier : %s\n",
    en = "          file: %s\n"
  ),
  loc_verdict_block = c(
    fr = "  Verdict : %d point(s) bloquant(s)%s. Calcul non lance.\n",
    en = "  Verdict: %d blocking point(s)%s. Calculation not started.\n"
  ),
  loc_fix_in_order = c(
    fr = "  Corrigez dans l'ordre des details, puis relancez le moteur.\n",
    en = "  Correct them in the order of the detail, then start the engine again.\n"
  ),
  loc_verdict_warn = c(
    fr = "  Verdict : aucun bloquant, %d attention. Calcul lance.\n",
    en = "  Verdict: nothing blocking, %d warning(s). Calculation started.\n"
  ),
  loc_verdict_ok = c(
    fr = "  Verdict : fichiers et reglages utilisables. Calcul lance.\n",
    en = "  Verdict: files and settings are usable. Calculation started.\n"
  ),
  loc_grade_short_budget = c(
    fr = "Pas assez d'historique pour un budget de drivers.",
    en = "Not enough history for a driver budget."
  ),
  loc_grade_months_hist = c(
    fr = " mois d'historique. Cela soutient environ ",
    en = " months of history. That supports about "
  ),
  loc_grade_confirmed = c(
    fr = " drivers confirmes statistiquement. Les autres resteront des hypotheses ",
    en = " drivers confirmed by the statistics. The others stay assumptions "
  ),
  loc_grade_or_rules = c(
    fr = "ou des regles mecaniques.",
    en = "or mechanical rules."
  ),
  loc_no_mech_euro = c(
    fr = "Aucun mecanisme ne relie cette serie au compte de resultat. L'euro ne se deduit pas d'une correlation.",
    en = "No mechanism links this series to the income statement. The euro amount is not derived from a correlation."
  ),
  loc_no_forecast_month = c(
    fr = "Pas de mois de prevision: l'euro ne bouge pas tant que l'horizon est a zero.",
    en = "No forecast month: the euro amount does not move while the horizon is zero."
  ),
  loc_engine_could_not = c(
    fr = "Le moteur n'a pas pu deplacer cette ligne.",
    en = "The engine could not move this line."
  ),
  loc_gap_engine_span = c(
    fr = "Ecart calcule par le moteur, une ligne a la fois, de prudent a ambitieux.",
    en = "Gap calculated by the engine, one line at a time, from cautious to ambitious."
  ),
  loc_hist_too_short_24 = c(
    fr = "Historique trop court pour un driver confirme (24 mois minimum).",
    en = "History too short for a confirmed driver (24 months minimum)."
  ),
  loc_series_copies = c(
    fr = "La serie recopie une autre serie. Ce n'est pas un driver.",
    en = "The series copies another series. It is not a driver."
  ),
  loc_series_sum = c(
    fr = "La serie est une somme d'autres series. Ce n'est pas un driver.",
    en = "The series is a sum of other series. It is not a driver."
  ),
  loc_mech_not_proof = c(
    fr = "Mecanisme: la serie compose une ligne. Le prior n'est pas reduit, et la correlation n'est pas une preuve.",
    en = "Mechanism: the series makes up a line. The prior is not reduced, and the correlation is not proof."
  ),
  loc_link_not_three = c(
    fr = "Un lien apparait, mais il ne gagne pas les trois tests en meme temps.",
    en = "A link appears, but it does not pass the three tests at the same time."
  ),
  loc_sign_reversed = c(
    fr = "ou le sens est inverse. Hors prevision.",
    en = "or the sign is reversed. Outside the forecast."
  ),
  loc_formula_col_missing = c(
    fr = "Une colonne de la formule est absente des fichiers.",
    en = "A column of the formula is missing from the files."
  ),
  loc_formula_checked = c(
    fr = "Formule verifiee sur les mois clos.",
    en = "Formula checked on the closed months."
  ),
  loc_formula_few_months = c(
    fr = "Moins de 8 mois communs : formule non verifiable.",
    en = "Fewer than 8 months in common: the formula cannot be checked."
  ),
  loc_line_in_bindings = c(
    fr = "Ligne declaree dans driver_bindings.",
    en = "Line declared in driver_bindings."
  ),
  loc_no_driver = c(
    fr = "aucun driver",
    en = "no driver"
  ),
  loc_euro_unavailable = c(
    fr = "Impact en euros : non disponible tant qu'aucun mecanisme ne relie la serie.",
    en = "Euro impact: not available until a mechanism links the series."
  ),
  loc_not_measured = c(
    fr = "Non mesuré",
    en = "Not measured"
  ),
  loc_redate_in = c(
    fr = " re-datages aleatoires de la meme serie, cet alignement est dans les ",
    en = " random re-datings of the same series, this alignment is in the "
  ),
  loc_percent_best = c(
    fr = " pour cent. Le meilleur hasard atteint ",
    en = " percent. The best chance reaches "
  ),
  loc_need_24_holdout = c(
    fr = "Il faut au moins 24 mois pour un holdout contre le naif saisonnier.",
    en = "At least 24 months are required for a holdout against the seasonal naive."
  ),
  loc_holdout_few_points = c(
    fr = "Pas assez de points d'apprentissage pour le holdout.",
    en = "Not enough training points for the holdout."
  ),
  loc_no_mech_zero = c(
    fr = " n'a ni mecanisme ni hypothese. Le zero n'est pas une prevision.",
    en = " has neither a mechanism nor an assumption. Zero is not a forecast."
  ),
  loc_no_budget_freeze = c(
    fr = "Pas de budget mensuel a figer.",
    en = "No monthly budget to freeze."
  ),
  loc_no_pnl_archive = c(
    fr = "Pas de compte de resultat a archiver.",
    en = "No income statement to archive."
  ),
  loc_stop_missing = c(
    fr = "Arrêt introuvable: ",
    en = "Close point not found: "
  ),
  loc_headers_fixed = c(
    fr = "En-tetes imposes: ne les renommez pas.",
    en = "Imposed headers: do not rename them."
  ),
  loc_figures_fictional = c(
    fr = "Les chiffres sont fictifs. Remplacez-les par les votres,",
    en = "The figures are fictional. Replace them with yours,"
  ),
  loc_save_at_config_path = c(
    fr = "puis enregistrez le fichier au chemin indique dans fpa-config.json.",
    en = "then save the file at the path given in fpa-config.json."
  ),
  loc_no_program_site = c(
    fr = "Aucun programme n'a ete telecharge depuis le site.",
    en = "No programme has been downloaded from the site."
  ),
  loc_files_written = c(
    fr = " fichier(s) ecrit(s), ",
    en = " file(s) written, "
  ),
  loc_same_month_no_effect = c(
    fr = "meme mois (aucun effet)",
    en = "same month (no effect)"
  ),
  loc_first_open_required = c(
    fr = "). Premier mois ouvert requis.",
    en = "). The first open month is required."
  ),
  loc_no_finer_line = c(
    fr = "Pas de ligne plus precise dans le nom. A confirmer: chiffre d'affaires propose par defaut.",
    en = "No finer line in the name. To confirm: revenue proposed by default."
  ),
  loc_label_col_missing = c(
    fr = "Colonne libelle introuvable dans le fichier large.",
    en = "Label column not found in the wide file."
  ),
  loc_assistant_amount_period = c(
    fr = "Dans l'assistant, etape Colonnes & lignes, pour chaque colonne de montant choisissez ",
    en = "In the setup, Columns & lines step, for each amount column choose "
  ),
  loc_amount_period_quote = c(
    fr = "\"Montants d'une periode\" et indiquez la date de fin d'exercice (N, 2026, mois...). ",
    en = "\"Amounts of one period\" and give the year-end date (N, 2026, month...). "
  ),
  loc_period_not_always_month = c(
    fr = "Une periode n'est pas forcement un mois. ",
    en = "A period is not necessarily a month. "
  ),
  loc_dont_period_on_amount = c(
    fr = "Ne choisissez pas Periode (dans les lignes) sur une colonne de montant.",
    en = "Do not choose Period (down the rows) on an amount column."
  ),
  loc_file_of_the = c(
    fr = "Le fichier du ",
    en = "The file for "
  ),
  loc_check_config_path = c(
    fr = ". Verifiez le chemin dans fpa-config.json.",
    en = ". Check the path in fpa-config.json."
  ),
  loc_cannot_read_file = c(
    fr = "Impossible de lire le fichier du ",
    en = "Cannot read the file for "
  ),
  loc_duplicate_months = c(
    fr = "Des mois sont en double dans le compte de resultat (meme ligne, meme mois).",
    en = "Some months are duplicated in the income statement (same line, same month)."
  ),
  loc_months_missing_pnl = c(
    fr = "Des mois manquent dans le compte de resultat: ",
    en = "Months are missing from the income statement: "
  ),
  loc_pnl_is_annual = c(
    fr = "Le compte de resultat est un historique annuel (annees closes), ",
    en = "The income statement is an annual history (closed years), "
  ),
  loc_bring_a_living_file = c(
    fr = "Pour faire avancer la prevision, apportez un fichier tenu a jour (mois ou plus souvent).",
    en = "To move the forecast forward, bring a file that is kept up to date (monthly or more often)."
  ),
  loc_cogs_negative = c(
    fr = "Les achats (COGS) sont negatifs pour ",
    en = "Purchases (COGS) are negative for "
  ),
  loc_budget_line_unmatched = c(
    fr = "Ligne budget sans correspondance dans le plan: \"",
    en = "Budget line with no match in the plan: \""
  ),
  loc_actual_without_budget = c(
    fr = "Ligne de realise sans budget: ",
    en = "Actual line with no budget: "
  ),
  loc_budget_read_zero = c(
    fr = " sur la periode lue. Budget lu a zero, l'ecart reste visible.",
    en = " over the period read. Budget read as zero, the gap stays visible."
  ),
  loc_no_rolling = c(
    fr = "Aucun suivi du rolling n'est declare. ",
    en = "No rolling track is declared. "
  ),
  loc_annual_does_not_move = c(
    fr = "L'historique annuel (une ligne par compte, les exercices en colonnes) ne fait pas avancer la prevision. ",
    en = "The annual history (one row per account, the years in columns) does not move the forecast forward. "
  ),
  loc_bring_one_living = c(
    fr = "Apportez un fichier tenu a jour (un seul fichier vivant, ou un second fichier de suivi).",
    en = "Bring a file that is kept up to date (one living file, or a second tracking file)."
  ),
  loc_remains_to_explain = c(
    fr = "reste à expliquer",
    en = "still to explain"
  ),
  loc_receivables = c(
    fr = "créances clients",
    en = "receivables"
  ),
  loc_stock_turn = c(
    fr = "Rotation des stocks",
    en = "Stock turn"
  ),
  loc_wc_open_release_short = c(
    fr = "Dénouement BFR d'ouverture",
    en = "Opening working-capital release"
  ),
  loc_scenario_hypothesis = c(
    fr = "hypothèse du scénario",
    en = "scenario assumption"
  ),
  loc_residual_unallocated = c(
    fr = "Résiduel non ventilé",
    en = "Residual not allocated"
  ),
  loc_gap_unexplained = c(
    fr = "écart non expliqué",
    en = "unexplained gap"
  ),
  loc_prior_month = c(
    fr = "Mois précédent ",
    en = "Previous month "
  ),
  loc_same_month_ly = c(
    fr = "Même mois l'an dernier ",
    en = "Same month last year "
  ),
  loc_eur_gap_sp = c(
    fr = " EUR, écart ",
    en = " EUR, gap "
  ),
  loc_delay_word = c(
    fr = "délai",
    en = "days"
  ),
  loc_year_word = c(
    fr = "année",
    en = "year"
  ),
  loc_subtotal_book = c(
    fr = "Sous-total / total (controle, avec niveau)",
    en = "Subtotal / total (control, with a level)"
  ),
  month_1 = c(
    fr = "janvier",
    en = "January"
  ),
  month_2 = c(
    fr = "février",
    en = "February"
  ),
  month_3 = c(
    fr = "mars",
    en = "March"
  ),
  month_4 = c(
    fr = "avril",
    en = "April"
  ),
  month_5 = c(
    fr = "mai",
    en = "May"
  ),
  month_6 = c(
    fr = "juin",
    en = "June"
  ),
  month_7 = c(
    fr = "juillet",
    en = "July"
  ),
  month_8 = c(
    fr = "août",
    en = "August"
  ),
  month_9 = c(
    fr = "septembre",
    en = "September"
  ),
  month_10 = c(
    fr = "octobre",
    en = "October"
  ),
  month_11 = c(
    fr = "novembre",
    en = "November"
  ),
  month_12 = c(
    fr = "décembre",
    en = "December"
  ),
  month_s_1 = c(
    fr = "janv.",
    en = "Jan"
  ),
  month_s_2 = c(
    fr = "févr.",
    en = "Feb"
  ),
  month_s_4 = c(
    fr = "avr.",
    en = "Apr"
  ),
  month_s_7 = c(
    fr = "juil.",
    en = "Jul"
  ),
  month_s_9 = c(
    fr = "sept.",
    en = "Sep"
  ),
  month_s_10 = c(
    fr = "oct.",
    en = "Oct"
  ),
  month_s_11 = c(
    fr = "nov.",
    en = "Nov"
  ),
  month_s_12 = c(
    fr = "déc.",
    en = "Dec"
  ),
  month_s_3 = c(fr = "mars", en = "Mar"),
  month_s_5 = c(fr = "mai", en = "May"),
  month_s_6 = c(fr = "juin", en = "Jun"),
  month_s_8 = c(fr = "août", en = "Aug"),
  nav_tab_map = c(fr = "Cartographie", en = "Map"),
  nav_tab_bs = c(fr = "Bilan", en = "Balance sheet"),
  nav_tab_anomalies = c(fr = "Anomalies", en = "Anomalies"),
  nav_tab_close = c(fr = "Clôture", en = "Month close"),
  nav_tab_impact = c(fr = "Impact", en = "Impact"),
  nav_tab_journal = c(fr = "Journal", en = "Journal"),
  nav_tab_drivers = c(fr = "Drivers", en = "Drivers"),
  loc_mech_capex_cal = c(fr = "Calendrier d'investissement", en = "Capital-expenditure calendar"),
  loc_mech_debt_cal = c(fr = "Calendrier de dette", en = "Debt calendar"),
  loc_opening_bs = c(fr = "Bilan d'ouverture", en = "Opening balance sheet"),
  loc_saved_clock = c(fr = "%d/%m/%Y à %H:%M", en = "%d/%m/%Y at %H:%M"),
  loc_series_col = c(fr = "Séries", en = "Series"),
  loc_subtotals = c(fr = "Soldes intermédiaires", en = "Subtotals"),
  loc_frame_track = c(fr = "Suivi", en = "Tracking"),
  loc_frame_plan = c(fr = "Plan", en = "Plan"),
  loc_hist_blurb = c(
    fr = "Exercices déjà clos. Lus tels quels, pas transformés en mois.",
    en = "Closed years. Read as they stand, not turned into months."
  ),
  loc_track_blurb = c(
    fr = "Mois déjà arrêtés, volumes, taux. Eux font avancer la prévision.",
    en = "Closed months, volumes, rates. These move the forecast."
  ),
  loc_plan_blurb = c(
    fr = "Budget voté. Figé. Il sert de comparaison, pas de prévision.",
    en = "Voted budget. Frozen. It is the comparison, not the forecast."
  ),
  loc_app_missing = c(
    fr = "app.R est introuvable a cote de fpa-config.json. ",
    en = "app.R was not found next to fpa-config.json. "
  ),
  loc_run_engine_first = c(
    fr = ". Lancez d'abord fpa-engine.R.",
    en = ". Run fpa-engine.R first."
  ),
  pm_lang_title = c(fr = "Langue", en = "Language"),
  pm_lang_lead = c(
    fr = "La langue des écrans. Les comptes restent dans la langue des livres.",
    en = "The language of the screens. The accounts stay in the language of the books."
  ),
  pm_lang_aria = c(fr = "Langue de l'écran", en = "Screen language"),
  pm_lang_save_fail = c(
    fr = "La langue n'a pas été enregistrée.",
    en = "The language was not saved."
  ),
  nav_tab_ai = c(fr = "Atelier de l'analyste", en = "Analyst workshop"),
  ai_title = c(fr = "Atelier de l'analyste", en = "Analyst workshop"),
  ai_workshop_lead = c(
    fr = "Posez des questions et enchaînez les prompts ici, sans produire de rapport. La note plus bas est facultative.",
    en = "Ask questions and follow up on prompts here, without producing a report. The note below is optional."
  ),
  ai_lead = c(
    fr = "L'application calcule sans cela. Si vous l'activez, les chiffres partent chez le fournisseur de votre clé.",
    en = "The application calculates without this. If you turn it on, the figures go to the provider of your key."
  ),
  ai_off = c(fr = "Désactivé", en = "Deactivated"),
  ai_on = c(fr = "Activé", en = "Activated"),
  ai_provider = c(fr = "Fournisseur", en = "Provider"),
  ai_model = c(fr = "Modèle", en = "Model"),
  ai_address = c(fr = "Adresse", en = "Address"),
  ai_key = c(fr = "Clé API", en = "API key"),
  ai_key_save = c(fr = "Enregistrer la clé", en = "Save the key"),
  ai_key_remove = c(fr = "Retirer la clé", en = "Remove the key"),
  ai_key_none = c(fr = "Aucune clé sur cet ordinateur.", en = "No key on this computer."),
  ai_key_saved = c(fr = "Clé enregistrée sur cet ordinateur : %s", en = "Key saved on this computer: %s"),
  ai_mode_sans = c(fr = "Sans montants", en = "Without amounts"),
  ai_mode_avec = c(fr = "Avec montants", en = "With amounts"),
  ai_mode_lead = c(
    fr = "Sans montants, les euros deviennent un pourcentage du chiffre d'affaires annuel. Avec montants, les totaux mensuels partent en euros. Jamais les fichiers bruts.",
    en = "Without amounts, euros become a percentage of annual revenue. With amounts, the monthly totals go out in euros. Never the raw files."
  ),
  ai_test = c(fr = "Essayer la connexion", en = "Test the connection"),
  ai_credits = c(fr = "Acheter des crédits", en = "Buy credits"),
  ai_keys_link = c(fr = "Créer une clé", en = "Create a key"),
  ai_ask = c(fr = "Demander", en = "Ask"),
  ai_clear = c(fr = "Effacer", en = "Clear"),
  ai_question = c(fr = "Question", en = "Question"),
  ai_sent = c(fr = "Ce qui a été envoyé", en = "What was sent"),
  ai_copy = c(fr = "Copier", en = "Copy"),
  ai_report_start = c(fr = "Démarrer une note", en = "Start a note"),
  ai_to_report = c(fr = "Mettre dans le rapport", en = "Put into the report"),
  ai_sealed_old = c(fr = "Note scellée (ancienne).", en = "Sealed note (older)."),
  ai_report_add = c(fr = "Ajouter cette réponse", en = "Add this answer"),
  ai_report_final = c(fr = "Mettre en forme", en = "Finalize"),
  ai_report_seal = c(fr = "Sceller", en = "Seal"),
  ai_report_download = c(fr = "Télécharger", en = "Download"),
  ai_report_folder = c(fr = "Ouvrir le dossier", en = "Open the folder"),
  ai_copied = c(fr = "Copié", en = "Copied"),
  ai_answer_head = c(fr = "Réponse de l'analyste", en = "Analyst answer"),
  ai_report_head = c(
    fr = "Note de l'analyste",
    en = "Analyst note"
  ),
  ai_report_default = c(fr = "Rapport de gestion", en = "Management report"),
  ai_report_lead = c(
    fr = "Un rapport réunit, dans l'ordre, les réponses que vous retenez. C'est un fichier texte enregistré dans le dossier du projet, sous reports/analyste. Il sert à garder la trace d'une revue et à la transmettre. Il ne quitte pas cet ordinateur, sauf à l'étape Mise en forme, qui l'envoie au fournisseur de votre clé.",
    en = "A report collects, in order, the answers you keep. It is a text file saved in the project folder, under reports/analyste. It keeps a record of a review and lets you pass it on. It stays on this computer, except at the Formatting step, which sends it to the provider of your key."
  ),
  ai_report_title_ph = c(
    fr = "Titre du rapport, par exemple Revue de septembre",
    en = "Report title, for example September review"
  ),
  ai_step1 = c(fr = "Titre", en = "Title"),
  ai_step2 = c(fr = "Réponses", en = "Answers"),
  ai_step3 = c(fr = "Mise en forme", en = "Formatting"),
  ai_step4 = c(fr = "Dans le rapport", en = "Into the report"),
  ai_step1_lead = c(
    fr = "Ceci est le titre, en tête du rapport. Le nom du fichier est créé tout seul, avec la date et l'heure.",
    en = "This is the title, at the top of the report. The file name is created on its own, with the date and the time."
  ),
  ai_step2_lead = c(
    fr = "Chaque réponse retenue devient une section. Vous pouvez en ajouter plusieurs avant la mise en forme.",
    en = "Each answer you keep becomes a section. You can add several before formatting."
  ),
  ai_step3_lead = c(fr = "L'analyste réécrit les sections en une note. Les originaux restent en annexe.", en = "The analyst rewrites the sections as one note. The originals stay in an appendix."),
  ai_step4_lead = c(
    fr = "Facultatif. La note entre dans le PDF. Vous pouvez continuer à poser des questions sans cette étape.",
    en = "Optional. The note goes into the PDF. You can keep asking questions without this step."
  ),
  ai_step_none = c(fr = "Aucun rapport ouvert.", en = "No report open."),
  ai_step_open = c(fr = "Titre : %s", en = "Title: %s"),
  ai_step_added = c(fr = "Cette réponse est déjà dans le rapport.", en = "This answer is already in the report."),
  ai_step_new = c(fr = "Dernier rapport scellé : %s. Vous pouvez en démarrer un autre.", en = "Last sealed report: %s. You can start another."),
  ai_step_after = c(fr = "Après l'étape %d.", en = "After step %d."),
  ai_step_ask = c(fr = "Posez une question, puis ajoutez la réponse.", en = "Ask a question, then add the answer."),
  ai_step_count = c(fr = "Sections : %d", en = "Sections: %d"),
  ai_step_batch_done = c(
    fr = "Sections : %d. La série les a déjà ajoutées.",
    en = "Sections: %d. The set already added them."
  ),
  ai_step_ready = c(fr = "Prêt.", en = "Ready."),
  ai_step_done = c(fr = "Fait.", en = "Done."),
  ai_step_optional = c(fr = "Possible dès maintenant, ou après la mise en forme.", en = "Possible now, or after formatting."),
  ai_step_more = c(
    fr = "Sections : %d. Un autre prompt à gauche, puis ajoutez la réponse.",
    en = "Sections: %d. Another prompt on the left, then add the answer."
  ),
  ai_step_more_in = c(
    fr = "Sections : %d. Celle-ci y est déjà. Un autre prompt à gauche, puis ajoutez.",
    en = "Sections: %d. This one is already in. Another prompt on the left, then add."
  ),
  ai_step_later = c(
    fr = "Quand les réponses vous suffisent.",
    en = "When you have enough answers."
  ),
  ai_report_file = c(fr = "Fichier généré", en = "Generated file"),
  ai_prompt_save = c(fr = "Enregistrer ce prompt", en = "Save this prompt"),
  ai_prompt_save_title = c(fr = "Nommer ce prompt", en = "Name this prompt"),
  ai_prompt_nick = c(fr = "Nom ou surnom", en = "Name or nickname"),
  ai_prompt_nick_ph = c(fr = "Par exemple Revue du mois", en = "For example Month review"),
  ai_prompt_save_ok = c(fr = "Enregistrer", en = "Save"),
  ai_prompt_cancel = c(fr = "Annuler", en = "Cancel"),
  ai_prompt_need = c(fr = "Écrivez d'abord la question.", en = "Write the question first."),
  ai_prompt_need_name = c(fr = "Donnez un nom au prompt.", en = "Give the prompt a name."),
  ai_prompt_saved = c(fr = "Prompt enregistré.", en = "Prompt saved."),
  ai_prompt_cluster = c(fr = "Prompts enregistrés", en = "User-built prompts"),
  ai_prompt_empty = c(fr = "Aucun prompt enregistré.", en = "No saved prompt."),
  ai_prompt_drop = c(fr = "Retirer", en = "Remove"),
  ai_report_nofile = c(fr = "Le fichier apparaît ici dès l'étape 1.", en = "The file appears here after step 1."),
  ai_report_erase = c(fr = "Effacer ce rapport", en = "Erase this report"),
  ai_report_erase_title = c(fr = "Effacer ce rapport ?", en = "Erase this report?"),
  ai_report_erase_body = c(
    fr = "Êtes-vous sûr ? Le fichier %s sera supprimé. Vous pourrez ensuite en démarrer un autre.",
    en = "Are you sure? The file %s will be deleted. You can then start another one."
  ),
  ai_report_erase_ok = c(fr = "Effacer", en = "Erase"),
  ai_fav_title = c(fr = "Favori", en = "Favorite"),
  ai_fav_lead = c(
    fr = "La case Favori retient le prompt pour une série. Le nom du prompt le lance seul.",
    en = "The Favorite box keeps the prompt for a set. The prompt name runs it on its own."
  ),
  ai_prompts_head = c(fr = "Prompts prêts", en = "Ready-made prompts"),
  ai_prompts_expand = c(fr = "Tout ouvrir", en = "Expand all"),
  ai_prompts_collapse = c(fr = "Tout fermer", en = "Collapse all"),
  ai_sets_head = c(fr = "Séries de prompts", en = "Prompt sets"),
  ai_sets_lead = c(
    fr = "Cochez des favoris ci-dessus, puis composez une série ordonnée. Un clic lance les questions une par une dans un nouveau rapport, puis la mise en forme.",
    en = "Tick favorites above, then build an ordered set. One click runs the questions one by one into a new report, then formatting."
  ),
  ai_sets_pick = c(fr = "Série", en = "Set"),
  ai_sets_none = c(fr = "Aucune série", en = "No set"),
  ai_sets_new = c(fr = "Nouvelle série", en = "New set"),
  ai_sets_rename = c(fr = "Renommer", en = "Rename"),
  ai_sets_drop = c(fr = "Supprimer", en = "Delete"),
  ai_sets_new_title = c(fr = "Nommer la série", en = "Name the set"),
  ai_sets_rename_title = c(fr = "Renommer la série", en = "Rename the set"),
  ai_sets_name = c(fr = "Nom de la série", en = "Set name"),
  ai_sets_name_ph = c(fr = "Par exemple Revue mensuelle", en = "For example Monthly review"),
  ai_sets_need_name = c(fr = "Donnez un nom à la série.", en = "Give the set a name."),
  ai_sets_drop_title = c(fr = "Supprimer cette série ?", en = "Delete this set?"),
  ai_sets_drop_body = c(
    fr = "Êtes-vous sûr ? La série %s sera supprimée. Les favoris restent.",
    en = "Are you sure? The set %s will be deleted. Favorites stay."
  ),
  ai_sets_drop_ok = c(fr = "Supprimer", en = "Delete"),
  ai_sets_empty = c(fr = "Cette série est vide. Ajoutez un favori.", en = "This set is empty. Add a favorite."),
  ai_sets_no_fav = c(
    fr = "Cochez d'abord des prompts pour les mettre en favoris.",
    en = "First tick prompts to mark them as favorites."
  ),
  ai_sets_add = c(fr = "Ajouter un favori", en = "Add a favorite"),
  ai_sets_add_go = c(fr = "Ajouter", en = "Add"),
  ai_sets_add_all = c(fr = "Ajouter tous les favoris", en = "Add all favorites"),
  ai_sets_add_all_done = c(fr = "%d favoris ajoutés à la série.", en = "%d favorites added to the set."),
  ai_sets_add_all_none = c(
    fr = "Ces favoris sont déjà dans la série.",
    en = "These favorites are already in the set."
  ),
  ai_sets_add_all_need = c(fr = "Choisissez d'abord une série.", en = "Choose a set first."),
  ai_sets_up = c(fr = "Monter", en = "Move up"),
  ai_sets_down = c(fr = "Descendre", en = "Move down"),
  ai_sets_top = c(fr = "Mettre en tête", en = "Send to top"),
  ai_sets_bottom = c(fr = "Mettre en bas", en = "Send to bottom"),
  ai_sets_drag = c(fr = "Glisser pour réordonner", en = "Drag to reorder"),
  ai_sets_remove = c(fr = "Retirer de la série", en = "Remove from set"),
  ai_sets_off_fav = c(fr = "retiré des favoris", en = "removed from favorites"),
  ai_sets_missing = c(fr = "prompt introuvable", en = "prompt missing"),
  ai_batch_run = c(fr = "Lancer la série", en = "Run the set"),
  ai_batch_stop = c(fr = "Arrêter", en = "Stop"),
  ai_batch_resume = c(fr = "Reprendre", en = "Resume"),
  ai_batch_plan = c(
    fr = "%d prompts · %d appels à votre fournisseur (%d réponses + la mise en forme)",
    en = "%d prompts · %d calls to your provider (%d answers + formatting)"
  ),
  ai_batch_progress = c(
    fr = "Réponses enregistrées : %d sur %d. En cours : %s",
    en = "Answers saved: %d of %d. Now: %s"
  ),
  ai_batch_format = c(
    fr = "Réponses enregistrées : %d sur %d. Mise en forme.",
    en = "Answers saved: %d of %d. Formatting."
  ),
  ai_batch_done = c(fr = "Série terminée. Le rapport est prêt à sceller.", en = "Set finished. The report is ready to seal."),
  ai_batch_fail = c(
    fr = "Échec sur « %s ». Les réponses déjà écrites restent dans le rapport.",
    en = "Failed on \"%s\". Answers already written stay in the report."
  ),
  ai_batch_stopped = c(
    fr = "Série arrêtée. Les réponses déjà écrites restent dans le rapport.",
    en = "Set stopped. Answers already written stay in the report."
  ),
  ai_batch_need_set = c(fr = "Choisissez une série avec au moins un prompt.", en = "Choose a set with at least one prompt."),
  ai_batch_busy = c(fr = "Une série est déjà en cours.", en = "A set is already running."),
  ai_busy = c(fr = "L'analyste écrit.", en = "The analyst is writing."),
  ai_need_key = c(fr = "Enregistrez une clé dans Paramètres.", en = "Save a key in Parameters."),
  ai_need_on = c(fr = "L'analyste est désactivé.", en = "The analyst is deactivated."),
  ai_err_offline = c(fr = "Le fournisseur ne répond pas.", en = "The provider does not answer."),
  ai_err_auth = c(fr = "La clé a été refusée.", en = "The key was refused."),
  ai_err_rate = c(fr = "Le fournisseur demande d'attendre.", en = "The provider asked to wait."),
  ai_err_http = c(fr = "Le fournisseur a répondu %s.", en = "The provider answered %s."),
  ai_err_empty = c(fr = "Réponse vide.", en = "Empty answer."),
  ai_err_timeout = c(fr = "Délai dépassé.", en = "Timed out."),
  ai_saved_key = c(fr = "Clé enregistrée sur cet ordinateur.", en = "Key saved on this computer."),
  ai_removed_key = c(fr = "Clé retirée de cet ordinateur.", en = "Key removed from this computer."),
  nav_rapports = c(fr = "Rapports", en = "Reports"),
  nav_tab_composer = c(fr = "Rapport PDF", en = "PDF report"),
  rep_title = c(fr = "Rapport PDF", en = "PDF report"),
  rep_send_lead = c(
    fr = "C'est le document que vous choisissez de partager. L'analyste, s'il est activé, envoie son contexte à votre fournisseur.",
    en = "This is the document you choose to share. If the analyst is on, it sends its context to your provider."
  ),
  rep_scope_line = c(
    fr = "Exercice %s · réalisé jusqu'à %s · horizon %d mois · scénario %s · %s",
    en = "Fiscal year %s · actuals through %s · horizon %d months · scenario %s · %s"
  ),
  rep_scenario_base = c(fr = "Attendu", en = "Expected"),
  rep_pre_month = c(fr = "Revue mensuelle", en = "Monthly review"),
  rep_pre_cash = c(fr = "Revue de trésorerie", en = "Cash review"),
  rep_pre_full = c(fr = "Dossier complet", en = "Full pack"),
  rep_texte_hint = c(
    fr = "Texte : la phrase déjà écrite par le modèle. Ce n'est pas un commentaire généré.",
    en = "Text: the sentence the model already wrote. It is not generated commentary."
  ),
  rep_card_rev = c(fr = "CA de l'exercice", en = "Fiscal-year revenue"),
  rep_card_oi = c(fr = "Résultat d'exploitation", en = "Operating income"),
  rep_card_cash = c(fr = "Trésorerie fin d'exercice", en = "Cash at year end"),
  rep_card_booked = c(fr = "Part déjà réalisée", en = "Share already booked"),
  rep_seal_means = c(
    fr = "Le sceau prouve que le fichier n'a pas changé. Il ne valide pas les chiffres.",
    en = "The seal proves the file has not changed. It does not validate the figures."
  ),
  rep_audit = c(fr = "Annexe d'audit", en = "Audit appendix"),
  ai_preview = c(fr = "Voir avant l'envoi", en = "Review before sending"),
  ai_preview_lead = c(
    fr = "Avant chaque envoi, le texte exact et le fournisseur s'affichent. La clé reste enregistrée sur cet ordinateur.",
    en = "Before each send, the exact text and the provider are shown. The key stays saved on this computer."
  ),
  ai_preview_title = c(fr = "Envoyer à l'analyste", en = "Send to the analyst"),
  ai_preview_go = c(fr = "Envoyer", en = "Send"),
  ai_preview_host = c(fr = "Destination : %s", en = "Destination: %s"),
  ai_preview_calls = c(fr = "%d appel(s).", en = "%d call(s)."),
  ai_key_where = c(
    fr = "La clé reste sur cet ordinateur, chiffrée pour votre session Windows. Vous ne la retapez pas à chaque ouverture.",
    en = "The key stays on this computer, encrypted for your Windows session. You do not type it again at each opening."
  ),
  rep_lead = c(
    fr = "Cochez les graphiques, tableaux et textes à réunir. Le fichier est un PDF A4 portrait, enregistré sous reports/rapports.",
    en = "Tick the charts, tables and texts to include. The file is an A4 portrait PDF, saved under reports/rapports."
  ),
  rep_period = c(fr = "Période du rapport : %s", en = "Report period: %s"),
  rep_title_field = c(fr = "Titre", en = "Title"),
  rep_title_ph = c(fr = "Titre du rapport, par exemple Revue de septembre", en = "Report title, for example September review"),
  rep_title_default = c(fr = "Rapport de gestion", en = "Management report"),
  rep_expand = c(fr = "Tout déplier", en = "Expand all"),
  rep_collapse = c(fr = "Tout replier", en = "Collapse all"),
  rep_tick_all = c(fr = "Tout cocher", en = "Check all"),
  rep_tick_none = c(fr = "Tout décocher", en = "Uncheck all"),
  rep_templates = c(fr = "Modèles prêts à l'emploi", en = "Ready-to-use templates"),
  rep_texte = c(fr = "Texte", en = "Text"),
  rep_chart = c(fr = "Graphique", en = "Chart"),
  rep_table = c(fr = "Tableau", en = "Table"),
  rep_cards = c(fr = "Cartons", en = "Cards"),
  rep_note = c(fr = "Note", en = "Note"),
  rep_ai_box = c(fr = "Commentaire de l'analyste", en = "Analyst commentary"),
  rep_ai_lead = c(
    fr = "L'analyste ajoute un commentaire par section et un résumé en tête. Les chiffres restent ceux du modèle.",
    en = "The analyst adds a comment per section and a summary at the top. The figures stay those of the model."
  ),
  rep_ai_label = c(fr = "Commentaire de l'analyste", en = "Analyst commentary"),
  rep_ai_regen = c(fr = "Régénérer le commentaire", en = "Regenerate commentary"),
  rep_ai_cost = c(
    fr = "%d sections · %d appels à votre fournisseur",
    en = "%d sections · %d calls to your provider"
  ),
  rep_ai_need_on = c(fr = "Activez l'analyste et enregistrez une clé pour le commentaire.", en = "Turn the analyst on and save a key for commentary."),
  rep_outline = c(fr = "Plan du rapport", en = "Report outline"),
  rep_outline_empty = c(fr = "Cochez au moins un élément.", en = "Tick at least one element."),
  rep_pages = c(fr = "Environ %d page(s)", en = "About %d page(s)"),
  rep_build_pdf = c(fr = "Générer le PDF", en = "Build PDF"),
  rep_build_html = c(fr = "HTML seulement", en = "HTML only"),
  rep_stop = c(fr = "Arrêter", en = "Stop"),
  rep_busy = c(fr = "Rapport en cours…", en = "Report in progress…"),
  rep_done = c(fr = "Rapport prêt.", en = "Report ready."),
  rep_done_html = c(fr = "HTML prêt. Le PDF n'a pas pu être créé (%s).", en = "HTML ready. The PDF could not be created (%s)."),
  rep_fail_empty = c(fr = "Cochez au moins un élément avant de générer.", en = "Tick at least one element before building."),
  rep_file = c(fr = "Fichier", en = "File"),
  rep_nofile = c(fr = "Aucun fichier généré.", en = "No file generated."),
  rep_download = c(fr = "Télécharger", en = "Download"),
  rep_folder = c(fr = "Ouvrir le dossier", en = "Open the folder"),
  rep_grp_synthese = c(fr = "Synthèse", en = "Summary"),
  rep_grp_pnl = c(fr = "Compte de résultat", en = "Income statement"),
  rep_grp_cash = c(fr = "Trésorerie", en = "Cash"),
  rep_grp_bs = c(fr = "Bilan", en = "Balance sheet"),
  rep_grp_var = c(fr = "Écarts", en = "Variances"),
  rep_grp_health = c(fr = "Santé", en = "Health"),
  rep_grp_analyst = c(fr = "Analyste", en = "Analyst"),
  rep_el_syn_cards = c(fr = "Cartons d'atterrissage", en = "Landing cards"),
  rep_el_syn_cash = c(fr = "Trajectoire de trésorerie", en = "Cash path"),
  rep_el_pnl_cone = c(fr = "Cône de chiffre d'affaires", en = "Revenue cone"),
  rep_el_pnl_wf = c(fr = "Cascade de période", en = "Period cascade"),
  rep_el_pnl_table = c(fr = "Tableau du compte de résultat", en = "Income statement table"),
  rep_el_cash_bridge = c(fr = "Pont de trésorerie", en = "Cash bridge"),
  rep_el_cash_table = c(fr = "Tableau de trésorerie", en = "Cash table"),
  rep_el_bs_table = c(fr = "Tableau du bilan", en = "Balance sheet table"),
  rep_el_var_main = c(fr = "Écart mensuel", en = "Monthly gap"),
  rep_el_var_bridge = c(fr = "Du budget à l'atterrissage", en = "From budget to landing"),
  rep_el_var_pv = c(fr = "Prix, volume ou calendrier", en = "Price, volume or calendar"),
  rep_el_var_table = c(fr = "Tableau mois par mois", en = "Month-by-month table"),
  rep_el_health_table = c(fr = "Tableau des ratios", en = "Ratio table"),
  rep_health_month = c(fr = "Mois clos %s.", en = "Closed month %s."),
  rep_el_ai_note = c(fr = "Note de l'analyste (atelier)", en = "Analyst note (workshop)"),
  rep_note_none = c(fr = "Aucune note.", en = "No note."),
  rep_note_open = c(fr = "Ouvrir l'atelier", en = "Open the workshop"),
  rep_note_ready = c(fr = "Note ouverte : %s", en = "Open note: %s"),
  rep_seal = c(fr = "Sceller le PDF", en = "Seal the PDF"),
  rep_sealed = c(fr = "Scellé · sha256 %s", en = "Sealed · sha256 %s"),
  rep_drop_cols = c(fr = "Colonnes masquées (vides) : %s", en = "Hidden empty columns: %s"),
  rep_chart_empty = c(
    fr = "Pas de données pour ce graphique sur la fenêtre disponible.",
    en = "No data for this chart on the available window."
  ),
  rep_ai_prompt_section = c(
    fr = "Rédige un commentaire court (6 à 10 lignes) pour la section « %s » du rapport de gestion. Appuie-toi uniquement sur le contexte fourni. Ne remplace pas les chiffres : commente-les. Pas de titre.",
    en = "Write a short commentary (6 to 10 lines) for the \"%s\" section of the management report. Use only the provided context. Do not replace the figures: comment on them. No title."
  ),
  rep_ai_prompt_summary = c(
    fr = "Rédige un résumé de direction (8 à 12 lignes) pour ce rapport de gestion. Les sections retenues sont : %s. Appuie-toi uniquement sur le contexte. Pas de titre.",
    en = "Write an executive summary (8 to 12 lines) for this management report. The retained sections are: %s. Use only the provided context. No title."
  ),
  nav_tab_chart = c(fr = "Graphique Expert", en = "Chart Expert"),
  chart_kicker = c(fr = "Pilotage · essai", en = "Steering · experiment"),
  chart_lead = c(
    fr = "Essai. Le graphique est en trois dimensions : un montant bouge quand deux hypothèses bougent. Les chiffres viennent du dossier ouvert.",
    en = "An experiment. The chart is three-dimensional: one amount moves when two assumptions move. The figures come from the open file."
  ),
  chart_be = c(fr = "Seuil de rentabilité", en = "Break-even surface"),
  chart_dupont = c(fr = "Trajectoire DuPont", en = "DuPont path"),
  chart_terrain = c(fr = "Relief du résultat", en = "P&L terrain"),
  chart_cube = c(fr = "Cube d'encaissement", en = "Cash conversion cube"),
  chart_sky = c(fr = "Skyline du bilan", en = "Balance sheet skyline"),
  chart_cloud = c(fr = "Nuage des scénarios", en = "Scenario cloud"),
  chart_sky_all = c(fr = "Tout cocher", en = "Check all"),
  chart_sky_assets = c(fr = "Actif", en = "Assets"),
  chart_sky_funding = c(fr = "Passif et capitaux propres", en = "Liabilities and equity"),
  chart_sky_wall = c(fr = "Séparation actif / passif", en = "Assets / funding divider"),
  chart_mark_vol_hi = c(fr = "Plus gros volumes", en = "Highest volumes"),
  chart_mark_vol_lo = c(fr = "Plus faibles volumes", en = "Lowest volumes"),
  chart_mark_px_hi = c(fr = "Prix les plus hauts", en = "Highest prices"),
  chart_mark_px_lo = c(fr = "Prix les plus bas", en = "Lowest prices"),
  chart_tip_mark = c(
    fr = "%s · %s<br>Volume %s · %s par rapport à la moyenne<br>Prix %s EUR · %s par rapport à la moyenne<br><b>Résultat d'exploitation %s EUR</b>",
    en = "%s · %s<br>Volume %s · %s versus the average<br>Price %s EUR · %s versus the average<br><b>Operating income %s EUR</b>"
  ),
  chart_def_margin = c(
    fr = "Marge nette : le résultat net divisé par le chiffre d'affaires.",
    en = "Net margin: net income divided by revenue."
  ),
  chart_def_turn = c(
    fr = "Rotation des actifs : le chiffre d'affaires divisé par l'actif (trésorerie, clients, stocks, immobilisations et autres actifs).",
    en = "Asset turnover: revenue divided by assets (cash, receivables, inventory, fixed assets and other assets)."
  ),
  chart_def_roe = c(
    fr = "Rentabilité des capitaux : le résultat net divisé par les capitaux propres.",
    en = "Return on equity: net income divided by equity."
  ),
  chart_def_dso = c(
    fr = "Clients (jours) : les créances divisées par le chiffre d'affaires du mois, fois 30.",
    en = "Customers (days): receivables divided by the month's revenue, times 30."
  ),
  chart_def_dio = c(
    fr = "Stocks (jours) : les stocks divisés par les achats du mois, fois 30.",
    en = "Stock (days): inventory divided by the month's purchases, times 30."
  ),
  chart_def_dpo = c(
    fr = "Fournisseurs (jours) : les dettes fournisseurs divisées par les achats, les salaires et les autres charges, fois 30.",
    en = "Suppliers (days): payables divided by purchases, payroll and other operating costs, times 30."
  ),
  chart_def_month = c(fr = "Mois : le mois du point ou de la barre.", en = "Month: the month of the point or the bar."),
  chart_def_line = c(
    fr = "Ligne : le poste du compte de résultat, du chiffre d'affaires au résultat net.",
    en = "Line: the profit and loss item, from revenue to net income."
  ),
  chart_def_side = c(
    fr = "Ligne : un poste de l'actif, ou un poste du passif et des capitaux propres. Le mur sépare les deux côtés.",
    en = "Line: an asset, or an item of liabilities and equity. The wall separates the two sides."
  ),
  chart_def_amount = c(fr = "Montant : la hauteur, en euros.", en = "Amount: the height, in euros."),
  chart_def_cash = c(fr = "Trésorerie : le solde en fin de mois.", en = "Cash: the balance at the month end."),
  chart_def_price = c(
    fr = "Prix : le tarif du mois comparé au tarif moyen des mois clos.",
    en = "Price: the month's rate compared with the average rate of the closed months."
  ),
  chart_def_volume = c(
    fr = "Volume : les nuitées du mois comparées à la moyenne des mois clos.",
    en = "Volume: the month's nights compared with the average of the closed months."
  ),
  chart_def_oi = c(
    fr = "Résultat d'exploitation : le relief pour ce prix et ce volume, à partir du coût fixe et de la part variable.",
    en = "Operating income: the surface at this price and volume, from the fixed cost and the variable share."
  ),
  chart_math_days = c(fr = "jours", en = "days"),
  chart_math_centre = c(fr = "Moyenne", en = "Average"),
  chart_math_compare = c(fr = "Comparer deux mois", en = "Compare two months"),
  chart_src_h = c(fr = "Fichiers source", en = "Source files"),
  chart_be_months_h = c(fr = "Mois sur le graphique", en = "Months on the chart"),
  chart_be_none = c(fr = "Tout décocher", en = "Uncheck all"),
  chart_tip_month = c(
    fr = "%s<br>Volume %s · %s par rapport à la moyenne<br>Prix %s EUR · %s par rapport à la moyenne<br><b>Résultat d'exploitation %s EUR</b>",
    en = "%s<br>Volume %s · %s versus the average<br>Price %s EUR · %s versus the average<br><b>Operating income %s EUR</b>"
  ),
  chart_gloss_price = c(
    fr = "%s tarif du mois · %s tarif moyen · %s écart de prix",
    en = "%s month's rate · %s average rate · %s price gap"
  ),
  chart_gloss_volume = c(
    fr = "%s nuitées du mois · %s nuitées moyennes · %s écart de volume",
    en = "%s month's nights · %s average nights · %s volume gap"
  ),
  chart_gloss_oi = c(
    fr = "%s chiffre d'affaires moyen · %s facteur de prix · %s facteur de volume · %s coût fixe · %s résultat",
    en = "%s average revenue · %s price factor · %s volume factor · %s fixed cost · %s operating income"
  ),
  chart_gloss_oi_var = c(
    fr = "%s chiffre d'affaires moyen · %s facteur de prix · %s facteur de volume · %s coût fixe · %s coût variable · %s résultat",
    en = "%s average revenue · %s price factor · %s volume factor · %s fixed cost · %s variable cost · %s operating income"
  ),
  chart_gloss_price_zero = c(
    fr = "Écart nul : le tarif moyen des mois clos.",
    en = "No gap: the average rate of the closed months."
  ),
  chart_gloss_volume_zero = c(
    fr = "Écart nul : les nuitées moyennes des mois clos.",
    en = "No gap: the average nights of the closed months."
  ),
  chart_gloss_oi_centre = c(
    fr = "%s chiffre d'affaires moyen · %s part variable · %s coût fixe · %s résultat",
    en = "%s average revenue · %s variable share · %s fixed cost · %s operating income"
  ),
  chart_gloss_margin = c(
    fr = "%s résultat net · %s chiffre d'affaires · %s marge nette",
    en = "%s net income · %s revenue · %s net margin"
  ),
  chart_gloss_turn = c(
    fr = "%s chiffre d'affaires · %s actif · %s rotation des actifs",
    en = "%s revenue · %s assets · %s asset turnover"
  ),
  chart_gloss_roe = c(
    fr = "%s résultat net · %s capitaux propres · %s rentabilité des capitaux",
    en = "%s net income · %s equity · %s return on equity"
  ),
  chart_gloss_dso = c(
    fr = "%s clients · %s chiffre d'affaires · 30 jours dans le mois · %s jours clients",
    en = "%s receivables · %s revenue · 30 days in the month · %s customer days"
  ),
  chart_gloss_dio = c(
    fr = "%s stocks · %s achats · 30 jours dans le mois · %s jours de stock",
    en = "%s inventory · %s purchases · 30 days in the month · %s stock days"
  ),
  chart_gloss_dpo = c(
    fr = "%s fournisseurs · %s charges · 30 jours dans le mois · %s jours fournisseurs",
    en = "%s payables · %s operating costs · 30 days in the month · %s supplier days"
  ),
  chart_gloss_rev = c(fr = "%s chiffre d'affaires du mois", en = "%s revenue for the month"),
  chart_gloss_gp = c(
    fr = "%s chiffre d'affaires · %s achats · %s marge brute",
    en = "%s revenue · %s purchases · %s gross profit"
  ),
  chart_gloss_ebitda = c(
    fr = "%s marge brute · %s salaires · %s autres charges · %s EBE",
    en = "%s gross profit · %s payroll · %s other costs · %s EBITDA"
  ),
  chart_gloss_oi_line = c(
    fr = "%s EBE · %s dotations · %s résultat d'exploitation",
    en = "%s EBITDA · %s depreciation · %s operating income"
  ),
  chart_gloss_ni = c(fr = "%s résultat net du mois", en = "%s net income for the month"),
  chart_gloss_bs_cash = c(fr = "%s solde de trésorerie", en = "%s cash balance"),
  chart_gloss_bs_ar = c(fr = "%s solde clients", en = "%s receivables balance"),
  chart_gloss_bs_inv = c(fr = "%s solde des stocks", en = "%s inventory balance"),
  chart_gloss_bs_ppe = c(fr = "%s solde des immobilisations", en = "%s fixed-asset balance"),
  chart_gloss_bs_ap = c(fr = "%s solde fournisseurs", en = "%s payables balance"),
  chart_gloss_bs_debt = c(fr = "%s solde de la dette", en = "%s debt balance"),
  chart_gloss_bs_eq = c(fr = "%s solde des capitaux propres", en = "%s equity balance"),
  chart_gloss_sky_assets = c(
    fr = "%s trésorerie · %s clients · %s stocks · %s immobilisations · %s actif",
    en = "%s cash · %s receivables · %s inventory · %s fixed assets · %s assets"
  ),
  chart_gloss_sky_fund = c(
    fr = "%s fournisseurs · %s dette · %s capitaux propres · %s passif et capitaux propres",
    en = "%s payables · %s debt · %s equity · %s liabilities and equity"
  ),
  chart_gloss_cloud_m = c(
    fr = "%s résultat d'exploitation · %s chiffre d'affaires · %s marge",
    en = "%s operating income · %s revenue · %s margin"
  ),
  chart_gloss_cloud_c = c(fr = "%s trésorerie en fin de mois", en = "%s cash at the month end"),
  chart_spin = c(fr = "Rotation", en = "Auto-rotate"),
  chart_reset = c(fr = "Vue de départ", en = "Reset view"),
  chart_see_h = c(fr = "Ce que l'on voit", en = "What you see"),
  chart_hover_h = c(fr = "Ce que dit le survol", en = "What the tooltip says"),
  chart_empty = c(fr = "Pas assez de mois clos pour ce graphique.", en = "Not enough closed months for this chart."),
  chart_ax_price = c(fr = "Prix", en = "Price"),
  chart_ax_volume = c(fr = "Volume", en = "Volume"),
  chart_ax_oi = c(fr = "Résultat d'exploitation", en = "Operating income"),
  chart_ax_margin = c(fr = "Marge nette", en = "Net margin"),
  chart_ax_turn = c(fr = "Rotation des actifs", en = "Asset turnover"),
  chart_ax_roe = c(fr = "Rentabilité des capitaux", en = "Return on equity"),
  chart_ax_month = c(fr = "Mois", en = "Month"),
  chart_ax_line = c(fr = "Ligne", en = "Line"),
  chart_ax_amount = c(fr = "Montant", en = "Amount"),
  chart_ax_dso = c(fr = "Clients (jours)", en = "Customers (days)"),
  chart_ax_dio = c(fr = "Stocks (jours)", en = "Stock (days)"),
  chart_ax_dpo = c(fr = "Fournisseurs (jours)", en = "Suppliers (days)"),
  chart_ax_cash = c(fr = "Trésorerie", en = "Cash"),
  chart_line_rev = c(fr = "Chiffre d'affaires", en = "Revenue"),
  chart_line_gp = c(fr = "Marge brute", en = "Gross profit"),
  chart_line_ebitda = c(fr = "EBE", en = "EBITDA"),
  chart_line_oi = c(fr = "Résultat d'exploitation", en = "Operating income"),
  chart_line_ni = c(fr = "Résultat net", en = "Net income"),
  chart_bs_cash = c(fr = "Trésorerie", en = "Cash"),
  chart_bs_ar = c(fr = "Clients", en = "Receivables"),
  chart_bs_inv = c(fr = "Stocks", en = "Inventory"),
  chart_bs_ppe = c(fr = "Immobilisations", en = "Fixed assets"),
  chart_bs_ap = c(fr = "Fournisseurs", en = "Payables"),
  chart_bs_debt = c(fr = "Dette", en = "Debt"),
  chart_bs_eq = c(fr = "Capitaux propres", en = "Equity"),
  chart_case_low = c(fr = "Prudent", en = "Cautious"),
  chart_case_base = c(fr = "Attendu", en = "Expected"),
  chart_case_high = c(fr = "Ambitieux", en = "Ambitious"),
  chart_st_act = c(fr = "réalisé", en = "actual"),
  chart_st_hist = c(fr = "historique", en = "history"),
  chart_st_fc = c(fr = "prévision", en = "forecast"),
  chart_name_oi = c(fr = "Résultat d'exploitation", en = "Operating income"),
  chart_name_zero = c(fr = "Plan du seuil", en = "Break-even plane"),
  chart_name_line = c(fr = "Ligne du seuil", en = "Break-even line"),
  chart_name_now = c(fr = "Mois clos, tels quels", en = "Closed months, as they stand"),
  chart_name_path = c(fr = "Mois du dossier", en = "Months in the file"),
  chart_name_plane = c(fr = "Cycle nul", en = "Zero cycle"),
  chart_tip_be = c(
    fr = "Prix %s · Volume %s<br><b>Résultat d'exploitation %s EUR</b><br>Chiffre d'affaires %s EUR · marge %s<br>Coût fixe %s EUR · variable %s du chiffre d'affaires<br>Écart au seuil : %s<br>%s",
    en = "Price %s · Volume %s<br><b>Operating income %s EUR</b><br>Revenue %s EUR · margin %s<br>Fixed cost %s EUR · variable %s of revenue<br>Distance to break-even: %s<br>%s"
  ),
  chart_tip_be_ok = c(
    fr = "Rentable : le volume peut encore baisser de %s avant les pertes.",
    en = "Profitable: volume can fall another %s before losses."
  ),
  chart_tip_be_loss = c(
    fr = "En perte : il faut encore %s de volume pour revenir au seuil.",
    en = "Loss-making: volume must rise another %s to break even."
  ),
  chart_tip_be_never = c(
    fr = "Ce prix ne couvre pas le coût variable. Aucun volume ne rattrape.",
    en = "This price does not cover the variable cost. No volume recovers it."
  ),
  chart_tip_now = c(
    fr = "Point de départ · mois clos moyen<br><b>Résultat d'exploitation %s EUR</b><br>Chiffre d'affaires %s EUR · marge %s<br>Prix 0 · volume 0. C'est le centre du graphique.<br>%s",
    en = "Starting point · average closed month<br><b>Operating income %s EUR</b><br>Revenue %s EUR · margin %s<br>Price 0 · volume 0. This is the centre of the chart.<br>%s"
  ),
  chart_tip_dupont = c(
    fr = "%s · %s<br><b>Rentabilité des capitaux %s</b><br>Marge nette %s · rotation des actifs %s · levier %s<br>Résultat net %s EUR · chiffre d'affaires %s EUR<br>%s",
    en = "%s · %s<br><b>Return on equity %s</b><br>Net margin %s · asset turnover %s · leverage %s<br>Net income %s EUR · revenue %s EUR<br>%s"
  ),
  chart_tip_move = c(
    fr = "Face à %s, c'est %s qui a le plus bougé (%s).",
    en = "Against %s, %s moved the most (%s)."
  ),
  chart_tip_first = c(fr = "Premier mois de la fenêtre.", en = "First month in the window."),
  chart_tip_land = c(
    fr = "%s · %s<br><b>%s EUR</b> · %s du chiffre d'affaires<br>Face à %s : %s EUR<br>%s",
    en = "%s · %s<br><b>%s EUR</b> · %s of revenue<br>Against %s: %s EUR<br>%s"
  ),
  chart_tip_land_share_up = c(
    fr = "La ligne prend une plus grande part du chiffre d'affaires.",
    en = "The line takes a larger share of revenue."
  ),
  chart_tip_land_share_down = c(
    fr = "La ligne prend une plus petite part du chiffre d'affaires.",
    en = "The line takes a smaller share of revenue."
  ),
  chart_tip_cube = c(
    fr = "%s · %s<br><b>Cycle d'encaissement %s jours</b><br>Clients %s j · stocks %s j · fournisseurs %s j<br>Environ %s EUR de chiffre d'affaires sont pris dans le cycle.<br>%s",
    en = "%s · %s<br><b>Cash cycle %s days</b><br>Customers %s d · stock %s d · suppliers %s d<br>About %s EUR of revenue sits in the cycle.<br>%s"
  ),
  chart_tip_cube_short = c(
    fr = "Cycle court : les fournisseurs financent une partie des clients et des stocks.",
    en = "Short cycle: suppliers fund part of customers and stock."
  ),
  chart_tip_cube_long = c(
    fr = "Cycle long : la société avance la trésorerie.",
    en = "Long cycle: the company advances the cash."
  ),
  chart_tip_sky = c(
    fr = "%s · %s<br><b>%s EUR</b> · %s du bilan<br>Face à %s : %s EUR",
    en = "%s · %s<br><b>%s EUR</b> · %s of the balance sheet<br>Against %s: %s EUR"
  ),
  chart_tip_cloud = c(
    fr = "%s · %s<br><b>Marge %s</b><br>Chiffre d'affaires %s EUR · trésorerie %s EUR<br>Face à l'attendu : marge %s, trésorerie %s EUR",
    en = "%s · %s<br><b>Margin %s</b><br>Revenue %s EUR · cash %s EUR<br>Against expected: margin %s, cash %s EUR"
  ),
  chart_see_be = c(
    fr = "Le relief est le résultat d'exploitation pour chaque couple prix et volume, à partir du coût fixe et du coût variable lus sur les mois clos. Le plan doré est le zéro. La ligne dorée est l'endroit où le relief le traverse.",
    en = "The surface is operating income for every price and volume pair, from the fixed and variable cost read on the closed months. The gold plane is zero. The gold line is where the surface crosses it."
  ),
  chart_hover_be = c(
    fr = "Prix, volume, résultat d'exploitation, chiffre d'affaires, marge, coût fixe, part variable, et de combien le volume peut encore baisser avant les pertes.",
    en = "Price, volume, operating income, revenue, margin, fixed cost, variable share, and how far volume can still fall before losses."
  ),
  chart_see_dupont = c(
    fr = "Chaque point est un mois. La hauteur est la rentabilité des capitaux. La nappe montre ce que donnerait la même rentabilité si le levier restait celui des mois clos.",
    en = "Each point is a month. Height is the return on equity. The sheet shows the return if leverage stayed at the closed-month level."
  ),
  chart_hover_dupont = c(
    fr = "Marge nette, rotation des actifs, levier, résultat net, et lequel des trois a le plus bougé face au mois d'avant.",
    en = "Net margin, asset turnover, leverage, net income, and which of the three moved most against the previous month."
  ),
  chart_see_terrain = c(
    fr = "Les mois vont d'un côté, les lignes du compte de résultat de l'autre. La hauteur est le montant. Un creux qui s'installe est une marge qui se comprime.",
    en = "Months run one way, income-statement lines the other. Height is the amount. A valley that forms is a margin compressing."
  ),
  chart_hover_terrain = c(
    fr = "La ligne, le mois, le montant, sa part du chiffre d'affaires, et l'écart avec le mois d'avant.",
    en = "The line, the month, the amount, its share of revenue, and the gap against the previous month."
  ),
  chart_see_cube = c(
    fr = "Chaque point est un mois : jours clients, jours de stock, jours fournisseurs. Le plan est le cycle nul. Un point sous le plan avance de la trésorerie.",
    en = "Each point is a month: customer days, stock days, supplier days. The plane is a zero cycle. A point under the plane advances cash."
  ),
  chart_hover_cube = c(
    fr = "Les trois délais, le cycle en jours, et le chiffre d'affaires qui dort dans ce cycle.",
    en = "The three delays, the cycle in days, and the revenue sitting in that cycle."
  ),
  chart_see_sky = c(
    fr = "Chaque barre est un poste du bilan, mois après mois. La trésorerie, les clients et les immobilisations d'un côté. Les fournisseurs, la dette et les capitaux de l'autre.",
    en = "Each bar is a balance-sheet item, month after month. Cash, receivables and fixed assets on one side. Payables, debt and equity on the other."
  ),
  chart_hover_sky = c(
    fr = "Le poste, le mois, le montant, sa part du bilan, et l'écart avec le mois d'avant.",
    en = "The item, the month, the amount, its share of the balance sheet, and the gap against the previous month."
  ),
  chart_see_cloud = c(
    fr = "Trois trajectoires : prudent, attendu, ambitieux. L'écart entre elles est le scénario, pas un autre hôtel.",
    en = "Three paths: cautious, expected, ambitious. The gap between them is the scenario, not another hotel."
  ),
  chart_hover_cloud = c(
    fr = "Le scénario, le mois, la marge, le chiffre d'affaires, la trésorerie, et l'écart avec l'attendu.",
    en = "The scenario, the month, the margin, revenue, cash, and the gap against expected."
  ),
  chart_sub_be = c(
    fr = "Coût fixe %s EUR · variable %s du chiffre d'affaires · %d mois clos. Mois moyen : chiffre d'affaires %s EUR, résultat d'exploitation %s EUR.",
    en = "Fixed cost %s EUR · variable %s of revenue · %d closed months. Average month: revenue %s EUR, operating income %s EUR."
  ),
  chart_sub_be_flat = c(
    fr = "Sur les mois clos, le coût ne monte pas avec le chiffre d'affaires : la part variable est tenue à zéro.",
    en = "On the closed months, cost does not rise with revenue: the variable share is held at zero."
  ),
  chart_sub_win = c(
    fr = "Fenêtre %s · %d mois.",
    en = "Window %s · %d months."
  )
)

#' Tab titles. The value stays the French word so navigation does not move.
#' Glossary keys shown in the Drivers fold, in reading order.
fpa_drv_glossary_keys <- c(
  "serie", "ligne", "verdict", "formule", "soutien", "indep", "resultat", "bilan", "r", "residu", "partial",
  "lag", "mois", "rank", "seuil", "seuil_commun", "mecanisme", "confirme", "veille",
  "attendu", "bruit", "constante", "reverse", "plus_cout", "score", "prior", "part", "holdout"
)

#' "fr" or "en". Anything else, including a missing value, is French.
fpa_lang_norm <- function(x) {
  x <- tolower(trimws(as.character(if (is.null(x) || !length(x)) "" else x[[1]])))
  if (!nzchar(x) || is.na(x)) return("fr")
  if (identical(x, "en")) "en" else "fr"
}

#' Screen language lives beside the published configuration, not inside it.
fpa_lang_sidecar <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "work", "ui-language.json")
}

fpa_lang_read_config <- function(config_path) {
  path <- fpa_lang_sidecar(config_path)
  if (!nzchar(path) || !file.exists(path)) return("fr")
  got <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(got)) return("fr")
  fpa_lang_norm(got$language)
}

#' Write the screen language to the sidecar. The published JSON is not opened.
fpa_lang_write_config <- function(config_path, lang) {
  lang <- fpa_lang_norm(lang)
  path <- fpa_lang_sidecar(config_path)
  if (!nzchar(path)) return(FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch({
    jsonlite::write_json(list(language = lang), path, auto_unbox = TRUE, pretty = TRUE)
    TRUE
  }, error = function(e) FALSE)
  isTRUE(ok)
}

fpa_lang_current <- function() {
  lang <- fpa_lang_norm(getOption("fpa.lang", "fr"))
  if (!requireNamespace("shiny", quietly = TRUE)) return(lang)
  domain <- shiny::getDefaultReactiveDomain()
  if (is.null(domain)) return(lang)
  reader <- domain$userData$fpa_lang
  if (!is.function(reader)) return(lang)
  hit <- tryCatch(reader(), error = function(e) {
    if (grepl("reactive context", conditionMessage(e), fixed = TRUE)) return(NULL)
    stop(e)
  })
  if (is.null(hit)) lang else fpa_lang_norm(hit)
}

#' Whole-string swaps for text already on the page. A French sentence with two
#' English translations is left out, so the walker does not guess.
fpa_lang_pairs <- function(target) {
  target <- fpa_lang_norm(target)
  source <- if (identical(target, "en")) "fr" else "en"
  from <- character()
  to <- character()
  for (item in fpa_copy) {
    a <- unname(item[[source]])
    b <- unname(item[[target]])
    if (!length(a) || !length(b)) next
    a <- a[[1]]
    b <- b[[1]]
    if (!is.character(a) || !is.character(b)) next
    if (!nzchar(a) || !nzchar(b) || identical(a, b)) next
    if (grepl("%", a, fixed = TRUE) || nchar(a) < 3L) next
    from <- c(from, a)
    to <- c(to, b)
  }
  src <- unique(from)
  agree <- vapply(src, function(s) length(unique(to[from == s])) == 1L, logical(1))
  src <- src[agree]
  idx <- match(src, from)
  stats::setNames(as.list(to[idx]), from[idx])
}

#' Tab value stays the French word. Only the visible title moves.
fpa_lang_tabs <- function() {
  list(
    list(value = "Synthèse", key = "loc_summary"),
    list(value = "Cartographie", key = "nav_tab_map"),
    list(value = "Drivers", key = "nav_tab_drivers"),
    list(value = "Scénarios", key = "nav_scn"),
    list(value = "Compte de résultat", key = "synth_go_pnl"),
    list(value = "Trésorerie", key = "health_lab_cash"),
    list(value = "Bilan", key = "nav_tab_bs"),
    list(value = "Anomalies", key = "nav_tab_anomalies"),
    list(value = "Clôture", key = "nav_tab_close"),
    list(value = "Écarts", key = "loc_variances"),
    list(value = "Santé", key = "loc_health"),
    list(value = "Impact", key = "nav_tab_impact"),
    list(value = "Journal", key = "nav_tab_journal"),
    list(value = "Graphique", key = "nav_tab_chart"),
    list(value = "Composer", key = "nav_tab_composer"),
    list(value = "Analyste", key = "nav_tab_ai"),
    list(value = "Paramètres", key = "nav_params"),
    list(value = "Fiche", key = "drv_tab_fiche"),
    list(value = "Carte", key = "drv_tab_map"),
    list(value = "Tests", key = "drv_tab_tests"),
    list(value = "Décalage", key = "drv_tab_lag"),
    list(value = "Alignement", key = "drv_tab_align"),
    list(value = "Brut et résidus", key = "drv_tab_raw"),
    list(value = "Holdout", key = "drv_tab_hold"),
    list(value = "Revue", key = "drv_tab_review")
  )
}

fpa_lang_groups <- function() {
  list(
    list(group = "sources", key = "nav_sources"),
    list(group = "etats", key = "nav_etats"),
    list(group = "controle", key = "nav_controle"),
    list(group = "pilotage", key = "nav_pilotage"),
    list(group = "rapports", key = "nav_rapports")
  )
}

#' Engine sentence. Always French. Published keys, role ids, file names, and
#' saved labels do not follow the screen language.
fpa_txt <- function(key, ...) {
  item <- fpa_copy[[key]]
  if (is.null(item)) return("")
  txt <- unname(item[["fr"]])
  if (!length(txt) || !nzchar(txt[[1]])) return("")
  dots <- list(...)
  if (!length(dots)) return(txt[[1]])
  sprintf(txt[[1]], ...)
}

#' Screen sentence. The Shiny page calls this. The engine does not.
fpa_screen <- function(key, ...) {
  item <- fpa_copy[[key]]
  if (is.null(item)) return("")
  lang <- fpa_lang_current()
  txt <- unname(item[[lang]])
  if (!length(txt) || !nzchar(txt[[1]])) txt <- unname(item[["fr"]])
  if (!length(txt) || !nzchar(txt[[1]])) return("")
  dots <- list(...)
  if (!length(dots)) return(txt[[1]])
  sprintf(txt[[1]], ...)
}
