# Die geometrische brownsche Bewegung und Anwendungen
Hier finden Sie die Quellcode für die Bachelorarbeit und den Vortrag für das Kolloquium, so wie die R-Skripte zur Erzeugung der Visualisierungen in der Arbeit. Die Code-Ausschnitte aus der Arbeit sind hier eingebettet.

### R-Quellcode Übersicht

- `gbm.R` - Kernlogik GBM (Parameterschätzung, Prediktion, Simulation)
- `cev.R` - Kernlogik CEV (mit Sim.Diffproc)
---
- `backtest.R` - Kernlogik Backtest
---
- `gbm_visualisation.R` - Visualisierungen GBM (Simulation, Konfidenzbänder)
- `cev_visualisation.R` - Visualisierungen CEV (Simulation, Konfidenzbänder)
- `gbm_backtest.R` - Backtest Metriken und Visualisierung GBM
- `csv_backtest.R` - Backtest Metriken und Visualisierung CEV
---
- `bs.R` - Berechnung und Visualisierung des Black-Scholes-Modells
- `discrete_bb.R` - Visualisierung der diskreten Brownschen Bewegung
- `bb_cov.R` - Visualisierung der Kovarianzstruktur der Brownschen Bewegung
- `bootstrap.R` - Bootsrap-Verfahren zur probabilistischen Kalibrierung der GBM
- `custom_cev_estim.R` - Eigene Implementierung des Euler-PseudoMLE-Schätzers für das CEV-Modell
---
- `backtest_loop.R` - Erzeugt die Metriken zum empirischen Vergleich von GBM und CEV
- `post_process_backtest.R`, `compare_models.R` - Fügen die obigen Daten zu einer CSV Datei zusammen

