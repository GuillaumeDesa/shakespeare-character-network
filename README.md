# Shakespeare Character Networks

An interactive R Shiny app that visualises character co-occurrence networks for Shakespeare plays, fetching texts live from [Project Gutenberg](https://www.gutenberg.org).

![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-latest-blue)
![License](https://img.shields.io/badge/license-CC%20BY--NC--ND%204.0-lightgrey)

## Features

- **15 plays** selectable from a dropdown — tragedies, comedies, and histories
- **Cumulative network**: step through scenes one by one and watch the character graph grow
- **Hairball reduction**: an edge-weight slider hides infrequent co-appearances, keeping late-play networks readable
- **Rich node encoding**: size = degree; colour = betweenness centrality (blue → orange → red)
- **Interactive graph**: drag nodes, hover for statistics, highlight neighbourhoods
- **Character timeline**: dotplot showing who appears when, coloured by whether they have appeared yet at the current scene
- **Auto-installs** any missing R packages on first run

## Screenshot

> _Add a screenshot here once the app is running — `Tools > Publish > Take screenshot` in RStudio, or use `webshot2::appshot()`._

## Quick start

```r
# 1. Clone the repo
# git clone https://github.com/YOUR_USERNAME/shakespeare-networks.git

# 2. Open app.R in RStudio and click "Run App"
#    — or from the R console:
shiny::runApp("app.R")
```

Missing packages are installed automatically on first run.

## How to read the network

| Visual property | Meaning |
|---|---|
| Node **size** | Degree — how many other characters this character is connected to |
| Node **colour** | Betweenness centrality — blue (low) → orange → red (high); red nodes are dramatic bridges |
| Edge **thickness** | Number of scenes shared by the two characters |
| **Scene slider** | The network is cumulative: it shows all co-occurrences up to and including the selected scene |
| **Edge filter slider** | Only show pairs who have shared at least *N* scenes — raise this to tame late-play hairballs |

## Plays included

| Title | Gutenberg ID |
|---|---|
| Macbeth | 1533 |
| Hamlet | 1524 |
| Othello | 1531 |
| King Lear | 1532 |
| A Midsummer Night's Dream | 1514 |
| Romeo and Juliet | 1513 |
| The Merchant of Venice | 2243 |
| Much Ado About Nothing | 2269 |
| Twelfth Night | 1526 |
| The Tempest | 23042 |
| Julius Caesar | 1522 |
| Antony and Cleopatra | 1534 |
| Richard III | 1542 |
| Henry V | 2253 |
| The Taming of the Shrew | 1508 |

To add a play, find its Gutenberg ID at [gutenberg.org](https://www.gutenberg.org) and append it to the `SHAKESPEARE_PLAYS` vector at the top of `app.R`.

## Dependencies

All packages are installed automatically if missing. They are all available on CRAN.

| Package | Role |
|---|---|
| `shiny` | Web app framework |
| `bslib` | Bootstrap 5 UI theme |
| `dplyr` / `tidyr` / `stringr` | Data wrangling |
| `ggplot2` | Timeline plot |
| `reshape2` | Co-occurrence matrix reshaping |
| `scales` | Axis formatting |
| `visNetwork` | Interactive network graph |
| `gutenbergr` | Project Gutenberg text download |
| `igraph` | Graph metrics (degree, betweenness) |

## Gutenberg mirror note

`gutenbergr` selects a mirror automatically. If downloads fail (firewall, timeout), set a preferred mirror before launching:

```r
options(gutenbergr_mirror = "https://gutenberg.pglaf.org")
shiny::runApp("app.R")
```

## Parsing note

Gutenberg plain-text Shakespeare plays use the convention `SPEAKER. dialogue` or `SPEAKER: dialogue` with speaker names in ALL CAPS. The `parse_play()` function in `app.R` handles both separators, carries dialogue over continuation lines, and strips stage-direction tokens (`EXEUNT`, `EXIT`, `ENTER`, `ALL`, `BOTH`, etc.). Different editions may have idiosyncratic formatting; if a play parses poorly, inspect the raw Gutenberg text and adjust the speaker-detection regex in `parse_play()`.

## History

This app is a substantial rewrite of a 2015 Shiny sketch inspired by [David Robinson's](http://varianceexplained.org) original Macbeth analysis. The original used a single hand-edited plain-text file and `networkD3::simpleNetwork`. The current version fetches any play dynamically, replaces the network renderer with `visNetwork` (physics layout, tooltips, centrality encoding), and upgrades the UI to `bslib`.

## License

[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/) — see [`LICENSE`](LICENSE)

You are free to share this work with attribution. Commercial use and derivative works are not permitted.
