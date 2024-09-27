
<img src="graphics/RESL_Circular_logo.png" width="30%" style="display: block; margin: auto;" />

## Rea and Van der Veer (202X) Replication Code

This repository contains all of the code used in Rea and Van der Veer
(202X), on the influence of judge characteristics on outcomes in
environmental litigation.

The repository contains seven primary scripts and a range of other
supporting files including supporting functions, data crosswalks,
manually coded variables, and supporting data sources, all of which are
needed to fully replicate all visualizations and analyses. We share this
full suite of data and code in the spirit of full transparency and open
science.

## Getting Started

Running this replication code requires having the underlying source
data, which is accessible via X.

## Running the Code

This code repository is not organized around a primary or “meta” script
that, with a single call, calls each individual sub-script in the
repository to run through all the code.

Instead, each Script is numbered and must be run manually and
sequentially to move from pre-processing the raw data all the way to the
final outputs included in Rea and Van der Veer (202X). This includes all
of the visualizations and analyses included in the Supplemental
Information.

Thus, once the source data files are located in the proper directories,
run each script in its entirety, sequentially, to reproduce all analyses
and visualizations in Rea and Van der Veer (202X).

## Output

The scripts will automatically write out all output to the appropriate
directories.

All figures, including visual representations of regression results
(forest plots) will be populated in the /Figures directory.

Regression results tables and summary statistics will populate their
corresponding folders in the /Data directory.

In instances where data produced by the script had to be hand coded,
e.g., for reliability checks, those hand-coded files are included in the
repository.

## Further Documentation

Basic documentation of the FJC IDB data is available via the Federal
Judicial Center directly, at <https://www.fjc.gov/research/idb>.

Basic documentation of the Court Listener data is available via Court
Listener directly, at
<https://www.courtlistener.com/help/api/bulk-data/>.

Detailed documentation on the content and assembly of the RESL ELD is
available via the [RESL ELD Process
Documentation](https://docs.google.com/document/d/1cLVq71dIXMKAhXpye3WG9Iy0PVMU2x3o9vp2YoWa7Ow/edit?usp=sharing).

A detailed comparison of FJC IDB and RESL ELD data, which makes
strengths and weaknesses of each data source apparent, is available in
the Supplemental Information to Rea, Merten, and Rife (2024). All of the
code used to produce this comparison is, of course, included in this
repository.

## Questions?

Please direct all questions, including discoveries of suspected errors
or missteps, to Dr. Chris Rea, founding PI of the Rea Environment and
Society Lab (RESL).
