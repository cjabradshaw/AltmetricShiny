# AltmetricShiny

## Preamble
Ever wanted to collate the <a href="https://www.altmetric.com/about-altmetrics/what-are-altmetrics/">Altmetric data</a> for your articles, but couldn't be bothered to do it manually? I've made the process substantially easier by designing this Shiny <a href="https://cjabradshaw.shinyapps.io/AltmetricShiny/">app</a>. All you need to do is collate a list of '<a href="https://www.doi.org/">digital object identifiers</a>' ('doi') for the articles of interest, and the <a href="https://cjabradshaw.shinyapps.io/AltmetricShiny/">app</a> does it all for you. The app also produces outputs that plot the distribution of not only the relevant Altmetric scores, but also the rank percentiles for each article relative to articles of the same age in the journal, and to all articles in the journal with Altmetric data.

The app also gives you the option of fetching citation data from <a href="https://www.crossref.org/">Crossref</a> to examine the patterns between Altmetric and citation trends.

This Github repository provides all the 'under-the-bonnet' code for the app.

## Instructions
- Create a delimited text file of <strong>exactly the same format</strong> as the example file, although you can specify the delimit character (comma, space, tab).
- Load your delimited text file in the app by clicking the <strong>choose file</strong> button.
- Select whether you want to include Crossref citation data (<strong>include Crossref citation data?</strong>). Downloading these data will increase processing time.
- Choose how you want the output file to be sorted by selecting one of the four choices in the drop-down menu: <em>Altmetric score</em>, <em>context rank percentile</em>, <em>all-time rank percentile</em>, or <em>publication date</em>.
- Click the <strong>fetch data</strong> button.
- Download the results table as a tab-delimited file by clicking the <strong>download</strong> button.
