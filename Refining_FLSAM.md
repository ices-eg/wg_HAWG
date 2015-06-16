_Email from Anders Nielsen, DTU Aqua, about an approach to refining your model._

First and most importantly it should never be an automated process.

Model selection is the hard part. It is where your experience enters
the process.

I always start by expanding the model enough to be happy about the
fit and especially residuals plotted against a lot of thing e.g. age,
fleet, year, cohort, and most importantly prediction. Call this
"mother of all models" model A.

Then I look for logical ways to reduce the model. Say if two surveys
are supposed to be similar, then I test for similar variances, and
possibly similar catchabilities.

If in doubt I choose not to reduce. Remember that a test can only tell
you that you don't (yet?) have evidence to proof a difference - not that
it is not there.

I also don't test all possible similarities, only ones where there is a
logicial explanation.

This process leads to a series of sub models B, then C, then ...

At the end I also test the final one of those (say F) against the first
one (A) to ensure that I did move from my good model into a wrong one in
steps small enough for me not to notice.

Finally, I look at residuals for the final model.

First and most importantly it should never be an automated process.
