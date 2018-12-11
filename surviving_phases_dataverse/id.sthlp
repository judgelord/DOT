{smcl}
{* *! version 2.0.4 10jun2009}{...}
{cmd:help id} 
{hline}

{title:Title}

{p 4 16 2}
{hi:id} {hline 2} Generates a new variable to be used for merging
{p_end}


{title:Syntax}

{p 4 16 2}
{hi:id} {varlist}{cmd:,} {opt gen:erate(newvar)} [{opt ch:eck} {opt nd:(varlist)}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{synopt :{opt gen:erate}}required, name of the new variable{p_end}
{synopt :{opt ch:eck}}checks to see if the variable created uniquely identifies the observations{p_end}
{synopt :{opt nd:(varlist)}}for directed-dyad data, creates a non-directed ID, where the varlist contains the unit identifiers.  Best explained via illustration in "Examples" section below.{p_end}
{synoptline}

{p 4 6 2}
All variables in {it:varlist} must be integers with maximum values greater than zero.  If not, Stata
will return an error message.{p_end}


{title:Description}

{pstd}
{cmd:id} takes a list of variables and, in the order they are listed, generates a new variable 
that suffers no loss of precision.  Such a variable can be helpful in generating unique merge identifiers for
multiple datasets (e.g., for international relations scholars, unique dyad identifiers).

{title:Examples}

{pstd}Generates variable named "dyadid" using variables named "ccode1", "ccode2", and "year"{p_end}
{phang2}{cmd:. id ccode1 ccode2 year, gen(dyadid)}{p_end}

{pstd}In a directed-dyad-year dataset, generates non-directed dyad identifier named "ndDyadid" using variables named "ccode1", "ccode2", and "year"{p_end}
{phang2}{cmd:. id ccode1 ccode2 year, gen(ndDyadid) nd(ccode1 ccode2)}{p_end}

{pstd}Generates variable named "yearMonth" using variables "year" and "month", and then checks to see if "yearMonth" uniquely identifies all observations{p_end}
{phang2}{cmd:. id year month, gen(yearMonth) check}{p_end}

{title:Contact}

{p 4 4 2}
Shawna Metzger{break}
University of Pittsburgh{break}
skm29@pitt.edu
{p_end}

{p 0 0 0}
{bf:Version} - 2.71
{p_end}

