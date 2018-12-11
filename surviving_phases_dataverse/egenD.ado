*! egenD(etailed), v2.25
*! Author: Shawna Metzger, skm29@pitt.edu
*! Last Modified: 08JAN13
*! Egens variable, but also records the command used to generate into the variable's notes, along with recording the do-file

// v2.25: fixed label, in case gen procedure has quotation in cmdline (JAN13)
// v2.21: stores label to variable notes, 
	//	  in case it's too long and gets cut off (JUL12)
// v2.2: fixing byable to gen correctly 		 (MAR11)
// v2: byable support that actually works.		 (FEB11)

capture program drop egenD
program define egenD, byable(onecall) sortpreserve
version 8.0

// Step 0: Housekeeping - parse
* Get byable
if _by() {
	local by `_byvars'
}

* Get name for gen
	gettoken first 0:0, parse("=")
	local newVar `first'
	
* Get name for gen, options
	gettoken second 0:0
	syntax anything [if] [in] [, Label(string) *]
	local function `anything'

local short = ltrim(rtrim(itrim(`"`function' `if' `in'"')))
	
// Step 1: Gen.
if("`by'"!=""){
	local short = `"`short'"' + ", by `by'" 
	if("`options'"!=""){
		local fullCommand = `"bysort `by': egen `newVar' = `function' `if' `in', `options'"'
		qui bysort `by': egen double `newVar' = `function' `if' `in', `options'
	}
	else{
		local fullCommand = trim(`"bysort `by': egen `newVar' = `function' `if' `in'"')
		qui bysort `by': egen double `newVar' = `function' `if' `in'
	}	
}

else{
	if("`options'"!=""){
		local fullCommand = `"egen `newVar' = `function' `if' `in', `options'"'
		qui egen double `newVar' = `function' `if' `in', `options'
	}
	else{
		local fullCommand = trim(`"egen `newVar' = `function' `if' `in'"')
		qui egen double `newVar' = `function' `if' `in'
	}
	
}

qui count if(`newVar'==.) `in'
qui return list
di as green "`r(N)' missing values generated"
qui compress

	
// Step 2: Annotate.
qui notes `newVar': TS `fullCommand'

if("`label'"!=""){
	qui label variable `newVar' `"`label'"'
	qui notes `newVar': `"FULL DESCRIPTION: `label'"'
}
else{
	qui label variable `newVar' `"`short'"'
}


end

