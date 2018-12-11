*! genD(etailed), v2.22
*! Author: Shawna Metzger, skm29@pitt.edu
*! Last Modified: 08JAN13
*! Generates a variable, but also records the command used to generate into the variable's notes, along with recording the do-file

// v2.22: fixed label, in case gen procedure has quotation in cmdline (JAN13)
// v2.21: stores label to variable notes, 
     //   in case it's too long and gets cut off (JUL12)
// v2.2: fixing byable to gen correctly 		 (MAR11)
// v2.15: dummy shortcut 						 (FEB11)
// v2: byable support that actually works		 (FEB11)

capture program drop genD
program define genD, byable(onecall) sortpreserve
version 8.0

// Step 0: Housekeeping - parse
* Get byable
if _by() {
	local by `"`_byvars'"'
}

* Get name for gen
	gettoken first 0:0, parse("=")
	local newVar `first'
	
* Get name for gen, options
	gettoken second 0:0
	syntax anything [if] [in] [, DUMmy Label(string)]
	local function `anything'
	
local short = ltrim(rtrim(itrim(`"`function' `if' `in'"')))
	
// Step 1: Gen.
if("`by'"!=""){
	local fullCommand = `"bysort `by': gen `newVar' = `function' `if' `in'"'
	qui bysort `by': gen double `newVar' = `function' `if' `in'
	local short = `"`short'"' + ", by `by'" 
}

else{
	local fullCommand = trim(`"gen `newVar' = `function' `if' `in'"')
	qui gen double `newVar' = `function' `if `in''
}

// Step 1.5: Recode, if dummy.
qui count if(`newVar'==.) `in'
qui return list
local missing = r(N)	

if("`dummy'"!=""){
	local fullCommand = `"`fullCommand'"' + ", recode `newVar' (.=0)"
	tempvar old
	qui gen `old' = `newVar'
	qui recode `newVar' (.=0)
	
	qui count if(`newVar'!=`old')
	qui return list
	local change = r(N)
	
	di as green "(`missing' missing values generated)"
	di as green "(`newVar': `change' changes made)"
}

else{
	di as green "(`missing' missing values generated)"
}	

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

