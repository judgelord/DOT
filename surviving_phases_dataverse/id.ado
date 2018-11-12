*! id, v2.71
*! Author: Shawna Metzger, skm29@pitt.edu
*! Last Modified: 20JUL12
*! Easily creates unique merge identifiers from a list of variables, and ensures that precision is not lost due to Stata's data storage 

// v2.51 (09AUG09)
// v2.6  (24SEP10) adds labels, generating code to program (like genD and egenD).  Finally fixed the integer code, too.
// v2.61 (05OCT10) gets rid of zero code in the counter!=1 loop.  
// v2.62 (06JUN12) tweaks error code for ch so that it distinguishes between non-unique IDs and IDs with missings
// v2.7	 (18JUL12) adds shortcut code for non-directed dyad gen (when ccode1 and ccode2 are directed), added _power subroutine to 
		 // 	   help; got rid of masterKill by using exit statements
// v2.71 (19JUL12) made ndirect specifiable, making it possible to gen (e.g.) a non-directed-dyad-year ID from directed-dyad data 
		 //		   by typing "id ccode1 ccode2 year, gen(dyadidYr) nd(ccode1 ccode)" (vs. "id ccode1 ccode2, gen(dyadid) nd", and
		 //		   then "id dyadid year, gen(dyadidYr)")
		 
capture program drop id
program define id
version 8.0
syntax varlist(min=2 numeric), GENerate(string) [CHeck NDirect(string)]
	// check: checks to see if var is unique ID
	// ndirect: form non-directed dyad ID.  If there are only two vars in vlist, enter "*" as option.
			//	Otherwise, name of vars must be specified in option.
local name `generate'
local isID `check'
local nd `ndirect'

local numVarsT: word count `varlist' // total count varlist
local ndDone = 1
local posVar1 = ""
	
if("`nd'"!=""){		// ND option specified: check to make sure that two vars specified 
	// either there are two vars in ND or two in varlist.
	if("`nd'"=="*"){		// two in varlist
		local numVars = `numVarsT' 
		local errorM = "only two variables allowed when nd(*) is specified"
		local ndVars = "`varlist'"
	}
	else{
		local numVars = wordcount("`nd'")
		local errorM = "must include two variables in nd()"
		local ndVars = "`nd'"
	}
	
	if(`numVars'!=2){
		di as error `"`error'"'
		exit 198
	}
	
	tokenize "`ndVars'"
	local ndDone = 0
}	
	
local command = "`0'"

quietly generate double `name' = .
local counter = 1

foreach x of local varlist {
	// Checking to see if all variables are integers
	quietly compress `x'
	tempvar rounder
	qui gen double `rounder' = round(`x')
	if(`rounder'!=`x'){
		display as error "`x' is not an integer."
		quietly drop `name'
		exit 126
	}
		
		//  capture confirm int variable test => won't work all the time, because long and byte can contain integers, too.
		//										 Float and double both have the ability to store decimals, but can contain just integers, too.			


/* Note:	Contents of _rc are whether or not variable x is an integer.
				If x is an integer, then _rc is true 
				If x isn't an integer, then ~_rc is true				*/

*** Start of actual program code here ***
	local voi `x'	//voi = variable of interest
	local labAdd "`x'"
	
	// prep check for ND				
	local posVar: word `counter' of `varlist'
	if(`ndDone'==0){	
		local next = `counter'+1
		local posVar1: word `next' of `varlist'
		
		// if the set of two are the ND vars
		if("`posVar'"=="`1'" & "`posVar1'"=="`2'"){
			tempvar ndHolder
			_power `2'
			gen `ndHolder' = (`1' * (10^`r(power)'))+`2'

			_power `1'
			quietly replace `ndHolder' = (`2' * (10^`r(power)'))+`1' if(`2'<`1')
			
			local voi `ndHolder'
			local labAdd "`1' * `2'"
			local ndDone = 1
		}
	}
			
	if(`counter'==1){					//If the variable is the first one in the list, then put it into the unique identifier.
		quietly replace `name'=`voi'
		local label = "MERGE: `labAdd'"
	}  

	if(`counter'!=1 & ("`nd'"=="" | ("`nd'"!="" & "`x'"!="`posVar1'"))){					//If we're past the first variable, then figure out how many zeros we need to tack on.
		_power `voi'
		quietly replace `name' = (`name' * (10^`r(power)'))+`voi'
		local label = "`label' * `labAdd'"
	}

	
	local `counter++'
}

if("`nd'"!=""){
	local label = "`label' [ND]"
}

quietly compress
display as text "variable `name' generated"

qui notes `name': TS `command'
qui label variable `name' `"`label'"'


if("`isID'"!=""){
	capture:isid `name'
	if(!_rc){
		display as text "variable `name' uniquely identifies the observations"
	}
	else{		//JUN12 tweak--specify whether there are doubles or if the ID has missing values
		qui count if(`name'==.)
		if(r(N)>0){
			display as error "variable `name' should never be missing"
		}
		else{
			display as error "variable `name' does not uniquely identify the observations"
		}
	}
}

end
*************************************************************************
program define _power, rclass
   * Returns the number of zeros to tack on to the end of the variable
	* Inputs: Name of variable to perform calcs on
	* Outputs: r(power): Number of places 
	version 6.0
	args var 

	quietly sum `var'
	local power = (trunc(log10(`r(max)')))+1

	return local power `power'
	
end
