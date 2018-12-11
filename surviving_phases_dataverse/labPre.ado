*! lab(el)Pre(fix), v1.24
*! Author: Shawna Metzger, skm29@pitt.edu
*! Last Modified: 11FEB11
*! Adds the ability to add a prefix to already existing varlabels.  (e.g., you merge in variables from Pevehouse and Russett, and you want to keep their variable labels, but have "P&R: (the existing variable label)"

capture program drop labPre
program define labPre
version 8.0
syntax varlist, [Stub(string) UNdo]
local vars `varlist'

/* Step 0: Housekeeeping 
		- Stub is optional above, only because it does not need to be specified if undo is inputted.
		- Therefore, check to make sure that stub is present IF undo is NOT specified.
		- If stub isn't present, kick out an error message, and exit the program.	*/
		
if("`stub'"=="" & "`undo'"==""){
	display as error "Prefix to add must be specified"
	exit 198
}
		
// Step 1: The actual program		
foreach x of local vars{
	// Retrieve the existing label
	local current: variable label `x'
		
	// First, remove any prefix that's already there, if requested
	if("`undo'"!=""){				
		// Find the colon, and split it off
		gettoken irr1 current:current, parse(": ")
		gettoken irr2 current:current
		local current = trim("`current'")
	}
	
	// Then, add the prefix (if it's specified, including if it's a new prefix after removing)
	if("`stub'"!=""){
		// Add the prefix
		local current = "`stub': `current'"
	}
	
	// Save the new label
	qui label variable `x' `"`current'"'
}


end

