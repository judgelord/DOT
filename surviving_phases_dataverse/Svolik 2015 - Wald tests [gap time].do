/*	
	Overall punch line: 
	- Coefficient pairs aren't significantly different from one another (could collapse) [lines ~49-50]
		> however, exec_pres4 = exec_pres5 is weakly sig (0.0792)
	- the full model (each var with transition-specific coefficient) is definitely better than the restricted model (each var has one coefficient) [line 73]
	- many coefficients for gdp are better than just one [line 94]
	- one coefficient for pres is better than many [line 113]
	
*/


** Run under Stata 14.






















**********Compare restricted model with 1 coefficient per variable, to full model with 4 coefficients per variable

*estimate full model
#delimit ;

stcox 	lgdp_1_std1  growth_1_std1  exec_pres1  prevd_mil1  
		lgdp_1_std2  growth_1_std2  exec_pres2  prevd_mil2  
		lgdp_1_std4  growth_1_std4  exec_pres4  prevd_mil4  
		lgdp_1_std5  growth_1_std5  exec_pres5  prevd_mil5  
	
	, breslow nohr strata(trans)
	;
#delimit cr

estimates store a

	// Test whether coefficient pairs are different for GDP and pres
	est res a
	foreach s in lgdp_1_std exec_pres{
		test `s'1 = `s'2	// the two outward transitions from dem
		test `s'4 = `s'5	// the two inward transitions to dem
	}

	
*estimate restricted model

foreach s in lgdp_1_std growth_1_std exec_pres prevd_mil{
	cap ecap gen `s' = rowtotal(`s'1-`s'6)
}

#delimit ;

stcox 	lgdp_1_std growth_1_std exec_pres prevd_mil
	, breslow nohr strata(trans)
	;
#delimit cr

estimates store b



*the full and restricted models are significantly different at the 0.001 level, chisq is 40.84
lrtest a b
	** a remains current incumbent model

**********Test Whether 1 coefficient for GDP or many are better
#delimit ;

stcox 	lgdp_1_std  growth_1_std1  exec_pres1  prevd_mil1  
					growth_1_std2  exec_pres2  prevd_mil2  
					growth_1_std4  exec_pres4  prevd_mil4  
					growth_1_std5  exec_pres5  prevd_mil5  
	, breslow nohr strata(trans)
	;
#delimit cr

estimates store c

*Model with transition-specific gdp covariates performs better than a model with 1 coefficient for gdp, p < 0.000
lrtest a c
	** a remains current incumbent

**********Test Whether 1 coefficient for pres or many are better
#delimit ;

stcox 	lgdp_1_std1  growth_1_std1  exec_pres  	prevd_mil1  
		lgdp_1_std2  growth_1_std2  			prevd_mil2  
		lgdp_1_std4  growth_1_std4  			prevd_mil4  
		lgdp_1_std5  growth_1_std5  			prevd_mil5  
	
	, breslow nohr strata(trans)
	;
#delimit cr

estimates store d

*Model with transition-specific pres covariates does NOT perform any better than a model with 1 coefficient for pres, p = 0.1783
lrtest a d
	** Makes d the current incumbent
	
*********************************************************************************
// Start running through the other permutations, starting from the last result--that it's best to collapse all the pres effects.

// ** GROWTH **
	* best to collapse all the growths?
	#delimit ;

	stcox 	lgdp_1_std1  growth_1_std  exec_pres  	prevd_mil1  
			lgdp_1_std2  				 			prevd_mil2  
			lgdp_1_std4  				  			prevd_mil4  
			lgdp_1_std5  				 			prevd_mil5  
		
		, breslow nohr strata(trans)
		;
	#delimit cr

	estimates store d_a

	*Model with transition-specific growth covariates does NOT perform any better than a model with 1 coefficient for growth, p = 0.5331
	lrtest d d_a
		** Makes d_a the current incumbent

// ** MIL **
	* best to collapse all the prev_mils?
	#delimit ;

	stcox 	lgdp_1_std1  growth_1_std  exec_pres  	prevd_mil  
			lgdp_1_std2  				 			
			lgdp_1_std4  				  			
			lgdp_1_std5  				 			  
		
		, breslow nohr strata(trans)
		;
	#delimit cr

	estimates store d_a_a

	*Model with transition-specific mil covariates DOES perform better than a model with 1 coefficient for mil, p = 0.0063
	lrtest d_a d_a_a
		** d_a stays the current incumbent
	
	* are the mil transitions different?
	est res a // load up the orig model to tell (just for a fixed point of reference)
	foreach s in prevd_mil  {
		test `s'1 = `s'2	// the two outward transitions from dem		** SS
		test `s'4 = `s'5	// the two inward transitions to dem		** NS
		
		/*test `s'1 = `s'4	// the transitions involving D and Ex
		test `s'2 = `s'5	// the transitions involving D and En*/
	}
		// verdict: the two outward trans from D can't be collapsed, but the two inward to D can
		
	cap gen prevd_milIn = cond(trans==4 | trans==5, prevd_mil, 0)
	
	
	* try what we just figured out above
	#delimit ;

	stcox 	lgdp_1_std1  growth_1_std  exec_pres  	prevd_mil1 
			lgdp_1_std2  				 			prevd_mil2 
			lgdp_1_std4 				  			    				  			
			lgdp_1_std5   				  			prevd_milIn 				 			  
		
		, breslow nohr strata(trans)
		;
	#delimit cr

	estimates store d_a_b
	
	
	*Full model with four specific effects does NOT perform any better than a model with 3 coefficients for mil, p = 0.0892
	lrtest d_a d_a_b
		** Makes d_a_b the current incumbent
	

// ** GDPPC **	
	* From earlier Walds, we know the outward and inward transitions can be collapsed.  Check this.
	cap gen lgdp_1_stdOut = cond(trans==1 | trans==2, lgdp_1_std, 0)
	cap gen lgdp_1_stdIn  = cond(trans==4 | trans==5, lgdp_1_std, 0)
	
	
	#delimit ;
	stcox 	lgdp_1_stdOut  growth_1_std  exec_pres  	prevd_mil1 
														prevd_mil2 
						  			    				  			
			lgdp_1_stdIn   				  				prevd_milIn 				 			  
		
		, breslow nohr strata(trans)
		;
	#delimit cr

	estimates store d_a_b_a
	
	*Full model with four specific effects does NOT perform any better than a model with 2 coefficients for gdppc, p = 0.1831
	lrtest d_a_b d_a_b_a
		** Makes d_a_b_a the current incumbent
		*** WINNER ***
		
// We've now run through all four covariates, for the reasonable theoretical combinations.  Does our incumbent model have best GOF?
est stats _all
	// It does, for BIC, which is the more punishing of the two ICs.
		
