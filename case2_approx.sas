

data df_approx ;
  a1 = 1 ; 
  a2 = 16 ;
  s1 =  4 *0.5 ;
  s2 =  0.5 ;
  r1 = 4 ;
  r2 = 2 ;
  rs = (a1*s1+a2*s2)**2/( (a1 * s1 )**2/r1+(a2*s2)**2/r2) ;
  do i = 1 to 10000 ;    
    MS1 = rand("chisquare", r1)*s1/r1 ;
	MS2 = rand("chisquare", r2)*s2/r2 ;
	rs_hat = (a1*ms1+a2*ms2)**2/( (a1*ms1)**2/r1+(a2*ms2)**2/r2 ) ;
	Vs_hat = a1*MS1 + a2*MS2 ;
    Vs_approx_exact = rand("chisquare", rs) ;
    Vs_approx = rand("chisquare", rs_hat) ;
    output ;
  end ;
run ;

proc transpose data=df_approx out=df_approx_n ;
  var VS_: ;
  by i ;
run ;

data df_approx_n ;
  set df_approx_n ;
  where _NAME_ ne "Vs_approx_exact" ;
run ;


proc npar1way data=df_approx_n ;
  class _NAME_ ;
  var col1 ;
run ;

proc sgplot data=df_approx_n ;
  histogram col1 / group=_name_ transparency=0.5;       /* SAS 9.4m2 */
  density col1 / type=kernel group= _name_ ; /* overlay density estimates */
run ;




proc univariate data=df_approx ;
  var VS_hat VS_approx VS_approx_exact ;
  histogram ;
run ;


