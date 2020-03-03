%macro CASE1(_data) ;
ODS LISTING CLOSE;
  ods trace on ;
  ods output OverallANOVA=anova_result ;
  proc glm data=&_data ;
    class i target ;
    model x= target / ss3 ;
    random target  ;
  run ;
  quit ;

  data df_case1 ;
    set anova_result ;
    keep Dependent Source df MS  lab ;
    if source = "Error" then lab="WMS" ;
    else if source = "Model" then lab="BMS" ;
  run ;

  proc sort data=df_case1 ; by source ; run ;

  proc transpose data=df_case1 out = df_case1_w  ;
    var ms ;
    by  dependent ;
    id lab ;
  run ;

  data df_case1_w ;
    merge df_case1_w(in=in_r) &_data  ;
    hat_rho   = (BMS-WMS)/(BMS+(k-1)*WMS) ;
    hat_rho_k = (BMS-WMS)/BMS ;

    * calculating C.I. ;
    alpha = 0.05 ;
    F0 = BMS/WMS ;
    F_L = F0 / ( finv(1-alpha/2, n-1, n*(k-1)) ) ;
    F_U = F0 * finv(1-alpha/2,n*(k-1), n-1) ;
    rho_L = (F_L-1)/(F_L+k-1) ;
    rho_U = (F_U-1)/(F_U+k-1) ;
    if in_r ;
    drop Dependent _NAME_ _LABEL_ target COL1 i x F_L F_U F0 ;
  run ;
  ODS LISTING;
  proc print ; run ;


%mend CASE1;








%macro CASE2(_data) ;
ODS LISTING CLOSE;
ods trace on ;
ods output OverallANOVA=anova_result ModelANOVA = anova_result2 ;
proc glm data=&_data ;
class i target ;
model x= i target  / ss3 ;
random target  ;
run ;
quit ;

data df_case2 ;
set anova_result anova_result2  ;
keep Dependent Source df MS  lab ;
if source = "Error" then lab="EMS" ;
else if source = "i" then lab="JMS" ;
else if source = "target" then lab="BMS" ;
run ;

data df_case2 ;
  set df_case2 ;
  where lab ne "" ;
run ;

proc sort data=df_case2 ; by source ; run ;

proc transpose data=df_case2 out = df_case2_w  ;
var ms ;
by  dependent ;
id lab ;
run ;

data df_case2_w ;
merge df_case2_w(in=in_r) &_data  ;
hat_rho   = (BMS-EMS)/( BMS+(k-1)*EMS +k*(JMS-EMS)/n ) ;
hat_rho_k = (BMS-EMS)/( BMS+(JMS-EMS)/n ) ;
if in_r ;


* calculating C.I. ;
alpha = 0.05 ;
FJ = BMS/EMS ;
nu = (k-1)*(n-1)*( k*hat_rho*FJ+n*(1+(k-1)*hat_rho)-k*hat_rho )**2/( (n-1)*k**2*hat_rho**2*FJ**2 + (n*(1+(k-1)*hat_rho)-k*hat_rho)**2 ) ;
F_L = finv(1-alpha/2, n-1, nu)  ;
F_U = finv(1-alpha/2,nu, n-1) ;
rho_L = n*(BMS-EMS*F_U)/( n*BMS+ ( k*JMS +(k*n-k-n)*EMS )*F_U ) ;
rho_U = n*(BMS*F_L-EMS)/( n*BMS*F_L + k*JMS +(k*n-k-n)*EMS  ) ;
if in_r ;
drop Dependent _NAME_ _LABEL_ target COL1 i x F_L F_U F0 ;
run ;
ODS LISTING;
proc print ; run ;
%mend CASE2 ;




%macro CASE3(_data) ;
ODS LISTING CLOSE;
ods trace on ;
ods output OverallANOVA=anova_result ModelANOVA = anova_result2 ;
proc glm data=&_data ;
class i target ;
model x= i target  / ss3 ;
random target  ;
run ;
quit ;

data df_case3 ;
set anova_result anova_result2  ;
keep Dependent Source df MS  lab ;
if source = "Error" then lab="EMS" ;
else if source = "i" then lab="JMS" ;
else if source = "target" then lab="BMS" ;
run ;

data df_case3 ;
  set df_case3 ;
  where lab ne "" ;
run ;

proc sort data=df_case3 ; by source ; run ;

proc transpose data=df_case3 out = df_case3_w  ;
var ms ;
by  dependent ;
id lab ;
run ;

data df_case3_w ;
merge df_case3_w(in=in_r) &_data  ;
hat_rho   = (BMS-EMS)/( BMS+(k-1)*EMS  ) ;
hat_rho_k = (BMS-EMS)/BMS   ;
if in_r ;

* calculating C.I. ;
alpha = 0.05 ;
F0 = BMS/EMS ;
F_L = F0 / ( finv(1-alpha/2, n-1, n*(k-1)) ) ;
F_U = F0 * finv(1-alpha/2,n*(k-1), n-1) ;
rho_L = (F_L-1)/(F_L+k-1) ;
rho_U = (F_U-1)/(F_U+k-1) ;
if in_r ;
drop Dependent _NAME_ _LABEL_ target COL1 i x F_L F_U F0 ;
run ;
ODS LISTING;
proc print ; run ;


%mend CASE3 ;
