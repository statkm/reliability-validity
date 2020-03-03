%let path = ; * set the appropriate path ;

%include "&path.\reliability_macro.sas" ;

data df_1 ;
  mu = 0 ;
  n=10 ;  k=5 ;
  do target = 1 to n ;
    do i = 1 to k ;
	  b = rand("normal") ;
      x = mu + b + rand("normal") ;
	  output ;
	end ;
  end ;
run ;

%CASE1(_data=df_1) 





%include "&path.\df_79_table2.sas" ;

data df_tab2n_mod ;
  set df_tab2n ;
  i =_name_ ;
  x = col1 ;
  k = 4 ;
  n = 6 ;
run ;


%CASE1(_data=df_tab2n_mod) 
%CASE2(_data=df_tab2n_mod)
%CASE3(_data=df_tab2n_mod)





