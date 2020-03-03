data df_tab2 ;
    target = _n_ ;
    input judge1 judge2 judge3 judge4 ;
    datalines;
    9 2 5 8 
    6 1 3 2
    8 4 6 8
    7 1 2 6
    10 5 6 9
    6 2 4 7
    ;
run ;

PROC TRANSPOSE data=df_tab2 out=df_tab2n ;
    by target ;
    var judge: ;
RUN;
