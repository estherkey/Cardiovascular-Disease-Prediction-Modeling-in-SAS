PROC IMPORT OUT=CARDIO
    DATAFILE= "/home/u63551057/STAT660/cardio_train.csv"
    DBMS=CSV
    REPLACE;
    DELIMITER=";";
    GETNAMES=YES;
RUN;

proc surveyselect data=CARDIO
    out=CARDIO
    method=srs 
    sampsize=7000 
    seed=123; 
Run;

DATA CARDIO;   
  SET CARDIO;
  age = age/365;
  BMI = WEIGHT / (HEIGHT*0.01) ** 2;
RUN;


/* Subset no disease group */
DATA NoDisease;
  SET CARDIO;
  WHERE CARDIO = 0;
RUN;

DATA NoDisease;
  SET NoDisease;
  /* Replace 'X' with the variable you want to winsorize */
  IF ap_hi > 750 THEN ap_hi = 130;
  IF ap_lo > 750 THEN ap_lo = 97;
  IF BMI > 80 THEN BMI = 28;
run;


proc means data=NoDisease;
   var bmi; /* Request standard deviation for each variable */
run;

bmi age ap_hi ap_lo

/* Subset the heart disease group */
DATA HaveDisease;
  SET CARDIO;
  WHERE CARDIO = 1;
RUN;

DATA HaveDisease;
  SET HaveDisease;
  /* Replace 'X' with the variable you want to winsorize */
  IF ap_hi > 750 THEN ap_hi = 130;
  IF ap_lo > 750 THEN ap_lo = 97;
  IF BMI > 80 THEN BMI = 28;
run;


*Changing format for the output;
proc format;
	value cardiofm
		0 = "No Disease"
		1 = "Heart Disease";
	value genderfm
		1 = "Women"
		2 = "Men";
	value cholesfm
		1 = "Normal"
		2 = "Above Normal"
		3 = "Well Above Normal";
	value glucfm
		1 = "Normal"
		2 = "Above Normal"
		3 = "Well Above Normal";
	value smokefm
		0 = "No"
		1 = "Yes";
	value activefm
		0 = "Not Active"
		1 = "Active";
RUN;

Data CARDIO1;
	Set CARDIO;
	format Cardio cardiofm.
			gender genderfm.
			cholesterol cholesfm.
			gluc glucfm.
			smoke smokefm.
			active activefm.;
  cardio_num = input(cardio, 8.);
run;


/******** EDA ********/
/* Correlation matrix of all numerical variables */
proc corr data=CARDIO;
VAR age height weight ap_hi ap_lo;
run;

/* Distribution of heart disease */
proc sgplot data=CARDIO1;
VBAR cardio;
TITLE 'Distribution of Heart Disease';
RUN;

/* Distribution of heart disease by gender */
proc sgplot data=CARDIO1;
VBAR cardio / GROUP = gender;
TITLE 'Gender';
RUN;

/* Bar plot of cholesterol */
proc sgplot data=CARDIO1;
VBAR cholesterol / GROUP = cardio GROUPDISPLAY = CLUSTER;
TITLE 'Bar Plot of Cholesterol by target variable';
RUN;

proc contents data = cardio;
run;

proc sgplot data=CARDIO1;
VBAR cardio / group = cholesterol;
TITLE "Cholesterol";
RUN;

/* Bar plot of glucose level */
proc sgplot data=CARDIO1;
VBAR gluc / GROUP = cardio GROUPDISPLAY = CLUSTER;
TITLE 'Bar Plot of Glucose Level by target variable';
RUN;

proc sgplot data=CARDIO1;
VBAR cardio / GROUP = gluc;
TITLE 'Glucose Level';
RUN;

/* Bar plot of smoking */
proc sgplot data=CARDIO1;
VBAR smoke / GROUP = cardio GROUPDISPLAY = CLUSTER;
TITLE 'Bar Plot of Smoking by target variable';
RUN;

proc sgplot data=CARDIO1;
VBAR cardio / GROUP = smoke;
TITLE 'Smoking';
RUN;

/* Bar plot of physical activity */
proc sgplot data=CARDIO1;
VBAR active / GROUP = cardio GROUPDISPLAY = CLUSTER;
TITLE 'Bar Plot of Physical Activity by target variable';
RUN;

proc sgplot data=CARDIO1;
VBAR cardio / GROUP = active;
TITLE 'Physical Activity';
RUN;


/* Box plot of age */
proc sgplot data=CARDIO1;
vbox age/ category=cardio;
TITLE 'Distribution of Age';
 run;


/* Replace outliers in ap_hi, ap_lo, and BMI */
DATA CARDIO1;
  SET CARDIO1;
  /* Replace 'X' with the variable you want to winsorize */
  IF ap_hi > 750 THEN ap_hi = 130;
  IF ap_lo > 750 THEN ap_lo = 97;
  IF BMI > 80 THEN BMI = 28;
run;

/* Box plot of systolic blood pressure */
proc sgplot data=CARDIO1;
vbox ap_hi/ category=cardio;
TITLE 'Distribution of Systolic Blood Pressure';
 run;

/* Box plot of diastolic blood pressure */
proc sgplot data=CARDIO1;
vbox ap_lo/ category=cardio;
TITLE 'Distribution of Diastolic Blood Pressure';
run;
 
/* Box plot of BMI */
proc sgplot data=CARDIO1;
vbox bmi / category=cardio;
TITLE 'Distribution of BMI';
run;

/* Box plot of Height */
proc sgplot data=CARDIO1;
vbox height / category=cardio;
TITLE 'Distribution of Height';
run;

/* Box plot of Weight */
proc sgplot data=CARDIO1;
vbox weight / category=cardio;
TITLE 'Distribution of Weight';
run;

/******** T-Tests ********/
PROC TTEST data=CARDIO;
CLASS cardio;
VAR age height weight ap_hi ap_lo BMI;
run;

/* Chi-square test of independence - categorical variables */
proc freq data=cardio1;
table gluc*cholesterol / chisq;
run;
* there is an association between glucose level and cholesterol;

proc freq data=cardio1;
table gluc*gender / chisq;
run;
* no association between gender and glucose level;

proc freq data=cardio1;
table gluc*smoke / chisq;
run;
* no association between smoking and glucose level;

proc freq data=cardio1;
table gluc*active / chisq;
run;
* no association between active and glucose level;

proc freq data=cardio1;
table smoke*gender/ chisq;
run;
* smoking is also assoicated with cholesterol and gender;

%let cat_vars = cholesterol gluc smoke gender active; /* Replace with your categorical variable names */

%macro run_chi_square_tests;
  %let n = %sysfunc(countw(&cat_vars)); /* Get the number of categorical variables */
  
  /* Outer loop for the first categorical variable */
  %do i = 1 %to &n;
    %let var1 = %scan(&cat_vars, &i); /* Get the first variable name */
    
    /* Inner loop for the second categorical variable */
    %do j = %eval(&i + 1) %to &n;
      %let var2 = %scan(&cat_vars, &j); /* Get the second variable name */
      
      /* Run the chi-square test for the pair of variables */
      proc freq data=cardio1;
        tables &var1 * &var2 / chisq;
      run;
    %end;
  %end;
%mend;

%run_chi_square_tests;

proc freq data=cardio1;
table cardio*active / chisq;
run;
* no association between active and glucose level;

*Dividing the data into train and test;
*Sorting the dataset;
proc sort data = Cardio out= Cardio1;
by Cardio;
run;

*Perform Simple random sampling;
proc surveyselect data= Cardio1 rate=0.7 outall out=Cardio2 seed=123;
run;

*Split the data into train 70% and test30%;
data train test;
set Cardio2;
if selected = 1 then output train;
else output test;
drop selected;
run;


*Checking the frequency of Cardio in each train and testd dataset;
title "Train data";
proc freq data = train;
table Cardio;
run;
title "Test data";
proc freq data = test;
table Cardio;
run;

*Model Building;* with gluc and smoke;
proc logistic descending data = train;
	class gender cholesterol(ref="1") gluc(ref="1") smoke(ref="0") alco(ref="0") active(ref="0")/ param = ref;
	model cardio(event = "Heart Disease") = age gender BMI ap_hi ap_lo cholesterol gluc smoke alco active/selection=stepwise lackfit;
	output out = mod_train p = Probability predprobs = individual;
	store cardio_logistic;
run;

