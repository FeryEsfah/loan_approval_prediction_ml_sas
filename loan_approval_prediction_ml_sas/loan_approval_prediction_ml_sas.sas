
PROC IMPORT OUT= WORK.loan 
            DATAFILE= "C:\Users\fanas\Desktop\Fery_SAS Prpject_under process\loan_approval_dataset-SAS Project.csv"
            DBMS=CSV REPLACE;
     GETNAMES=YES;
	 DATAROW=2; 
RUN;
proc contents data=loan;run;


/*Q1-	Display records: Show the first 5 and last 5 records of the dataset.*/
proc print data=loan(obs=5);
run;

proc print DATA=loan(firstobs=4265 obs=4269);
run;


/* Q2 -	Identify data types: Check and note the data types for each variable.*/
proc contents data=loan;
run;


/*Q3-	Missing entries: Determine the number of missing entries per variable.*/
proc means data=loan nmiss;
run;

proc freq data=loan;
    tables _education _self_employed _loan_status / missing;
run;


/*Q4 -	Duplicate records: Identify and count any duplicate records.*/
proc sql;
    create table duplicate_counts as
    select count(*) as Count
    from (
        select loan_id
        from loan
        group by loan_id
        having count(*) > 1
    ) as subquery;
quit;

data result_message;
    set duplicate_counts;
    length message $50;

    if Count = 0 then 
        message = "There are no duplicates in the dataset.";
    else 
        message = "There are duplicates in the dataset.";
run;

title 'Evaluating the Uniqueness of The Dataset';
proc print data=result_message noobs;
    var message;
run;


/*Q5 -	Univariate analysis: Conduct this analysis on all variables, creating appropriate visualizations.*/

/* Univariate analysis for Numeric Variables */
proc univariate data=loan;
var _income _loan_amount _loan_term credit_score _residential_assets_value _bank_asset_value;
histogram / normal
kernel;
run;
/* Univariate analysis for Categorical Variables */
proc freq data=loan;
    tables _education _self_employed _loan_status / plots=freqplot;
run;


/* Q6 -	Outlier detection:*/
/* Box plots for Numeric Variables */
/* Box plot for _income */
proc sgplot data=loan;
    vbox _income;
    title "Box Plot for Income";
run;
/* Box plot for _loan_amount */
proc sgplot data=loan;
    vbox _loan_amount;
    title "Box Plot for Loan Amount";
run;
/* Box plot for _loan_term */
proc sgplot data=loan;
    vbox _loan_term;
    title "Box Plot for Loan Term";
run;
/* Box plot for credit_score */
proc sgplot data=loan;
    vbox credit_score;
    title "Box Plot for Credit Score";
run;
/* Box plot for _residential_assets_value */
proc sgplot data=loan;
    vbox _residential_assets_value;
    title "Box Plot for Residential Assets Value";
run;
/* Box plot for _bank_asset_value */
proc sgplot data=loan;
    vbox _bank_asset_value;
    title "Box Plot for Bank Asset Value";
run;


/* Q7 -	Bivariate analysis: */

/*A) H0:Self-employment status has no effect on loan approval status.*/
title "Chi-Square Test: Self-Employment vs. Loan Approval Status";  
proc freq data=loan;  
    tables _loan_status * _self_employed / chisq;  
run;  

title "Loan Approval Status by Self-Employment Status";  
proc sgplot data=loan;  
    vbar _self_employed / group=_loan_status groupdisplay=cluster;  
run;  
title; 

title "Chi-Square Test: Self-Employment vs. Education";  
proc freq data=loan;  
    tables  _self_employed  * _education / chisq;  
run;  


/* B) two numeric:*/
title "Correlation Analysis Between Income and Loan Amount";
proc corr data=loan;
    var _income _loan_amount;
run;
title "Scatter Plot of Income vs Loan Amount";
proc sgplot data=loan;
    scatter x=_income y=_loan_amount;
run;
title; 

/* C)categorical vs. numerical
Test of normallity: H0: the data is normally distributed. */  
proc univariate data=loan normal;
class _loan_status;
var _income;
run;
/* Mann-Whitney U test: H0:The median income is the same for both loan_status groups.*/
proc npar1way data=loan wilcoxon;
    class _loan_status;  
    var _income;       
run;


/* Q8)-	Split the dataset into training and testing subsets.*/
proc surveyselect data=loan rate=0.70 outall out=result seed=12345; 
run;
data traindata testdata;
set result;
if selected='Rejected' then output traindata;
else output testdata;
run;
proc freq data=result;
table Selected;
run;


/*Q9 -	Select one categorical variable as the target(_loan_status). Perform logistic regression to predict the target variable*/

/* Logistic regression model for predicting loan approval */
proc logistic data=loan plots=(ROC);  
   class _education _self_employed / param=ref;
   model _loan_status(event="Approved") = _income _loan_amount _loan_term credit_score 
                                          _residential_assets_value _bank_asset_value 
                                          _education _self_employed
                                          / details lackfit outroc=troc;
   score data=testdata out=testpred outroc=vroc;
   roc; 
   roccontrast;
   output out=outputedata p=prob_predicted xbeta=linpred;
run; 
quit;

/* Confusion matrix */
ods html close;  
ods html;        
proc freq data=testpred;
   tables I__loan_status * F__loan_status;
run;


/* Precision & Recall and Accuracy */
ods html close;  
ods html;        
proc freq data=testpred;
   tables  I__loan_status * F__loan_status / out=ConfMatrix nopercent norow nocol;
run;

data Metrics;
   set ConfMatrix;
   if F__loan_status = "Approved" and I__loan_status = "Approved" then TP = count;
   if F__loan_status = "Rejected" and I__loan_status = "Approved" then FP = count;
   if F__loan_status = "Approved" and I__loan_status = "Rejected" then FN = count;
   if F__loan_status = "Rejected" and I__loan_status = "Rejected" then TN = count;
run;

proc means data=Metrics sum;
   var TP FP FN TN;
   output out=Results sum=TP FP FN TN;
run;

data FinalMetrics;
   set Results;
   Precision_Approved = TP / (TP + FP); 
   Recall_Approved = TP / (TP + FN);     
   Accuracy = (TP + TN) / (TP + TN + FP + FN);  
   Specificity = TN / (TN + FP);  
run;

proc print data=FinalMetrics;
run;
ods html close; 


/* Odds Ratio */
proc logistic data=loan plots(only)=(effect oddsratio); 
   class _education _self_employed / param=ref;
   model _loan_status(event="Approved") = _income _loan_amount _loan_term credit_score 
                                          _residential_assets_value _bank_asset_value 
                                          _education _self_employed
                                          / details lackfit; 
   score data=testdata out=testpred outroc=vroc;
   roc; 
   roccontrast;
   output out=outputedata p=prob_predicted xbeta=linpred;
run; 
quit;


/* Multicollinearity*/
proc reg data=loan;
   model  _loan_amount = _income _loan_term credit_score _residential_assets_value _bank_asset_value
         / vif collinoint;
   output out=outstat 
          p=Predicted 
          r=Residual 
          stdr=se_resid 
          rstudent=RStudent 
          h=Leverage 
          cookd=CooksD;
run;
quit;


