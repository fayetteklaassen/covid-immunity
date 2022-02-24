
// The input data 
data {
  int<lower=0> N; // number of unique rows
  int<lower=1> W; //number of weeks
  matrix[N,W] xweek; // week data
  vector<lower=0,upper=1>[N] xcovid; // covid data
  int<lower=0> y[N]; // number of cases y = 1 per row
  int<lower=0> u[N]; // number of cases total per row
}


// The parameters
parameters {
  real b0;
  vector[W] b_week;
  real b_covid;
}
transformed parameters{
 
  vector[N] theta;

  theta = b0 + xweek*b_week + xcovid*b_covid;

}
// The model
model {
     target += binomial_logit_lpmf(y | u, theta);
}

//generated quantities
generated quantities{
    real OR;
  OR = exp(b_covid);
}
