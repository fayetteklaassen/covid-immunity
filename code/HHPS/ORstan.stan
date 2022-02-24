// The input data 
data {
  int<lower=0> N; // number of unique rows
  int<lower=1> L; // number of locations
  int<lower=1> W; //number of weeks
  matrix[N,W] xweek; // week data
  matrix[N,L] xloc; //location data
  matrix[N,L] xloccov; //location * covid
  vector<lower=0,upper=1>[N] xcovid; // covid data
  int<lower=0> y[N]; // number of cases y = 1 per row
  int<lower=0> u[N]; // number of cases total per row
}


// The parameters
parameters {
  real b0;
  vector[W] b_week;
  vector[L] b_loc;
  real b_covid;
  real<lower=0> tau;
  vector[L] g_loc_covid;
}
transformed parameters{
  vector[N] theta;
  vector[L] beta;
  beta = b_covid + g_loc_covid;
  
  theta = b0 + xweek*b_week + xloc*b_loc + xloccov*beta;
}

// The model
model {

  for(l in 1:L){
    g_loc_covid[l] ~ normal(0, tau);
  }

     target += binomial_logit_lpmf(y | u, theta);
    
}

//generated quantities
generated quantities{
  vector<lower=0>[L] OR;
  OR = exp(beta);
}
