data{
  int N;
  int Nschool;
  real Y[N];
  real LRT[N];
  int school[N];
}

parameters{
  real<lower=0> sigma;
  real<lower=0> omega0;
  real<lower=0> omega1;
  real beta0[Nschool];
  real beta1[Nschool];

}

transformed parameters{
  real mu[N];
  
  for(ii in 1:N){
    mu[ii] = beta0[school[ii]]+ LRT[ii]*beta1[school[ii]];
  }
}

model{
  
  Y~normal(mu,sigma);
  
  beta0~normal(0,omega0);
  beta1~normal(0,omega1);
  
  sigma~uniform(0,100);
  omega0~uniform(0,100);
  omega1~uniform(0,100);
  
}

