// Admissions discharge model
// relate influenza admissions and occupancy time series by estimating discharge probabilities
// allows independence of regional fits
// partial pooling: lognormal_mu
// no pooling: beta
// complete pooling: lognormal_sigma

data {
  int < lower = 1 > N; // Sample size
  int < lower = 1 > T; // Max number of time steps
  int < lower = 1 > R; // Max number of region
  int < lower = 1 > J; // Max Length of Stay lookback
  array[N] int<lower=1, upper=T> day;
  array[N] int<lower=1, upper=R> region;
  vector[N] y; // occupancy
  vector[N] x; // admissions
}

transformed data {
  array[R, T] real occupancy_regional = rep_array(0, R, T);
  array[R, T] real admissions_regional = rep_array(0, R, T);

  // convert to 2D array
  for (n in 1:N) {
    occupancy_regional[region[n], day[n]] += y[n];
    admissions_regional[region[n], day[n]] += x[n];
  }
}

parameters {
  // we want to parameterise a lognormal_cdf to be our discharge probability over j days stayed
  array[R] real<lower=0> lognormal_mu;  //length of stay mean, allows variation regionally
  real<lower=0> mu_mean; // pooling mean of lognormal_mu
  real<lower=0> mu_sd; // pooling sd of lognormal_mu

  real<lower=0> lognormal_sigma; // length of stay distribution standard deviation term
  // error term
  array[R] real<lower=0> beta;  //allows variation regionally

}




model {

  vector[J] h; // recent history of admissions for a given time
  vector[J] f; // recent history minus discharge


  lognormal_sigma ~ cauchy(1,1.5); // prior on discharge distribution standard deviation
  beta ~ normal(0, 0.5); // prior on error shape parameter

  // pooling of the lognormal_mu
  lognormal_mu ~ normal(mu_mean, mu_sd); // prior on discharge distribution
  mu_mean ~ normal(log(8), 2); // assuming average length of stay is 8 days
  mu_sd ~ normal(log(5), 2); // assuming 5 is the standard deviation of the length of stay


  // iterate over the regions
  for (r in 1:R) {
  // iterate over the time series, excluding first lookback length
  for (i in 1+J:T) {

    // iterate over lookback period for a given time point
    for (j in 1:J) {

      // calculate how many patients at this lookback, converted to admissions
      h[j] = admissions_regional[r, i-j+1];

      // estimate how many patients remain after removing discharged patients
      f[j] = h[j] - h[j]*lognormal_cdf(j-1 | lognormal_mu[r], lognormal_sigma);

    }

    // fit on the sum of remaining patients
    // mean = alpha / beta, therefore we can get alpha from sum(f)*
    occupancy_regional[r, i] ~ gamma(sum(f)*beta[r], beta[r]);

  }
  }
}

// using generated quantities to show what the historic fit would have looked like
generated quantities {

  array[R, T] real y_hat = rep_array(0, R, T); // predicted output
  vector[J] h_hat; // recent history of admissions for a given time
  vector[J] f_hat; // recent history minus discharge

  for (r in 1:R) {

  // fillin pre max length of stay lookback as the true data
  for (i in 1:J) {
    y_hat[r, i] = occupancy_regional[r, i];

  }

  // produce predictions in same manner as model calculation
  for (i in 1+J:T) {

    for (j in 1:J) {

      {
      h_hat[j] = admissions_regional[r, i-j+1];
      f_hat[j] = h_hat[j] - h_hat[j] * lognormal_cdf(j-1 | lognormal_mu[r], lognormal_sigma);
    }

    }

    y_hat[r, i] = gamma_rng(sum(f_hat)*beta[r], beta[r]);


  }
  }

}
