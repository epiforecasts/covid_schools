model schools {
  // Inputs
  dim s(100)
  input sample[s];          // total number of pupils tested in SIS wave
  input prev[s];          // prevalence in community
  input prev_past[s];          // prevalence in community

  input school_size[s];    // number of pupils in school

  param alpha;                // rate of imporation (scaled by prevalence)
  param beta;                 // SAR of infected pupil at school

  noise cases[s];

  obs detected[s];

  sub parameter {
    alpha ~ truncated_gaussian(0.4, 0.8, lower=0);
    beta ~ truncated_gaussian(4, 3, lower=0);
  }

  sub observation {
    detected[s] ~ binomial(sample[s], alpha * (beta * prev_past[s] + prev[s]))
  }
}
