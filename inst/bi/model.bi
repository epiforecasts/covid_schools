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
    alpha ~ uniform(0, 1);
    beta ~ uniform(0, 10);
  }

  sub observation {
    detected[s] ~ binomial(sample[s], alpha * (beta * prev_past[s] + prev[s]))
  }
}
