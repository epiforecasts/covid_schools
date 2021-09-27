model schools {
  // Inputs
  dim s(100)
  input sample[s];          // total number of pupils tested in SIS wave
  input prev[s];          // prevalence in community
  input prev_past[s];          // prevalence in community

  input school_size[s];    // number of pupils in school

  param alpha;                // rate of imporation (scaled by prevalence)
  param beta;                 // SAR of infected pupil at school

  noise imports[s];
  noise imports_past[s];
  noise in_school_cases[s];

  obs detected[s];

  sub parameter {
    alpha ~ uniform(0, 1);
    beta ~ uniform(0, 10);
  }

  sub transition {
    imports_past[s] ~ poisson(alpha * prev_past[s] * school_size[s]); // simulated number of imports
    imports[s] ~ poisson(alpha * prev[s] * school_size[s]); // simulated number of imports

    in_school_cases[s] ~ poisson(imports_past[s] * beta);       // simulated number of secondary cases in school
  }

  sub observation {
    detected[s] ~ binomial(sample[s], (in_school_cases[s] + imports[s]) / school_size[s])
  }
}
