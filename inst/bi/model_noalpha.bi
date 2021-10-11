
model schools {
  // Inputs


  dim s(100)

  input sample[s];          // total number of pupils tested in SIS wave
  input prev[s];          // prevalence in community
  input prev_past[s];          // prevalence in community

  input school_size[s];    // number of pupils in school

  param beta;                 // SAR of infected pupil at school

  noise imports[s];
  //noise imports_past[s];
  noise in_school_cases[s];

  //noise cases[s];

  obs detected[s];

  sub parameter {
    beta ~ uniform(0., 10.);
  }

  sub transition {
    imports[s] ~ poisson(0.8 * prev[s] * school_size[s]); // simulated number of imports
    in_school_cases[s] ~ poisson(imports[s] * beta);
  }

  sub observation {
    detected[s] ~ binomial(sample[s], (imports[s] + in_school_cases[s])/school_size[s]);

  }
  
  sub proposal_parameter {
    beta ~ truncated_gaussian(beta, 0.5, lower=0, upper=15); // local proposal
    beta ~ truncated_gaussian(5, 1.0, lower=0,  upper=15); // independent proposal
  }

}
