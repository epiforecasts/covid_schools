
model schools {
  // Inputs
<<<<<<< HEAD
  dim s(10)
=======
  dim s(100)
>>>>>>> 20ea340fe340cc8819ee9ba92ee20234fee67f25
  input sample[s];          // total number of pupils tested in SIS wave
  input prev[s];          // prevalence in community
  input prev_past[s];          // prevalence in community

  input school_size[s];    // number of pupils in school

  param alpha;                // rate of imporation (scaled by prevalence)
  param beta;                 // SAR of infected pupil at school

<<<<<<< HEAD
  //noise imports[s];
  noise imports_past[s];
  //noise in_school_cases[s];
=======
>>>>>>> 20ea340fe340cc8819ee9ba92ee20234fee67f25
  noise cases[s];

  obs detected[s];

  sub parameter {
<<<<<<< HEAD
    alpha ~ truncated_gaussian(0.4, 0.8, lower=0);
    beta ~ truncated_gaussian(4, 3, lower=0);
  }

  sub transition {
    imports_past[s] ~ poisson(alpha * prev_past[s] * school_size[s]); // simulated number of imports
    cases[s] ~ poisson(imports_past[s] * beta + alpha * prev[s] * school_size[s])
  }

  sub observation {
    detected[s] ~ binomial(sample[s], cases[s]/school_size[s]);
=======
    alpha ~ uniform(0, 1);
    beta ~ uniform(0, 10);
  }

  sub observation {
    detected[s] ~ binomial(sample[s], alpha * (beta * prev_past[s] + prev[s]))
>>>>>>> 20ea340fe340cc8819ee9ba92ee20234fee67f25
  }
  
  sub proposal_parameter {
    alpha ~ truncated_gaussian(alpha, 0.2, lower=0, upper=2); // local proposal
    alpha ~ truncated_gaussian(0.5, 0.2, lower=0, upper=2); // independent proposal
    beta ~ truncated_gaussian(beta, 0.5, lower=0, upper=15); // local proposal
    beta ~ truncated_gaussian(5, 1.0, lower=0,  upper=15); // independent proposal
  }

}
