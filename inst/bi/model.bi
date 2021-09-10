model schools {
  // Inputs
  const observations = 2;     // number of positive tests in SIS wave
  const sample = 40;          // total number of pupils tested in SIS wave
  const prev = 0.02;          // prevalence in community
  const school_size = 300;    // number of pupils in school

  param alpha;                // rate of imporation (scaled by prevalence)
  param beta;                 // SAR of infected pupil at school

  noise imports;
  noise in_school_cases;

  obs detected;

  sub parameter {
    alpha ~ uniform(0, 1);
    beta ~ uniform(0, 1);
  }

  sub transition {
    imports ~ poisson(alpha * prev * school_size); // simulated number of imports
    in_school_cases ~ poisson(imports * beta);       // simulated number of secondary cases in school
  }

  sub observation {
    detected ~ binomial(sample, (imports + in_school_cases) / school_size)
  }
}
