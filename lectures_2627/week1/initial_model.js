// Use u for the treatment-only error, consistent with the conceptual interlude.
module.exports = {
  treatmentOnlyModel: String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+u_{ij}`,
  treatmentOnlyAssumption: String.raw`u_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma_u^2)`,
};
