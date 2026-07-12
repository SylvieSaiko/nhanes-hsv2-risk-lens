#!/usr/bin/env node

const fs = require("node:fs");
const path = require("node:path");

const artifactPath = path.resolve(__dirname, "../artifacts/hsv2-models.json");
const artifact = JSON.parse(fs.readFileSync(artifactPath, "utf8"));

function scoreModel(model, values) {
  let logit = Number(model.intercept);

  Object.entries(model.numericCoefficients || {}).forEach(([name, spec]) => {
    const raw = values[name];
    const missing = raw === "" || raw === null || raw === undefined || !Number.isFinite(Number(raw));
    const value = missing ? Number(spec.median) : Number(raw);
    logit += Number(spec.coefficient || 0) * value;
    if (missing) logit += Number(spec.missingCoefficient || 0);

    (spec.derivedTerms || []).forEach((term) => {
      let derivedValue = Number(term.median || 0);
      if (!missing && term.transform && term.transform.type === "centeredSquare") {
        derivedValue = Math.pow(value - Number(term.transform.center), 2);
      }
      logit += Number(term.coefficient || 0) * derivedValue;
      if (missing) logit += Number(term.missingCoefficient || 0);
    });
  });

  Object.entries(model.categoricalContributions || {}).forEach(([name, spec]) => {
    const selected = values[name] || "Unknown";
    const contribution = Object.prototype.hasOwnProperty.call(spec.levels, selected)
      ? spec.levels[selected]
      : (spec.levels.Unknown || 0);
    logit += Number(contribution || 0);
  });

  return { logit, probability: 1 / (1 + Math.exp(-logit)) };
}

let checked = 0;
let maximumError = 0;

artifact.models.forEach((model) => {
  model.parityCases.forEach((fixture) => {
    const actual = scoreModel(model, fixture.inputs);
    const error = Math.max(
      Math.abs(actual.logit - Number(fixture.expectedLogit)),
      Math.abs(actual.probability - Number(fixture.expectedProbability))
    );
    maximumError = Math.max(maximumError, error);
    checked += 1;
    if (!Number.isFinite(error) || error > 5e-9) {
      throw new Error(`${model.id}/${fixture.id} parity error ${error}`);
    }
  });
});

if (checked !== 16) throw new Error(`Expected 16 parity cases, received ${checked}.`);
console.log(`JavaScript parity passed: ${checked}/16 cases; maximum error ${maximumError.toExponential(3)}.`);
