#!/usr/bin/env node

const fs = require("node:fs");
const path = require("node:path");

const artifactPath = path.resolve(__dirname, "../artifacts/hsv2-models.json");
const artifact = JSON.parse(fs.readFileSync(artifactPath, "utf8"));

if (artifact.schemaVersion !== "2.0.0") {
  throw new Error(`Expected schema 2.0.0, received ${artifact.schemaVersion}.`);
}
if (!Array.isArray(artifact.models) || artifact.models.length !== 5) {
  throw new Error("Schema-v2 artifact must contain Baseline LR plus four XGBoost models.");
}

const expectedIds = ["baseline_lr", "xgb_model1", "xgb_model2", "xgb_model3", "xgb_model4"];
if (artifact.models.map((model) => model.id).join("|") !== expectedIds.join("|")) {
  throw new Error("Five-model order or identifiers do not match the locked schema.");
}

function isMissing(raw) {
  return raw === "" || raw === null || raw === undefined || !Number.isFinite(Number(raw));
}

function scoreLogistic(model, values) {
  const scoring = model.scoring;
  let logit = Number(scoring.intercept);

  Object.entries(scoring.numericCoefficients || {}).forEach(([name, spec]) => {
    const raw = values[name];
    const missing = isMissing(raw);
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

  Object.entries(scoring.categoricalContributions || {}).forEach(([name, spec]) => {
    const selected = values[name] || "Unknown";
    const levels = spec.levels || {};
    const contribution = Object.prototype.hasOwnProperty.call(levels, selected)
      ? levels[selected]
      : (levels.Unknown || 0);
    logit += Number(contribution || 0);
  });

  return { logit, probability: 1 / (1 + Math.exp(-logit)) };
}

function encodeFeatures(preprocessor, values) {
  const features = Object.fromEntries(
    (preprocessor.featureNames || []).map((feature) => [feature, 0])
  );

  Object.entries(preprocessor.numericInputs || {}).forEach(([name, spec]) => {
    const raw = values[name];
    const missing = isMissing(raw);
    const value = missing ? Number(spec.median) : Number(raw);
    features[spec.valueFeature] = value;
    features[spec.missingFeature] = missing ? 1 : 0;

    (spec.derivedTerms || []).forEach((term) => {
      let derivedValue = Number(term.median || 0);
      if (!missing && term.transform && term.transform.type === "centeredSquare") {
        derivedValue = Math.pow(value - Number(term.transform.center), 2);
      }
      features[term.feature] = derivedValue;
      features[term.missingFeature] = missing ? 1 : 0;
    });
  });

  Object.entries(preprocessor.categoricalInputs || {}).forEach(([name, spec]) => {
    const levels = spec.levels || [];
    const raw = values[name];
    const selected = raw !== null && raw !== undefined && levels.includes(String(raw))
      ? String(raw)
      : spec.unknownLevel;
    const feature = (spec.featureByLevel || {})[selected];
    if (feature) features[feature] = 1;
  });

  return features;
}

function compileTree(node) {
  if (Object.prototype.hasOwnProperty.call(node, "leaf")) {
    return { leaf: Number(node.leaf) };
  }
  const childMap = new Map();
  (node.children || []).forEach((child) => {
    childMap.set(Number(child.nodeid), compileTree(child));
  });
  return {
    split: String(node.split),
    splitCondition: Number(node.split_condition),
    yes: Number(node.yes),
    no: Number(node.no),
    missing: Number(node.missing),
    childMap
  };
}

function scoreTree(node, features) {
  if (Object.prototype.hasOwnProperty.call(node, "leaf")) return node.leaf;
  const raw = features[node.split];
  const missing = raw === null || raw === undefined || !Number.isFinite(Number(raw));
  const featureValue = Math.fround(Number(raw));
  const splitValue = Math.fround(node.splitCondition);
  const nextId = missing ? node.missing : (featureValue < splitValue ? node.yes : node.no);
  const child = node.childMap.get(nextId);
  if (!child) throw new Error(`Missing child ${nextId} below split ${node.split}.`);
  return scoreTree(child, features);
}

const compiledTrees = new Map();
artifact.models.forEach((model) => {
  if (model.scoring.kind === "xgboost") {
    compiledTrees.set(model.id, (model.scoring.trees || []).map(compileTree));
  }
});

function scoreXgboost(model, values) {
  const preprocessor = artifact.preprocessors[model.preprocessorId];
  if (!preprocessor) throw new Error(`Missing preprocessor ${model.preprocessorId}.`);
  const features = encodeFeatures(preprocessor, values);
  const treeMargin = compiledTrees.get(model.id)
    .reduce((sum, tree) => sum + scoreTree(tree, features), 0);
  const logit = Number(model.scoring.baseMargin) + treeMargin;
  return { logit, probability: 1 / (1 + Math.exp(-logit)) };
}

function scoreModel(model, values) {
  if (model.scoring.kind === "logistic") return scoreLogistic(model, values);
  if (model.scoring.kind === "xgboost") return scoreXgboost(model, values);
  throw new Error(`Unsupported scorer ${model.scoring.kind}.`);
}

let checked = 0;
let maximumError = 0;
const byKind = { logistic: 0, xgboost: 0 };

artifact.models.forEach((model) => {
  if (!Array.isArray(model.parityCases) || model.parityCases.length !== 4) {
    throw new Error(`${model.id} must contain four deterministic synthetic parity cases.`);
  }
  // XGBoost's JSON dump rounds leaf values, allowing up to two accumulated
  // margin parts per million while keeping logistic serialization much tighter.
  const tolerance = model.scoring.kind === "xgboost" ? 2e-6 : 5e-9;
  model.parityCases.forEach((fixture) => {
    if (fixture.synthetic !== true) throw new Error(`${model.id}/${fixture.id} is not marked synthetic.`);
    const actual = scoreModel(model, fixture.inputs);
    const error = Math.max(
      Math.abs(actual.logit - Number(fixture.expectedLogit)),
      Math.abs(actual.probability - Number(fixture.expectedProbability))
    );
    maximumError = Math.max(maximumError, error);
    checked += 1;
    byKind[model.scoring.kind] += 1;
    if (!Number.isFinite(error) || error > tolerance) {
      throw new Error(`${model.id}/${fixture.id} parity error ${error}; tolerance ${tolerance}.`);
    }
  });
});

if (checked !== 20 || byKind.logistic !== 4 || byKind.xgboost !== 16) {
  throw new Error(`Expected 4 logistic and 16 XGBoost checks; received ${JSON.stringify(byKind)}.`);
}

console.log(
  `Schema ${artifact.schemaVersion} JavaScript parity passed: ${checked}/20 cases ` +
  `(4 logistic, 16 XGBoost); maximum error ${maximumError.toExponential(3)}.`
);
