(function () {
  "use strict";

  const artifact = window.HSV2_MODELS;
  if (!artifact || artifact.schemaVersion !== "2.0.0" || !Array.isArray(artifact.models) || artifact.models.length !== 5) {
    document.body.innerHTML = "<main style='padding:2rem;font-family:sans-serif'><h1>Model artifact unavailable</h1><p>Run the website exporter before opening this page.</p></main>";
    return;
  }

  const modelOrder = ["baseline_lr", "xgb_model1", "xgb_model2", "xgb_model3", "xgb_model4"];
  const models = artifact.models.slice().sort((a, b) => modelOrder.indexOf(a.id) - modelOrder.indexOf(b.id));
  const inputs = artifact.inputs;
  const preprocessors = artifact.preprocessors;

  const groupDefinitions = [
    {
      id: "eligibility",
      index: "A",
      tier: 1,
      title: "Eligibility",
      description: "The research cohort was restricted before modeling. These answers establish whether the estimate applies.",
      badge: "Required",
      fields: ["prior_diagnosis", "age"]
    },
    {
      id: "baseline",
      index: "B",
      tier: 1,
      title: "Demographic and social context",
      description: "The four-variable baseline uses sex, race/ethnicity, and education here. ML Model 1 additionally requests partnership and income ratio.",
      badge: "4-variable LR · 6-variable ML1",
      fields: ["sex", "race_ethnicity", "education", "partnership", "pir"]
    },
    {
      id: "lifestyle",
      index: "C",
      tier: 2,
      title: "Lifestyle and healthcare access",
      description: "Adds insurance, smoking, alcohol, and drug-use history without requiring examination or laboratory data.",
      badge: "Added in Model 2",
      fields: ["insured", "current_smoker", "alcohol_drinks_day", "ever_hard_drug", "ever_injection_drug"]
    },
    {
      id: "clinical",
      index: "D",
      tier: 3,
      title: "Routine examination and laboratory values",
      description: "Use the measured values and units shown. If these are unavailable, Model 2 is the appropriate lower-burden option.",
      badge: "Added in Model 3",
      fields: ["bmi", "waist", "sbp", "hba1c", "hdl", "total_chol", "alt", "ast", "alp", "ggt", "creatinine", "wbc", "hemoglobin", "platelets"]
    },
    {
      id: "sensitive",
      index: "E",
      tier: 4,
      title: "Direct sexual and STI history",
      description: "These questions produced the clearest final improvement. Select a lower tier if direct sexual history is not appropriate to request.",
      badge: "Sensitive · Model 4",
      sensitive: true,
      fields: ["age_first_sex_group", "partners_group", "condomless_sex", "prior_other_sti", "circumcision"]
    }
  ];

  const modelCopy = {
    baseline_lr: {
      marker: "REF",
      title: "Baseline logistic model",
      body: "Uses age, sex, race/ethnicity, and education in a transparent weighted logistic regression. It deliberately omits partnership status and poverty-income ratio.",
      burden: "Lowest information burden",
      questionTitle: "Four-variable baseline information"
    },
    xgb_model1: {
      marker: "ML1",
      title: "ML Model 1 · demographic",
      body: "Applies XGBoost to six sociodemographic inputs, adding partnership status and poverty-income ratio to the four-variable logistic benchmark.",
      burden: "Two inputs beyond Baseline-LR",
      questionTitle: "Six-variable demographic information"
    },
    xgb_model2: {
      marker: "ML2",
      title: "ML Model 2 · lifestyle and access",
      body: "Adds healthcare access, smoking, alcohol, and drug-use history. It still requires no examination or laboratory values.",
      burden: "No laboratory data",
      questionTitle: "Baseline + lifestyle information"
    },
    xgb_model3: {
      marker: "ML3",
      title: "ML Model 3 · routine clinical",
      body: "Adds examination and routine laboratory measurements while deliberately omitting direct sexual-history questions.",
      burden: "Sexual-history-sparing",
      questionTitle: "Routine clinical information"
    },
    xgb_model4: {
      marker: "ML4",
      title: "ML Model 4 · sensitive history",
      body: "Adds direct sexual and STI history to the complete clinical model. This tier had the highest temporal-validation discrimination.",
      burden: "Highest information burden",
      questionTitle: "Full clinical + sensitive history"
    }
  };

  const fieldOverrides = {
    age: { help: "The study model applies only to adults aged 20–49 years." },
    sex: { label: "Sex recorded in NHANES", help: "The public source data used male/female categories; this is a limitation of the model." },
    race_ethnicity: { help: "A social and structural proxy in this model—not a biological risk factor." },
    pir: { label: "Family poverty-income ratio", placeholder: "e.g. 2.5" },
    partnership: { label: "Marital or partnership status" },
    alcohol_drinks_day: { help: "Average number of drinks on days alcohol was consumed; enter 0 for no current drinking." },
    sbp: { label: "Average systolic blood pressure", help: "Use the average of available systolic readings when possible." },
    condomless_sex: { label: "Any sex reported without a condom" },
    prior_other_sti: { help: "Non-HSV STI history; genital herpes is handled by the eligibility question above." },
    circumcision: { help: "Shown only when male sex is selected; female participants are coded as not applicable." }
  };

  const exampleValues = {
    prior_diagnosis: "No",
    age: 40,
    sex: "Male",
    race_ethnicity: "Non-Hispanic White",
    education: "High school/GED",
    partnership: "Not partnered",
    pir: 2,
    insured: "Yes",
    current_smoker: "No",
    alcohol_drinks_day: 2,
    ever_hard_drug: "No",
    ever_injection_drug: "No",
    bmi: 27,
    waist: 94,
    sbp: 116,
    hba1c: 5.2,
    hdl: 49,
    total_chol: 191,
    alt: 22,
    ast: 22,
    alp: 64,
    ggt: 19,
    creatinine: 0.8,
    wbc: 7.1,
    hemoglobin: 14.5,
    platelets: 259,
    age_first_sex_group: "18 or older",
    partners_group: "2-4",
    condomless_sex: "None reported",
    prior_other_sti: "No",
    circumcision: "Circumcised"
  };

  const refs = {
    form: document.getElementById("riskForm"),
    formSections: document.getElementById("formSections"),
    formStatus: document.getElementById("formStatus"),
    tierRail: document.getElementById("tierRail"),
    tierExplainer: document.getElementById("tierExplainer"),
    questionTitle: document.getElementById("questionTitle"),
    completionCount: document.getElementById("completionCount"),
    completionBar: document.getElementById("completionBar"),
    exampleButton: document.getElementById("exampleButton"),
    clearButton: document.getElementById("clearButton"),
    resultEmpty: document.getElementById("resultEmpty"),
    resultContent: document.getElementById("resultContent"),
    resultModel: document.getElementById("resultModel"),
    riskOrbit: document.getElementById("riskOrbit"),
    riskValue: document.getElementById("riskValue"),
    decileValue: document.getElementById("decileValue"),
    riskNarrative: document.getElementById("riskNarrative"),
    comparisonRows: document.getElementById("comparisonRows"),
    adaptiveNote: document.getElementById("adaptiveNote"),
    addSensitiveButton: document.getElementById("addSensitiveButton")
  };

  let selectedModelId = "baseline_lr";
  let lastResults = null;

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
  }

  function fieldSpec(name) {
    if (name === "prior_diagnosis") {
      return {
        label: "Has a clinician ever diagnosed genital herpes?",
        type: "select",
        options: ["No", "Yes", "Not sure"],
        introducedIn: 1,
        help: "The primary study excluded prior diagnosis. Select Yes or Not sure to see the appropriate stop message."
      };
    }
    return Object.assign({}, inputs[name], fieldOverrides[name] || {});
  }

  function usableOptions(spec) {
    return (spec.options || []).filter((option) => option !== "Unknown");
  }

  function renderChoiceField(name, spec, options) {
    const optionHtml = options.map((option) => `
      <label class="choice-option">
        <input type="radio" name="${escapeHtml(name)}" value="${escapeHtml(option)}" />
        <span>${escapeHtml(option)}</span>
      </label>
    `).join("");
    return `<div class="choice-set" role="radiogroup" aria-label="${escapeHtml(spec.label)}">${optionHtml}</div>`;
  }

  function renderSelectField(name, spec, options) {
    const optionHtml = options.map((option) => `<option value="${escapeHtml(option)}">${escapeHtml(option)}</option>`).join("");
    return `
      <select id="${escapeHtml(name)}" name="${escapeHtml(name)}">
        <option value="">Select an option</option>
        ${optionHtml}
      </select>
    `;
  }

  function renderNumberField(name, spec) {
    const placeholder = spec.placeholder || (spec.min !== undefined && spec.max !== undefined ? `${spec.min}–${spec.max}` : "Enter value");
    const input = `
      <input
        id="${escapeHtml(name)}"
        name="${escapeHtml(name)}"
        type="number"
        inputmode="decimal"
        min="${escapeHtml(spec.min)}"
        max="${escapeHtml(spec.max)}"
        step="${escapeHtml(spec.step || 1)}"
        placeholder="${escapeHtml(placeholder)}"
      />
    `;
    if (!spec.unit) return input;
    return `<div class="field-inline">${input}<span class="field-suffix">${escapeHtml(spec.unit)}</span></div>`;
  }

  function renderField(name) {
    const spec = fieldSpec(name);
    const options = usableOptions(spec);
    const useChoices = spec.type === "select" && options.length <= 3;
    const control = spec.type === "number"
      ? renderNumberField(name, spec)
      : useChoices
        ? renderChoiceField(name, spec, options)
        : renderSelectField(name, spec, options);
    const help = spec.help ? `<small class="field-help" id="${escapeHtml(name)}-help">${escapeHtml(spec.help)}</small>` : "";

    return `
      <div class="field" data-field="${escapeHtml(name)}">
        <div class="field-label"><label${useChoices ? "" : ` for="${escapeHtml(name)}"`}>${escapeHtml(spec.label)}</label></div>
        ${control}
        ${help}
      </div>
    `;
  }

  function renderForm() {
    refs.formSections.innerHTML = groupDefinitions.map((group) => `
      <section class="form-section${group.sensitive ? " is-sensitive" : ""}" data-tier="${group.tier}" data-group="${group.id}">
        <header class="form-section-header">
          <span class="form-section-index">${group.index}</span>
          <div>
            <h3>${group.title}</h3>
            <p>${group.description}</p>
          </div>
          <span class="form-section-badge">${group.badge}</span>
        </header>
        <div class="field-grid">${group.fields.map(renderField).join("")}</div>
        ${group.id === "eligibility" ? '<div class="eligibility-stop" id="eligibilityStop" hidden></div>' : ""}
      </section>
    `).join("");
    updateCircumcisionField();
  }

  function getRadioValue(name) {
    const checked = refs.form.querySelector(`input[name="${CSS.escape(name)}"]:checked`);
    return checked ? checked.value : "";
  }

  function getValue(name) {
    const spec = fieldSpec(name);
    const options = usableOptions(spec);
    if (spec.type === "select" && options.length <= 3) return getRadioValue(name);
    const control = refs.form.elements[name];
    return control ? control.value : "";
  }

  function setValue(name, value) {
    const spec = fieldSpec(name);
    const options = usableOptions(spec);
    if (spec.type === "select" && options.length <= 3) {
      refs.form.querySelectorAll(`input[name="${CSS.escape(name)}"]`).forEach((input) => {
        input.checked = input.value === String(value);
        input.closest(".choice-option").classList.toggle("is-checked", input.checked);
      });
      return;
    }
    const control = refs.form.elements[name];
    if (control) control.value = value === null || value === undefined ? "" : String(value);
  }

  function selectedModel() {
    return models.find((model) => model.id === selectedModelId);
  }

  function modelInputNames(model) {
    const preprocessor = preprocessors[model.preprocessorId];
    if (!preprocessor) return [];
    return Array.from(new Set([
      ...Object.keys(preprocessor.numericInputs || {}),
      ...Object.keys(preprocessor.categoricalInputs || {})
    ]));
  }

  function activeInputNames() {
    return modelInputNames(selectedModel());
  }

  function readValues() {
    const values = {};
    Object.keys(inputs).forEach((name) => {
      const raw = getValue(name);
      values[name] = inputs[name].type === "number" && raw !== "" ? Number(raw) : raw;
    });
    return values;
  }

  function updateCircumcisionField() {
    const wrapper = refs.form.querySelector('[data-field="circumcision"]');
    if (!wrapper) return;
    if (!activeInputNames().includes("circumcision")) {
      wrapper.hidden = true;
      return;
    }
    const notApplicableInput = wrapper.querySelector('input[value="Not applicable (female)"]');
    if (notApplicableInput) notApplicableInput.closest(".choice-option").hidden = true;
    const sex = getValue("sex");
    if (sex === "Female") {
      setValue("circumcision", "Not applicable (female)");
      wrapper.hidden = true;
    } else {
      wrapper.hidden = false;
      if (getValue("circumcision") === "Not applicable (female)") setValue("circumcision", "");
    }
  }

  function clearInvalidState() {
    refs.form.querySelectorAll(".field.is-invalid").forEach((field) => field.classList.remove("is-invalid"));
    refs.form.querySelectorAll('[aria-invalid="true"]').forEach((control) => control.setAttribute("aria-invalid", "false"));
    refs.formStatus.textContent = "";
  }

  function markInvalid(name) {
    const wrapper = refs.form.querySelector(`[data-field="${CSS.escape(name)}"]`);
    if (!wrapper) return;
    wrapper.classList.add("is-invalid");
    wrapper.querySelectorAll("input, select").forEach((control) => control.setAttribute("aria-invalid", "true"));
  }

  function validate() {
    clearInvalidState();
    const invalid = [];
    const priorDiagnosis = getValue("prior_diagnosis");
    if (!priorDiagnosis) invalid.push({ name: "prior_diagnosis", message: "Answer the prior-diagnosis eligibility question." });
    if (priorDiagnosis && priorDiagnosis !== "No") {
      invalid.push({ name: "prior_diagnosis", message: "This model was not developed for people with a prior or uncertain clinician diagnosis of genital herpes." });
    }

    activeInputNames().forEach((name) => {
      const spec = inputs[name];
      const raw = getValue(name);
      if (raw === "") {
        invalid.push({ name, message: `Complete ${spec.label.toLowerCase()}.` });
        return;
      }
      if (spec.type === "number") {
        const value = Number(raw);
        if (!Number.isFinite(value) || value < Number(spec.min) || value > Number(spec.max)) {
          invalid.push({ name, message: `${spec.label} must be between ${spec.min} and ${spec.max}${spec.unit ? ` ${spec.unit}` : ""}.` });
        }
      }
    });

    invalid.forEach((item) => markInvalid(item.name));
    if (invalid.length) {
      refs.formStatus.textContent = invalid[0].message + (invalid.length > 1 ? ` ${invalid.length - 1} additional field${invalid.length > 2 ? "s" : ""} need attention.` : "");
      const first = refs.form.querySelector(`[data-field="${CSS.escape(invalid[0].name)}"] input, [data-field="${CSS.escape(invalid[0].name)}"] select`);
      if (first) first.focus({ preventScroll: false });
      return false;
    }
    return true;
  }

  function scoreLogistic(model, values) {
    const scoring = model.scoring;
    let logit = Number(scoring.intercept);

    Object.entries(scoring.numericCoefficients || {}).forEach(([name, spec]) => {
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

    Object.entries(scoring.categoricalContributions || {}).forEach(([name, spec]) => {
      const selected = values[name] || "Unknown";
      const levels = spec.levels || {};
      const contribution = Object.prototype.hasOwnProperty.call(levels, selected)
        ? levels[selected]
        : (levels.Unknown || 0);
      logit += Number(contribution || 0);
    });

    return 1 / (1 + Math.exp(-logit));
  }

  function encodeXgboostFeatures(model, values) {
    const preprocessor = preprocessors[model.preprocessorId];
    if (!preprocessor) throw new Error(`Missing preprocessor ${model.preprocessorId}`);
    const features = {};
    (preprocessor.featureNames || []).forEach((name) => { features[name] = 0; });

    Object.entries(preprocessor.numericInputs || {}).forEach(([name, spec]) => {
      const raw = values[name];
      const missing = raw === "" || raw === null || raw === undefined || !Number.isFinite(Number(raw));
      const value = missing ? Number(spec.median) : Number(raw);
      features[spec.valueFeature] = value;
      features[spec.missingFeature] = missing ? 1 : 0;
      (spec.derivedTerms || []).forEach((term) => {
        let derived = Number(term.median || 0);
        if (!missing && term.transform && term.transform.type === "centeredSquare") {
          derived = Math.pow(value - Number(term.transform.center), 2);
        }
        features[term.feature] = derived;
        features[term.missingFeature] = missing ? 1 : 0;
      });
    });

    Object.entries(preprocessor.categoricalInputs || {}).forEach(([name, spec]) => {
      let selected = values[name];
      if (!selected || !(spec.levels || []).includes(selected)) selected = spec.unknownLevel;
      const feature = (spec.featureByLevel || {})[selected];
      if (feature) features[feature] = 1;
    });
    return features;
  }

  function scoreXgboostTree(node, features) {
    if (Object.prototype.hasOwnProperty.call(node, "leaf")) return Number(node.leaf);
    const raw = features[node.split];
    const value = Math.fround(Number(raw));
    const threshold = Math.fround(Number(node.split_condition));
    const childId = !Number.isFinite(value)
      ? Number(node.missing)
      : value < threshold
        ? Number(node.yes)
        : Number(node.no);
    const child = (node.children || []).find((candidate) => Number(candidate.nodeid) === childId);
    if (!child) throw new Error(`Missing XGBoost child ${childId}`);
    return scoreXgboostTree(child, features);
  }

  function scoreXgboost(model, values) {
    const features = encodeXgboostFeatures(model, values);
    const margin = (model.scoring.trees || []).reduce(
      (sum, tree) => sum + scoreXgboostTree(tree, features),
      Number(model.scoring.baseMargin)
    );
    return 1 / (1 + Math.exp(-margin));
  }

  function scoreModel(model, values) {
    if (model.scoring.kind === "logistic") return scoreLogistic(model, values);
    if (model.scoring.kind === "xgboost") return scoreXgboost(model, values);
    throw new Error(`Unsupported scoring kind: ${model.scoring.kind}`);
  }

  function getDecile(model, probability) {
    let decile = 1;
    (model.deciles.thresholds || []).forEach((threshold) => {
      if (probability > Number(threshold)) decile += 1;
    });
    return Math.min(10, decile);
  }

  function formatPercent(value, digits) {
    return `${(Number(value) * 100).toFixed(digits)}%`;
  }

  function decileNarrative(decile, stats) {
    const position = decile >= 9
      ? "at the upper end"
      : decile >= 7
        ? "above the middle"
        : decile >= 4
          ? "near the middle"
          : "at the lower end";
    const observed = stats ? formatPercent(stats.observedRate, 1) : "not available";
    return `The estimate falls ${position} of the model’s weighted 2013–2016 reference distribution. Observed HSV-2 seroprevalence in this decile was ${observed}; later-cycle calibration indicates that the raw probability may overestimate absolute risk.`;
  }

  function renderComparison(results) {
    const probabilities = results.map((result) => result.probability);
    const scaleMax = Math.max(0.2, Math.ceil(Math.max.apply(null, probabilities) * 10) / 10);
    refs.comparisonRows.innerHTML = results.map((result, index) => {
      const previousResult = index > 0 ? results[index - 1] : null;
      const previous = previousResult ? previousResult.probability : null;
      const comparison = previousResult && previousResult.model.id === "baseline_lr" && result.model.id === "xgb_model1"
        ? " vs four-variable LR · two added inputs"
        : " vs prior ML tier";
      const delta = previous === null ? "Transparent reference model" : `${result.probability - previous >= 0 ? "+" : ""}${((result.probability - previous) * 100).toFixed(1)} percentage points${comparison}`;
      const width = Math.max(2, Math.min(100, (result.probability / scaleMax) * 100));
      return `
        <div class="comparison-row">
          <span class="comparison-row-label">${escapeHtml(result.model.shortName)}</span>
          <span class="comparison-bar" aria-hidden="true"><i style="width:${width.toFixed(2)}%"></i></span>
          <strong class="comparison-value">${formatPercent(result.probability, 1)}</strong>
          <small class="comparison-delta">${delta}</small>
        </div>
      `;
    }).join("");
  }

  function showResult(results) {
    lastResults = results;
    const current = results[results.length - 1];
    const model = current.model;
    const decile = getDecile(model, current.probability);
    const stats = (model.deciles.stats || []).find((entry) => Number(entry.decile) === decile);

    refs.resultEmpty.hidden = true;
    refs.resultContent.hidden = false;
    refs.resultModel.textContent = model.shortName;
    refs.riskValue.textContent = formatPercent(current.probability, 1);
    refs.riskOrbit.style.setProperty("--risk-angle", `${Math.max(1, Math.min(360, current.probability * 360))}deg`);
    refs.decileValue.textContent = `Decile ${decile} / 10`;
    refs.riskNarrative.textContent = decileNarrative(decile, stats);
    renderComparison(results);

    const adaptive = artifact.metadata.adaptiveThreshold;
    refs.adaptiveNote.hidden = !(model.id === "xgb_model3" && adaptive && current.probability >= Number(adaptive.developmentGateProbability));
    refs.resultContent.style.animation = "none";
    void refs.resultContent.offsetWidth;
    refs.resultContent.style.animation = "";

    if (window.matchMedia("(max-width: 880px)").matches) {
      document.querySelector(".result-column").scrollIntoView({ behavior: "smooth", block: "start" });
    }
  }

  function resetResult() {
    lastResults = null;
    refs.resultEmpty.hidden = false;
    refs.resultContent.hidden = true;
    refs.adaptiveNote.hidden = true;
    refs.resultModel.textContent = selectedModel().shortName;
  }

  function updateEligibilityStop() {
    const stop = document.getElementById("eligibilityStop");
    const value = getValue("prior_diagnosis");
    if (!stop) return;
    if (value && value !== "No") {
      stop.hidden = false;
      stop.textContent = value === "Yes"
        ? "This calculator does not apply after a clinician-diagnosed genital-herpes history. Clinical evaluation—not this research estimate—should guide next steps."
        : "The study required an explicit report of no prior clinician diagnosis. If diagnosis history is uncertain, clarify it with a clinician before interpreting this model.";
    } else {
      stop.hidden = true;
      stop.textContent = "";
    }
  }

  function updateCompletion() {
    const names = activeInputNames();
    const completed = names.filter((name) => getValue(name) !== "").length;
    refs.completionCount.textContent = `${completed} of ${names.length}`;
    refs.completionBar.style.width = `${names.length ? (completed / names.length) * 100 : 0}%`;
  }

  function updateModel(modelId, options) {
    selectedModelId = modelId;
    const model = selectedModel();
    const inputTier = Number(model.tier);
    const copy = modelCopy[selectedModelId];
    const opts = options || {};

    refs.tierRail.querySelectorAll(".tier-option").forEach((option) => {
      const input = option.querySelector("input");
      const selected = input.value === selectedModelId;
      input.checked = selected;
      option.classList.toggle("is-selected", selected);
    });

    const activeNames = new Set(activeInputNames());
    refs.form.querySelectorAll(".field").forEach((field) => {
      field.hidden = !activeNames.has(field.dataset.field) && field.dataset.field !== "prior_diagnosis";
    });
    refs.form.querySelectorAll(".form-section").forEach((section) => {
      const groupId = section.dataset.group;
      const hasActiveField = Array.from(section.querySelectorAll(".field")).some((field) => !field.hidden);
      section.hidden = groupId !== "eligibility" && (!hasActiveField || Number(section.dataset.tier) > inputTier);
    });

    refs.tierExplainer.innerHTML = `
      <span class="tier-explainer-number">${copy.marker}</span>
      <p><strong>${copy.title}</strong> ${copy.body}</p>
      <span class="burden-label">${copy.burden}</span>
    `;
    refs.questionTitle.textContent = copy.questionTitle;
    refs.resultModel.textContent = model.shortName;
    clearInvalidState();
    updateCircumcisionField();
    updateCompletion();
    if (!opts.keepResult) resetResult();
  }

  refs.form.addEventListener("submit", (event) => {
    event.preventDefault();
    if (!validate()) return;
    const values = readValues();
    const currentModel = selectedModel();
    const results = models
      .filter((model) => model.id === "baseline_lr" || (
        currentModel.id !== "baseline_lr" && model.algorithm === "XGBoost" && Number(model.tier) <= Number(currentModel.tier)
      ))
      .map((model) => ({ model, probability: scoreModel(model, values) }));
    showResult(results);
  });

  refs.tierRail.addEventListener("change", (event) => {
    if (event.target.matches('input[name="modelChoice"]')) updateModel(event.target.value);
  });

  refs.form.addEventListener("change", (event) => {
    const choice = event.target.closest(".choice-option");
    if (choice) {
      const name = event.target.name;
      refs.form.querySelectorAll(`input[name="${CSS.escape(name)}"]`).forEach((input) => {
        input.closest(".choice-option").classList.toggle("is-checked", input.checked);
      });
    }
    if (event.target.name === "sex") updateCircumcisionField();
    if (event.target.name === "prior_diagnosis") updateEligibilityStop();
    clearInvalidState();
    updateCompletion();
    if (lastResults) resetResult();
  });

  refs.form.addEventListener("input", () => {
    updateCompletion();
    if (lastResults) resetResult();
  });

  refs.exampleButton.addEventListener("click", () => {
    updateModel(selectedModelId);
    Object.entries(exampleValues).forEach(([name, value]) => setValue(name, value));
    updateCircumcisionField();
    updateEligibilityStop();
    updateCompletion();
    clearInvalidState();
  });

  refs.clearButton.addEventListener("click", () => {
    refs.form.reset();
    refs.form.querySelectorAll(".choice-option").forEach((option) => option.classList.remove("is-checked"));
    updateModel("baseline_lr");
    updateEligibilityStop();
    window.scrollTo({ top: document.getElementById("assessment").offsetTop - 80, behavior: "smooth" });
  });

  refs.addSensitiveButton.addEventListener("click", () => {
    updateModel("xgb_model4");
    const sensitiveSection = refs.form.querySelector('[data-group="sensitive"]');
    if (sensitiveSection) sensitiveSection.scrollIntoView({ behavior: "smooth", block: "start" });
  });

  renderForm();
  updateModel("baseline_lr");
  updateEligibilityStop();
})();
