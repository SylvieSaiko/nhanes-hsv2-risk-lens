# HSV-2 Risk Lens

`website_hsv2/` is a static, research-only demonstrator for the four population-weighted logistic-regression information tiers in Attempt 7.

## What it does

- Switches among Model 1 baseline, Model 2 lifestyle/access, Model 3 routine clinical, and Model 4 sensitive-history tiers.
- Preserves shared answers while progressively revealing newly required fields.
- Calculates the selected tier and every completed lower tier locally in the browser.
- Shows the unrecalibrated development-fit probability, weighted temporal-validation decile, and within-person tier comparison.
- Offers the prespecified Model 3-to-Model 4 adaptive-question prompt when its development threshold is crossed.

The site does not diagnose HSV-2 and does not recommend routine serologic screening. It applies only to the study population: U.S. adults aged 20–49 years who explicitly reported no previous clinician diagnosis of genital herpes.

## Privacy boundary

The page is fully static. It contains no analytics, cookies, browser storage, form submission, or URL-encoded answers. The exported artifact contains only aggregate coefficients, preprocessing constants, validation summaries, and synthetic parity fixtures. It does not contain participant rows, identifiers, survey weights, predictions, fitted model frames, or `.rds` objects.

Keep the repository and any deployment private until the investigators authorize publication of the model coefficients.

## Preview locally

From the repository root:

```bash
python3 -m http.server 8000 --directory website_hsv2
```

Then open `http://127.0.0.1:8000`.

## Regenerate the browser artifact

Run the analysis first so the ignored fitted model bundles and participant-level validation predictions exist locally. Then run:

```bash
Rscript website_hsv2/scripts/export_hsv2_web_artifact.R
```

This writes:

- `website_hsv2/artifacts/hsv2-models.json`
- `website_hsv2/model-data.js`

The exporter checks 16 deterministic synthetic parity cases against the fitted R models and rejects unexpected row-level fields before writing either artifact.

Verify the serialized artifact with the independent JavaScript scorer:

```bash
node website_hsv2/scripts/check_model_parity.js
```

## Design references

The interface is original rather than a copy of Attempt 5. Its interaction architecture was informed by:

- [Heart Foundation Australia Heart Age Calculator](https://heartagecalculator.heartfoundation.org.au/): concise entry and optional-information framing.
- [NHS Heart Age](https://www.nhs.uk/health-assessment-tools/calculate-your-heart-age): eligibility, provenance, and input transparency.
- [CDC HIV Risk Estimator](https://hivrisk.cdc.gov/risk-estimator-tool/): privacy-first handling of sensitive sexual-health information.
- [NCI Breast Cancer Risk Assessment Tool](https://bcrisktool.cancer.gov/calculator.html): explicit eligibility and limitations.
- [Cambridge Predict](https://breast.predict.cam/index.html): side-by-side comparison of progressively richer scenarios.

No branding, artwork, exact copy, or trade dress from these tools is reproduced.
