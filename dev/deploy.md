# MG2 Deployment Checklist

Work through each section in order before and after deploying.

---

## 1. Pre-flight checks

Run in the R console:

```r
devtools::document()   # regenerate NAMESPACE + man/
devtools::load_all()   # confirm package loads cleanly
run_mg2()              # smoke test: launch app, check all tabs
devtools::check()      # must pass: 0 ERRORs · 0 WARNINGs · 1 NOTE (bare dplyr calls)
```

Resolve any NOTEs or WARNINGs beyond the baseline before proceeding.

---

## 2. Git

```bash
git add <files>
git commit -m "description of change"
git push origin main
```

No version bump unless this is a real release (see step 5).

---

## 3. Connect Cloud

1. Go to <https://connect.posit.cloud/magicglasses>
2. Click **Republish**

`deploy/app.R` will automatically:
- Detect the server OS from `/etc/os-release`
- Install `qs2`, `future.apply`, `feasts`, `ggtime` from PPM if missing
- Read the git HEAD SHA and reinstall MG2 only if the SHA has changed

Check the app log for:
```
=== MG2 <version> starting ===
PPM target OS: <codename>
```

Confirm evaluation completes end-to-end using the PDR Lao demo.

---

## 4. shinyapps.io

```r
rsconnect::deployApp(
  appDir  = "deploy",
  appName = "MG2-MagicGlasses2"
)
```

Verify demo loads and evaluation completes.

---

## 5. GitHub release (real version bumps only)

Only when bumping the version in `DESCRIPTION` for a true release:

```bash
git tag vX.Y.Z
git push origin vX.Y.Z
```

`.github/workflows/release.yml` will build and attach the source tarball automatically.

---

## 6. Post-deploy smoke test

**Connect Cloud** — <https://connect.posit.cloud/magicglasses>

- [ ] Load Sierra Leone demo → navigate all tabs
- [ ] Load PDR Lao demo → run Evaluation → confirm result appears
- [ ] Confirm Outliers Summary chart visible (not cut off)

**shinyapps.io** — <https://jpainter.shinyapps.io/MG2-MagicGlasses2/>

- [ ] Load demo → spot-check DQA and Evaluation tabs
