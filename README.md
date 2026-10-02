# Stabilitas dan Informativeness Lasso Regression pada Data Energy Efficiency

**Benchmarking terhadap Ridge Regression dan Linear Regression (OLS)**

Laporan ilmiah — Program Magister Statistika Terapan, Universitas Padjadjaran, 2026

**Penulis:** Yusrifa Audzar (170420260001) · Illa Rosyidah (170420260003)

---

## Ringkasan

Efisiensi energi bangunan dipengaruhi oleh parameter desain yang saling berkorelasi, sehingga regresi linear biasa (OLS) mengalami multikolinearitas dan menghasilkan estimasi koefisien yang tidak stabil. Proyek ini menguji seberapa **stabil** dan **informatif** Lasso Regression pada dataset *Energy Efficiency* ketika difokuskan pada proses **benchmarking** dan **validasi**.

**Rumusan masalah:** Seberapa stabil dan informatif hasil Lasso Regression pada data Energy Efficiency ketika difokuskan pada proses benchmarking dan validation?

**Tujuan:**
1. Mengevaluasi performa prediktif Lasso dibandingkan Ridge dan Linear Regression.
2. Menilai stabilitas seleksi fitur Lasso melalui 5-fold cross-validation.
3. Menilai sejauh mana koefisien Lasso informatif untuk menjelaskan determinan efisiensi energi bangunan.

## Dataset

Dataset *Energy Efficiency* berisi **768 observasi** hasil simulasi desain bangunan (12 bentuk bangunan dasar), tanpa missing value.

| Variabel | Peran | Deskripsi |
|---|---|---|
| X1 | Fitur | Relative Compactness |
| X2 | Fitur | Surface Area |
| X3 | Fitur | Wall Area |
| X4 | Fitur | Roof Area |
| X5 | Fitur | Overall Height |
| X6 | Fitur | Orientation |
| X7 | Fitur | Glazing Area |
| X8 | Fitur | Glazing Area Distribution |
| Y1 | Target | Heating Load (beban pemanasan) |
| Y2 | Target | Cooling Load (beban pendinginan) |

Y1 dan Y2 dimodelkan secara terpisah.

**Catatan multikolinearitas:** Roof Area (X4) adalah kombinasi linear sempurna dari Surface Area (X2) dan Wall Area (X3): `X4 = 0,5·X2 − 0,5·X3`. Akibatnya OLS tidak dapat menduga koefisien X4 secara unik (aliased), dan VIF tidak dapat dihitung. Ini konsekuensi geometris dari desain bangunan dalam dataset.

## Metode

1. Pemeriksaan missing value dan statistik deskriptif.
2. Uji multikolinearitas awal (OLS penuh, `alias()`, VIF).
3. Partisi data 80:20, stratifikasi berdasarkan target (`initial_split` dengan `strata`).
4. 5-fold cross-validation pada data latih untuk tuning.
5. Standardisasi prediktor numerik, dilakukan terisolasi di tiap fold untuk mencegah data leakage.
6. Model: **Lasso** (`mixture = 1`, mesin `glmnet`), dengan **Ridge** (`mixture = 0`) dan **Linear Regression (OLS)** sebagai pembanding.
7. Tuning λ: grid 50 nilai pada rentang 10⁻⁴ hingga 10¹, dipilih berdasarkan RMSE CV terkecil.
8. Model final dilatih ulang pada seluruh data latih dan dievaluasi satu kali pada data uji (`last_fit`).
9. Stabilitas seleksi fitur: model final (λ dikunci) dilatih ulang pada 5 fold yang sama, lalu dihitung persentase tiap variabel terpilih (koefisien ≠ 0).
10. Diagnostik residual: actual vs predicted, residual vs predicted, distribusi residual, dan 10 observasi dengan galat terbesar.

Seluruh proses menggunakan `set.seed(123)` untuk reproduktibilitas.

## Hasil Utama

### Benchmarking pada test set (20%)

| Model | RMSE (Y1) | R² (Y1) | RMSE (Y2) | R² (Y2) |
|---|---|---|---|---|
| **Lasso** | 2,96 | 0,914 | 3,62 | 0,863 |
| Ridge | 3,28 | 0,895 | 3,89 | 0,846 |
| Linear Regression (OLS) | 2,95 | 0,915 | 3,63 | 0,863 |

λ optimal Lasso: 0,0001 untuk Y1 (mendekati batas bawah grid) dan 0,00212 untuk Y2.

Lasso setara dengan OLS dan lebih unggul dari Ridge. Ridge tidak pernah menghilangkan variabel redundan (X3 tetap dipertahankan), sehingga penalti L2 ikut menyusutkan koefisien variabel penting tanpa manfaat eliminasi.

### Koefisien (Heating Load, Y1)

| Variabel | Lasso | Ridge | OLS |
|---|---|---|---|
| X5 — Overall Height | 7,83 | 4,91 | 7,11 |
| X4 — Roof Area | −5,13 | −2,36 | NA (rank-deficient) |
| X1 — Relative Compactness | −4,63 | −0,464 | −6,66 |
| X7 — Glazing Area | 2,63 | 2,42 | 2,64 |
| X3 — Wall Area | **0 (dibuang)** | 2,49 | 2,79 |
| X8 — Glazing Area Distribution | 0,333 | 0,365 | 0,338 |
| X6 — Orientation | −0,040 | −0,052 | −0,046 |
| X2 — Surface Area | −0,0009 | −1,18 | −7,66 |

Pola serupa pada Cooling Load (Y2): Lasso membuang X3, X5 dominan positif (+7,69), diikuti X1 (−6,92) dan X4 (−3,79).

### Stabilitas seleksi fitur (5-fold, λ dikunci)

- **7 variabel** (X1, X2, X4, X5, X6, X7, X8) terpilih di **seluruh 5 fold** (stabilitas 100%).
- **Wall Area (X3)** dibuang di **seluruh fold** (stabilitas 0%).
- Pola ini identik untuk Y1 dan Y2. Tidak ada variabel dengan seleksi yang berubah-ubah antar fold.

### Diagnostik residual

- Model mengikuti pola nilai sebenarnya dengan baik pada beban rendah hingga menengah, tetapi cenderung meremehkan beban yang sangat tinggi, terutama pada Cooling Load.
- Sebaran error tidak seragam: kecil pada prediksi rendah dan melebar pada prediksi menengah–tinggi (heteroskedastisitas). Asumsi homoskedastik tidak terpenuhi sempurna.

## Kesimpulan

Lasso Regression pada data Energy Efficiency:

- **Prediktif:** setara dengan OLS (R² 0,914 untuk Y1 dan 0,863 untuk Y2) dan lebih unggul dari Ridge (0,895 dan 0,846).
- **Stabil:** seleksi fitur konsisten di seluruh fold meskipun multikolinearitas struktural ekstrem, berlawanan dengan dugaan teoretis bahwa Lasso akan tidak stabil pada kondisi ini.
- **Informatif:** mengidentifikasi Overall Height, Relative Compactness, dan Roof Area sebagai determinan utama, serta Wall Area sebagai variabel paling redundan.

## Batas Interpretasi dan Keterbatasan

- Koefisien Lasso **bukan** bukti hubungan sebab-akibat; data bersifat simulasi/observasional.
- Variabel yang dibuang (Wall Area) redundan secara statistik dalam sampel ini, bukan berarti tidak penting secara fisik.
- Hasil spesifik untuk 12 bentuk bangunan dalam dataset dan belum tentu berlaku pada bangunan nyata.
- Baru tiga model yang dibandingkan (belum Elastic Net); λ optimal Y1 dekat batas bawah grid; evaluasi test dilakukan satu kali.

**Pengembangan berikutnya:** Elastic Net, nested/repeated cross-validation untuk rentang ketidakpastian, perluasan grid λ, dan validasi eksternal dengan data bangunan riil.

## Struktur Repositori

<!-- Sesuaikan dengan isi repositori kamu, misalnya: -->
```
├── data/            # Data_Energy_Efficiency.csv
├── analysis/        # skrip/notebook analisis (R)
├── report/          # DRAFT_LAPORAN.docx
├── slides/          # Presentasi_Lasso_Energy_Efficiency.pptx
└── README.md
```

## Referensi

- Tibshirani, R. (1996). Regression Shrinkage and Selection via the Lasso. *Journal of the Royal Statistical Society: Series B*, 58(1), 267–288.
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). *An Introduction to Statistical Learning* (2nd ed.). Springer.
- Tsanas, A., & Xifara, A. (2012). Accurate quantitative estimation of energy performance of residential buildings using statistical machine learning tools. *Energy and Buildings*.
