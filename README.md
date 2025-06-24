**Here is my code for the work ``General auxiliary variables with approximate neighbourhood interference''.**


## The original synthetic result:


| Method (linear-in-means)                       | HT    | Haj   | F     | L     | F- $\phi_0(G_1)$ | F- $\phi_0(G_2)$ | ND-F  | ND- $\phi_0(G_1)$ | ND- $G_1$ | ND-L  | ND- $\phi_0(G_2)$ | ND- $G_2$ |
|----------------------------------|-------|-------|-------|-------|------------------|------------------|-------|-------------------|-----------|-------|-------------------|-----------|
| **Empirical absolute bias**      | 0.001 | 0.001 | 0.000 | 0.000 | 0.001            | 0.001            | 0.000 | 0.001             | 0.895     | 0.000 | 0.002             | 0.998     |
| **Oracle SE**         | 0.087 | 0.086 | 0.068 | 0.068 | 0.055 | 0.054 | 0.068 | 0.055 | 0.217 | 0.068 | 0.056 | 0.676 |
| **Estimated SE**     | 0.086 | 0.085 | 0.067 | 0.067 | 0.054 | 0.054 | 0.067 | 0.053 | 0.053 | 0.067 | 0.054 | 0.053 |
| **Oracle coverage probability**  | 0.949 | 0.949 | 0.950 | 0.950 | 0.953            | 0.951            | 0.953 | 0.949             | 0.015     | 0.950 | 0.949             | 0.679     |
| **Empirical coverage probability**| 0.946 | 0.947 | 0.948 | 0.948 | 0.944            | 0.946            | 0.947 | 0.945             | 0.000     | 0.947 | 0.941             | 0.039     |









| Method (nonlinear contagion)     | HT    | Haj   | F     | L     | F- $\phi_0(G_1)$ | F- $\phi_0(G_2)$ | ND-F  | ND- $\phi_0(G_1)$ | ND- $G_1$ | ND-L  | ND- $\phi_0(G_2)$ | ND- $G_2$ |
|----------------------------------|-------|-------|-------|-------|------------------|------------------|-------|-------------------|-----------|-------|-------------------|-----------|
| **Empirical absolute bias**      | 0.000 | 0.000 | 0.000 | 0.000 | 0.000            | 0.000            | 0.000 | 0.000             | 0.176     | 0.000 | 0.000             | 0.044     |
| **Oracle SE**        | 0.052 | 0.020 | 0.018 | 0.018 | 0.017 | 0.017 | 0.018 | 0.017 | 0.073 | 0.018 | 0.017 | 0.212 |
| **Estimated SE**     | 0.052 | 0.021 | 0.020 | 0.019 | 0.018 | 0.018 | 0.020 | 0.018 | 0.018 | 0.019 | 0.018 | 0.017 |
| **Oracle coverage probability**  | 0.950 | 0.947 | 0.949 | 0.949 | 0.947            | 0.948            | 0.947 | 0.949             | 0.337     | 0.949 | 0.949             | 0.945     |
| **Empirical coverage probability**| 0.952 | 0.959 | 0.961 | 0.959 | 0.957            | 0.961            | 0.960 | 0.952             | 0.024     | 0.958 | 0.958             | 0.134     |

**Table 1**: Simulation results: network size $n=3000\left(b_n=3\right) . \tau=0.934$ for the linear-in-means setting and $\tau=0.193$ for the nonlinear contagion setting. We report the square of oracle SE and estimated SE in our tables for abbreviation.


## The original real-world result:

| Method                          | HT    | Haj   | F     | L     | ND-F | ND- $\phi_0\left(G_1\right)$ | ND-L | ND- $\phi_0\left(G_2\right)$ |
|----------------------------------|-------|-------|-------|-------|------|----------------------------|-------|----------------------------|
| **Direct effect**                   | 0.0082 | 0.0146 | 0.0170 | 0.0168 | 0.0167 | 0.0161                     | 0.0164 | 0.0175                    |
| **Estimated SE**                 | 0.0272 | 0.0225 | 0.0218 | 0.0197 | 0.0211 | 0.0205                     | 0.0195 | 0.0193                    |
| **Spillover effect**                    | 0.0381 | 0.0611 | 0.0604 | 0.0581 | 0.0660 | 0.0686                     | 0.0592 | 0.0603                    |
| **Estimated SE**                 | 0.0447 | 0.0292 | 0.0270 | 0.0241 | 0.0258 | 0.0250                     | 0.0236 | 0.0233                    |

**Table 2**: Estimates $\hat{\tau}$ and Estimated SE of empirical experiments.












## The original counter-example:
| Method                          | HT    | Haj   | F     | L     | ND-F | ND-L | ND- $\phi_0$ (G) |
|----------------------------------|-------|-------|-------|-------|------|------|-------------------|
| **Empirical absolute bias**      | 0.001 | 0.001 | 0.000 | 0.000 | 0.000 | 0.000 | 0.001             |
| **Oracle SE**                    | 0.037 | 0.037 | 0.041 | 0.050 | 0.037 | 0.043 | 0.038             |
| **Estimated SE**                 | 0.066 | 0.066 | 0.068 | 0.058 | 0.066 | 0.054 | 0.066             |
| **Oracle coverage probability**  | 0.950 | 0.950 | 0.950 | 0.950 | 0.950 | 0.950 | 0.950             |
| **Empirical coverage probability**| 0.999 | 0.999 | 0.999 | 0.978 | 0.999 | 0.986 | 0.999             |

**Table S.1**: The counter-example. $\tau=0.024$.




## Additional experiments: response to the Review

#### 1. The proposed framework relies on computationally intensive steps (e.g., Monte Carlo approximations for normalization). How does the method scale with large networks, especially when n is very large? The computational complexity, and could we reduce it?

Network size diverging from $400, 1000$ to $3000$.

The complexity:
a) Theoretically, O(..) + "O(..)", Fangzhen 1+2； real-data 1
b) Empirically, MC optimization; beta optimization;


#### 2. How does it perform when we select different $G$, and how do we select it?



syn (report $r^2$ + generation adding iteration term) & real-data (report $r^2$): 
Guideline: refer to previous knowledge...




#### 3. Bandwidth selection and comparison?

We test the Bandwidth $=1,2,3,4$.

| Method (linear-in-means)         | HT            | Haj           | F             | L             | F- $\phi_0(G_1)$ | F- $\phi_0(G_2)$ | ND-F          | ND- $\phi_0(G_1)$ | ND- $G_1$     | ND-L          | ND- $\phi_0(G_2)$ | ND- $G_2$     |
|----------------------------------|----------------|----------------|----------------|----------------|------------------|------------------|----------------|-------------------|---------------|----------------|-------------------|---------------|
| **Empirical absolute bias**      | 0.935<br>0.934<br>0.934<br>0.935 | 0.935<br>0.933<br>0.933<br>0.934 | 0.935<br>0.933<br>0.933<br>0.934 | 0.935<br>0.933<br>0.933<br>0.934 | 0.935<br>0.934<br>0.933<br>0.933   | 0.935<br>0.935<br>0.934<br>0.933   | 0.935<br>0.933<br>0.933<br>0.934 | 0.935<br>0.934<br>0.933<br>0.933     | 0.0463<br>0.044<br>0.037<br>0.039 | 0.935<br>0.933<br>0.933<br>0.934 | 0.935<br>0.935<br>0.934<br>0.934     | -0.072<br>-0.055<br>-0.067<br>-0.070 |
| **Oracle SE**                    | 0.085<br>0.085<br>0.087<br>0.085 | 0.084 <br>0.084<br>0.086<br>0.084 | 0.067<br>0.066<br>0.068<br>0.067 | 0.067<br>0.066<br>0.068<br>0.067 | 0.055<br>0.055<br>0.055<br>0.054   | 0.054<br>0.054<br>0.055<br>0.053   | 0.067<br>0.066<br>0.068<br>0.067 | 0.055<br>0.055<br>0.056<br>0.054     | 0.021<br>0.216<br>0.217<br>0.222 | 0.067<br>0.066<br>0.068<br>0.067 | 0.055<br>0.055<br>0.055<br>0.054     | 13.46<br>0.666<br>0.674<br>0.680 |
| **Estimated SE**                 | 0.086<br>0.087<br>0.087<br>0.087 | 0.084<br>0.085<br>0.085<br>0.085 | 0.067<br>0.067<br>0.067<br>0.067| 0.067<br>0.067<br>0.067<br>0.067 | 0.054<br>0.054<br>0.054<br>0.054   | 0.054<br>0.054<br>0.054<br>0.054   | 0.067<br>0.067<br>0.067<br>0.067 | 0.054<br>0.054<br>0.054<br>0.054     | 0.053<br>0.053<br>0.053<br>0.053 | 0.067<br>0.067<br>0.067<br>0.067 | 0.054<br>0.053<br>0.053<br>0.053     | 0.053<br>0.053<br>0.053<br>0.053 |
| **Oracle coverage probability**  | 0.951<br>0.951<br>0.950<br>0.950 | 0.951<br>0.951<br>0.951<br>0.952 | 0.951<br>0.950<br>0.949<br>0.950 | 0.950<br>0.950<br>0.949<br>0.951 | 0.951<br>0.950<br>0.949<br>0.951   | 0.950<br>0.951<br>0.948<br>0.951   | 0.950<br>0.952<br>0.949<br>0.951 | 0.951<br>0.951<br>0.950     | 0.012<br>0.016<br>0.015<br>0.019 | 0.950<br>0.951<br>0.948<br>0.951 | 0.950<br>0.951<br>0.951<br>0.950     | 0.991<br>0.681<br>0.680<br>0.681 |
| **Empirical coverage probability**| 0.953<br>0.952<br>0.949<br>0.954 | 0.953<br>0.951<br>0.948<br>0.954 | 0.951<br>0.953<br>0.947<br>0.952 | 0.950<br>0.953<br>0.947<br>0.952 | 0.946<br>0.945<br>0.941<br>0.948   | 0.948<br>0.949<br>0.944<br>0.951   | 0.950<br>0.953<br>0.946<br>0.951 | 0.945<br>0.944<br>0.940<br>0.949     | 0.000<br>0.000<br>0.000<br>0.000 | 0.950<br>0.953<br>0.946<br>0.950 | 0.944<br>0.942<br>0.939<br>0.947     | NA<br>0.041<br>0.039<br>0.042 |






改到oracle coverage pro了










#### 4. Clarify the experimental settings. a) The population selection; b) The indirect effect is counterintuitive.

a) The population is restricted to these individuals who took part in the second lecture to make sure they receive their friend's information;

b) The indirect effect is significantly positive, which indicates that farmers are more easily affected by their neighbourhood (friends), compared with taking part in the lecture themselves (they absorb the lecture's knowledge slightly, or do not trust it).

c) If these nodes are not excluded, they will significantly affect our results.

#### 5. When the network is misspecified.

When there is an additional/missed edge during the network generation.



- **Original data components**:
  - Adjacency matrix,
  - Node-level covariates \(X_i\),
  - Treatment indicators \(T_i\),
  - Outcome \(Y_i\).

- **Perturbation strategies**:
  1. **Binary-edge flips**: Randomly flip each \(A_{ij}\) with probability \(\varepsilon\in\{0.1\%, 0.5\%, 1\%, 2\%, ....\}\). Take attention to the covariance + variance..

  

Perturbed matrices denoted \(A^{(\varepsilon,k)}\), with \(k=1,\dots,K\) Monte Carlo repeats per \(\varepsilon\).


Causal inference with misspecified network interference structure
Bar Weinstein, Daniel Nevo








#### 6. The multi-valued treatments.

We take the treatments from $2$ to $10$.

#### my own: additional visualisation:



| Network | Covariate X | Noise ε |
|-------------|---------|-----------|
| ![](figures/output(1).png) | ![](figures/output(2).png) |![](figures/output(3).png) |

