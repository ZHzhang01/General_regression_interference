**Here is my code for the work ``General auxiliary variables with approximate neighbourhood interference''.**


## The original synthetic result:


| Method (linear-in-means)                       | HT    | Haj   | F     | L     | F- $\phi_0(G_1)$ | F- $\phi_0(G_2)$ | ND-F  | ND- $\phi_0(G_1)$ | ND- $G_1$ | ND-L  | ND- $\phi_0(G_2)$ | ND- $G_2$ |
|----------------------------------|-------|-------|-------|-------|------------------|------------------|-------|-------------------|-----------|-------|-------------------|-----------|
| **Empirical absolute bias**      | 0.001 | 0.001 | 0.000 | 0.000 | 0.001            | 0.001            | 0.000 | 0.001             | 0.895     | 0.000 | 0.002             | 0.998     |
| **Oracle SE**                    | 0.295 | 0.293 | 0.260 | 0.260 | 0.235            | 0.233            | 0.260 | 0.235             | 0.466     | 0.260 | 0.236             | 0.822     |
| **Estimated SE**                 | 0.294 | 0.292 | 0.259 | 0.259 | 0.232            | 0.232            | 0.259 | 0.231             | 0.230     | 0.259 | 0.233             | 0.230     |
| **Oracle coverage probability**  | 0.949 | 0.949 | 0.950 | 0.950 | 0.953            | 0.951            | 0.953 | 0.949             | 0.015     | 0.950 | 0.949             | 0.679     |
| **Empirical coverage probability**| 0.946 | 0.947 | 0.948 | 0.948 | 0.944            | 0.946            | 0.947 | 0.945             | 0.000     | 0.947 | 0.941             | 0.039     |





| Method (nonlinear contagion)     | HT    | Haj   | F     | L     | F- $\phi_0(G_1)$ | F- $\phi_0(G_2)$ | ND-F  | ND- $\phi_0(G_1)$ | ND- $G_1$ | ND-L  | ND- $\phi_0(G_2)$ | ND- $G_2$ |
|----------------------------------|-------|-------|-------|-------|------------------|------------------|-------|-------------------|-----------|-------|-------------------|-----------|
| **Empirical absolute bias**      | 0.000 | 0.000 | 0.000 | 0.000 | 0.000            | 0.000            | 0.000 | 0.000             | 0.176     | 0.000 | 0.000             | 0.044     |
| **Oracle SE**                    | 0.227 | 0.142 | 0.136 | 0.136 | 0.131            | 0.129            | 0.136 | 0.132             | 0.271     | 0.136 | 0.131             | 0.461     |
| **Estimated SE**                 | 0.228 | 0.146 | 0.140 | 0.139 | 0.134            | 0.133            | 0.140 | 0.134             | 0.134     | 0.139 | 0.133             | 0.132     |
| **Oracle coverage probability**  | 0.950 | 0.947 | 0.949 | 0.949 | 0.947            | 0.948            | 0.947 | 0.949             | 0.337     | 0.949 | 0.949             | 0.945     |
| **Empirical coverage probability**| 0.952 | 0.959 | 0.961 | 0.959 | 0.957            | 0.961            | 0.960 | 0.952             | 0.024     | 0.958 | 0.958             | 0.134     |

**Table 1**: Simulation results: network size $n=3000\left(b_n=3\right) . \tau=0.934$ for the linear-in-means setting and $\tau=0.193$ for the nonlinear contagion setting.


## The original real-world result:

| Method                          | HT    | Haj   | F     | L     | ND-F | ND- $\phi_0\left(G_1\right)$ | ND-L | ND- $\phi_0\left(G_2\right)$ |
|----------------------------------|-------|-------|-------|-------|------|----------------------------|-------|----------------------------|
| **Direct effect**  |       |       |       |       |      |                            |       |                            |
| **$\hat{\tau}$ **                    | 0.0082 | 0.0146 | 0.0170 | 0.0168 | 0.0167 | 0.0161                     | 0.0164 | 0.0175                    |
| **Estimated SE**                 | 0.0272 | 0.0225 | 0.0218 | 0.0197 | 0.0211 | 0.0205                     | 0.0195 | 0.0193                    |
| **Spillover effect** |       |       |       |       |      |                            |       |                            |
| $\hat{\tau}$                     | 0.0381 | 0.0611 | 0.0604 | 0.0581 | 0.0660 | 0.0686                     | 0.0592 | 0.0603                    |
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

#### The proposed framework relies on computationally intensive steps (e.g., Monte Carlo approximations for normalization). How does the method scale with large networks, especially when n is very large? The computational complexity, and could we reduce it?

#### How does it perform when we select different $G$, and how do we select it?

#### Bandwidth selection and comparison?

#### Clarify the experimental settings. a) The population selection; b) The indirect effect is counterintuitive.

#### When the network is misspecified.

#### The multi-valued treatments.

