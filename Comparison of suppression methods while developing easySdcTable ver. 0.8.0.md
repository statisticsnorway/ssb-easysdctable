
# Comparison of suppression methods 
### while developing easySdcTable ver. 0.8.0 using sdcTable ver. 0.32.0 and  SSBtools ver. 1.2.0 using 64-bit R ver. 4.1.0 on windows laptop. 

Below `Data` refers to the datasets in easySdcTable.
When `linked` is `no`, parameter `dimVar` is `c(1,2,4)` (when `z2`) or `c(1,2,4,5)`.
When `linked` is `yes`, parameter `dimVar` is `1:4` (when `z2`) or `1:5`.
The method `"SimpleSingle"` is  `"SIMPLEHEURISTIC"` with `detectSingletons=TRUE` when `protectZeros=FALSE` and
`"SIMPLEHEURISTIC"` with `threshold=1` when `protectZeros=TRUE`. The end `_FALSE` means that `solve_attackerprobs=FALSE`.

To reveal cells, `PTxyz` in easySdcTable is combined with `SuppressDec` in RegSDC. See PTxyz examples.

---


### Computing time in seconds of ProtectTable run 

Data | linked | protectZeros | maxN |  Gauss | SIMPLEHEURISTIC | SimpleSingle | SIMPLEHEURISTIC _FALSE | SimpleSingle _FALSE |
 ---:|   :---: |   :---: |   :---: |   ---: |   :---: |   :---: |   :---: | :---: |
**z2** | **no** | **FALSE** | **2** | 0.4 | 0.4 | 0.4 | 0.3 | 0.3 |  
**z2** | **no** | **FALSE** | **10** | 0.3 | 0.3 | 0.3 | 0.5 | 0.3 |  
**z2** | **no** | **TRUE** | **2** | 0.5 | 0.4 | 0.3 | 0.2 | 0.2 |  
**z2** | **no** | **TRUE** | **10** | 0.3 | 0.3 | 0.3 | 0.3 | 0.4 |  
**z2** | **yes** | **FALSE** | **2** | 0.4 | 0.5 | 0.5 | 0.5 | 0.4 |  
**z2** | **yes** | **FALSE** | **10** | 0.5 | 0.5 | 0.5 | 0.6 | 0.6 |  
**z2** | **yes** | **TRUE** | **2** | 0.6 | 0.5 | 0.6 | 0.5 | 0.5 |  
**z2** | **yes** | **TRUE** | **10** | 0.5 | 0.6 | 0.5 | 0.6 | 0.5 |  
**z3** | **no** | **FALSE** | **2** | 1.0 | *Error* | *Error* | 0.5 | *Error* |  
**z3** | **no** | **FALSE** | **10** | 0.8 | *Error* | *Error* | 0.6 | *Error* |  
**z3** | **no** | **TRUE** | **2** | 0.8 | *Error* | 1.0 | 0.5 | 0.4 |  
**z3** | **no** | **TRUE** | **10** | 0.6 | 1.0 | 0.8 | 0.5 | 0.5 |  
**z3** | **yes** | **FALSE** | **2** | 1.5 | 4.9 | *Error* | 2.6 | *Error* |  
**z3** | **yes** | **FALSE** | **10** | 1.9 | 4.8 | *Error* | 2.7 | *Error* |  
**z3** | **yes** | **TRUE** | **2** | 1.7 | 4.6 | 4.9 | 2.5 | 2.5 |  
**z3** | **yes** | **TRUE** | **10** | 1.4 | 5.1 | 3.1 | 2.8 | 3.0 |  
**sosialFiktiv** | **no** | **FALSE** | **2** | 23.4 | *Error* | *Error* | 6.6 | *Error* |  
**sosialFiktiv** | **no** | **FALSE** | **10** | 33.2 | *Error* | *Error* | 7.2 | *Error* |  
**sosialFiktiv** | **no** | **TRUE** | **2** | 24.4 | *Error* | *Error* | 6.8 | 6.7 |  
**sosialFiktiv** | **no** | **TRUE** | **10** | 24.8 | 156.4 | 26.8 | 7.4 | 7.1 |  
**sosialFiktiv** | **yes** | **FALSE** | **2** | 31.3 | 254.9 | *Error* | 124.8 | *Error* |  
**sosialFiktiv** | **yes** | **FALSE** | **10** | 33.6 | 386.7 | *Error* | 136.0 | *Error* |  
**sosialFiktiv** | **yes** | **TRUE** | **2** | 31.9 | 269.1 | 256.3 | 124.9 | 126.3 |  
**sosialFiktiv** | **yes** | **TRUE** | **10** | 34.5 | 415.9 | 171.6 | 141.1 | 142.7 |


---


### Number of suppressed cells (number of revealed cells in parenthesis)


Data | linked | protectZeros | maxN |  Gauss | SIMPLEHEURISTIC | SimpleSingle | SIMPLEHEURISTIC _FALSE | SimpleSingle _FALSE |
 ---:|   :---: |   :---: |   :---: |   ---: |   :---: |   :---: |   :---: | :---: |
**z2** | **no** | **FALSE** | **2** | 16 | 16 | 20 | 16 | 20 |  
**z2** | **no** | **FALSE** | **10** | 35 | 35 | 38 | 35 | 38 |  
**z2** | **no** | **TRUE** | **2** | 18 | 20 | 70 | 20 | 70 |  
**z2** | **no** | **TRUE** | **10** | 36 | 37 | 70 | 37 | 70 |  
**z2** | **yes** | **FALSE** | **2** | 20 | 28 | 31 | 28 | 31 |  
**z2** | **yes** | **FALSE** | **10** | 44 | 47 | 47 | 47 | 47 |  
**z2** | **yes** | **TRUE** | **2** | 25 | 32 | 79 | 32 | 79 |  
**z2** | **yes** | **TRUE** | **10** | 47 | 51 | 79 | 51 | 79 |  
**z3** | **no** | **FALSE** | **2** | 308 | *Error* | *Error* | 278 (2) | *Error* |  
**z3** | **no** | **FALSE** | **10** | 452 | *Error* | *Error* | 450 (1) | *Error* |  
**z3** | **no** | **TRUE** | **2** | 350 | *Error* | 593 | 336 | 593 |  
**z3** | **no** | **TRUE** | **10** | 553 | 554 | 631 | 554 | 631 |  
**z3** | **yes** | **FALSE** | **2** | 397 | 340 (21) | *Error* | 340 (21) | *Error* |  
**z3** | **yes** | **FALSE** | **10** | 502 | 494 (5) | *Error* | 494 (5) | *Error* |  
**z3** | **yes** | **TRUE** | **2** | 397 | 383 (4) | 701 | 383 (4) | 701 |  
**z3** | **yes** | **TRUE** | **10** | 603 | 603 (1) | 736 | 603 (1) | 736 |  
**sosialFiktiv** | **no** | **FALSE** | **2** | 2057 | *Error* | *Error* | 1973 (27) | *Error* |  
**sosialFiktiv** | **no** | **FALSE** | **10** | 3805 | *Error* | *Error* | 3795 (5) | *Error* |  
**sosialFiktiv** | **no** | **TRUE** | **2** | 2241 | *Error* | *Error* | 2238 | 6460 |  
**sosialFiktiv** | **no** | **TRUE** | **10** | 4289 | 4313 | 6783 | 4307 | 6783 |  
**sosialFiktiv** | **yes** | **FALSE** | **2** | 2168 | 1995 (53) | *Error* | 1995 (53) | *Error* |  
**sosialFiktiv** | **yes** | **FALSE** | **10** | 3825 | 3818 (8) | *Error* | 3818 (8) | *Error* |  
**sosialFiktiv** | **yes** | **TRUE** | **2** | 2268 | 2275 (18) | 6768 | 2275 (18) | 6768 |  
**sosialFiktiv** | **yes** | **TRUE** | **10** | 4309 | 4330 (2) | 7085 | 4330 (2) | 7085 |   
