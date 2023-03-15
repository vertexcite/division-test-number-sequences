# Experiment: Relationship between divisibility test cycles and multiplicative order

Many divisibility tests are similar to these tests for divisibilty:

* Divisibility by 7:  Add 5 times the last digit to the rest gives a multiple of 7. (Works because 49 is divisible by 7.) 
* Divisibility by 13: Add 4 times the last digit to the rest. The result must be divisible by 13. (Works because 39 is divisible by 13)
(Source: https://en.wikipedia.org/wiki/Divisibility_rule)

The general idea is something like:
Divisibility by $m$: Add $k$ times the last digit to the rest. The result must be divisible by $m$. 

This works because:

* $k$ is chosen such that $10 \times k - 1$ is divisible by $m$, and $k$ relatively prime to $m$. 
* the candidate $n$ can be written as  $10 \times r + u$, where $r$ is "rest of the digits" and $u$ is the last digit.

Generalising to any base, $b$, then:

```math
\begin{align}
k \times n \equiv k \times (b r + u) \pmod m \\
 \equiv (k \times b r) \pmod m + k \times u \pmod m \\
 \equiv (k \times b ) \pmod m \times r \pmod m + k \times u \pmod m \\
 \equiv 1 \pmod m \times r \pmod m + k \times u \pmod m \\
\equiv  1 \times r \pmod m + k \times u \pmod m \\
\equiv  r \pmod m + k \times u \pmod m \\
\equiv  (r + k \times u) \pmod m \\
 
\end{align}
```

In this experiment, k takes on values 1, 2, 3, ...
The test step consists of multiplying the last digit by k and adding it to remaining digits
The test step is repeated until a cycle is detected.
The length of the cycle is the periodicity.
This produces a list of periodicities, essentially indexed by k.
This is done for a range of number bases.
The resulting sequences are found to be equivalent to corresponding multiplicative orders.

Not sure what the reason is for this equivalence.

E.g.,

|base|correspondng multiplicative order|
|----|---------------------------------|
|   2| https://oeis.org/A002326        |
|   3| https://oeis.org/A003572        |
|   4| https://oeis.org/A003574        |
|   5| https://oeis.org/A217852        |
|  10| https://oeis.org/A128858        |