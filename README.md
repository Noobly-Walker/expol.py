# expol.py
It's like break_infinity, but in python and buggy because am dum.

Known issues:
- \_\_pow\_\_() throws decimal.Overflow after around 1000^^3^2
- Illions notation breaks before other notations do

Plans:
- Expol exponents could be expol as well. The operator functions would need minimal rework to accomodate this, and this SHOULD raise the maximum value to 10^^1024 or so.
  - Yes, 10^^1024. 10^10^10^10^10^... 1024 times. A power-tower of tens, 1024 units high. Ten tetrated to the one thousand twenty-fourth. E1 #1.024E3.
  - We will be running out of illions. Bowers' illions extend to 1000^^4 or so. CompactStar's illions get us to 1000^^7. I shall have to scour the Googology Wiki to construct all the way to 1000^^10 or 1000^^11, then I will have to develop a system to reuse existing illions, somehow.
  - Roman notation is just boned. I'm sorry. I'm lucky to have found a means of scaling it to exponential levels, but tetration is just too much.
