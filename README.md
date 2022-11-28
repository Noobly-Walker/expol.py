# expol.py
It's like break_infinity, but in python and buggy because am dum.

Known issues:
- \_\_pow\_\_() has numerous precision issues, which may break Illions and Illions Short notaitons.
  - Using \_\_pow\_\_() in Illions and Illions Short notaitons loses contiguity around Attillion (1000^^2^6), throwing IndexError: list index out of range
  - A printed number may appear as 10.0e999 or 1000.0k333 because the mantissa is actually 9.99999999999999964164.
  - Calculating numbers larger than eee2.442 with \_\_pow\_\_() throws OverflowError: cannot convert float infinity to integer.
  - Mantissa precision is lost entirely soon after entering Tier 2 illions (over ee3)
