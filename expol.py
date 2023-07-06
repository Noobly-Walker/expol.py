from math import log, ceil, floor, sin, cos
from decimal import Decimal
# Written and maintained by Noobly Walker.
# Liscensed with GNU General Public Liscense v3.

MANTISSA = 0
EXPONENT = 1
REAL = 0
IMAGINARY = 1
EMAX = 1000000

class MemoryOverflowSafeguard(Exception):
    def __init__(self, length, message="this operation would require a dangerous amount of memory. This action has been cancelled."):
        self.length = length
        self.size = int(self.length*100//225)
        if self.size < 10**30:
            prefixes = ['', 'kilo', 'mega', 'giga', 'tera', 'peta', 'exa', 'zeta', 'yotta', 'bronto']
            index = min(int(log(self.size, 1000)), len(prefixes)-1)
            self.size /= 1000**index
            self.message = f"conversion from expol to int would require a dangerous amount of memory (est. {round(self.size,3)} {prefixes[index]}bytes). This action has been cancelled."
        else: self.message = message

        super().__init__(self.message)
    

class expol:
    def __init__(self, obj=None):
        """Converts variables into exponent lists.

expol(int) -> expol: expol(123)
expol(float) -> expol: expol(123.0)
expol(str) -> expol: expol("123"), expol("1.23e2"), expol("123.0"), expol("[123,0]"), expol("[1.23,0]")
expol([int|float, int]) -> expol: expol([123,0]), expol([1.23,0])
-------------------------------------------------------------------------------------------------------
String Formats:                 Examples:
e   - Engineering               1.23e45680
el  - Engineering Log-looped    1.23ee4.660
k   - Engineering K             123.000k15226
kl  - Engineering K Log-looped  123.000kk1.394
s   - Scientific                1.23×10^45680
sl  - Scientific Log-looped     1.23×10^10^4.660
sk  - Scientific K              123.000×1000^15226
skl - Scientific K Log-looped   123.000×1000^1000^1.394
i   - Illions                   123.000 QuinVigintDucent-QuinDecMillillion
is  - Illions Shorthand         123.000 QiVgDt-QiDcMl
r   - Roman Numerals            CXXIIIˣᵛ⚂^ᶦ

.   - Round to Nth decimal place
,   - Insert thousands separators
%   - Append percent sign"""
        self.value = [0,0]
        self.isInfinite = False
        self.isNaN = False
        if obj != None:
            if type(obj) == str:
                #Case 1: Stringified exponent
                values = obj.split('e')
                if len(values) == 2:
                    if values[0] == '': values[0] = 1 #e0 = 1
                    else:
                        try: values[0] = Decimal(values[0])
                        except Exception: raise TypeError(f"Invalid string value '{values[0]}' passed in.")
                    if values[1] == '': values[1] = 0 #8e = 8
                    else:
                        try: values[1] = int(values[1])
                        except Exception: raise TypeError(f"Invalid string value '{values[1]}' passed in.")
                    self.value = [values[0], values[1]]
                else:
                    evaldStr = eval(obj)
                    #Case 2: Stringified list
                    if type(evaldStr) == list:
                        if len(evaldStr) == 2:
                            if type(evaldStr[0]) in [float, Decimal, int] and type(evaldStr[1]) == int: self.value = [Decimal(evaldStr[0]), evaldStr[1]]
                            else: #going to find out which one was the wrong type
                                if type(evaldStr[0]) not in [float, int, Decimal]: raise TypeError(f"Index 0: expected int, float, or decimal, but got {type(evaldStr[0])}")
                                if type(evaldStr[1]) not in [int]: raise TypeError(f"Index 1: expected int, but got {type(evaldStr[0])}")
                        else: raise IndexError(f"Expol list takes 2 positional variables but {len(evaldStr)} were given")
                    #Cases 3, 4, and 5: Stringified float, int, or decimal
                    elif type(evaldStr) in [float, int, Decimal]: self.value = self.expExtract(evaldStr)
                    else: raise ValueError(f"Invalid literal for expol() with value '{obj}'")
            #Case 6: List
            elif type(obj) == list:
                if len(obj) == 2:
                    if type(obj[0]) in [float, int, Decimal] and type(obj[1]) == int: self.value = [Decimal(obj[0]), obj[1]]
                    else: #going to find out which one was the wrong type
                        if type(obj[0]) not in [float, int, Decimal]: raise TypeError(f"Index 0: expected int, float, or decimal, but got {type(obj[0])}")
                        if type(obj[1]) not in [int]: raise TypeError(f"Index 1: expected int, but got {type(obj[0])}")
                else: raise IndexError(f"Expol list takes 2 positional variables but {len(obj)} were given")
            #Cases 7, 8 and 9: Float, int, or decimal
            elif type(obj) in [float, int, Decimal]: self.value = self.expExtract(obj)
            #Case 10: Expol
            elif type(obj) == expol: self.value = obj.value
            else: raise TypeError(f"Expected int, float, list, expol, Decimal, or str, but got {type(obj[0])}")

    @property
    def mantissa(self):
        return self.value[MANTISSA]

    @property
    def exponent(self):
        return self.value[EXPONENT]
            
    def expExtract(self, variable): #Converts integers, double floating point numbers, and decimal floating point numbers into exponent lists
        if type(variable) in [int, float, Decimal]:
            if variable != 0:
                exponent = int(log(abs(variable),10))
                mantissa = Decimal(variable) / Decimal(10**exponent)
                return self.expFixVar([mantissa, exponent])
            else: return [0,0]
        elif type(variable) == expol:
            if type(variable.mantissa) is float: return self.expFixVar([variable.mantissa, variable.exponent])
            else: return variable.value
        elif type(variable) == list:
            return variable

    def expFixVar(self, variable): #Corrals the mantissa between 1 and 10 and updates the exponent accordingly
        if type(variable[MANTISSA]) is float:
            variable = [Decimal(variable[MANTISSA]), variable[EXPONENT]]
        if variable[MANTISSA] != 0:
            while abs(variable[MANTISSA]) >= 10: #Rough adjustment
                variable[MANTISSA] /= 10
                variable[EXPONENT] += 1
            while abs(variable[0]) < 1:
                variable[MANTISSA] *= 10
                variable[EXPONENT] -= 1
            if abs(variable[MANTISSA]) >= 9.9999999999: #Fine adjustment
                variable[MANTISSA] = Decimal(round(variable[MANTISSA]))
                variable[MANTISSA] /= 10
                variable[EXPONENT] += 1
        else:
            variable[EXPONENT] = 0
        self.format_float(variable[MANTISSA])
        return variable

    def getSign(self, var): # Splits the sign from the number.
        if not var.isNaN:
            if var < 0: return var*-1, -1
            else: return var, 1

    def format_float(self, f):
        d = Decimal(str(f));
        return d.quantize(Decimal(1)) if d == d.to_integral() else d.normalize()

    def __add__(self, addend): #Addition operation +
        if self.isNaN or self.isInfinite: return self
        var1 = self.value; var2 = self.expExtract(addend)
        expDiff = var1[EXPONENT] - var2[EXPONENT]
        if abs(expDiff) <= 50:
            if expDiff < 0: mantOut = var1[MANTISSA]*10**Decimal(expDiff)+var2[MANTISSA]
            elif expDiff >= 0: mantOut = var1[MANTISSA]+var2[MANTISSA]/10**Decimal(expDiff)
            expOut = max(var1[EXPONENT], var2[EXPONENT])
        #the following is to prevent float overflow due to attempting to add two numbers of incomparable size
        elif expDiff > 50: mantOut = var1[MANTISSA]; expOut = var1[EXPONENT]
        elif expDiff < -50: mantOut = var2[MANTISSA]; expOut = var2[EXPONENT]
        return expol(self.expFixVar([mantOut, expOut]))

    def __sub__(self, subtrahend): #Subtraction operation -
        var1 = self.value; var2 = self.expExtract(subtrahend)
        return self.__add__([var2[MANTISSA]*-1, var2[EXPONENT]])

    def __mul__(self, factor): #Multiplication operation *
        var1 = self.value; var2 = self.expExtract(factor)
        mantOut = var1[MANTISSA] * var2[MANTISSA]
        expOut = var1[EXPONENT] + var2[EXPONENT]
        return expol(self.expFixVar([mantOut, expOut]))

    def __truediv__(self, divisor): #Division operation /
        var1 = self.value; var2 = self.expExtract(divisor)
        return self.__mul__([Decimal(1)/Decimal(var2[MANTISSA]), -var2[EXPONENT]])

    def __floordiv__(self, divisor): #Floor division operation //
        quotient = self.__truediv__(self.expExtract(divisor))
        if abs(quotient.value[EXPONENT]) < 100: #if the number is too large, the ones place might not be saved anyway
            quotient.value[MANTISSA] = Decimal(floor(quotient.value[MANTISSA]*Decimal(10**quotient.value[EXPONENT])))/Decimal(10**quotient.value[EXPONENT])
        return expol(self.expFixVar(quotient.value))

    def ceildiv(self, divisor): #Ceiling division operation
        return -(-self // self.expExtract(divisor))

    def round(self): #Rounding operation
        if self%1 < 0.5: return expol(self//1)
        else: return self.ceildiv(1)

    def __mod__(self, divisor): #Modulo division operation %
        quotient = self.__truediv__(self.expExtract(divisor))
        floor = self.__floordiv__(self.expExtract(divisor))
        return (quotient - floor) * divisor

    def __pow__(self, exponent): #Exponentiation operation **
        var1 = self.value; var2 = self.expExtract(exponent)
        a, b, c, d = var1[0], var1[1], var2[0], var2[1]
        #if d > EMAX: raise MemoryOverflowSafeguard(d)
        if d > EMAX: j = expol(10)**expol(d)
        else: j = Decimal(10**d)
        n = int((j*c*(b+Decimal(log(a, 10))))*10**30)
        if n >= 0: expOut = abs(n) // 10**30
        else: expOut = abs(n) // 10**30 * -1
        mantOut = round(10**(n % 10**30 / 10**30),10)
        # Thanks to Dorijanko, Feodoric, and mustache for help figuring out this nightmare.
        return expol(self.expFixVar([mantOut, expOut]))

    def root(self, root): #Root operation
        root = self.expExtract(root)
        return self ** (expol(1)/root)

    def log10(self): #Log10 operation
        mant,exp = self.value
        if abs(exp) >= 10:
            exp, expSign = self.getSign(exp) 
            exp = log(exp, 10)
            intExp = int(exp//1)
            mant = float(expol([log(mant, 10), -intExp])) + 10**(exp-intExp)*expSign
            return expol([mant, intExp])
        else:
            return expol(log(mant, 10)+exp)

    def log(self, base:Decimal): #Custom log operation
        mant,exp = self.value
        return expol(Decimal(log(mant, base))+Decimal(exp)/Decimal(log(base, 10)))

    def tet(self, tetraponent): #Rough tetration operation
        tetraponent = expol(tetraponent)
        out = expol(self)
        while tetraponent > 1:
            if out.exponent > EMAX: raise MemoryOverflowSafeguard(out.exponent)
            out = self**out
            tetraponent -= 1
        return out

    #a lot of this gets cut off, unfortunately
    pi = expol(3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679)

    def sin(self): #Sine function
        return expol(sin(float(self%(pi*2))))

    def cos(self): #Cosine function
        return expol(cos(float(self%(pi*2))))

    def tan(self): #Tangent function. To do it justice, however, we need millions of digits of pi.
        return self.sin()/self.cos()

    def cot(self): #Cotangent function. To do it justice, however, we need millions of digits of pi.
        return self.cos()/self.sin()

    def sec(self): #Secant function
        return expol(1)/self.cos()

    def scs(self): #Cosecant function
        return expol(1)/self.sin()

    def rad(self): #Degrees to radians
        return self * pi/180

    def deg(self): #Radians to degrees
        return self * expol(180)/pi

    def fact(self): #Factorial
        if self < 1000: #Bruteforce is more precise
            reps = self-1
            newVal = expol(self)
            while reps > 0:
                newVal *= reps
                reps -= 1
            return newVal
        else: #Stirling's approximation is faster
            sqrt_2pi_n = expol.root(expol(3.141592653589793) * 2 * self, expol(2))
            power_term = (self / expol(2.718281828459045)) ** self
            correction_terms = expol(1)
            for i in range(1, 5):
                correction_terms += expol((-1) ** i) * (expol(2 * i - 1) / (self * 2 * 12)) * (expol(1) / self) ** i
            return sqrt_2pi_n * power_term * correction_terms

    def __neg__(self): #Negate operation -expol
        return expol([self.value[MANTISSA]*-1,self.value[EXPONENT]])

    def __pos__(self): #Positive operation +expol
        return expol(self.value)

    def __abs__(self): #Absolute value operation
        if self.value[MANTISSA] < 0: return expol([self.value[MANTISSA]*-1,self.value[EXPONENT]])
        else: return expol(self.value)

    def compare(self, compared): #base function for comparisons
        try: compared = expol(compared)
        except (NameError, TypeError): return -2 #if it cannot be converted into expol, it cannot be compared, and thus cannot be equivalent
        val1, val2 = [x.value[EXPONENT] for x in (self, compared)]
        val3, val4 = [x.value[MANTISSA] for x in (self, compared)]
        if val1 > val2:
            if (val3 < 0 and val4 < 0) or (val3 > 0 and val4 > 0): return 0 #both are on the same side of 0, so the exponent was enough
            elif val3 <= 0 and val4 >= 0: return 2 #compared is negative, self is positive
            elif val3 >= 0 and val4 <= 0: return 0 #compared is positive, self is negative
        elif val1 < val2:
            if (val3 < 0 and val4 < 0) or (val3 > 0 and val4 > 0): return 2 #both are on the same side of 0, so the exponent was enough
            elif val3 <= 0 and val4 >= 0: return 2 #compared is negative, self is positive
            elif val3 >= 0 and val4 <= 0: return 0 #compared is positive, self is negative
        elif val1 == val2:
            if val3 > val4: return 0
            elif val3 == val4: return 1
            elif val3 < val4: return 2

    def __eq__(self, compared): #Equal comparison ==
        if self.compare(compared) == 1: return True
        else: return False

    def __ne__(self, compared): #Not equal comparison !=
        if self.compare(compared) != 1: return True
        else: return False

    def __gt__(self, compared): #Greater than comparison >
        if self.compare(compared) == 0: return True
        else: return False

    def __ge__(self, compared): #Greater or equal comparison >=
        if self.compare(compared) in [0,1]: return True
        else: return False

    def __lt__(self, compared): #Less than comparison <
        if self.compare(compared) == 2: return True
        else: return False

    def __le__(self, compared): #Less or equal comparison <=
        if self.compare(compared) in [1,2]: return True
        else: return False

    def __str__(self):#Conversion to string
        return f"{self:e}"

    def __format__(self, fmt): #String format codes
        mant = self.value[MANTISSA]; exp = self.value[EXPONENT]
        MANTISSA_ROUND = 10
        string = ""

        illionsShortList = [
            [
                ["k","M","B","T","Qa","Qi","Sx","Sp","O","N"],
                ["","U","D","T","Qa","Qi","Sx","Sp","O","N"],
                ["","Dc","Vg","Tg","Qag","Qig","Sxg","Spg","Og","Ng"],
                ["","Ct","Dt","Tt","Qat","Qit","Sct","Spt","Ot","Nt"]
            ],
            [
                ["","Ml","Mc","Na","Pc","Fm","At","Zp","Yc","Xn"],
                ["","M","D","Tr","Te","P","Hx","Hp","O","E"],
                ["","Vc","Ic","Trc","Tec","Pc","Hxc","Hpc","Oc","Ec"],
                ["","Hct","Dct","Trct","Tect","Pct","Hxct","Hpct","Oct","Ect"]
            ],
            [
                ["","Kl","Mg","Gg","Tr","P","E","Z","Y","X"],
                ["","H","D","Tr","Te","P","E","Z","Y","N"],
                ["","Dk","Ik","Trk","Tek","Pk","Ek","Zk","Yk","Nk"],
                ["","Hot","Bot","Trot","Tot","Pot","Eot","Zot","Yot","Not"]
            ]
        ]

        illionsList = [
            [
                ["Thousand","M","B","Tr","Quadr","Quint","Sext","Sept","Oct","Non"],
                ["","Un","Duo","Tre","Quattor","Quin","Sex","Septen","Octo","Novem"],
                ["","Dec","Vigint","Trigint","Quadragint","Quinquagint","Sexagint","Septuagint","Octagint","Nonagint"],
                ["","Cent","Ducent","Trecent","Quadringent","Quincent","Sescent","Septingent","Octingent","Nongent"]
            ],
            [
                ["","Mill","Micr","Nan","Pic","Femt","Att","Zept","Yoct","Xon"],
                ["","Me","Due","Trio","Tetre","Pente","Hexe","Hepte","Octe","Enne"],
                ["","Vec","Icos","Triacont","Tetracont","Pentacont","Hexacont","Heptacont","Octacont","Ennacont"],
                ["","Hect","Duehect","Triahect","Tetrahect","Pentahect","Hexahect","Heptahect","Octahect","Ennahect"]
            ],
            [
                ["","Kill","Meg","Gig","Ter","Pet","Ex","Zett","Yott","Xenn"],
                ["","Hen","Do","Tra","Te","Pe","Ex","Ze","Yo","Ne"],
                ["","Dak","Ik","Trak","Tek","Pek","Exac","Zak","Yok","Nek"],
                ["","Hot","Bot","Trot","Tot","Pot","Exot","Zot","Yoot","Not"]
            ]
        ]

        SIList = [
            [
                ["Kilo", "Mega", "Giga", "Tera", "Peta", "Exa", "Zetta", "Yotta", "Ronna", "Quetta"]
            ]
        ]
        SIListFract = [
            [
                ["Milli", "Micro", "Nano", "Pico", "Atto", "Femto", "Zepto", "Yocto", "Ronto", "Quecto"]
            ]
        ]

        SIShortList = [
            [
                ["k", "M", "G", "T", "P", "E", "Z", "Y", "R", "Q"]
            ]
        ]
        SIShortListFract = [
            [
                ["m", "µ", "n", "p", "a", "f", "z", "y", "r", "q"]
            ]
        ]

        def splitThou(number):
            number = f"{number:,}".split(",")
            return [int(n) for n in number]

        def parseNotationList(notList, _tier, index, highest=True):
            out = ""
            revindex = str(index)[:: -1]
            for power in range(len(revindex)):
                if revindex[power] == "-": continue
                if index < 10 and power == 0 and highest:
                    out += notList[_tier][power][int(revindex[power])]
                else:
                    out += notList[_tier][power+1][int(revindex[power])]
            return out

        def convToListNotation(mant, exp, _list):
            mant *= 10**(exp % 3) # mantissa convert e to k
            if exp < 0: isFraction = True
            else: isFraction = False
            exponentAtWorkingTier = (abs(exp)-isFraction) // 3 -1 + isFraction # exponent convert e to k
            name = ""
            workingTier = 0
            exponentAtLastTier = None #this is the exponent before the previous log1000()
            
            while exponentAtWorkingTier >= 1000: #figure out which tier -illion to use with repeated log1000()
                exponentAtLastTier = exponentAtWorkingTier #save the last tier's exponent, in case it's needed
                exponentAtWorkingTier = int(expol(exponentAtWorkingTier).log(1000))
                workingTier += 1 #workingTier is googological tier, with 0 starting million, 1 starting millillion, 2 starting killillion, etc.
                
            if exponentAtWorkingTier == -1 and not isFraction: # it's between zero and one thousand!
                pass # We don't need to do anything.
            
            elif exponentAtWorkingTier < 10 and exponentAtLastTier != None: # It is a normal illion
                exponentGroupsAtLastTier = splitThou(exponentAtLastTier) # split the exponent into groups of three digits, then combine the googolism of the last tier with the one for this tier
                sections = []
                for workingTierValue in range(exponentAtWorkingTier, -1, -1): # workingTierValue is the number of this tier. 7 in tier 2 is zetillion.
                    if workingTierValue == exponentAtWorkingTier:
                        if exponentGroupsAtLastTier[-(workingTierValue)+1] == 1: # if theres only one, then theres no need to bother with numbers before. It's millillion, not memillillion.
                            sections.insert(0, parseNotationList(_list, workingTier, workingTierValue))
                        else: #if there is more than one, then we do need to specify the quantity of that place.
                            sections.insert(0, parseNotationList(_list, workingTier-1, exponentGroupsAtLastTier[-(workingTierValue)+1], False) +
                                            parseNotationList(_list, workingTier, workingTierValue))
                    elif exponentGroupsAtLastTier[-(workingTierValue)] != 0:
                        sections.insert(0, parseNotationList(_list, workingTier-1, exponentGroupsAtLastTier[-(workingTierValue)+1], False) +
                                        parseNotationList(_list, workingTier, workingTierValue))
                    if sections[0] == "": sections.pop(0)
                if len(sections) > 1: name = "-".join(sections)
                else: name = sections[0]
                if _list == illionsList: name += "illion"
                
            else: # 0-illion to 9-illion is speshul and has a speshul list
                name = parseNotationList(_list, workingTier, exponentAtWorkingTier)
                if _list == illionsList and exponentAtWorkingTier != 0: name += "illion"
                
            if isFraction:
                if _list == illionsShortList: name += "þ"
                if _list == illionsList: name += "th"
                if _list == SIShortList: name = parseNotationList(SIShortListFract, workingTier, exponentAtWorkingTier)
                if _list == SIList: name = parseNotationList(SIListFract, workingTier, exponentAtWorkingTier)
            if len(name) > 80:
                name = "..." + name[-80:]
            if (workingTier <= 1 and exponentAtWorkingTier < 10) or (workingTier == 0):
                return f"{self.format_float(round(mant,MANTISSA_ROUND))} " + name
            else:
                return name

        def checkIndex(indexedList, index):
            try: return indexedList[index]
            except IndexError: return 0

        def romanize(mant, exp, notationstr, fractions):
            string = ""
            if mant < 0:
                mant = abs(mant)
                string += notationstr[-1]
            mant *= 10**(exp % 3)
            exp //= 3
            remainder = round(mant % 1 * 12)
            mant = int(mant // 1)
            if remainder >= 12:
                mant += 1
                remainder -= 12
            if mant >= 1000:
                mant /= 1000
                remainder = round(mant % 1 * 12)
                mant = int(mant // 1)
                exp += 1
            mant = str("".join(reversed(str(mant))))
            power = len(mant)-1
            while power >= 0:
                notmap = {"0":"",
                          "1":notationstr[power*2],
                          "2":notationstr[power*2]*2,
                          "3":notationstr[power*2]*3,
                          "4":notationstr[power*2]+notationstr[power*2+1],
                          "5":notationstr[power*2+1],
                          "6":notationstr[power*2+1]+notationstr[power*2],
                          "7":notationstr[power*2+1]+notationstr[power*2]*2,
                          "8":notationstr[power*2]*2+notationstr[power*2+2],
                          "9":notationstr[power*2]+notationstr[power*2+2]}
                string += notmap[mant[power]]
                power -= 1
            string += fractions[remainder]
            return string, exp
        
        if "." in fmt: #rounding
            fmtChunks = fmt.split(".")
            try:
                if fmtChunks[1][0] in "0123456789" : MANTISSA_ROUND = int(fmtChunks[1][0])
                else: MANTISSA_ROUND = 0
            except IndexError: #clearly, there wasn't another character after the period.
                MANTISSA_ROUND = 0
        
        if "el" in fmt: #engineering log looped
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                loops = 1
                while exp > 10:
                    exp = round(log(exp, 10),MANTISSA_ROUND)
                    loops += 1
                string = f"{self.format_float(round(mant,MANTISSA_ROUND))}" + "E" * loops + f"{exp}"
        
        elif "e" in fmt or fmt == "": #engineering
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp:,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp}"
        
        elif "skl" in fmt: #scientific k log looped
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                if "," in fmt: k = "1,000"
                else: k = "1000"
                mant *= 10**(exp % 3)
                exp //= 3
                loops = 1
                while exp > 1000:
                    exp = round(log(exp, 1000),MANTISSA_ROUND)
                    loops += 1
                string = f"{self.format_float(round(mant,MANTISSA_ROUND))}×" + k * loops + f"{exp}"
        
        elif "sk" in fmt: #scientific k
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                mant *= 10**(exp % 3)
                exp //= 3
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}×1,000^{exp:,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}×1000^{exp}"

        elif "sis" in fmt: #system international shorthand
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                try:
                    string = convToListNotation(mant, exp, SIShortList)
                except IndexError:
                    if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp:,}"
                    else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp}"
                    

        elif "si" in fmt: #system international
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                try:
                    string = convToListNotation(mant, exp, SIList)
                except IndexError:
                    if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp:,}"
                    else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp}"

        elif "is" in fmt: #illions shorthand
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                try:
                    string = convToListNotation(mant, exp, illionsShortList)
                except IndexError:
                    if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp:,}"
                    else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp}"

        elif "i" in fmt: #illions
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                try:
                    string = convToListNotation(mant, exp, illionsList)
                except IndexError:
                    if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp:,}"
                    else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}E{exp}"
            
        elif "sl" in fmt: #scientific log looped
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                loops = 1
                while exp > 10:
                    exp = round(log(exp, 10),MANTISSA_ROUND)
                    loops += 1
                string = f"{self.format_float(round(mant,MANTISSA_ROUND))}×" + "10^" * loops + f"{exp}"
        
        elif "s" in fmt: #scientific
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}×10^{exp:,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}×10^{exp}"
        
        elif "kl" in fmt: #engineering k log looped
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                mant *= 10**(exp % 3)
                exp //= 3
                loops = 1
                while exp > 1000:
                    exp = round(log(exp, 1000),MANTISSA_ROUND)
                    loops += 1
                string = f"{self.format_float(round(mant,MANTISSA_ROUND))}" + "K" * loops + f"{exp}"

        elif "k" in fmt: #engineering k
            if abs(exp) < 2*MANTISSA_ROUND:
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp)):,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND)*Decimal(10**exp))}"
            else:
                mant *= 10**(exp % 3)
                exp //= 3
                if "," in fmt: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}K{exp:,}"
                else: string = f"{self.format_float(round(mant,MANTISSA_ROUND))}K{exp}"

        elif "r" in fmt: #roman
            notationstr = "IVXLCDM-"
            notationexpstr = "ᶦᵛˣᴸᶜᴰᴹ⁻"
            fractions = ["","·",":","∴","∷","⁙","S","S·","S:","S∴","S∷","S⁙"]
            fractionsexp = ["","⚀","⚁","⚂","⚃","⚄","ˢ","ˢ⚀","ˢ⚁","ˢ⚂","ˢ⚃","ˢ⚄"]
            if mant == 0:
                string = "N"
            else:
                stringadd, exp = romanize(mant, exp, notationstr, fractions)
                string += stringadd
                if exp < 1000:
                    stringadd, exp2 = romanize(exp, 0, notationexpstr, fractionsexp)
                    string += "ᴹ"*exp2 + stringadd
                else:
                    exp2 = log(exp, 1000)
                    stringadd, exp3 = romanize(1000**(exp2%1), 0, notationexpstr, fractionsexp)
                    string += stringadd + "^"
                    stringadd, _ = romanize(exp2//1+exp3, 0, notationexpstr, fractionsexp)
                    string += stringadd

        if "%" in fmt: #percent sign
            string += "%"
        return string

    def __repl__(self): #Conversion to stringified list if normal printing doesn't work
        return str(self.value)

    def __int__(self): #Conversion to integer
        if self.value[EXPONENT] > EMAX: raise MemoryOverflowSafeguard(self.value[EXPONENT])
        elif self.value[EXPONENT] > 100: #must carefully step down to int to prevent float overflow
            x = int(self.value[MANTISSA]*10**100)
            expon = self.value[EXPONENT]-100
            out = x*10**expon
        elif self.value[EXPONENT] < 0:
            out = 0
        else: out = int(self.value[MANTISSA]*10**self.value[EXPONENT])
        expon2 = self.value[EXPONENT]
        if len(str(out)) >= 9:
            factor = 10**(expon2-9)
            if int(str(out)[8]) < 5:
                out = out//factor * factor
            else:
                out = ceil(out/factor) * factor
        return int(out)

    def __float__(self): #Conversion to double floating point
        if abs(self.value[EXPONENT]) > 308: raise OverflowError("Expol too large to convert to float") #will float overflow if too large, and could cause memory overflow.
        else: return float(self.value[MANTISSA])*10**self.value[EXPONENT]

    def __iter__(self): #Conversion to list
        return iter(self.value)

class expolComplex:
    def __init__(self, real=None, imag=None):
        self.value = [expol(0),expol(0)]
        self.validReal = [float, int, Decimal, expol]
        self.validComplex = [expolComplex, complex]
        if type(real) in self.validComplex:
            self.value[REAL] = real.real
            self.value[IMAGINARY] = real.imag
        else:
            if real != None:
                #Cases 1, 2 and 3: Float, int, or decimal
                if type(real) in [float, int, Decimal]: self.value[REAL] = expol(real)
                #Case 4: Expol
                elif type(real) == expol: self.value[REAL] = real.value
                else: raise TypeError(f"Expected int, float, list, Decimal, or expol, but got {type(obj[0])}")
            if imag != None:
                #Cases 1, 2 and 3: Float, int, or decimal
                if type(imag) in [float, int, Decimal]: self.value[IMAGINARY] = expol(imag)
                #Case 4: Expol
                elif type(imag) == expol: self.value[IMAGINARY] = imag.value
                else: raise TypeError(f"Expected int, float, list, Decimal, or expol, but got {type(obj[0])}")
            
    @property
    def real(self):
        return self.value[REAL]

    @property
    def imag(self):
        return self.value[IMAGINARY]

    def __add__(self, addend): #Addition operation +
        if type(addend) in self.validReal:
            self.value[REAL] += addend
        elif type(addend) in self.validComplex:
            self.value[REAL] += addend.real
            self.value[IMAGINARY] += addend.imag
        return self

    def __sub__(self, subtrahend): #Subtraction operation -
        if type(subtrahend) in self.validReal:
            self.value[REAL] -= subtrahend
        elif type(subtrahend) in self.validComplex:
            self.value[REAL] -= subtrahend.real
            self.value[IMAGINARY] -= subtrahend.imag
        return self

    def __mul__(self, factor): #Multiplication operation *
        if type(factor) in self.validReal:
            self.value[REAL] *= factor
            self.value[IMAGINARY] *= factor
        elif type(factor) in self.validComplex:
            r = self.real * factor.real - self.imag * factor.imag
            i = self.real * factor.imag + self.imag * factor.real
            self.value[REAL] = r
            self.value[IMAGINARY] = i
        return self

    def __truediv__(self, divisor): #Division operation /
        if type(divisor) in self.validReal:
            self.value[REAL] /= divisor
            self.value[IMAGINARY] /= divisor
        elif type(divisor) in self.validComplex:
            numerator = self * expolComplex(divisor.real, -divisor.imag)
            denominator = expolComplex(divisor) * expolComplex(divisor.real, -divisor.imag)
            self = numerator
            self.value[REAL] /= denominator.real
            self.value[IMAGINARY] /= denominator.real
        return self

    def __floordiv__(self, divisor): #Floor division operation //
        self /= divisor
        self.value[REAL] //= 1
        self.value[IMAGINARY] //= 1
        return self

    def ceildiv(self, divisor): #Ceiling division operation
        self /= divisor
        self.value[REAL] = self.real.ceildiv(1)
        self.value[IMAGINARY] = self.imag.ceildiv(1)
        return self
        
    def round(self): #Rounding operation
        self.value[REAL] = self.real.round()
        self.value[IMAGINARY] = self.imag.round()
        return self

    def __mod__(self, divisor): #Modulo division operation %
        if type(addend) in self.validReal:
            self.value[REAL]

    def __pow__(self, exponent): #Exponentiation operation **
        if type(addend) in self.validReal:
            self.value[REAL]

    def log10(self): #Log10 operation
        if type(addend) in self.validReal:
            self.value[REAL]

    def log(self, base:Decimal): #Custom log operation
        if type(addend) in self.validReal:
            self.value[REAL]

    def tet(self, tetraponent): #Rough tetration operation
        if type(addend) in self.validReal:
            self.value[REAL]
