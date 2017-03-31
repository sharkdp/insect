insect
======

A REPL-style scientific calculator with full support for physical units.

[![insect](media/insect-32x32.png)](https://shark.fish/insect/)

* [**Web version**](https://shark.fish/insect/)
* [Terminal version](#install)

Documentation
-------------
- **Evaluate mathematical expressions**:
  ```
  1920/16*9
  2^32
  sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2
  ```
  * **Operators**: addition (`+`), subtraction (`-`), multiplication (`*`, `·`, `×`), division (`/`, `÷`), exponentiation (`^`, `**`)
  
  * **Mathematical functions**: `acos`, `acosh`, `asin`, `asinh`, `atan`, `atanh`,
  `ceil`, `cos`, `cosh`, `exp`, `floor`, `ln`, `log`, `log10`, `round`, `sin`,
  `sinh`, `sqrt`, `tan`, `tanh`.
  
  * **Arbitrary-precision numeric type** that can handle *very* large (or small) exponents like *10^(10^10)*.
  
  * **Exponential notation**: `6.022e-23`.

- **Physical units**: parsing and handling, including metric prefixes:
  ```
  2min + 30s
  40kg * 9.8m/s² * 150cm
  sin(30°)
  ```
  * **Supported units**: see *Reference* section below.

  * **Implicit conversions**: `15km/h * 30min` evaluates to `7.5km`.

  * **Useful error messages**:
    ```
    > 2 watts + 4 newton meter

    Unification error:
      Cannot unify unit N·m (base units: kg·m²·s⁻²)
              with unit W (base units: kg·m²·s⁻³)
    ```

- **Explicit unit conversions**: the conversion operator `->` (aliases: `→`, `to`):
  ```
  60mph -> m/s
  500km/day -> km/h
  1mrad -> °
  52weeks -> days
  5in + 2ft -> cm
  atan(30cm / 2m) -> degree
  6Mbit/s * 1.5h -> GB
  ```

- **Variable assignments**:
  ```
  r = 6000km
  vol = 4/3 * pi * r³
  density = 5g/cm³
  vol * density -> kg
  ```
  ```
  len = 20cm
  2pi*sqrt(len/g0) -> ms
  ```
  * **Predefined constants** (type `list` to see them all): speed of light (`c`),
  Planck's constant (`hbar`), electron mass (`electronMass`), elementary charge
  (`elementaryCharge`), magnetic constant (`µ0`), electric constant (`eps0`),
  Bohr magneton (`µ_B`), Avogadro's constant (`N_A`), Boltzmann constant
  (`k_B`), gravitational acceleration (`g0`), ...

  * **Last result**: you can use `ans` (answer) to refer to the result of the last calculation.

- **Unicode support**:
  ```
  λ = 2 × 300µm
  ν = c/λ → GHz
  ```

- **And more**: tab completion, command history (arrow keys, `Ctrl`+`R`), pretty printing, syntax highlighting, ...

Reference
---------
- Operations

  | Operator       | Syntax          |
  | -------------- | --------------- |
  | addition       | `+`             |
  | subtraction    | `-`             |
  | multiplication | `*`, `·`, `×`   |
  | division       | `/`, `÷`        |
  | exponentiation | `^`, `**`       |
  | square         | `²`             |
  | cube           | `³`             |
  | conversion     | `->`, `→`, `to` |
  | assignment     | `=`             |

- Commands

  | Command           | Syntax             |
  | ----------------- | ------------------ |
  | help text         | `help`, `?`        |
  | list of variables | `list`, `ls`, `ll` |
  | reset environment | `reset`            |
  | clear screen      | `clear`, `cls`     |
  | quit (CLI)        | `quit`, `exit`     |

- Units

  `A`, `ampere`, `B`, `becquerel`, `bit`, `bits`, `bps`, `Bq`, `byte`, `Byte`, `bytes`, `Bytes`, `C`, `candela`, `cd`, `coulomb`, `d`, `day`, `days`, `deg`, `degree`, `degrees`, `electronvolt`, `eV`, `F`, `farad`, `feet`, `foot`, `ft`, `gallon`, `gal`, `g`, `gram`, `grams`, `gray`, `Gy`, `H`, `h`, `ha`, `hectare`, `henry`, `hertz`, `hour`, `hours`, `Hz`, `in`, `inch`, `inches`, `J`, `joule`, `joules`, `K`, `kat`, `katal`, `kelvin`, `L`, `lb`, `liter`, `liters`, `lm`, `lumen`, `lux`, `lx`, `m`, `meter`, `meters`, `mile`, `miles`, `min`, `minute`, `minutes`, `mol`, `mole`, `month`, `months`, `mph`, `N`, `newton`, `ohm`, `ounce`, `ounces`, `oz`, `Pa`, `pascal`, `pound`, `pounds`, `rad`, `radian`, `radians`, `S`, `s`, `sec`, `second`, `seconds`, `siemens`, `sievert`, `Sv`, `T`, `t`, `tesla`, `ton`, `tonne`, `tonnes`, `tons`, `V`, `volt`, `W`, `w`, `watt`, `watts`, `Wb`, `weber`, `week`, `weeks`, `yard`, `yards`, `yd`, `year`, `years`, `°`, `Ω`.

Install
-------
In addition to the web interface, there is also a command line version which can by installed via [npm](https://www.npmjs.com/package/insect):
```sh
npm install -g insect
```

Build
-----
```sh
bower install
npm install
pulp -w browserify --skip-entry-point -m Insect --standalone Insect -O -t insect.js
```
