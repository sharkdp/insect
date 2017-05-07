![insect](media/insect.png "insect - scientific calculator")

A high precision scientific calculator with full support for physical units.

* [**Web version**](https://insect.sh/)
* [Terminal version](#install-terminal-version)

Documentation
-------------
- **Evaluate mathematical expressions**:
  ```
  1920/16*9
  2^32
  sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2
  ```
  * **Operators**: addition (`+`), subtraction (`-`),
    multiplication (`*`, `·`, `×`), division (`/`, `÷`, `per`),
    exponentiation (`^`, `**`). Full list: see [*Reference*](#reference) below.

  * **Mathematical functions**: `acos`, `acosh`, `asin`, `asinh`, `atan`,
    `atanh`, `ceil`, `cos`, `cosh`, `exp`, `floor`, `gamma`, `ln`, `log`,
    `log10`, `round`, `sin`, `sinh`, `sqrt`, `tan`, `tanh`.

  * **Arbitrary-precision numeric type** that can handle *very* large (or
    small) exponents like *10^(10^10)*.

  * **Exponential notation**: `6.022e23`.

- **Physical units**: parsing and handling, including metric prefixes:
  ```
  2min + 30s
  40kg * 9.8m/s^2 * 150cm
  sin(30°)
  ```
  * **Supported units**: see [*Reference*](#reference) section below.

  * **Implicit conversions**: `15km/h * 30min` evaluates to `7.5km`.

  * **Useful error messages**:
    ```
    > 2 watts + 4 newton meter

    Unification error:
      Cannot unify unit N·m (base units: kg·m²·s⁻²)
              with unit W (base units: kg·m²·s⁻³)
    ```

- **Explicit unit conversions**: the `->` conversion operator (aliases: `→`, `to`):
  ```
  60mph -> m/s
  500km/day -> km/h
  1mrad -> degree
  52weeks -> days
  5in + 2ft -> cm
  atan(30cm / 2m) -> degree
  6Mbit/s * 1.5h -> GB
  ```

- **Variable assignments**:
  ```
  r = 6000km
  vol = 4/3 * pi * r^3
  density = 5g/cm^3
  vol * density -> kg
  ```
  ```
  len = 20cm
  2pi*sqrt(len/g0) -> ms
  ```
  * **Predefined constants** (type `list` to see them all): speed of light (`c`),
  Planck's constant (`h_bar`), electron mass (`electronMass`), elementary charge
  (`elementaryCharge`), magnetic constant (`µ0`), electric constant (`eps0`),
  Bohr magneton (`µ_B`), Avogadro's constant (`N_A`), Boltzmann constant
  (`k_B`), gravitational acceleration (`g0`), ...

  * **Last result**: you can use `ans` (answer) to refer to the result of the
    last calculation.

- **Unicode support**:
  ```
  λ = 2 × 300µm
  ν = c/λ → GHz
  ```

- **And more**: tab completion, command history (arrow keys, `Ctrl`+`R`), pretty printing, syntax highlighting, ...

Reference
---------
- Operators (ordered by precedence: high to low)

  | Operator                  | Syntax          |
  | ------------------------- | --------------- |
  | factorial                 | `!`             |
  | square/cube               | `²`/`³`         |
  | exponentiation            | `^`, `**`       |
  | multiplication (implicit) | *whitespace*    |
  | division                  | `/`, `÷`, `per` |
  | multiplication (explicit) | `*`, `·`, `×`   |
  | subtraction               | `-`             |
  | addition                  | `+`             |
  | unit conversion           | `->`, `→`, `to` |
  | assignment                | `=`             |

  Note that *implicit* multiplication has a higher precedence than division, i.e. `50cm/2m` will be parsed as `50cm/(2m)`.

- Commands

  | Command           | Syntax             |
  | ----------------- | ------------------ |
  | help text         | `help`, `?`        |
  | list of variables | `list`, `ls`, `ll` |
  | reset environment | `reset`            |
  | clear screen      | `clear`, `cls`     |
  | quit (CLI)        | `quit`, `exit`     |

- Units (remember that you can use tab completion):

  `A`, `ampere`, `B`, `becquerel`, `bit`, `bits`, `bps`, `Bq`, `byte`, `Byte`, `bytes`, `Bytes`, `C`, `candela`, `cd`, `coulomb`, `cup`, `cups`, `d`, `day`, `days`, `deg`, `degree`, `degrees`, `electronvolt`, `eV`, `F`, `farad`, `feet`, `foot`, `ft`, `gallon`, `gallons`, `gal`, `g`, `gram`, `grams`, `gray`, `Gy`, `H`, `h`, `ha`, `hectare`, `henry`, `hertz`, `hour`, `hours`, `Hz`, `in`, `inch`, `inches`, `J`, `joule`, `joules`, `K`, `kat`, `katal`, `kelvin`, `L`, `lb`, `liter`, `liters`, `lm`, `lumen`, `lux`, `lx`, `m`, `meter`, `meters`, `mile`, `miles`, `min`, `minute`, `minutes`, `mol`, `mole`, `month`, `months`, `mph`, `N`, `newton`, `ohm`, `ounce`, `ounces`, `oz`, `Pa`, `pascal`, `pint`, `pints`, `pound`, `pounds`, `rad`, `radian`, `radians`, `S`, `s`, `sec`, `second`, `seconds`, `siemens`, `sievert`, `Sv`, `T`, `t`, `tesla`, `tablespoon`, `tablespoons`, `tbsp`, `teaspoon`, `teaspoons`, `tsp`, `ton`, `tonne`, `tonnes`, `tons`, `V`, `volt`, `W`, `watt`, `watts`, `Wb`, `weber`, `week`, `weeks`, `yard`, `yards`, `yd`, `year`, `years`, `°`, `Ω`.

Install terminal version
------------------------
In addition to the web interface, there is also a command line version which can by installed via [npm](https://www.npmjs.com/package/insect):
```sh
npm install -g insect
```

For Arch Linux, there is a [package on AUR](https://aur.archlinux.org/packages/insect/):
```sh
yaourt -S insect
```

Development
-----------
Insect is written in PureScript (see [Getting Started](http://www.purescript.org/learn/getting-started/) guide). You can install all dependencies and build the whole project by running:
```
bower install
npm install
pulp -w browserify --skip-entry-point -m Insect --standalone Insect -O -t insect.js
```

Insect comes with a comprehensive set of [unit tests](test/Main.purs). You can run them by calling
```
pulp test
```
