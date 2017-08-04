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
    `atanh`, `ceil`, `cos`, `cosh`, `exp`, `floor`, `fromCelsius`,
    `fromFahrenheit`, `gamma`, `ln`, `log`, `log10`, `round`, `sin`, `sinh`,
    `sqrt`, `tan`, `tanh`, `toCelsius`, `toFahrenheit`.

  * **High-precision numeric type** with *30* significant digits that can handle
    *very* large (or small) exponents like *10^(10^10)*.

  * **Exponential notation**: `6.022e23`.

- **Physical units**: parsing and handling, including metric prefixes:
  ```
  2 min + 30 s
  40 kg * 9.8 m/s^2 * 150 cm
  sin(30°)
  ```
  * **Supported units**: see [*Reference*](#reference) section below.

  * **Implicit conversions**: `15 km/h * 30 min` evaluates to `7.5 km`.

  * **Useful error messages**:
    ```
    > 2 watts + 4 newton meter

    Unification error:
      Cannot convert unit N·m (base units: kg·m²·s⁻²)
                  to unit W (base units: kg·m²·s⁻³)
    ```

- **Explicit unit conversions**: the `->` conversion operator (aliases: `→`, `➞`, `to`):
  ```
  60 mph -> m/s
  500 km/day -> km/h
  1 mrad -> degree
  52 weeks -> days
  5 in + 2 ft -> cm
  atan(30 cm / 2 m) -> degree
  6 Mbit/s * 1.5 h -> GB
  ```

- **Variable assignments**:
  ```
  r = 6000km
  vol = 4/3 * pi * r^3
  density = 5 g/cm^3
  vol * density -> kg
  ```
  ```
  len = 20 cm
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
  λ = 2 × 300 µm
  ν = c/λ → GHz
  ```

- **And more**: tab completion, command history (arrow keys, `Ctrl`+`R`), pretty printing, syntax highlighting, ...

Reference
---------
- Operators (ordered by precedence: high to low)

  | Operator                  | Syntax               |
  | ------------------------- | -------------------- |
  | factorial                 | `!`                  |
  | square, cube, ...         | `²`, `³`, `⁻¹`, ...  |
  | exponentiation            | `^`, `**`            |
  | multiplication (implicit) | *whitespace*         |
  | division                  | `/`, `÷`, `per`      |
  | multiplication (explicit) | `*`, `·`, `×`        |
  | subtraction               | `-`                  |
  | addition                  | `+`                  |
  | unit conversion           | `->`, `→`, `➞`, `to` |
  | assignment                | `=`                  |

  Note that *implicit* multiplication has a higher precedence than division, i.e. `50 cm / 2 m` will be parsed as `50 cm / (2 m)`.

- Commands

  | Command           | Syntax             |
  | ----------------- | ------------------ |
  | help text         | `help`, `?`        |
  | list of variables | `list`, `ls`, `ll` |
  | reset environment | `reset`            |
  | clear screen      | `clear`, `cls`     |
  | quit (CLI)        | `quit`, `exit`     |

- Supported units (remember that you can use tab completion).

  All SI-accepted units support metric prefixes.
  In addition, [binary prefixes](https://en.wikipedia.org/wiki/Binary_prefix) (`MiB`, `GiB`, ...) are also supported.

  | Unit | Syntax |
  | ---- | ------ |
  | [Ampere](https://en.wikipedia.org/wiki/Ampere) | `amperes`, `ampere`, `A` |
  | [Ångström](https://en.wikipedia.org/wiki/Ångström) | `angstrom`, `Å` |
  | [Astronomical unit](https://en.wikipedia.org/wiki/Astronomical_unit) | `AU` |
  | [Atmosphere](https://en.wikipedia.org/wiki/Atmosphere_(unit)) | `atm` |
  | [Bar](https://en.wikipedia.org/wiki/Bar_(unit)) | `bar` |
  | [Barn](https://en.wikipedia.org/wiki/Barn_(unit)) | `barn` |
  | [Becquerel](https://en.wikipedia.org/wiki/Becquerel) | `becquerel`, `Bq` |
  | [Bel](https://en.wikipedia.org/wiki/Decibel) | `bel` |
  | [Bits per second](https://en.wikipedia.org/wiki/Data_rate_units) | `bps` |
  | [Bit](https://en.wikipedia.org/wiki/Bit) | `bits`, `bit` |
  | [British thermal unit](https://en.wikipedia.org/wiki/British_thermal_unit) | `BTU` |
  | [Byte](https://en.wikipedia.org/wiki/Byte) | `Bytes`, `bytes`, `Byte`, `byte`, `B`, `Octets`, `octets`, `Octet`, `octet`|
  | [Calorie](https://en.wikipedia.org/wiki/Calorie) | `calories`, `calorie`, `cal` |
  | [Candela](https://en.wikipedia.org/wiki/Candela) | `candela`, `cd` |
  | [Coulomb](https://en.wikipedia.org/wiki/Coulomb) | `coulomb`, `C` |
  | [Cup](https://en.wikipedia.org/wiki/Cup_(unit)) | `cups`, `cup` |
  | [Day](https://en.wikipedia.org/wiki/Day) | `days`, `day`, `d` |
  | [Degree](https://en.wikipedia.org/wiki/Degree_(angle)) | `degrees`, `degree`, `deg`, `°` |
  | [Electronvolt](https://en.wikipedia.org/wiki/Electronvolt) | `electronvolt`, `eV` |
  | [Farad](https://en.wikipedia.org/wiki/Farad) | `farad`, `F` |
  | [Fortnight](https://en.wikipedia.org/wiki/Fortnight) | `fortnights`, `fortnight` |
  | [Fluid ounce](https://en.wikipedia.org/wiki/Fluid_ounce) | `fluidounces`, `fluidounce`, `floz` |
  | [Furlong](https://en.wikipedia.org/wiki/Furlong) | `furlongs`, `furlong` |
  | [Foot](https://en.wikipedia.org/wiki/Foot_(unit)) | `feet`, `foot`, `ft` |
  | [Gallon](https://en.wikipedia.org/wiki/Gallon) | `gallons`, `gallon`, `gal` |
  | [Gauss](https://en.wikipedia.org/wiki/Gauss_(unit)) | `gauss` |
  | [Gram](https://en.wikipedia.org/wiki/Gram) | `grams`, `gram`, `grammes`, `gramme`, `g` |
  | [Gray](https://en.wikipedia.org/wiki/Gray_(unit)) | `gray`, `Gy` |
  | [Hectare](https://en.wikipedia.org/wiki/Hectare) | `hectare`, `ha` |
  | [Henry](https://en.wikipedia.org/wiki/Henry_(unit)) | `henry`, `H` |
  | [Hertz](https://en.wikipedia.org/wiki/Hertz) | `hertz`, `Hz` |
  | [Hour](https://en.wikipedia.org/wiki/Hour) | `hours`, `hour`, `h` |
  | [Inch](https://en.wikipedia.org/wiki/Inch) | `inches`, `inch`, `in` |
  | [Joule](https://en.wikipedia.org/wiki/Joule) | `joules`, `joule`, `J` |
  | [Katal](https://en.wikipedia.org/wiki/Katal) | `katal`, `kat` |
  | [Kelvin](https://en.wikipedia.org/wiki/Kelvin) | `kelvin`, `K` |
  | [Light-year](https://en.wikipedia.org/wiki/Light-year) | `lightyears`, `lightyear`, `ly` |
  | [Liter](https://en.wikipedia.org/wiki/Liter) | `liters`, `liter`, `litres`, `litre`, `L`, `l` |
  | [Lumen](https://en.wikipedia.org/wiki/Lumen_(unit)) | `lumen`, `lm` |
  | [Lux](https://en.wikipedia.org/wiki/Lux) | `lux`, `lx` |
  | [Meter](https://en.wikipedia.org/wiki/Meter) | `meters`, `meter`, `metres`, `metre`, `m` |
  | [Miles per hour](https://en.wikipedia.org/wiki/Miles_per_hour) | `mph` |
  | [Mile](https://en.wikipedia.org/wiki/Mile) | `miles`, `mile` |
  | [Millimeter of mercury](https://en.wikipedia.org/wiki/Millimeter_of_mercury) | `mmHg` |
  | [Minute](https://en.wikipedia.org/wiki/Minute) | `minutes`, `minute`, `min` |
  | [Mole](https://en.wikipedia.org/wiki/Mole_(unit)) | `mole`, `mol` |
  | [Month](https://en.wikipedia.org/wiki/Month) | `months`, `month` |
  | [Newton](https://en.wikipedia.org/wiki/Newton_(unit)) | `newton`, `N` |
  | [Ohm](https://en.wikipedia.org/wiki/Ohm) | `ohms`, `ohm`, `Ω` |
  | [Ounce](https://en.wikipedia.org/wiki/Ounce) | `ounces`, `ounce`, `oz` |
  | [Parsec](https://en.wikipedia.org/wiki/Parsec) | `parsecs`, `parsec`, `pc` |
  | [Pascal](https://en.wikipedia.org/wiki/Pascal_(unit)) | `pascal`, `Pa` |
  | [Pint](https://en.wikipedia.org/wiki/Pint) | `pints`, `pint` |
  | [Pound](https://en.wikipedia.org/wiki/Pound_(mass)) | `pounds`, `pound`, `lb` |
  | [Psi](https://en.wikipedia.org/wiki/Pounds_per_square_inch) | `psi` |
  | [RPM](https://en.wikipedia.org/wiki/RPM) | `RPM`, `rpm` |
  | [Radian](https://en.wikipedia.org/wiki/Radian) | `radians`, `radian`, `rad` |
  | [Second](https://en.wikipedia.org/wiki/Second) | `seconds`, `second`, `sec`, `s` |
  | [Siemens](https://en.wikipedia.org/wiki/Siemens_(unit)) | `siemens`, `S` |
  | [Sievert](https://en.wikipedia.org/wiki/Sievert) | `sievert`, `Sv` |
  | [Tablespoon](https://en.wikipedia.org/wiki/Tablespoon) | `tablespoons`, `tablespoon`, `tbsp` |
  | [Teaspoon](https://en.wikipedia.org/wiki/Teaspoon) | `teaspoons`, `teaspoon`, `tsp` |
  | [Tesla](https://en.wikipedia.org/wiki/Tesla_(unit)) | `tesla`, `T` |
  | [Tonne](https://en.wikipedia.org/wiki/Tonne) | `tonnes`, `tonne`, `tons`, `ton`, `t` |
  | [Volt](https://en.wikipedia.org/wiki/Volt) | `volts`, `volt`, `V` |
  | [Watt](https://en.wikipedia.org/wiki/Watt) | `watts`, `watt`, `W` |
  | [Watt-hour](https://en.wikipedia.org/wiki/Kilowatt_hour) | `Wh` |
  | [Weber](https://en.wikipedia.org/wiki/Weber_(unit)) | `weber`, `Wb` |
  | [Week](https://en.wikipedia.org/wiki/Week) | `weeks`, `week` |
  | [Yard](https://en.wikipedia.org/wiki/Yard) | `yards`, `yard`, `yd` |
  | [Year](https://en.wikipedia.org/wiki/Year) | `years`, `year` |

Install terminal version
------------------------
In addition to the web interface, there is also a command line version which can by installed via [npm](https://www.npmjs.com/package/insect):
```sh
npm install -g insect
```

For Arch Linux, there is a [package on AUR](https://aur.archlinux.org/packages/insect/):
```
yaourt -S insect
```

For Fedora, there is a [copr repository](https://copr.fedorainfracloud.org/coprs/fnux/insect/):
```
sudo dnf copr enable fnux/insect
sudo dnf install insect
```

For macOS, there is a [Homebrew package](http://braumeister.org/formula/insect):
```
brew install insect
```

Development
-----------
[![Build Status](https://api.travis-ci.org/sharkdp/insect.svg?branch=master)](https://travis-ci.org/sharkdp/insect)

Insect is written in PureScript (see [Getting Started](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) guide). You can install all dependencies and build the whole project by running:
```
bower install
npm install
pulp -w browserify --skip-entry-point -m Insect --standalone Insect -O -t insect.js
```

Insect comes with a comprehensive set of [unit tests](test/Main.purs). You can run them by calling
```
pulp test
```
