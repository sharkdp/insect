<!--

    --- IMPORTANT --- IMPORTANT --- IMPORTANT ---

     Do not edit this README file directly. Edit
     the files in the `docs` folder instead.

    --- IMPORTANT --- IMPORTANT --- IMPORTANT ---

-->

![insect](web/media/insect.png "insect - scientific calculator")

A high-precision scientific calculator with full support for physical units.

**Try the web version here**: https://insect.sh

Contents
--------

- [Documentation](#documentation)
- [Reference](#reference)
- [Pros and cons](#pros-and-cons)
- [FAQ](#faq)
- [Terminal version](#terminal-version)
- [Development](#development)
- [Maintainers](#maintainers)

## Documentation

- **Evaluate mathematical expressions**:

      1920/16*9
      2^32
      sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2

  - **Operators**: addition (`+`), subtraction (`-`), multiplication
    (`*`, `·`, `×`), division (`/`, `÷`, `per`), exponentiation (`^`,
    `**`). Full list: see [*Reference*](#reference) below.

  - **Mathematical functions**: `abs`, `acos`, `acosh`,
    `acot`/`arccotangent`, `acoth`/`archypcotangent`,
    `acsc`/`arccosecant`, `acsch`/`archypcosecant`, `arcsecant`,
    `asech`/`archypsecant`, `asin`, `asinh`, `atan2`, `atan`, `atanh`,
    `ceil`, `cos`, `cosh`, `cot`/`cotangent`, `coth`/`hypcotangent`,
    `csc`/`cosecant`, `csch`/`hypcosecant`, `exp`, `floor`,
    `fromCelsius`, `fromFahrenheit`, `gamma`, `ln`, `log10`, `log`,
    `maximum`, `mean`, `minimum`, `round`, `secant`, `sech`/`hypsecant`,
    `sin`, `sinh`, `sqrt`, `tan`, `tanh`, `toCelsius`, `toFahrenheit`.

  - **High-precision numeric type** with *30* significant digits that
    can handle *very* large (or small) exponents like *10^(10^10)*.

  - **Exponential notation**: `6.022e23`.

  - **Hexadecimal, octal and binary number input**:

        0xFFFF
        0b1011
        0o32
        0x2.F
        0o5p3

- **Physical units**: parsing and handling, including metric prefixes:

      2 min + 30 s
      40 kg * 9.8 m/s^2 * 150 cm
      sin(30°)

  - **Supported units**: see [*Reference*](#reference) section below.

  - **Implicit conversions**: `15 km/h * 30 min` evaluates to `7.5 km`.

  - **Useful error messages**:

        > 2 watts + 4 newton meter

        Conversion error:
          Cannot convert unit N·m (base units: kg·m²·s⁻²)
                      to unit W (base units: kg·m²·s⁻³)

- **Explicit unit conversions**: the `->` conversion operator (aliases:
  `→`, `➞`, `to`):

      60 mph -> m/s
      500 km/day -> km/h
      1 mrad -> degree
      52 weeks -> days
      5 in + 2 ft -> cm
      atan(30 cm / 2 m) -> degree
      6 Mbit/s * 1.5 h -> GB

- **Variable assignments**:

  Example: mass of the earth

      r = 6000km
      vol = 4/3 * pi * r^3
      density = 5 g/cm^3
      vol * density -> kg

  Example: oscillation period of a pendulum

      len = 20 cm
      2pi*sqrt(len/g0) -> ms

  - **Predefined constants** (type `list` to see them all): speed of
    light (`c`), Planck's constant (`h_bar`), electron mass
    (`electronMass`), elementary charge (`elementaryCharge`), magnetic
    constant (`µ0`), electric constant (`eps0`), Bohr magneton (`µ_B`),
    Avogadro's constant (`N_A`), Boltzmann constant (`k_B`),
    gravitational acceleration (`g0`), ideal gas constant (`R`), ...

  - **Last result**: you can use `ans` (answer) or `_` to refer to the
    result of the last calculation.

- **User-defined functions**:

  Example: kinetic energy

      kineticEnergy(mass, speed) = 0.5 * mass * speed^2 -> kJ

      kineticEnergy(800 kg, 120 km/h)

  Example: barometric formula

      P0 = 1 atm
      T0 = fromCelsius(15)
      tempGradient = 0.65 K / 100 m

      pressure(height) = P0 * (1 - tempGradient * height / T0)^5.255 -> hPa

      pressure(1500 m)

- **Sums and products**:

  Syntax:

      sum(<expression>, <index-variable>, <from>, <to>)
      product(<expression>, <index-variable>, <from>, <to>)

  Examples:

      # sum of the first ten squares
      sum(k^2, k, 1, 10)

      # the factorial of n as the product 1 × 2 × ... × n
      myFactorial(n) = product(k, k, 1, n)

- **Unicode support**:

      λ = 2 × 300 µm
      ν = c/λ → GHz

- **And more**: tab completion, command history (arrow keys,
  `Ctrl`+`R`), pretty printing, syntax highlighting, ...

## Reference

- Operators (ordered by precedence: high to low)

  | Operator                  | Syntax               |
  |---------------------------|----------------------|
  | factorial                 | `!`                  |
  | square, cube, ...         | `²`, `³`, `⁻¹`, ...  |
  | exponentiation            | `^`, `**`            |
  | multiplication (implicit) | *whitespace*         |
  | modulo                    | `%`                  |
  | division                  | `per`                |
  | division                  | `/`, `÷`             |
  | multiplication (explicit) | `*`, `·`, `×`        |
  | subtraction               | `-`                  |
  | addition                  | `+`                  |
  | unit conversion           | `->`, `→`, `➞`, `to` |
  | assignment                | `=`                  |

  Note that *implicit* multiplication has a higher precedence than
  division, i.e. `50 cm / 2 m` will be parsed as `50 cm / (2 m)`.

- Commands

  | Command                  | Syntax             |
  |--------------------------|--------------------|
  | help text                | `help`, `?`        |
  | list of variables        | `list`, `ls`, `ll` |
  | reset environment        | `reset`            |
  | clear screen             | `clear`, `cls`     |
  | copy result to clipboard | `copy`, `cp`       |
  | quit (CLI)               | `quit`, `exit`     |

- Supported units (remember that you can use tab completion).

  All SI-accepted units support metric prefixes and [binary
  prefixes](https://en.wikipedia.org/wiki/Binary_prefix) (`MiB`, `GiB`,
  ...).

  | Unit                                                                         | Syntax                                                                      |
  |------------------------------------------------------------------------------|-----------------------------------------------------------------------------|
  | [Ampere](https://en.wikipedia.org/wiki/Ampere)                               | `amperes`, `ampere`, `A`                                                    |
  | [Ångström](https://en.wikipedia.org/wiki/Ångström)                           | `angstroms`, `angstrom`, `Å`                                                |
  | [Astronomical unit](https://en.wikipedia.org/wiki/Astronomical_unit)         | `AU`, `au`, `astronomicalunits`, `astronomicalunit`                         |
  | [Atmosphere](https://en.wikipedia.org/wiki/Atmosphere_(unit))                | `atm`                                                                       |
  | [Bar](https://en.wikipedia.org/wiki/Bar_(unit))                              | `bars`, `bar`                                                               |
  | [Barn](https://en.wikipedia.org/wiki/Barn_(unit))                            | `barns`, `barn`                                                             |
  | [Becquerel](https://en.wikipedia.org/wiki/Becquerel)                         | `becquerels`, `becquerel`, `Bq`                                             |
  | [Bel](https://en.wikipedia.org/wiki/Decibel)                                 | `bels`, `bel`                                                               |
  | [Bit](https://en.wikipedia.org/wiki/Bit)                                     | `bits`, `bit`                                                               |
  | [Bits per second](https://en.wikipedia.org/wiki/Data_rate_units)             | `bps`                                                                       |
  | [British thermal unit](https://en.wikipedia.org/wiki/British_thermal_unit)   | `BTU`                                                                       |
  | [Byte](https://en.wikipedia.org/wiki/Byte)                                   | `Bytes`, `bytes`, `Byte`, `byte`, `B`, `Octets`, `octets`, `Octet`, `octet` |
  | [Calorie](https://en.wikipedia.org/wiki/Calorie)                             | `calories`, `calorie`, `cal`                                                |
  | [Candela](https://en.wikipedia.org/wiki/Candela)                             | `candelas`, `candela`, `cd`                                                 |
  | [Coulomb](https://en.wikipedia.org/wiki/Coulomb)                             | `coulombs`, `coulomb`, `C`                                                  |
  | [Cup](https://en.wikipedia.org/wiki/Cup_(unit))                              | `cups`, `cup`                                                               |
  | [DPI](https://en.wikipedia.org/wiki/Dots_per_inch)                           | `dpi`                                                                       |
  | [Day](https://en.wikipedia.org/wiki/Day)                                     | `days`, `day`, `d`                                                          |
  | [Degree](https://en.wikipedia.org/wiki/Degree_(angle))                       | `degrees`, `degree`, `deg`, `°`                                             |
  | [Dot](https://en.wikipedia.org/wiki/Dots_per_inch)                           | `dots`, `dot`                                                               |
  | [Electronvolt](https://en.wikipedia.org/wiki/Electronvolt)                   | `electronvolts`, `electronvolt`, `eV`                                       |
  | [Euro](https://en.wikipedia.org/wiki/Euro)                                   | `euros`, `euro`, `EUR`, `€`                                                 |
  | [Farad](https://en.wikipedia.org/wiki/Farad)                                 | `farads`, `farad`, `F`                                                      |
  | [Fluid ounce](https://en.wikipedia.org/wiki/Fluid_ounce)                     | `fluidounces`, `fluidounce`, `floz`                                         |
  | [Foot](https://en.wikipedia.org/wiki/Foot_(unit))                            | `feet`, `foot`, `ft`                                                        |
  | [Fortnight](https://en.wikipedia.org/wiki/Fortnight)                         | `fortnights`, `fortnight`                                                   |
  | [Frame](https://en.wikipedia.org/wiki/Film_frame)                            | `frames`, `frame`                                                           |
  | [Frames per second](https://en.wikipedia.org/wiki/Frame_rate)                | `fps`                                                                       |
  | [Furlong](https://en.wikipedia.org/wiki/Furlong)                             | `furlongs`, `furlong`                                                       |
  | [Gallon](https://en.wikipedia.org/wiki/Gallon)                               | `gallons`, `gallon`, `gal`                                                  |
  | [Gauss](https://en.wikipedia.org/wiki/Gauss_(unit))                          | `gauss`                                                                     |
  | [Gram](https://en.wikipedia.org/wiki/Gram)                                   | `grams`, `gram`, `grammes`, `gramme`, `g`                                   |
  | [Gray](https://en.wikipedia.org/wiki/Gray_(unit))                            | `grays`, `gray`, `Gy`                                                       |
  | [Hectare](https://en.wikipedia.org/wiki/Hectare)                             | `hectares`, `hectare`, `ha`                                                 |
  | [Henry](https://en.wikipedia.org/wiki/Henry_(unit))                          | `henrys`, `henries`, `henry`, `H`                                           |
  | [Hertz](https://en.wikipedia.org/wiki/Hertz)                                 | `hertz`, `Hz`                                                               |
  | [Hogshead](https://en.wikipedia.org/wiki/Hogshead)                           | `hogsheads`, `hogshead`                                                     |
  | [Hour](https://en.wikipedia.org/wiki/Hour)                                   | `hours`, `hour`, `hr`, `h`                                                  |
  | [Inch](https://en.wikipedia.org/wiki/Inch)                                   | `inches`, `inch`, `in`                                                      |
  | [Joule](https://en.wikipedia.org/wiki/Joule)                                 | `joules`, `joule`, `J`                                                      |
  | [Katal](https://en.wikipedia.org/wiki/Katal)                                 | `katals`, `katal`, `kat`                                                    |
  | [Knot](https://en.wikipedia.org/wiki/Knot_(unit))                            | `knots`, `knot`, `kn`, `kt`                                                 |
  | [Kelvin](https://en.wikipedia.org/wiki/Kelvin)                               | `kelvins`, `kelvin`, `K`                                                    |
  | [Light-year](https://en.wikipedia.org/wiki/Light-year)                       | `lightyears`, `lightyear`, `ly`                                             |
  | [Liter](https://en.wikipedia.org/wiki/Liter)                                 | `liters`, `liter`, `litres`, `litre`, `L`, `l`                              |
  | [Lumen](https://en.wikipedia.org/wiki/Lumen_(unit))                          | `lumens`, `lumen`, `lm`                                                     |
  | [Lux](https://en.wikipedia.org/wiki/Lux)                                     | `lux`, `lx`                                                                 |
  | [Meter](https://en.wikipedia.org/wiki/Meter)                                 | `meters`, `meter`, `metres`, `metre`, `m`                                   |
  | [Mile](https://en.wikipedia.org/wiki/Mile)                                   | `miles`, `mile`                                                             |
  | [Miles per hour](https://en.wikipedia.org/wiki/Miles_per_hour)               | `mph`                                                                       |
  | [Millimeter of mercury](https://en.wikipedia.org/wiki/Millimeter_of_mercury) | `mmHg`                                                                      |
  | [Minute](https://en.wikipedia.org/wiki/Minute)                               | `minutes`, `minute`, `min`                                                  |
  | [Molal](https://en.wikipedia.org/wiki/Molality#Unit)                         | `molals`, `molal`                                                           |
  | [Molar](https://en.wikipedia.org/wiki/Molar_concentration#Units)             | `molars`, `molar`                                                           |
  | [Mole](https://en.wikipedia.org/wiki/Mole_(unit))                            | `moles`, `mole`, `mol`                                                      |
  | [Month](https://en.wikipedia.org/wiki/Month)                                 | `months`, `month`                                                           |
  | [Nautical mile](https://en.wikipedia.org/wiki/Nautical_mile)                 | `M`, `NM`, `nmi`                                                            |
  | [Newton](https://en.wikipedia.org/wiki/Newton_(unit))                        | `newtons`, `newton`, `N`                                                    |
  | [Ohm](https://en.wikipedia.org/wiki/Ohm)                                     | `ohms`, `ohm`, `Ω`                                                          |
  | [Ounce](https://en.wikipedia.org/wiki/Ounce)                                 | `ounces`, `ounce`, `oz`                                                     |
  | [PPI](https://en.wikipedia.org/wiki/Pixels_per_inch)                         | `ppi`                                                                       |
  | [Parsec](https://en.wikipedia.org/wiki/Parsec)                               | `parsecs`, `parsec`, `pc`                                                   |
  | [Parts-per-million](https://en.wikipedia.org/wiki/Parts-per_notation)        | `ppm`                                                                       |
  | [Parts-per-billion](https://en.wikipedia.org/wiki/Parts-per_notation)        | `ppb`                                                                       |
  | [Parts-per-trillion](https://en.wikipedia.org/wiki/Parts-per_notation)       | `ppt`                                                                       |
  | [Parts-per-quadrillion](https://en.wikipedia.org/wiki/Parts-per_notation)    | `ppq`                                                                       |
  | [Pascal](https://en.wikipedia.org/wiki/Pascal_(unit))                        | `pascals`, `pascal`, `Pa`                                                   |
  | [Percent](https://en.wikipedia.org/wiki/Parts-per_notation)                  | `percent`, `pct`                                                            |
  | [Person](https://en.wiktionary.org/wiki/person)                              | `persons`, `person`, `people`                                               |
  | [Piece](https://en.wiktionary.org/wiki/piece)                                | `pieces`, `piece`                                                           |
  | [Pint](https://en.wikipedia.org/wiki/Pint)                                   | `pints`, `pint`                                                             |
  | [Pixel](https://en.wikipedia.org/wiki/Pixel)                                 | `pixels`, `pixel`, `px`                                                     |
  | [Pound-force](https://en.wikipedia.org/wiki/Pound_%28force%29)               | `pound_force`, `lbf`                                                        |
  | [Pound](https://en.wikipedia.org/wiki/Pound_(mass))                          | `pounds`, `pound`, `lb`                                                     |
  | [Psi](https://en.wikipedia.org/wiki/Pounds_per_square_inch)                  | `psi`                                                                       |
  | [RPM](https://en.wikipedia.org/wiki/RPM)                                     | `RPM`, `rpm`                                                                |
  | [Radian](https://en.wikipedia.org/wiki/Radian)                               | `radians`, `radian`, `rad`                                                  |
  | [Rod](https://en.wikipedia.org/wiki/Rod_(unit))                              | `rods`, `rod`                                                               |
  | [Second](https://en.wikipedia.org/wiki/Second)                               | `seconds`, `second`, `sec`, `s`                                             |
  | [Siemens](https://en.wikipedia.org/wiki/Siemens_(unit))                      | `siemens`, `S`                                                              |
  | [Sievert](https://en.wikipedia.org/wiki/Sievert)                             | `sieverts`, `sievert`, `Sv`                                                 |
  | [Tablespoon](https://en.wikipedia.org/wiki/Tablespoon)                       | `tablespoons`, `tablespoon`, `tbsp`                                         |
  | [Teaspoon](https://en.wikipedia.org/wiki/Teaspoon)                           | `teaspoons`, `teaspoon`, `tsp`                                              |
  | [Tesla](https://en.wikipedia.org/wiki/Tesla_(unit))                          | `teslas`, `tesla`, `T`                                                      |
  | [Thou](https://en.wikipedia.org/wiki/Thousandth_of_an_inch)                  | `thou`, `mils`, `mil`                                                       |
  | [Tonne](https://en.wikipedia.org/wiki/Tonne)                                 | `tonnes`, `tonne`, `tons`, `ton`, `t`                                       |
  | [US Dollar](https://en.wikipedia.org/wiki/USD)                               | `dollars`, `dollar`, `USD`, `$`                                             |
  | [Volt](https://en.wikipedia.org/wiki/Volt)                                   | `volts`, `volt`, `V`                                                        |
  | [Watt-hour](https://en.wikipedia.org/wiki/Kilowatt_hour)                     | `Wh`                                                                        |
  | [Watt](https://en.wikipedia.org/wiki/Watt)                                   | `watts`, `watt`, `W`                                                        |
  | [Weber](https://en.wikipedia.org/wiki/Weber_(unit))                          | `webers`, `weber`, `Wb`                                                     |
  | [Week](https://en.wikipedia.org/wiki/Week)                                   | `weeks`, `week`                                                             |
  | [Yard](https://en.wikipedia.org/wiki/Yard)                                   | `yards`, `yard`, `yd`                                                       |
  | [Gregorian year](https://en.wikipedia.org/wiki/Gregorian_year)               | `years`, `year`                                                             |
  | [Julian year](https://en.wikipedia.org/wiki/Julian_year_(astronomy))         | `julianYears`, `julianYear`                                                 |

## Pros and cons

**Reasons to use Insect**

- Insect is **open-source**.
- There is a [web version](https://Insect.sh/) that requires **no
  installation**.
- With both browser and terminal versions available, Insect is truly
  **cross-platform**.
- Insect has *first-class* support for **physical units**, including
  metric and binary prefixes. While evaluating your calculation, Insect
  ensures that you did not accidentally make any mistakes in combining
  the physical quantities.
- Insect supports an
  [**interactive**](https://en.wikipedia.org/wiki/REPL) style with its
  readline-like interface. There is a saved history that can be browsed
  by pressing the up and down arrow keys. The history is also searchable
  via *Ctrl-R*.
- Insect's syntax is rather strict. The parser does not try to be
  "smart" on syntactically incorrect input, so there shouldn't be any
  surprises - and you can trust the result of your calculation. The
  parsed user input is always pretty-printed for a quick double-check.
- Insect is written in [PureScript](http://www.purescript.org/) and
  therefore benefits from all the safety guarantees that a strictly
  typed functional programming language gives you.
- The source code of
  [purescript-quantities](https://github.com/sharkdp/purescript-quantities)
  (the underlying library for physical units) as well as the code of
  Insect itself is **extensively tested**.

**Reasons to choose an alternative**

- Insect is a scientific calculator. It's not a computer algebra system
  that solves differential equations or computes integrals. Try
  *[WolframAlpha](http://www.wolframalpha.com/)* instead.
- There is no graphical user interface with buttons for each action
  (*x²*, *1/x*, *DEG/RAD*, etc.).
  *[Qalculate!](http://qalculate.github.io/)* is a fantastic tool that
  supports both text as well as graphical input.
- Insect supports a huge range of physical units: all [SI
  units](https://en.wikipedia.org/wiki/International_System_of_Units),
  all non-SI units that are accepted by SI as well as most units of the
  imperial and US customary systems (and many more). However, if you
  need something even more comprehensive, try *[GNU
  units](https://www.gnu.org/software/units/)*.
- Insect is not a general-purpose programming language. You could try
  *[Frink](https://frinklang.org/)*.
- Insect does not have a special mode for hexadecimal, octal, or binary
  numbers (yet), though it does support inputting them.

## FAQ

- Why are Celsius and Fahrenheit not supported?

  In contrast to the SI unit of temperature, the
  [Kelvin](https://en.wikipedia.org/wiki/Kelvin), and to all other
  units, Celsius and Fahrenheit both require an additive offset when
  converting into and from other temperature units. This additive offset
  leads to all kinds of ambiguities when performing calculations in
  these units. Adding two temperatures in Celsius, for example, is only
  meaningful if one of them is seen as an offset value (rather than as
  an absolute temperature). Insect is primarily a scientific calculator
  (as opposed to a unit conversion tool) and therefore focuses on
  getting physical calculations right.

  Even though *°C* and *°F* are not supported as built-in units, there
  are helper functions to convert to and from Celsius (and Fahrenheit):

  - `fromCelsius` takes a **scalar value** that represents a temperature
    in Celsius and returns a corresponding **temperature in Kelvin**:

        > fromCelsius(0)

           = 273.15 K

        > k_B * fromCelsius(23) to meV

           = 25.5202 meV

  - `toCelsius` takes a **temperature in Kelvin** and returns a **scalar
    value** that represents the corresponding temperature in Celsius:

        > toCelsius(70 K)

           = -203.15

        > toCelsius(25 meV / k_B)

           = 16.963

- Why is `1/2 x` parsed as `1/(2x)`?

  *Implicit* multiplication (without an explicit multiplication sign)
  has a higher precedence than division (see [operator precedence
  rules](#reference)). This is by design, in order to parse inputs like
  `50 cm / 2 m` as `(50 cm) / (2 m)`. If you meant *½ · x*, write
  `1/2 * x`.

- What is the internal numerical precision?

  By default, Insect shows 6 significant digits in the result of the
  calculation. However, the internal numerical precision is much higher
  (30 digits).

- How does the conversion operator work?

  The conversion operator `->` attempts to convert the physical quantity
  on its left hand side to the *unit of the expression* on its right
  hand side. This means that you can write an arbitrary expression on
  the right hand side (but only the unit part will be extracted). For
  example:

      # simple unit conversion:
      > 120 km/h -> mph

        = 74.5645 mi/h

      # expression on the right hand side:
      > 120 m^3 -> km * m^2

        = 0.12 m²·km

      # convert x1 to the same unit as x2:
      > x1 = 50 km / h
      > x2 = 3 m/s -> x1

        x2 = 10.8 km/h

- What is the relation between the units `RPM`, `rad/s`, `deg/s` and
  `Hz`?

  The unit [`RPM`](https://en.wikipedia.org/wiki/Revolutions_per_minute)
  (revolutions per minute) is defined via `1 RPM = 1 / minute` where the
  `1` on the right hand side symbolizes "1 revolution".

  As the base unit is the same (`1 / second`), `RPM` can be converted to
  `rad / s`, `deg / s` or `Hz`. Note, however, that `1 RPM` does *not*
  equal `2π rad / min` or `360° / min` or `1 Hz`, as some might expect.
  If you're interested in computing the traversed angle of something
  that rotates with a given number of revolutions per minute, you need
  to multiply by `2π rad` or `360°` because:

      1 RPM · (360°/revolution) = (1 revolution / minute) · (360° / revolution) = 360° / minute

## Terminal version

In addition to the web interface, there is also a command-line version
(supporting Node.js 10 and later) which can by installed via
[npm](https://www.npmjs.com/package/insect):

    npm install -g insect

Note that you should almost always never run this as root or with
`sudo`. If the command fails due to permission issues, [set up a prefix
directory](https://github.com/sindresorhus/guides/blob/master/npm-global-without-sudo.md#install-npm-packages-globally-without-sudo-on-macos-and-linux)
and call `npm install` as a user instead.

For Arch Linux, there is an [AUR
package](https://aur.archlinux.org/packages/insect/):

    yaourt -S insect

For macOS, there is a [Homebrew
formula](https://formulae.brew.sh/formula/insect):

    brew install insect

For Android, install [Termux](https://termux.com/) from
[F-Droid](https://f-droid.org/packages/com.termux/). Install Node.js in
Termux and then install `insect` from npm:

    pkg install nodejs-lts
    npm install -g insect

## Development

Insect is written in PureScript (see the [Getting
Started](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md)
guide). First, install all dependencies:

    npm install

To start the web version:

    npm start

To build a bundled JavaScript file that you can run from the terminal
(note that this builds the web version too):

    npm run build

To run the `index.cjs` file which the previous command creates:

    node index.cjs
    # Or simply on Un*x
    ./index.cjs

Note that it's not possible to just move this file anywhere and then run
it there, since it depends on packages in `node_modules`.

Insect comes with a comprehensive set of [unit tests](test/Main.purs).
To run them:

    npm test

Note that Node.js 12 or above is required to work on/build Insect
(despite Insect itself requiring only Node.js 10 or later to run). If
you don't have or want to install Node.js 12 or later, you can use the
following Dockerfile to build or run Insect on Node.js 18:

``` Dockerfile
FROM node:18

WORKDIR /usr/src/insect

COPY . .

RUN npm install && \
    npm run build

CMD ["node", "index.cjs"]
```

After creating the image (`docker build -t sharkdp/insect .`), you can
create the container and copy out the build artifacts:

    docker create sharkdp/insect:latest
    # copy SHA (e.g. 71f0797703e8)
    docker cp 71f0797703e8:/usr/src/insect/index.cjs .
    docker cp -r 71f0797703e8:/usr/src/insect/node_modules .

To directly run Insect inside Docker (paying a heavy startup time
penalty), you can use:

    docker run -it --rm -v ~/.local/share/insect-history:/root/.local/share/insect-history sharkdp/insect:latest

## Maintainers

- [sharkdp](https://github.com/sharkdp)
- [mhmdanas](https://github.com/mhmdanas)
