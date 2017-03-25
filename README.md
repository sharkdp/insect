insect
======

A REPL-style scientific calculator with unit support.

[![insect](media/insect-32x32.png)](https://shark.fish/insect/)

* [**Web version**](https://shark.fish/insect/)
* [Terminal version](#install)

Features
--------
- Evaluation of mathematical expressions:
  ```
  1920/16*9
  2^32
  sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2
  ```
  Supported functions: `acos`, `acosh`, `asin`, `asinh`, `atan`, `atanh`,
  `ceil`, `cos`, `cosh`, `exp`, `floor`, `ln`, `log`, `log10`, `round`, `sin`,
  `sinh`, `sqrt`, `tan`, `tanh`.

- Parsing and handling of physical units:
  ```
  2min + 30s
  40kg * 9.8m/s² * 150cm
  sin(30°)
  ```
  Supported units: `A`, `ampere`, `B`, `becquerel`, `bit`, `bits`, `bps`, `Bq`, `byte`, `Byte`, `bytes`, `Bytes`, `C`, `candela`, `cd`, `coulomb`, `d`, `day`, `days`, `deg`, `degree`, `degrees`, `electronvolt`, `eV`, `F`, `farad`, `feet`, `foot`, `ft`, `g`, `gram`, `grams`, `gray`, `Gy`, `H`, `h`, `ha`, `hectare`, `henry`, `hertz`, `hour`, `hours`, `Hz`, `in`, `inch`, `inches`, `J`, `joule`, `joules`, `K`, `kat`, `katal`, `kelvin`, `L`, `lb`, `liter`, `liters`, `lm`, `lumen`, `lux`, `lx`, `m`, `meter`, `meters`, `mile`, `miles`, `min`, `minute`, `minutes`, `mol`, `mole`, `month`, `months`, `mph`, `N`, `newton`, `ohm`, `ounce`, `ounces`, `oz`, `Pa`, `pascal`, `pound`, `pounds`, `rad`, `radian`, `radians`, `S`, `s`, `sec`, `second`, `seconds`, `siemens`, `sievert`, `Sv`, `T`, `t`, `tesla`, `ton`, `tonne`, `tonnes`, `tons`, `V`, `volt`, `W`, `w`, `watt`, `watts`, `Wb`, `weber`, `week`, `weeks`, `yard`, `yards`, `yd`, `year`, `years`, `°`, `Ω`.

- Explicit unit conversions with the `->` operator:
  ```
  60mph -> m/s
  500km/day -> km/h
  1mrad -> °
  52weeks -> days
  5in + 2ft -> cm
  atan(30cm / 2m) -> °
  6Mbit/s * 1.5h -> GB
  ```

- Variable assigments:
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
  Predefined constants (type `list` to see them all): speed of light (`c`),
  Plancks constant (`hbar`), electron mass (`electronMass`), elementary charge
  (`elementaryCharge`), magnetic constant (`µ0`), electric constant (`eps0`),
  bohr magneton (`µ_B`), Avogadro's constant (`N_A`), Boltzmann constant
  (`k_B`), gravitational acceleration (`g0`), ...


  **Note:** You can use `ans` (answer) to refer the result of the last calculation.

- Commands:
  ```
  help, ?
  list, ls
  reset
  clear, cls
  quit, exit
  ```

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
