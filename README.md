insect
======

A fast, repl-style scientific calculator for the web and for the terminal.

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
  Supported functions: `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `log`.

- Parsing and handling of physical units:
  ```
  2min + 30s
  40kg * 9.8m/s² * 150cm
  sin(30°)
  ```

- Explicit unit conversions
  ```
  60mph -> m/s
  500km/day -> km/h
  1mrad -> °
  52weeks -> days
  5in + 2ft -> cm
  atan(30cm / 2m) -> °
  ```

- Variable assigments:
  ```
  r = 6000km
  vol = 4/3 * pi * r³
  density = 5g/cm³
  vol * density -> kg
  ```
  ```
  grav = 9.81m/s²
  L = 20cm
  2pi*sqrt(L/grav) -> ms
  ```
  Predefined constants: speed of light (`c`), Plancks constant (`hbar`), ...

- Commands:
  ```
  help, ?
  list, ls
  reset
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
pulp -w build --skip-entry-point -m Insect -O -t insect.js
```
