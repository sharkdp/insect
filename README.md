insect
======

A fast, repl-style scientific calculator for the web.

[![insect](media/insect-32x32.png)](https://shark.fish/insect/)

[**Try it out**](https://shark.fish/insect/)

Features
--------
- Evaluation of mathematical expressions:
  ```
  1920/16*9
  2^32
  pi(1.4+2)²
  ```

- Parsing and handling of physical units:
  ```
  4/3 pi (6000km)³
  2min + 30s
  ```

- Explicit unit conversions
  ```
  60mph -> m/s
  500km/day -> km/h
  1mrad -> °
  52weeks -> days
  5in -> cm
  ```

- Mathematical functions:
  ```
  cos(pi/4)
  sin(30°)
  atan(30cm/(2m)) -> °
  ```
  Currently supported: `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `log`.

- Variable assigments:
  ```
  r = 6000km
  vol = 4/3 pi r³
  density = 5g/cm³
  vol * density -> kg
  ```
  ```
  grav = 9.81m/s²
  L = 20cm
  2pi*sqrt(L/grav) -> ms
  ```

- Commands:
  ```
  help, ?
  list, ls
  reset
  ```

Build
-----
```sh
bower install
pulp -w build --skip-entry-point -m Insect -O -t insect.js
```
