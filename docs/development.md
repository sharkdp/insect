Development
-----------

Insect is written in PureScript (see [Getting Started](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) guide). You can install all dependencies and build the whole project by running:

    npm install
    npm start

Open [web/index.html](web/index.html) in your browser.

Insect comes with a comprehensive set of [unit tests](test/Main.purs). You can run them by calling

    npm test

Note that Node 12 and above is required to work on/build Insect (despite Insect
itself requiring only Node 10 or later to run). If you don't have or want to
install Node 12 or later, you can use the following Dockerfile to build or run
Insect on Node 18:

```Dockerfile
FROM node:18

WORKDIR /usr/src/insect

COPY . .

RUN npm install && \
    npm run build

CMD ["node", "index.cjs"]
```

After creating the image (`docker build -t sharkdp/insect .`), you can create
the container and copy out the build artifacts:

    docker create sharkdp/insect:latest
    # copy SHA (e.g. 71f0797703e8)
    docker cp 71f0797703e8:/usr/src/insect/index.cjs .
    docker cp -r 71f0797703e8:/usr/src/insect/node_modules .


To directly run insect inside the Docker (paying a heavy startup time penalty),
you can use

    docker run -it --rm -v ~/.local/share/insect-history:/root/.local/share/insect-history sharkdp/insect:latest

Maintainers
-----------

* [sharkdp](https://github.com/sharkdp)
* [mhmdanas](https://github.com/mhmdanas)
