FROM haskell:8.4.4

RUN apt-get update && apt-get install --assume-yes libpq-dev

RUN ln -sf /usr/share/zoneinfo/Europe/Warsaw /etc/localtime

WORKDIR /opt/app

COPY package.yaml /opt/app
COPY stack.yaml /opt/app
RUN stack install --only-dependencies

COPY . /opt/app
RUN stack install

CMD ["tracnologia-exe"]
