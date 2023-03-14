FROM haskell:9-slim-buster as haskell-builder

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
   && apt-get -y install --no-install-recommends \
    # HKNT build requirements
    ocamlbuild \
    ocaml-nox \
   # Clean up
   && apt-get autoremove -y \
   && apt-get clean -y \
   && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /chorgram/aux
WORKDIR /chorgram/aux
RUN curl http://perso.ens-lyon.fr/damien.pous/hknt/hknt-1.0.tar.bz2 --output hknt-1.0.tar.bz2 \
    && echo "b4620745e8e6453811cffb998e9ed78f626c8b5e53a186d35484e613e0877ad6779621555e2e5732d8ea237217acaf6dcba969327da3c47035818795b89b7562  hknt-1.0.tar.bz2" | sha512sum --check \
    && tar xf hknt-1.0.tar.bz2 \
    && rm hknt-1.0.tar.bz2 \
    && cd hknt-1.0 \
    && make && cd /chorgram/aux

RUN curl https://www.lsi.upc.edu/\~jordicf/petrify/distrib/petrify-5.2-linux.tar.gz --output petrify-5.2-linux.tar.gz \
    && echo "157dbad34dccc7ed6bcb4a7e5a5d20ab52ebc49656c53645d18cf01dc1916eaf284cbcd02147500ec15804de08152c7e4f0bef78ed36cb2911c7093bc43c4769  petrify-5.2-linux.tar.gz" | sha512sum --check \
    && tar xf petrify-5.2-linux.tar.gz \
    && rm petrify-5.2-linux.tar.gz

WORKDIR /chorgram

RUN cabal update \
   && cabal install --lib MissingH hxt \
   && cabal install happy

ADD . /chorgram

RUN make setup

ENV PATH="/chorgram:/chorgram/chortest:${PATH}"

FROM python:3.11-slim-buster

RUN apt-get update \
    && apt-get -y install --no-install-recommends \
        gcc \
        gir1.2-gtk-3.0 \
        gobject-introspection \
        libcairo2-dev \
        libgirepository1.0-dev \
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY --from=haskell-builder /chorgram /chorgram
ENV PATH="/chorgram:/chorgram/chortest:${PATH}"