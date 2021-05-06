FROM ubuntu:20.10

ARG RACKET_VERSION=8.1
ARG ROSETTE_VERSION=9f6322c9da5761da63126c7ef936bd77c210aeec
ARG Z3_VERSION=4.8.9
ARG BOOLECTOR_VERSION=3.2.1
ARG UNICORN_VERSION=1.0.2-rc3

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    autoconf \
    automake \
    autotools-dev \
    bc \
    bison \
    build-essential \
    clang \
    clang-format \
    cmake \
    curl \
    flex \
    gawk \
    gcc \
    gcc-riscv64-linux-gnu \
    git \
    gperf \
    llvm \
    make \
    patchutils \
    pkg-config \
    python3 \
    python3-pip \
    texinfo \
    vim \
    wget \
    zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /code

# Install Racket
RUN wget "https://mirror.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-cs.sh" && \
    chmod +x ./racket-${RACKET_VERSION}-x86_64-linux-cs.sh && \
    echo 'yes\n1' | ./racket-${RACKET_VERSION}-x86_64-linux-cs.sh && \
	rm -f racket-${RACKET_VERSION}-x86_64-linux-cs.sh

# Install Unicorn
RUN git clone 'https://github.com/unicorn-engine/unicorn.git' && \
    cd unicorn && git checkout ${UNICORN_VERSION} && \
    env UNICORN_ARCHS="x86,arm,aarch64" ./make.sh && \
    env UNICORN_ARCHS="x86,arm,aarch64" ./make.sh install && \
    cd /code && rm -rfv unicorn

# Install Boolector twice with CaDiCal and Lingeling backends
RUN git clone 'https://github.com/boolector/boolector.git' && \
    cd boolector && git checkout ${BOOLECTOR_VERSION} && \
    ./contrib/setup-cadical.sh && \
    ./contrib/setup-btor2tools.sh && \
    ./configure.sh --prefix /opt/boolector/${BOOLECTOR_VERSION}-cadical && cd build && make -j $(nproc) && make install && \
    cd .. && git clean -xffd \
    ./contrib/setup-lingeling.sh && \
    ./contrib/setup-btor2tools.sh && \
    ./configure.sh --prefix /opt/boolector/${BOOLECTOR_VERSION}-lingeling && cd build && make -j $(nproc) && make install && \
    cd ../.. && rm -rfv ./boolector

# Install Z3
RUN git clone 'https://github.com/Z3Prover/z3.git' && \
    cd z3 && git checkout z3-${Z3_VERSION} && \
    python3 scripts/mk_make.py --prefix /opt/z3/${Z3_VERSION} && \
    cd build && \
    make -j $(nproc) && make install && \
    cd .. && rm -rfv z3

# Install Rosette
RUN git clone 'https://github.com/emina/rosette.git' && \
    cd rosette && git checkout ${ROSETTE_VERSION} && \
    raco pkg install --auto -i --no-setup && raco setup -Dl rosette

ADD . /serval

WORKDIR /serval
