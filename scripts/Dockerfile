FROM ubuntu:19.10

RUN apt-get update && apt-get install -y \
    autoconf \
    automake \
    autotools-dev \
    bc \
    bison \
    build-essential \
    clang \
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
    python \
    python-pip \
    python3 \
    python3-pip \
    texinfo \
    vim \
    wget \
    zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /code

# Install Racket v7.6
RUN wget 'https://mirror.racket-lang.org/installers/7.6/racket-7.6-x86_64-linux.sh' && \
    chmod +x ./racket-7.6-x86_64-linux.sh && \
    echo 'yes\n1' | ./racket-7.6-x86_64-linux.sh && \
	rm -f racket-7.6-x86_64-linux.sh

# Install Unicorn
RUN git clone 'https://github.com/unicorn-engine/unicorn.git' && \
    cd unicorn && \
    env UNICORN_ARCHS="x86,arm,aarch64" ./make.sh && \
    env UNICORN_ARCHS="x86,arm,aarch64" ./make.sh install && \
    cd /code && rm -rfv unicorn

# Install HEAD Rosette
RUN git clone 'https://github.com/emina/rosette.git' && \
    cd rosette && \
    raco pkg install --auto -i --no-setup && raco setup -Dl rosette

# Install Boolector version v3.2.0
RUN git clone 'https://github.com/boolector/boolector.git' && \
    cd boolector && git checkout '365eaab616562eedb1680ebbf8784cf8ecb25d1a' && \
    ./contrib/setup-lingeling.sh && \
    ./contrib/setup-btor2tools.sh && \
    ./configure.sh --prefix /opt/boolector/3.2.0 && cd build && make -j4 && make install && \
    cd /code && rm -rfv ./boolector

# Install Z3 v4.8.7
RUN git clone 'https://github.com/Z3Prover/z3.git' && \
    cd z3 && \
    git checkout 'z3-4.8.7' && \
    python scripts/mk_make.py --prefix '/opt/z3/4.8.7' && \
    cd build && \
    make -j4 && make install && \
    cd .. && rm -rfv z3

ADD . /serval

# RUN raco pkg install /serval/serval

WORKDIR /serval
