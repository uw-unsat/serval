FROM ubuntu:19.04

RUN apt-get update && apt-get install -y \
    autoconf \
    automake \
    autotools-dev \
    bc \
    bison \
    build-essential \
    clang \
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

RUN wget 'https://mirror.racket-lang.org/installers/7.5/racket-7.5-x86_64-linux-cs.sh' && \
    chmod +x ./racket-7.5-x86_64-linux-cs.sh && \
    echo 'yes\n1' | ./racket-7.5-x86_64-linux-cs.sh && \
	rm -f racket-7.5-x86_64-linux-cs.sh

# Install Unicorn
RUN git clone 'https://github.com/unicorn-engine/unicorn.git' && \
    cd unicorn && \
    env UNICORN_ARCHS="x86" ./make.sh && \
    env UNICORN_ARCHS="x86" ./make.sh install && \
    cd /code && rm -rfv unicorn

# Install Rosette
RUN git clone 'https://github.com/emina/rosette.git' && \
    cd rosette && \
    raco pkg install --auto -i --no-setup && raco setup -Dl rosette

ADD . /serval

RUN raco pkg install /serval/serval

WORKDIR /serval