FROM ubuntu
SHELL ["/bin/bash", "-c"]

RUN apt-get -y update

# noninteractive is needed to avoid config of tzlocal package
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install curl xz-utils make git python3 ripgrep sudo python3-sphinx python3-sphinx-rtd-theme texinfo

RUN adduser --disabled-password --gecos '' tester
RUN adduser tester sudo
RUN install -d -m755 -o $(id tester -u) -g $(id tester -g) /nix
USER tester
ENV USER tester
WORKDIR /home/tester

# Install nix
RUN curl -L https://nixos.org/nix/install | sh

# Install cachix
RUN . ~/.nix-profile/etc/profile.d/nix.sh && \
    nix-env -iA cachix -f https://cachix.org/api/v1/install

# Install emacs from nix-emacs-ci
ARG EMACS_VERSION=28-2
RUN . ~/.nix-profile/etc/profile.d/nix.sh && \
    cachix use emacs-ci && \
    nix-env -iA emacs-${EMACS_VERSION} -f https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz && \
    emacs --version

# Install cask
RUN . ~/.nix-profile/etc/profile.d/nix.sh && \
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python3

ENV PATH=/home/tester/.cask/bin:$PATH
ENTRYPOINT ["bash", "-c", "source ~/.nix-profile/etc/profile.d/nix.sh && \"$@\"", "-s"]
