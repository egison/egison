FROM phusion/baseimage:latest
MAINTAINER Satoshi Egi

RUN locale-gen en_US.UTF-8  
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8

RUN apt-get update
RUN apt-get -y install libncurses-dev
RUN apt-get -y install haskell-platform
RUN cabal update
RUN cabal install egison
ENV PATH /root/.cabal/bin:$PATH

WORKDIR /docker

ENTRYPOINT ["egison"]
CMD ["--prompt", "> "]
