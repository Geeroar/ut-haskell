FROM ubuntu
MAINTAINER George Boyle <george@thebuds.net>
RUN apt-get update && apt-get install -y cabal-install ghc git libghc-zlib-dev
RUN git clone https://github.com/Dockheas23/ut-haskell /opt/ut-haskell
RUN mkdir /opt/ut-haskell/log
RUN touch /opt/ut-haskell/log/{access,error}.log
RUN cabal update
RUN cd /opt/ut-haskell && cabal install
EXPOSE 8080
CMD cd /opt/ut-haskell && /root/.cabal/bin/ut-haskell -p 8080
