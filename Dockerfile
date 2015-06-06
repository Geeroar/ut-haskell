FROM dockheas23/ut-haskell-base:v1
MAINTAINER George Boyle <george@thebuds.net>
RUN git clone https://github.com/Dockheas23/ut-haskell /opt/ut-haskell
RUN mkdir /opt/ut-haskell/log
RUN touch /opt/ut-haskell/log/{access,error}.log
RUN cd /opt/ut-haskell && cabal install
EXPOSE 8080
CMD cd /opt/ut-haskell && /root/.cabal/bin/ut-haskell -p 8080
