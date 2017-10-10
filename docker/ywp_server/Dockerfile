FROM samdoshi/haskell-stack

WORKDIR /opt/ywp_server

COPY ./stack.yaml /opt/ywp_server/stack.yaml
RUN stack setup

COPY ./package.yaml /opt/ywp_server/package.yaml
RUN stack build --fast --only-dependencies

COPY ./src/Server /opt/ywp_server/src/Server/
RUN stack build

CMD ["stack","exec","ywp"]
EXPOSE 3000
