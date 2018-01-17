FROM samdoshi/haskell-stack

WORKDIR /opt/ywp_server

COPY /server/stack.yaml /opt/ywp_server/stack.yaml
RUN stack setup

COPY /server/package.yaml /opt/ywp_server/package.yaml
RUN stack build --only-dependencies

COPY /server/src/ /opt/ywp_server/src/
RUN stack build

CMD ["stack","exec","ywp"]
EXPOSE 3000
