FROM samdoshi/haskell-stack

WORKDIR /opt/ywp_server

COPY ./stack.yaml /opt/ywp_server/stack.yaml
RUN stack setup

COPY . /opt/ywp_server
RUN stack install
RUN stack build

CMD ["stack", "exec", "you-will-pay"]
EXPOSE 3000
