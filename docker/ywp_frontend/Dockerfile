FROM node:8

WORKDIR /opt/ywp_frontend
RUN npm install -g bower pulp

COPY ./package.json /opt/ywp_frontend/package.json
RUN npm install

COPY ./bower.json /opt/ywp_frontend/bower.json
RUN bower --allow-root install

RUN cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.11.5/linux64.tar.gz \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz

ENV PATH /opt/purescript:$PATH

COPY . /opt/ywp_frontend

CMD ["pulp", "server"]
EXPOSE 1337
