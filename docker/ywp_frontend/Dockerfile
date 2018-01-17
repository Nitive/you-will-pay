FROM node:8

WORKDIR /opt/ywp_frontend
RUN npm install -g bower pulp

COPY ./package.json /opt/ywp_frontend/package.json
COPY ./package-lock.json /opt/ywp_frontend/package-lock.json
RUN npm install

COPY ./bower.json /opt/ywp_frontend/bower.json
RUN bower --allow-root install

RUN cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.11.5/linux64.tar.gz \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz

ENV PATH /opt/purescript:$PATH

COPY . /opt/ywp_frontend

RUN mkdir -p /opt/public/assets
RUN pulp browserify --optimise > /opt/public/assets/app.js
RUN gzip --best -c /opt/public/assets/app.js > /opt/public/assets/app.js
COPY resources /opt/public/assets/

WORKDIR /opt/public

CMD ["python","-m","SimpleHTTPServer","1337"]
EXPOSE 1337
