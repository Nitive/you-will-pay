FROM node:8

# install purescript
RUN cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.11.7/linux64.tar.gz \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz
ENV PATH /opt/purescript:$PATH
WORKDIR /opt/ywp_frontend

# install node dependencies
COPY /frontend/package.json /opt/ywp_frontend/package.json
COPY /frontend/package-lock.json /opt/ywp_frontend/package-lock.json
RUN npm install

# install bower dependencies
COPY /frontend/bower.json /opt/ywp_frontend/bower.json
RUN npx bower --allow-root install

# compile purescript dependencies
RUN npx pulp build --skip-entry-point

# compile my own purescript code
RUN mkdir -p /opt/public/assets
COPY /frontend/src/ /opt/ywp_frontend/src/
RUN npx pulp browserify --to /opt/public/assets/app.js
RUN gzip --best --keep /opt/public/assets/app.js

# copy resources
COPY /frontend/resources /opt/public/assets/
WORKDIR /opt/public

# run server
CMD ["npx", "http-server",".","--gzip","-p","1337"]
EXPOSE 1337
