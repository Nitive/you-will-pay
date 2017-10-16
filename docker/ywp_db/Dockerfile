FROM postgres:9.6

ENV POSTGRES_USER=ywp_user \
    POSTGRES_DB=ywp_db

ADD init_db/create_shape.sql /docker-entrypoint-initdb.d/
