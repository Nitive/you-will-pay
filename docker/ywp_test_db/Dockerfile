FROM postgres:9.6

ENV POSTGRES_USER=ywp_user \
    POSTGRES_DB=ywp_db

ADD init_db/create_shape.sql /docker-entrypoint-initdb.d/1.create_shape.sql
ADD init_db/create_data.sql /docker-entrypoint-initdb.d/2.create_data.sql
