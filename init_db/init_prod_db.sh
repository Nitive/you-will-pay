# create db (skip if exist)
createdb ywp_db || true

# create user (skip if exist)
createuser ywp_user || true

# create tables
psql -f init_local_db/create_shape.sql ywp_db
