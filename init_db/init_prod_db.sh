# create user (skip if exist)
createuser ywp_user || true

# create db (skip if exist)
createdb ywp_db --username=ywp_user || true

# create tables
psql -f init_db/create_shape.sql ywp_db
