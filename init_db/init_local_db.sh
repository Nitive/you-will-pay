# create user (remove if exist)
dropuser ywp_user || true
createuser ywp_user

# create db (remove if exist)
dropdb ywp_db --username=ywp_user || true
createdb ywp_db --username=ywp_user

# create tables and insert data
psql -f init_db/create_shape.sql ywp_db
psql -f init_db/create_data.sql ywp_db
