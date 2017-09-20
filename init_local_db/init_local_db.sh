# create db (remove if exist)
dropdb ywp_db || true
createdb ywp_db

# create user (remove if exist)
dropuser ywp_user || true
createuser ywp_user

# create tables and insert data
psql -f init_local_db/create_data.sql ywp_db
